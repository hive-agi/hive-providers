(ns hive-providers.oauth.authority
  "Token authority: when to reuse, refresh, or give up on a stored token.

   The decision is pure (`decide`); `access-program` wraps it around the
   :store/load and :store/save ops so any harness gets the same policy:

     no stored token                      -> err :oauth/no-credentials
     stored, not near expiry, not forced  -> reuse
     forced, but refreshed within the
       coalesce window                    -> reuse (a herd of 401s after a
                                             rotation must not replay the
                                             rotated refresh token)
     otherwise                            -> refresh, save, return

   Serializing concurrent callers is the host's job: run the program under a
   per-key lock (hive-providers.host.jvm/single-flight does this). Because
   the program re-reads the store first, the second caller through the lock
   sees the token the first one saved.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.effect :as fx]
            [hive-providers.oauth.token :as token]
            [hive-providers.result :as r]
            [hive-dsl.adt :refer [defadt adt-case]]))

(def default-coalesce-ms
  "A refresh within this window satisfies a forced refresh."
  10000)

(defadt Decision
  "What to do with a stored token. Closed: `adt-case` over it is exhaustive."
  :decision/absent
  :decision/reuse
  :decision/refresh)

(defn decide
  "The Decision for the stored TOKEN.
   Options: :now-ms (required), :margin-ms, :force?, :coalesce-ms."
  [stored {:keys [now-ms margin-ms force? coalesce-ms]
           :or   {margin-ms   token/default-margin-ms
                  coalesce-ms default-coalesce-ms}}]
  (let [obtained (:obtained-at-ms stored)
        recent?  (and (number? obtained) (< (- now-ms obtained) coalesce-ms))]
    (->decision
     (cond
       (nil? (:access-token stored))            :decision/absent
       (and force? recent?)                     :decision/reuse
       force?                                   :decision/refresh
       (token/expired? stored now-ms margin-ms) :decision/refresh
       :else                                    :decision/reuse))))

(defn access-program
  "Program yielding Result<token> for the credential stored under STORE-KEY.

   CLIENT is the OAuth client used to refresh. OPTS as for `decide`, plus:
     :refresh-opts  passed to token/refresh-program
     :decode        stored value -> canonical token (default identity)
     :encode        (fn [token stored]) -> value to save (default: token)
   :decode/:encode let a vendor-shaped file (Claude Code's credentials,
   codex's auth.json, see hive-providers.oauth.vendor) stay in its format."
  [client store-key opts]
  (let [decode (or (:decode opts) identity)
        encode (or (:encode opts) (fn [t _] t))]
    (fx/bind
     (fx/perform :store/load {:key store-key})
     (fn [raw]
       (let [stored (when raw (decode raw))]
         (fx/bind
          (fx/perform :clock/now-ms)
          (fn [now]
            (adt-case Decision (decide stored (assoc opts :now-ms now))
              :decision/absent  (fx/done (r/err :oauth/no-credentials {:key store-key}))
              :decision/reuse   (fx/done (r/ok stored))
              :decision/refresh (fx/then-ok
                                 (token/refresh-program client stored (:refresh-opts opts))
                                 (fn [fresh]
                                   (fx/fmap (fx/perform :store/save {:key store-key :value (encode fresh raw)})
                                            (fn [_] (r/ok fresh)))))))))))))

(defn header-program
  "Program yielding Result<{\"Authorization\" \"Bearer ..\"}> for STORE-KEY."
  [client store-key opts]
  (fx/then-ok (access-program client store-key opts)
              (fn [t] (fx/done (r/ok (token/bearer-header t))))))
