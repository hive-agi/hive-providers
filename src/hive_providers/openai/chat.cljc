(ns hive-providers.openai.chat
  "Chat completions against any OpenAI-compatible provider, as effect
   programs, and the provider protocol harnesses depend on.

   PARAMS are those of hive-providers.openai.request, plus:

     :provider     keyword; else a `<provider>:` prefix on :model; else
                   registry/default-provider
     :model        model id
     :api-key      explicit key, wins over everything
     :access-token explicit OAuth bearer token (for OAuth-backed entries)
     :oauth        {:client c :store-key k :opts {..}} to obtain the bearer
                   token through hive-providers.oauth.authority
     :on-delta     fn called with each streamed delta (with :stream true)
     :retry        retry policy map (see hive-providers.openai.retry)

   The program yields Result<message> (see hive-providers.openai.response).
   A failure carries :provider, :status, :attempts and a :body excerpt.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.effect :as fx]
            [hive-providers.oauth.authority :as authority]
            [hive-providers.openai.registry :as reg]
            [hive-providers.openai.request :as request]
            [hive-providers.openai.response :as response]
            [hive-providers.openai.retry :as retry]
            [hive-providers.result :as r]))

;; =============================================================================
;; Credentials
;; =============================================================================

(defn credential-program
  "Program yielding Result<key-or-token-or-nil> for ENTRY and PARAMS."
  ([entry params] (credential-program entry params false))
  ([entry params force-refresh?]
   (cond
     (:api-key params)
     (fx/done (r/ok (:api-key params)))

     (and (:access-token params) (not force-refresh?))
     (fx/done (r/ok (:access-token params)))

     (:oauth params)
     (let [{:keys [client store-key opts]} (:oauth params)]
       (fx/then-ok (authority/access-program client store-key
                                             (cond-> (or opts {}) force-refresh? (assoc :force? true)))
                   (fn [t] (fx/done (r/ok (:access-token t))))))

     (reg/oauth? entry)
     (fx/done (r/err :provider/missing-oauth-token
                     {:hint "pass :access-token or :oauth {:client :store-key}"}))

     (:api-key-env entry)
     (fx/fmap (fx/perform :env/get {:name (:api-key-env entry)})
              (fn [v] (reg/choose-api-key entry {:env v})))

     :else
     (fx/done (r/ok nil)))))

;; =============================================================================
;; The request loop
;; =============================================================================

(defn- excerpt [s]
  (let [s (str s)] (if (> (count s) 2000) (subs s 0 2000) s)))

(defn- jitter
  "Deterministic jitter in [0, 1) for ATTEMPT, so a run is reproducible."
  [attempt]
  (/ (mod (* (inc attempt) 37) 100) 100.0))

(defn- with-line-callback
  [req on-delta]
  (if on-delta
    (assoc req :on-line (fn [line]
                          (let [d (response/sse-data line)]
                            (when (map? d) (on-delta (response/chunk->delta d))))))
    req))

(defn- parse
  [resp entry stream?]
  (if stream?
    (if-let [lines (:lines resp)]
      (response/lines->message lines)
      (response/parse-body (:body resp) (:response-extras entry)))
    (response/parse-body (:body resp) (:response-extras entry))))

(defn- send-program
  "Send REQ with retries. Yields [resp attempts]."
  [req policy]
  (letfn [(attempt [n]
            (fx/bind (fx/perform :http/request req)
                     (fn [resp]
                       (let [{:keys [action wait-ms]} (retry/next-step policy resp n (jitter n))]
                         (if (= action :retry)
                           (fx/bind (fx/perform :sleep/ms {:ms wait-ms})
                                    (fn [_] (attempt (inc n))))
                           (fx/done [resp (inc n)]))))))]
    (attempt 0)))

(defn chat-program
  "Program yielding Result<message> for PARAMS against REGISTRY."
  [registry params]
  (let [resolved (reg/resolve-provider registry params)]
    (if (r/err? resolved)
      (fx/done resolved)
      (let [{:keys [provider entry model]} (r/value resolved)
            policy  (merge retry/default-policy (:retry params))
            stream? (boolean (:stream params))
            run     (fn run [cred auth-retried?]
                      (let [req (-> (request/http-request entry model cred params)
                                    (with-line-callback (:on-delta params)))]
                        (fx/bind
                         (send-program req policy)
                         (fn [[resp attempts]]
                           (let [status (:status resp)]
                             (cond
                               (and status (<= 200 status 299))
                               (fx/done (parse resp entry stream?))

                               (and (= 401 status) (:oauth params) (not auth-retried?))
                               (fx/then-ok (credential-program entry params true)
                                           (fn [fresh] (run fresh true)))

                               :else
                               (fx/done (r/err (retry/category resp)
                                               {:provider provider
                                                :status   status
                                                :attempts attempts
                                                :message  (:error resp)
                                                :body     (excerpt (:body resp))}))))))))]
        (fx/then-ok (credential-program entry params)
                    (fn [cred] (run cred false)))))))

;; =============================================================================
;; Provider protocol
;; =============================================================================

(defprotocol IChatProvider
  (complete [this params]
    "Result<message> for PARAMS (OpenAI chat-completions vocabulary)."))

(defrecord OpenAICompatProvider [registry handlers]
  IChatProvider
  (complete [_ params]
    (fx/run-sync handlers (chat-program registry params))))

(defn provider
  "An IChatProvider over REGISTRY (default `registry/seed`) whose I/O runs
   through the synchronous effect HANDLERS (see hive-providers.effect)."
  ([handlers] (provider reg/seed handlers))
  ([registry handlers] (->OpenAICompatProvider registry handlers)))

(defn chat
  "Result<message> for PARAMS from PROVIDER."
  [provider params]
  (complete provider params))

(defn chat-async
  "Run a chat for PARAMS against REGISTRY with asynchronous HANDLERS, calling
   ON-DONE with Result<message>."
  [registry handlers params on-done]
  (fx/run-async handlers (chat-program registry params) on-done))
