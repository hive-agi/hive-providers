(ns hive-providers.result
  "Results are hive-dsl's: {:ok v} or {:error category & data}, built and
   tested by hive-dsl.result (railway-oriented). This namespace re-exports
   the core operations under the names the library uses and adds the few
   combinators hive-dsl does not carry (value, chain, of-nilable,
   unwrap-or). Nothing here throws.

   ClojureElisp cannot compile hive-dsl.result's macros yet, so under :cljel
   the five core operations are defined inline with the same value shape.

   SPDX-License-Identifier: MIT"
  (:require #?@(:cljel [] :default [[hive-dsl.result :as dsl]])))

#?(:cljel
   (do
     (defn ok [v] {:ok v})
     (defn err
       ([category] {:error category})
       ([category data] (merge {:error category} data)))
     (defn ok? [r] (and (map? r) (contains? r :ok)))
     (defn err? [r] (and (map? r) (contains? r :error)))
     (defn bind [r f] (if (ok? r) (f (:ok r)) r))
     (defn fmap [r f] (if (ok? r) (ok (f (:ok r))) r)))
   :default
   (do
     (def ok "A success carrying V (hive-dsl.result/ok)." dsl/ok)
     (def err "A failure of CATEGORY with optional DATA (hive-dsl.result/err)." dsl/err)
     (def ok? "True when R is a success (hive-dsl.result/ok?)." dsl/ok?)
     (def err? "True when R is a failure (hive-dsl.result/err?)." dsl/err?)
     (def bind "F of the value of a success; a failure passes through (hive-dsl.result/bind)." dsl/bind)
     (def fmap "F applied to the value of a success, rewrapped (hive-dsl.result/map-ok)." dsl/map-ok)))

(defn value
  "The value of a success, else nil."
  [r]
  (:ok r))

(defn chain
  "Thread R through Result-returning FS, stopping at the first failure."
  [r & fs]
  (reduce (fn [acc f] (if (ok? acc) (f (:ok acc)) (reduced acc))) r fs))

(defn of-nilable
  "ok V when V is non-nil, else err CATEGORY with DATA."
  ([v category] (of-nilable v category nil))
  ([v category data]
   (if (nil? v) (err category data) (ok v))))

(defn unwrap-or
  "The value of a success, else DEFAULT."
  [r default]
  (if (ok? r) (:ok r) default))
