(ns hive-providers.result
  "Result values: {:ok v} or {:error category & data}.

   The same shape as hive-dsl.result, so a caller that already speaks
   hive-dsl can pass these values through untouched. Nothing here throws.

   SPDX-License-Identifier: MIT")

(defn ok
  "A success carrying V."
  [v]
  {:ok v})

(defn err
  "A failure of CATEGORY (a keyword), with optional DATA merged in."
  ([category] {:error category})
  ([category data] (merge {:error category} data)))

(defn ok?
  "True when R is a success."
  [r]
  (and (map? r) (contains? r :ok)))

(defn err?
  "True when R is a failure."
  [r]
  (and (map? r) (contains? r :error)))

(defn value
  "The value of a success, else nil."
  [r]
  (:ok r))

(defn bind
  "F applied to the value of a success (F returns a Result); a failure
   passes through unchanged."
  [r f]
  (if (ok? r) (f (:ok r)) r))

(defn fmap
  "F applied to the value of a success, rewrapped."
  [r f]
  (if (ok? r) (ok (f (:ok r))) r))

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
