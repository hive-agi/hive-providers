(ns hive-providers.openai.retry
  "Classifying HTTP failures and deciding when to retry. Pure: the random
   jitter is an input, so a schedule is reproducible.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.text :as text]
            [hive-dsl.adt :refer [defadt]]))

(def retryable-statuses
  "Transient statuses: rate limit, overload, and the credit errors gateways
   return while a balance tops up."
  #{402 408 409 425 429 500 502 503 504 529})

(def default-policy
  {:max-retries 5 :initial-ms 1000 :max-ms 60000})

(defn category
  "Result category keyword for a failed RESP (HTTP status, or none)."
  [{:keys [status]}]
  (cond
    (nil? status)            :llm/network
    (= status 401)           :llm/unauthorized
    (= status 403)           :llm/forbidden
    (= status 404)           :llm/not-found
    (= status 429)           :llm/rate-limited
    (= status 402)           :llm/payment-required
    (contains? #{529 503} status) :llm/overloaded
    (<= 400 status 499)      :llm/bad-request
    :else                    :llm/server-error))

(defn retryable?
  "True when RESP is worth retrying: no response at all, or a transient
   status."
  [{:keys [status]}]
  (or (nil? status) (contains? retryable-statuses status)))

(defn retry-after-ms
  "Milliseconds the server asked to wait (Retry-After in seconds), or nil."
  [{:keys [headers]}]
  (let [v (some-> (or (get headers "retry-after") (get headers "Retry-After")) str str/trim)]
    (when (text/digits? v)
      (* 1000 (parse-long v)))))

(defn backoff-ms
  "Delay before retry ATTEMPT (0-based): exponential from :initial-ms, capped
   at :max-ms, plus up to a quarter of jitter scaled by JITTER in [0, 1)."
  ([attempt jitter] (backoff-ms default-policy attempt jitter))
  ([{:keys [initial-ms max-ms]} attempt jitter]
   (let [base (min max-ms (* initial-ms (reduce * 1 (repeat attempt 2))))]
     (long (+ base (* jitter (quot base 4)))))))

(defadt Step
  "What follows one HTTP attempt."
  :step/done
  [:step/retry {:wait-ms number?}]
  [:step/fail {:category keyword?}])

(defn next-step
  "The Step after attempt ATTEMPT (0-based) produced RESP: done on a 2xx,
   retry after :wait-ms, or fail with a :category."
  ([resp attempt jitter] (next-step default-policy resp attempt jitter))
  ([{:keys [max-retries] :as policy} resp attempt jitter]
   (let [status (:status resp)]
     (cond
       (and status (<= 200 status 299))                 (->step :step/done)
       (and (retryable? resp) (< attempt max-retries))  (step :step/retry
                                                              {:wait-ms (or (retry-after-ms resp)
                                                                            (backoff-ms policy attempt jitter))})
       :else                                            (step :step/fail {:category (category resp)})))))
