(ns hive-providers.oauth.device
  "Device authorization grant (RFC 8628), sans I/O.

   The flow a headless harness uses when it cannot open a browser: ask the
   server for a user code, show it, then poll the token endpoint until the
   user approves, declines, or the code expires.

   The CLIENT map is the one hive-providers.oauth.token takes, plus
   :device-authorization-url.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.json :as json]
            [hive-providers.effect :as fx]
            [hive-providers.oauth.token :as token]
            [hive-providers.result :as r]
            [hive-dsl.adt :refer [defadt adt-case]]))

(defn authorization-request
  "Request for a device code and user code (RFC 8628 3.1)."
  ([client] (authorization-request client nil))
  ([{:keys [device-authorization-url] :as client} {:keys [scopes extra]}]
   (assoc (token/token-request client (concat [[:scope (when (seq scopes) (str/join " " scopes))]]
                                              extra))
          :url device-authorization-url)))

(defn parse-authorization
  "Result for the device authorization RESP (RFC 8628 3.2):
   {:device-code :user-code :verification-uri :verification-uri-complete
    :expires-in-s :interval-s}."
  [{:keys [status body error]}]
  (cond
    (nil? status) (r/err :oauth/network-failed {:message error})
    (not (<= 200 status 299))
    (let [parsed (json/read-str! body)]
      (if (and (map? parsed) (:error parsed))
        (r/err (keyword "oauth" (str (:error parsed)))
               {:status status :description (:error_description parsed)})
        (r/err :oauth/http-failed {:status status :body body})))
    :else
    (r/bind (json/read-str body)
            (fn [d]
              (if (and (string? (:device_code d)) (string? (:user_code d)))
                (r/ok {:device-code               (:device_code d)
                       :user-code                 (:user_code d)
                       :verification-uri          (or (:verification_uri d) (:verification_url d))
                       :verification-uri-complete (:verification_uri_complete d)
                       :expires-in-s              (or (:expires_in d) 900)
                       :interval-s                (or (:interval d) 5)})
                (r/err :oauth/bad-response {:message "no device_code or user_code"}))))))

(defadt PollOutcome
  "One token-endpoint answer while polling a device authorization."
  [:poll/approved {:token map?}]
  :poll/pending
  :poll/slow-down
  [:poll/failed {:error map?}])

(defn classify-poll
  "The PollOutcome of one token-endpoint RESP while polling (RFC 8628 3.5)."
  [resp now-ms]
  (let [res (token/parse-response resp now-ms nil)]
    (cond
      (r/ok? res)                                   (poll-outcome :poll/approved {:token (r/value res)})
      (= :oauth/authorization_pending (:error res)) (->poll-outcome :poll/pending)
      (= :oauth/slow_down (:error res))             (->poll-outcome :poll/slow-down)
      :else                                         (poll-outcome :poll/failed {:error res}))))

(defn poll-program
  "Program polling for the token of DEVICE-AUTH (from `parse-authorization`)
   until it is approved, refused, or expires. Yields Result<token>.
   Sleeps through the :sleep/ms op; `slow_down` adds five seconds to the
   interval as the RFC requires."
  [client {:keys [device-code expires-in-s interval-s]}]
  (fx/bind
   (fx/perform :clock/now-ms)
   (fn [start]
     (let [deadline (+ start (* 1000 expires-in-s))]
       (letfn [(attempt [interval]
                 (fx/bind
                  (fx/perform :sleep/ms {:ms (* 1000 interval)})
                  (fn [_]
                    (fx/bind
                     (fx/perform :http/request (token/device-code-grant client device-code))
                     (fn [resp]
                       (fx/bind
                        (fx/perform :clock/now-ms)
                        (fn [now]
                          (let [outcome (classify-poll resp now)
                                waited  (fn [next-interval]
                                          (if (>= now deadline)
                                            (fx/done (r/err :oauth/expired_token))
                                            (attempt next-interval)))]
                            (adt-case PollOutcome outcome
                              :poll/approved  (fx/done (r/ok (:token outcome)))
                              :poll/failed    (fx/done (:error outcome))
                              :poll/slow-down (waited (+ interval 5))
                              :poll/pending   (waited interval))))))))))]
         (attempt interval-s))))))

(defn flow-program
  "Program running the whole device flow: request codes, hand them to
   ON-CODES (a fn of the authorization map, for the harness to display),
   then poll. Yields Result<token>."
  [client opts on-codes]
  (fx/bind (fx/perform :http/request (authorization-request client opts))
           (fn [resp]
             (let [auth (parse-authorization resp)]
               (if (r/err? auth)
                 (fx/done auth)
                 (do (on-codes (r/value auth))
                     (poll-program client (r/value auth))))))))
