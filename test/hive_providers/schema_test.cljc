(ns hive-providers.schema-test
  "The values real flows produce conform to hive-providers.schema, and a
   value that breaks the contract is caught with a humanized reason."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-providers.codec.json :as json]
            [hive-providers.oauth.device :as device]
            [hive-providers.oauth.pkce :as pkce]
            [hive-providers.oauth.token :as token]
            [hive-providers.openai.registry :as reg]
            [hive-providers.openai.request :as request]
            [hive-providers.openai.response :as response]
            [hive-providers.result :as r]
            [hive-providers.schema :as s]))

(def client {:token-url "https://auth.example/token" :client-id "cid"})

(defn- json-resp [status m] {:status status :headers {} :body (json/write-str m)})

(deftest http-requests-conform
  (is (s/validate s/HttpRequest (token/refresh-grant client "rt")))
  (is (s/validate s/HttpRequest (token/authorization-code-grant client {:code "c" :verifier "v" :redirect-uri "u"})))
  (is (s/validate s/HttpRequest (request/http-request (:groq reg/seed) "m" "k" {:messages [] :stream true})))
  (is (some? (s/explain s/HttpRequest {:method :fetch :url 1}))))

(deftest tokens-and-bundles-conform
  (let [t (r/value (token/parse-response (json-resp 200 {:access_token "a" :expires_in 60 :scope "x y"}) 0 nil))]
    (is (s/validate s/Token t))
    (is (s/validate (s/Result s/Token) {:ok t})))
  (is (s/validate (s/Result s/Token) (token/parse-response (json-resp 400 {:error "invalid_grant"}) 0 nil)))
  (is (s/validate s/PkceBundle (pkce/bundle (vec (range 32)) (vec (range 32 64)))))
  (is (s/validate s/DeviceAuthorization
                  (r/value (device/parse-authorization
                            (json-resp 200 {:device_code "d" :user_code "u" :verification_uri "v"})))))
  (testing "a token without an access token is refused, with a reason"
    (is (not (s/validate s/Token {:refresh-token "r"})))
    (is (contains? (s/explain s/Token {:refresh-token "r"}) :access-token))))

(deftest registry-and-messages-conform
  (is (s/validate s/Registry reg/seed))
  (is (s/validate s/Message (r/value (response/parse-body
                                      (json/write-str {:choices [{:message {:role "assistant" :content "hi"
                                                                            :tool_calls [{:id "1" :function {:name "f" :arguments {}}}]}}]})))))
  (is (s/validate s/Message (r/value (response/lines->message
                                      ["data: {\"choices\":[{\"delta\":{\"content\":\"x\"}}]}" "data: [DONE]"])))))

#?(:cljw nil
   :clj
   (deftest contracts-instrument-on-the-jvm
     ;; JVM and Babashka: malli.instrument pulls test.check, which
     ;; ClojureWasm (it also claims :clj) does not ship.
     (require 'hive-providers.contracts 'malli.instrument)
     (let [instrument!   (resolve 'malli.instrument/instrument!)
           unstrument!   (resolve 'malli.instrument/unstrument!)
           filter-ns     (resolve 'malli.instrument/-filter-ns)]
       (instrument! {:filters [(filter-ns 'hive-providers.oauth.token)]})
       (try
         (is (s/validate s/HttpRequest (token/refresh-grant client "rt")))
         (is (thrown? Exception (token/refresh-grant {:client-id "no token url"} "rt"))
             "an instrumented fn refuses a client that breaks the schema")
         (finally
           (unstrument! {:filters [(filter-ns 'hive-providers.oauth.token)]}))))))
