(ns hive-providers.host-jvm-test
  "The JVM/Babashka host adapter against real files and threads; HTTP is
   still a stub port, so no network is touched."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-providers.codec.json :as json]
            [hive-providers.host.jvm :as jvm]
            [hive-providers.oauth.authority :as auth]
            [hive-providers.oauth.vendor :as vendor]
            [hive-providers.ports :as ports]
            [hive-providers.result :as r])
  (:import [java.nio.file Files]
           [java.nio.file.attribute PosixFilePermissions]))

(defn- temp-file [name]
  (io/file (str (Files/createTempDirectory "hive-providers" (make-array java.nio.file.attribute.FileAttribute 0)))
           name))

(deftest file-store-round-trip
  (let [f     (temp-file "creds/auth.json")
        store (jvm/file-store {:k (str f)})]
    (is (nil? (ports/load-token store :k)))
    (ports/save-token! store :k {:a 1 :tokens {:x "y"}})
    (is (= {:a 1 :tokens {:x "y"}} (ports/load-token store :k)))
    (testing "owner-only permissions"
      (is (= "rw-------" (PosixFilePermissions/toString (Files/getPosixFilePermissions (.toPath f) (make-array java.nio.file.LinkOption 0))))))))

(deftest entropy-is-bytes
  (let [bs (jvm/random-bytes 32)]
    (is (= 32 (count bs)))
    (is (every? #(<= 0 % 255) bs))
    (is (not= bs (jvm/random-bytes 32)))))

(deftest single-flight-refreshes-once
  (let [f        (temp-file ".credentials.json")
        _        (spit f (json/write-str {:claudeAiOauth {:accessToken "old" :refreshToken "r" :expiresAt 1}}))
        requests (atom 0)
        http     (reify ports/IHttpClient
                   (request! [_ _]
                     (swap! requests inc)
                     (Thread/sleep 50)
                     {:status 200 :body (json/write-str {:access_token "new" :expires_in 3600})}))
        h        (ports/handlers (assoc (jvm/ports {:stores {:claude (str f)}}) :http http))
        client   {:token-url "https://auth.example/token" :client-id "c"}
        program  (auth/access-program client :claude {:decode vendor/claude->token
                                                       :encode vendor/claude-file-encode})
        results  (doall (map deref (for [_ (range 8)]
                                     (future (jvm/single-flight h :claude (str f) program)))))]
    (is (every? r/ok? results))
    (is (= #{"new"} (set (map (comp :access-token r/value) results))))
    (is (= 1 @requests) "eight concurrent callers, one refresh")
    (is (= "new" (get-in (jvm/read-json-file f) [:claudeAiOauth :accessToken])))))

(deftest http-transport-failure-is-a-value
  (let [resp (jvm/http-request! {:method :get :url "http://127.0.0.1:9/" :timeouts {:connect-ms 500 :read-ms 500}})]
    (is (nil? (:status resp)))
    (is (string? (:error resp)))))
