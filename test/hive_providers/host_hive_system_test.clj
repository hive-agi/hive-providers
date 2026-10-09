(ns hive-providers.host-hive-system-test
  "The hive-system adapter: secret refs resolve through the registry port
   (stubbed fetch, never a real pass store), and the single flight holds
   hive-system's real file lock. JVM only: hive-system is provided scope."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as dsl]
            [hive-providers.codec.json :as json]
            [hive-providers.host.hive-system :as hsys]
            [hive-providers.host.jvm :as jvm]
            [hive-providers.oauth.authority :as auth]
            [hive-providers.openai.chat :as chat]
            [hive-providers.openai.registry :as reg]
            [hive-providers.ports :as ports]
            [hive-providers.result :as r]
            [hive-system.secrets.core :as secret])
  (:import [java.nio.file Files]))

(defn- stub-fetch
  "A secret registry that knows one pass entry and records each lookup."
  [seen]
  (fn [ref]
    (swap! seen conj ref)
    (if (= ref {:backend :pass :key "Venice/key"})
      (dsl/ok (secret/make-secret "venice-secret" :pass "Venice/key"))
      (dsl/err :pass/not-found {:key (:key ref)}))))

(deftest secret-refs-resolve-through-hive-system
  (let [seen    (atom [])
        secrets (hsys/hive-system-secrets {:fetch (stub-fetch seen)
                                           :env (ports/->MapEnv {"X_KEY" "from-env"})})]
    (is (= "venice-secret" (ports/secret secrets "pass:Venice/key")))
    (is (= "from-env" (ports/secret secrets "env:X_KEY")))
    (is (nil? (ports/secret secrets "pass:missing")))
    (is (= [{:backend :pass :key "Venice/key"} {:backend :pass :key "missing"}] @seen)
        "env refs never reach the secret registry")))

(deftest chat-reads-the-key-from-the-secret-store-before-the-environment
  (let [seen     (atom [])
        http     (ports/scripted-http [{:status 200 :body (json/write-str {:choices [{:message {:role "assistant" :content "ok"}}]})}])
        handlers (ports/handlers {:http    http
                                  :env     (ports/->MapEnv {"VENICE_API_KEY" "stale-shell-value"})
                                  :secrets (hsys/hive-system-secrets {:fetch (stub-fetch seen)})})
        registry (assoc-in reg/seed [:venice :api-key-ref] "pass:Venice/key")
        res      (chat/chat (chat/provider registry handlers) {:provider :venice :messages []})]
    (is (r/ok? res))
    (is (= "Bearer venice-secret" (get-in (first @(:seen http)) [:headers "Authorization"])))))

(deftest single-flight-under-hive-system-lock
  (let [dir      (str (Files/createTempDirectory "hp-hsys" (make-array java.nio.file.attribute.FileAttribute 0)))
        f        (io/file dir "auth.json")
        _        (jvm/write-json-file! f {:access-token "old" :refresh-token "r" :expires-at-ms 1})
        requests (atom 0)
        http     (reify ports/IHttpClient
                   (request! [_ _]
                     (swap! requests inc)
                     (Thread/sleep 30)
                     {:status 200 :body (json/write-str {:access_token "new" :expires_in 3600})}))
        h        (ports/handlers (assoc (jvm/ports {:stores {:acct (str f)}}) :http http))
        program  (auth/access-program {:token-url "https://auth/token" :client-id "c"} :acct {})
        results  (doall (map deref (for [_ (range 6)]
                                     (future (hsys/single-flight h :acct (str f) program)))))]
    (is (every? r/ok? results))
    (is (= 1 @requests) "six concurrent callers, one refresh")
    (testing "the lock file is hive-system's, beside the credentials"
      (is (.exists (io/file (str f ".lock")))))))
