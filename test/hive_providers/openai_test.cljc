(ns hive-providers.openai-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-providers.codec.json :as json]
            [hive-providers.oauth.authority]
            [hive-providers.openai.chat :as chat]
            [hive-providers.openai.registry :as reg]
            [hive-providers.openai.request :as request]
            [hive-providers.openai.response :as response]
            [hive-providers.openai.retry :as retry]
            [hive-providers.ports :as ports]
            [hive-providers.result :as r]))

(defn- json-resp [status m] {:status status :headers {} :body (json/write-str m)})

(def ok-body
  {:model "m1"
   :choices [{:message {:role "assistant" :content "hi"
                        :reasoning_content "think"
                        :tool_calls [{:id "c1" :type "function"
                                      :function {:name "f" :arguments {:x 1}}}]}
              :finish_reason "tool_calls"}]
   :usage {:prompt_tokens 1 :completion_tokens 2 :total_tokens 3}})

(deftest registry-resolution
  (is (= {:provider :groq :model "llama"} (reg/parse-model-prefix reg/seed "groq:llama")))
  (is (= {:provider nil :model "a:b"} (reg/parse-model-prefix reg/seed "a:b")))
  (let [{:keys [provider model]} (r/value (reg/resolve-provider reg/seed {:model "deepseek:deepseek-chat"}))]
    (is (= [:deepseek "deepseek-chat"] [provider model])))
  (is (= "deepseek/deepseek-chat" (:model (r/value (reg/resolve-provider reg/seed {})))))
  (is (= :provider/unknown (:error (reg/resolve-provider reg/seed {:provider :nope}))))
  (let [r2 (reg/overlay reg/seed {:axon {:api-url "https://axon/v1/chat/completions" :secret-key :axon-api-key}
                                  :groq nil
                                  :venice {:default-model "x"}
                                  :claude {:dispatch :anthropic}})]
    (is (= "AXON_API_KEY" (get-in r2 [:axon :api-key-env])))
    (is (not (contains? r2 :groq)))
    (is (= "x" (get-in r2 [:venice :default-model])))
    (is (= "VENICE_API_KEY" (get-in r2 [:venice :api-key-env])))
    (is (not (contains? r2 :claude))))
  (is (= "https://res.openai.azure.com/openai/deployments/dep/chat/completions?api-version=2024-10-21"
         (reg/api-url (:azure-openai reg/seed) {:resource "res" :deployment "dep"})))
  (is (= {:ok "k"} (reg/choose-api-key (:groq reg/seed) {:explicit " " :stored "k" :env "e"})))
  (is (= {:ok nil} (reg/choose-api-key (:ollama-compat reg/seed) {})))
  (is (= :provider/missing-api-key (:error (reg/choose-api-key (:groq reg/seed) {}))))
  (is (= #{:ollama-compat :kimi :groq}
         (set (reg/available (select-keys reg/seed [:ollama-compat :kimi :groq :openai]) ["GROQ_API_KEY"])))))

(deftest request-building
  (let [entry (:mistral reg/seed)
        body  (request/body entry "m" {:messages [{:role "user" :content "x"}]
                                       :tools [{:type "function"}] :temperature 0.5
                                       :reasoning {:effort :high} :body-extra {:junk 1}})]
    (is (= #{:model :messages :tools :temperature} (set (keys body))) "allowlist strips junk"))
  (is (not (contains? (request/body (:perplexity reg/seed) "sonar" {:messages [] :tools [{:type "function"}]}) :tools)))
  (is (= {:effort "high"} (:reasoning (request/body (:openrouter reg/seed) "m" {:messages [] :reasoning {:effort :high}}))))
  (is (= "low" (:reasoning_effort (request/body (:moonshot reg/seed) "m" {:messages [] :reasoning {:effort :low}}))))
  (testing "reasoning keys on assistant messages follow the provider"
    (let [msg {:role "assistant" :content "a" :reasoning "r" :reasoning_details [1]}]
      (is (= {:role "assistant" :content "a" :reasoning_content "r"}
             (request/outbound-message (:moonshot reg/seed) msg)))
      (is (= {:role "assistant" :content "a"} (request/outbound-message (:groq reg/seed) msg)))))
  (is (= {"Content-Type" "application/json" "api-key" "K"}
         (request/headers (:azure-openai reg/seed) "K" nil)))
  (is (= "Bearer K" (get (request/headers (:groq reg/seed) "K" {"X" "1"}) "Authorization")))
  (testing "assertions use (= true ..): clojurust's `is` mis-evaluates (true? ..)"
    (let [req (request/http-request (:groq reg/seed) "llama" "K" {:messages [] :stream true})]
      (is (= true (:stream? req)))
      (is (= true (:stream (json/read-str! (:body req))))))))

(deftest response-parsing
  (let [m (r/value (response/parse-body (json/write-str ok-body)))]
    (is (= "hi" (:content m)))
    (is (= "think" (:reasoning m)))
    (is (= "{\"x\":1}" (get-in m [:tool_calls 0 :function :arguments])))
    (is (= "tool_calls" (:finish_reason m)))
    (is (= 3 (get-in m [:usage :total_tokens]))))
  (is (= :llm/empty-choices (:error (response/parse-body "{\"choices\":[]}"))))
  (is (= :llm/api-error (:error (response/parse-body "{\"error\":{\"message\":\"x\"}}"))))
  (is (= ["c1"] (get-in (r/value (response/parse-body (json/write-str (assoc ok-body :citations ["c1"])) :perplexity))
                        [:extras :citations]))))

(deftest sse-streaming
  (let [lines ["data: {\"model\":\"m\",\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"He\"}}]}"
               ""
               ": keepalive"
               "data: {\"choices\":[{\"delta\":{\"content\":\"llo\",\"reasoning_content\":\"r\"}}]}"
               "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"t1\",\"function\":{\"name\":\"f\",\"arguments\":\"{\\\"a\\\"\"}}]}}]}"
               "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\":1}\"}}]}}]}"
               "data: {\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}],\"usage\":{\"total_tokens\":9}}"
               "data: [DONE]"]
        m (r/value (response/lines->message lines))]
    (is (= "Hello" (:content m)))
    (is (= "r" (:reasoning m)))
    (is (= [{:id "t1" :type "function" :function {:name "f" :arguments "{\"a\":1}"}}] (:tool_calls m)))
    (is (= "tool_calls" (:finish_reason m)))
    (is (= 9 (get-in m [:usage :total_tokens]))))
  (let [[s1 l1] (response/sse-feed {} "data: a\nda")
        [s2 l2] (response/sse-feed s1 "ta: b\r\n\n")]
    (is (= ["data: a"] l1))
    (is (= ["data: b" ""] l2))
    (is (= "" (:buffer s2)))))

(defn- step-of [s] (dissoc s :adt/type))

(deftest retry-policy
  (is (retry/step? (retry/next-step {:status 200} 0 0)))
  (is (= {:adt/variant :step/done} (step-of (retry/next-step {:status 200} 0 0))))
  (is (= {:adt/variant :step/retry :wait-ms 3000}
         (step-of (retry/next-step {:status 429 :headers {"retry-after" "3"}} 0 0))))
  (is (= {:adt/variant :step/retry :wait-ms 2000} (step-of (retry/next-step {:status 503} 1 0))))
  (is (= {:adt/variant :step/fail :category :llm/unauthorized} (step-of (retry/next-step {:status 401} 0 0))))
  (is (= {:adt/variant :step/fail :category :llm/rate-limited} (step-of (retry/next-step {:status 429} 5 0))))
  (is (= :llm/network (retry/category {:status nil})))
  (is (= 60000 (retry/backoff-ms 10 0))))

(defrecord StreamingHttp [lines]
  ports/IHttpClient
  (request! [_ req]
    (doseq [l lines] ((:on-line req) l))
    {:status 200 :headers {} :lines lines}))

(deftest chat-end-to-end-with-stub-ports
  (testing "key from env, retry on 429, parse"
    (let [http  (ports/scripted-http [{:status 429 :headers {}} (json-resp 200 ok-body)])
          slept (atom [])
          p     (chat/provider (ports/handlers {:http http
                                                :env (ports/->MapEnv {"GROQ_API_KEY" "gk"})
                                                :sleeper (ports/->NoSleep slept)}))
          res   (chat/chat p {:model "groq:llama" :messages [{:role "user" :content "x"}]})
          req   (first @(:seen http))]
      (is (= "hi" (:content (r/value res))))
      (is (= 2 (count @(:seen http))))
      (is (= 1 (count @slept)))
      (is (= "https://api.groq.com/openai/v1/chat/completions" (:url req)))
      (is (= "Bearer gk" (get-in req [:headers "Authorization"])))
      (is (= "llama" (:model (json/read-str! (:body req)))))))
  (testing "missing key fails before any request"
    (let [http (ports/scripted-http [])
          res  (chat/chat (chat/provider (ports/handlers {:http http :env (ports/->MapEnv {})}))
                          {:provider :groq :messages []})]
      (is (= :provider/missing-api-key (:error res)))
      (is (empty? @(:seen http)))))
  (testing "non-retryable status becomes a typed failure"
    (let [http (ports/scripted-http [{:status 400 :body "bad"}])
          res  (chat/chat (chat/provider (ports/handlers {:http http}))
                          {:provider :groq :api-key "k" :messages []})]
      (is (= :llm/bad-request (:error res)))
      (is (= 400 (:status res)))
      (is (= "bad" (:body res)))))
  (testing "OAuth-backed entry: token from the authority, 401 forces one refresh"
    (let [client {:token-url "https://auth/token" :client-id "c"}
          store  (ports/atom-store {:kimi {:access-token "old" :refresh-token "r"}})
          http   (ports/scripted-http [{:status 401 :body ""}
                                       (json-resp 200 {:access_token "new" :expires_in 60})
                                       (json-resp 200 ok-body)])
          res    (chat/chat (chat/provider (ports/handlers {:http http :store store
                                                            :clock (ports/->FixedClock 0)}))
                            {:provider :kimi :messages [] :oauth {:client client :store-key :kimi}})
          seen   @(:seen http)]
      (is (r/ok? res))
      (is (= "Bearer old" (get-in (nth seen 0) [:headers "Authorization"])))
      (is (= "https://auth/token" (:url (nth seen 1))))
      (is (= "Bearer new" (get-in (nth seen 2) [:headers "Authorization"])))))
  (testing "streaming collects lines and reports deltas"
    (let [lines  ["data: {\"choices\":[{\"delta\":{\"content\":\"a\"}}]}"
                  "data: {\"choices\":[{\"delta\":{\"content\":\"b\"},\"finish_reason\":\"stop\"}]}"
                  "data: [DONE]"]
          deltas (atom [])
          res    (chat/chat (chat/provider (ports/handlers {:http (->StreamingHttp lines)}))
                            {:provider :ollama-compat :messages [] :stream true
                             :on-delta #(swap! deltas conj %)})]
      (is (= "ab" (:content (r/value res))))
      (is (= ["a" "b"] (mapv :content @deltas))))))
