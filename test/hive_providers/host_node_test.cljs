(ns hive-providers.host-node-test
  "The Node host adapter: real files and CSPRNG; HTTP through a local server
   so fetch and the streaming reader are exercised without the network."
  (:require [cljs.test :refer [deftest is async]]
            [hive-providers.effect :as fx]
            [hive-providers.host.node :as node]
            [hive-providers.openai.chat :as chat]
            [hive-providers.openai.registry :as reg]
            [hive-providers.result :as r]))

(def ^:private os (js/require "os"))
(def ^:private path (js/require "path"))
(def ^:private http (js/require "http"))

(deftest file-store-and-entropy
  (let [f (.join path (.tmpdir os) (str "hive-providers-" (js/Date.now)) "auth.json")]
    (is (nil? (node/read-json-file f)))
    (node/write-json-file! f {:a 1})
    (is (= {:a 1} (node/read-json-file f))))
  (is (= 16 (count (node/random-bytes 16)))))

(deftest streamed-chat-through-fetch
  (async done
    (let [server (.createServer http
                   (fn [_req ^js res]
                     (.writeHead res 200 #js {"content-type" "text/event-stream"})
                     (.write res "data: {\"choices\":[{\"delta\":{\"content\":\"he\"}}]}\n\n")
                     (.write res "data: {\"choices\":[{\"delta\":{\"content\":\"llo\"},\"finish_reason\":\"stop\"}]}\n\n")
                     (.end res "data: [DONE]\n\n")))]
      (.listen server 0 "127.0.0.1"
               (fn []
                 (let [port     (.-port (.address server))
                       registry (assoc reg/seed :local {:api-url (str "http://127.0.0.1:" port "/v1/chat/completions")
                                                        :api-key-env nil :default-model "m"})
                       deltas   (atom [])]
                   (chat/chat-async registry (node/handlers)
                                    {:provider :local :messages [{:role "user" :content "hi"}]
                                     :stream true :on-delta #(swap! deltas conj (:content %))}
                                    (fn [res]
                                      (is (= "hello" (:content (r/value res))))
                                      (is (= ["he" "llo"] @deltas))
                                      (.close server)
                                      (done)))))))))
