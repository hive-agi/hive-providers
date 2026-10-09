(ns hive-providers.test-runner
  "One entry point for the suite on every dialect. Exits non-zero on failure
   where the host has a process exit. Host-adapter tests join on their own
   host: JVM/Babashka (not ClojureWasm, which also claims :clj) and Node."
  (:require [clojure.test :as t]
            [hive-providers.core-test]
            [hive-providers.oauth-test]
            [hive-providers.openai-test]
            #?@(:cljw [] :clj [[hive-providers.host-jvm-test]] :cljs [[hive-providers.host-node-test]])))

(def test-namespaces
  (into '[hive-providers.core-test
          hive-providers.oauth-test
          hive-providers.openai-test]
        #?(:cljw [] :clj '[hive-providers.host-jvm-test] :default [])))

#?(:cljs
   (defmethod t/report [:cljs.test/default :end-run-tests] [{:keys [fail error] :as m}]
     (println "SUMMARY" (pr-str (select-keys m [:test :pass :fail :error])))
     (when (pos? (+ fail error))
       (set! (.-exitCode js/process) 1))))

(defn- exit-failed! []
  #?(:clj (System/exit 1) :default nil))

(defn -main [& _]
  #?(:cljs
     (t/run-tests 'hive-providers.core-test
                  'hive-providers.oauth-test
                  'hive-providers.openai-test
                  'hive-providers.host-node-test)
     :default
     (let [{:keys [fail error] :as summary} (apply t/run-tests test-namespaces)
           failed (+ (or fail 0) (or error 0))]
       (println "SUMMARY" (pr-str (select-keys summary [:test :pass :fail :error])))
       (when (pos? failed) (exit-failed!))
       failed)))
