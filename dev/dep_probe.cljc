(ns dep-probe
  "Which candidate dependencies load and work on which dialect.
   Run from the repo root with each runtime's classpath pointing at the
   dependency sources (see bin/probe-deps)."
  (:require [hive-dsl.result :as r]
            [hive-dsl.adt :refer [defadt adt-case]]
            [malli.core :as m]))

(defadt Grant
  "OAuth grant kinds."
  [:grant/code {:code string?}]
  :grant/refresh)

(defn- step [label f]
  (try (println label (pr-str (f)))
       (catch #?(:clj Throwable :cljs :default :default Exception) e
         (println label "ERR" (pr-str (ex-message e))))))

(step "result-let-ok" (fn [] (r/let-ok [a (r/ok 1) b (r/ok (inc a))] (r/ok (+ a b)))))
(step "result-err" (fn [] (r/bind (r/err :x/y {:n 1}) (fn [_] (r/ok 2)))))
(step "adt-ctor" (fn [] (grant :grant/code {:code "c"})))
(step "adt-case" (fn [] (adt-case Grant (grant :grant/code {:code "c"})
                          :grant/code :code-branch
                          :grant/refresh :refresh-branch)))
(step "malli-validate" (fn [] (m/validate [:map [:access-token :string] [:expires-at-ms {:optional true} [:maybe :int]]]
                                          {:access-token "a"})))
(step "malli-explain" (fn [] (some? (m/explain [:map [:a :int]] {:a "x"}))))
