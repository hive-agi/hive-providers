(ns dsl-probe
  "hive-dsl alone, per dialect (bin/probe-deps)."
  (:require [hive-dsl.result :as r]
            [hive-dsl.adt :refer [defadt adt-case]]))

(defadt Grant
  "OAuth grant kinds."
  [:grant/code {:code string?}]
  :grant/refresh)

(println "result-let-ok" (pr-str (r/let-ok [a (r/ok 1) b (r/ok (inc a))] (r/ok (+ a b)))))
(println "adt-case" (pr-str (adt-case Grant (grant :grant/code {:code "c"})
                              :grant/code :code-branch
                              :grant/refresh :refresh-branch)))
