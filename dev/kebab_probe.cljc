(ns kebab-probe
  "hive-dsl.adt constructor names per dialect (camel->kebab through defadt)."
  (:require [hive-dsl.adt :refer [defadt]]))

(defadt PollOutcome "p" :poll/a)
(defadt HTTPRequestKind "h" :h/a)
(defadt Http2Thing "t" :t/a)
(defadt Step "s" :s/a)

(println :kebab (pr-str [(:adt/type (poll-outcome :poll/a {}))
                         (:adt/type (http-request-kind :h/a {}))
                         (:adt/type (http2-thing :t/a {}))
                         (:adt/type (step :s/a {}))]))
