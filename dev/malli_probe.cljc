(ns malli-probe
  "malli alone, per dialect (bin/probe-deps)."
  (:require [malli.core :as m]))

(println "malli-validate" (pr-str (m/validate [:map [:a :string]] {:a "x"})))
