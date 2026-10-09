(ns malli-forms-probe
  "Which malli schema forms validate on this dialect (Basilisp gap hunt)."
  (:require [malli.core :as m]))

(doseq [[label schema value]
        [[:maybe-int [:maybe :int] nil]
         [:maybe-string [:maybe :string] "x"]
         [:sequential [:sequential :string] ["a"]]
         [:set [:set :keyword] #{:a}]
         [:map-of [:map-of :keyword :int] {:a 1}]
         [:or [:or :string [:map [:a :int]]] "x"]
         [:eq [:= :all] :all]
         [:enum [:enum :a :b] :a]
         [:string-min [:string {:min 2}] "ab"]
         [:optional-map [:map [:a {:optional true} [:maybe :int]]] {}]
         [:pos-int pos-int? 3]
         [:any [:maybe :any] nil]]]
  (println label (try (pr-str (m/validate schema value))
                      (catch #?(:clj Exception :cljs :default :lpy python/Exception :default Exception) e
                        (str "ERR " (ex-message e))))))
