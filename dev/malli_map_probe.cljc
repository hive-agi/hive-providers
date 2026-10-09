(ns malli-map-probe
  "malli :map schemas by entry count, per dialect (Basilisp eager-entry-parser gap)."
  (:require [malli.core :as m]))

(doseq [n [1 4 7 8 9 12 16]]
  (let [schema (into [:map] (map (fn [i] [(keyword (str "k" i)) {:optional true} :int]) (range n)))]
    (println :entries n
             (try (pr-str (m/validate schema {}))
                  (catch #?(:clj Exception :cljs :default :lpy python/Exception :default Exception) e
                    (str "ERR " (ex-message e)))))))
