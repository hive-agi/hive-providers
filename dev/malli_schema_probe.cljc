(ns malli-schema-probe
  "Each hive-providers schema compiled alone, per dialect."
  (:require [malli.core :as m]
            [hive-providers.schema :as s]))

(doseq [[label schema] [[:token s/Token] [:message s/Message] [:tool-call s/ToolCall]
                        [:entry s/RegistryEntry] [:pkce s/PkceBundle] [:device s/DeviceAuthorization]
                        [:client s/Client] [:chat-params s/ChatParams] [:secret-ref s/SecretRef]
                        [:maybe-map [:map [:x {:optional true} [:maybe :map]]]]
                        [:maybe-seq [:map [:x {:optional true} [:maybe [:sequential :string]]]]]
                        [:nested-map [:map [:f [:map [:n [:maybe :string]] [:a :string]]]]]
                        [:string-props [:map [:v [:string {:min 43 :max 128}]]]]]]
  (println label (try (pr-str (some? (m/schema schema)))
                      (catch #?(:clj Exception :cljs :default :lpy python/Exception :default Exception) e
                        (str "ERR " (ex-message e))))))
