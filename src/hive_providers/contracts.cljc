(ns hive-providers.contracts
  "Function schemas for the public API, stated once and registered under
   each function's own namespace (what `m/=>` would do in place). Inert data
   until a harness instruments them:

     (require 'hive-providers.contracts '[malli.instrument :as mi])
     (mi/instrument! {:filters [(mi/-filter-ns 'hive-providers.oauth.token)]})

   Kept apart from the implementation namespaces so those load where malli
   does not run yet (clojurust).

   SPDX-License-Identifier: MIT"
  (:require [malli.core :as m]
            [hive-providers.oauth.device]
            [hive-providers.oauth.pkce]
            [hive-providers.oauth.token]
            [hive-providers.openai.registry]
            [hive-providers.openai.request]
            [hive-providers.openai.response]
            [hive-providers.schema :as s]))

(def function-schemas
  "Qualified fn symbol -> malli function schema."
  {'hive-providers.oauth.pkce/bundle
   [:=> [:cat [:sequential :int] [:sequential :int]] s/PkceBundle]

   'hive-providers.oauth.pkce/challenge
   [:=> [:cat :string] :string]

   'hive-providers.oauth.token/token-request
   [:=> [:cat s/Client :any] s/HttpRequest]

   'hive-providers.oauth.token/refresh-grant
   [:function
    [:=> [:cat s/Client :string] s/HttpRequest]
    [:=> [:cat s/Client :string [:maybe :map]] s/HttpRequest]]

   'hive-providers.oauth.token/parse-response
   [:=> [:cat s/HttpResponse :int [:maybe :map]] (s/Result s/Token)]

   'hive-providers.oauth.device/parse-authorization
   [:=> [:cat s/HttpResponse] (s/Result s/DeviceAuthorization)]

   'hive-providers.openai.registry/resolve-provider
   [:=> [:cat s/Registry :map]
    (s/Result [:map [:provider :keyword] [:entry s/RegistryEntry] [:model [:maybe :string]]])]

   'hive-providers.openai.registry/api-url
   [:=> [:cat s/RegistryEntry :map] :string]

   'hive-providers.openai.request/http-request
   [:=> [:cat s/RegistryEntry :string [:maybe :string] s/ChatParams] s/HttpRequest]

   'hive-providers.openai.response/parse-body
   [:function
    [:=> [:cat :string] (s/Result s/Message)]
    [:=> [:cat :string [:maybe :keyword]] (s/Result s/Message)]]

   'hive-providers.openai.response/lines->message
   [:=> [:cat [:sequential :string]] (s/Result s/Message)]})

(defn register!
  "Register every function schema with malli. Idempotent."
  []
  (doseq [[qsym schema] function-schemas]
    (m/-register-function-schema! (symbol (namespace qsym)) (symbol (name qsym)) schema {}))
  (count function-schemas))

(register!)
