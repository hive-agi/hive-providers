(ns hive-providers.schema
  "malli schemas for every value that crosses a hive-providers boundary:
   the HTTP request/response maps the effect handlers exchange, OAuth
   clients and tokens, chat params and messages, registry entries.

   `explain` gives a humanized reason; `contracts!` attaches function schemas
   (`m/=>`) to the public API so `malli.instrument` can check a running
   harness. Schemas are plain data, so a harness can extend or narrow them.

   SPDX-License-Identifier: MIT"
  (:require [malli.core :as m]
            [malli.error :as me]))

;; =============================================================================
;; Results (hive-dsl)
;; =============================================================================

(defn Result
  "Schema of a hive-dsl Result whose success carries OK-SCHEMA."
  [ok-schema]
  [:or
   [:map [:ok ok-schema]]
   [:map [:error :keyword]]])

;; =============================================================================
;; HTTP (the :http/request op)
;; =============================================================================

(def HttpMethod [:enum :get :post :put :patch :delete :head])

(def Headers [:map-of :string :string])

(def HttpRequest
  [:map
   [:method HttpMethod]
   [:url :string]
   [:headers {:optional true} Headers]
   [:body {:optional true} [:maybe :string]]
   [:stream? {:optional true} :boolean]
   [:on-line {:optional true} fn?]
   [:timeouts {:optional true} [:map
                                [:connect-ms {:optional true} pos-int?]
                                [:read-ms {:optional true} pos-int?]]]])

(def HttpResponse
  [:or
   [:map
    [:status :int]
    [:headers {:optional true} [:map-of :string :string]]
    [:body {:optional true} [:maybe :string]]
    [:lines {:optional true} [:sequential :string]]]
   [:map
    [:status :nil]
    [:error [:maybe :string]]]])

;; =============================================================================
;; OAuth
;; =============================================================================

(def Client
  [:map
   [:token-url :string]
   [:client-id :string]
   [:client-secret {:optional true} :string]
   [:auth-method {:optional true} [:enum :none :client-secret-post :client-secret-basic]]
   [:body-encoding {:optional true} [:enum :form :json]]
   [:authorize-url {:optional true} :string]
   [:device-authorization-url {:optional true} :string]
   [:scopes {:optional true} [:sequential :string]]
   [:headers {:optional true} Headers]])

(def Token
  [:map
   [:access-token :string]
   [:refresh-token {:optional true} [:maybe :string]]
   [:expires-at-ms {:optional true} [:maybe :int]]
   [:token-type {:optional true} [:maybe :string]]
   [:scopes {:optional true} [:maybe [:sequential :string]]]
   [:id-token {:optional true} [:maybe :string]]
   [:obtained-at-ms {:optional true} :int]
   [:extra {:optional true} [:maybe :map]]])

(def PkceBundle
  [:map
   [:verifier [:string {:min 43 :max 128}]]
   [:challenge [:string {:min 43 :max 43}]]
   [:state :string]])

(def DeviceAuthorization
  [:map
   [:device-code :string]
   [:user-code :string]
   [:verification-uri [:maybe :string]]
   [:verification-uri-complete {:optional true} [:maybe :string]]
   [:expires-in-s pos-int?]
   [:interval-s pos-int?]])

;; =============================================================================
;; OpenAI-compatible chat
;; =============================================================================

(def ToolCall
  [:map
   [:id [:maybe :string]]
   [:type :string]
   [:function [:map [:name [:maybe :string]] [:arguments :string]]]])

(def Message
  [:map
   [:role :string]
   [:content {:optional true} [:maybe :any]]
   [:tool_calls {:optional true} [:sequential ToolCall]]
   [:finish_reason {:optional true} [:maybe :string]]
   [:usage {:optional true} [:maybe :map]]
   [:model {:optional true} [:maybe :string]]
   [:reasoning {:optional true} [:maybe :string]]
   [:reasoning_content {:optional true} [:maybe :string]]])

(def SecretRef
  "A secret reference: \"backend:key\" (\"env:X\", \"pass:Venice/key\") or a map."
  [:or :string [:map [:backend :keyword] [:key :string]]])

(def RegistryEntry
  [:map
   [:api-url :string]
   [:api-key-env {:optional true} [:maybe :string]]
   [:api-key-ref {:optional true} SecretRef]
   [:default-model {:optional true} [:maybe :string]]
   [:auth {:optional true} [:enum :oauth]]
   [:auth-header-name {:optional true} :string]
   [:auth-scheme {:optional true} [:enum :bearer :raw :api-key]]
   [:url-template {:optional true} [:map
                                    [:path-extra {:optional true} [:maybe :string]]
                                    [:vars {:optional true} [:set :keyword]]]]
   [:field-allowlist {:optional true} [:set :string]]
   [:tool-incompatible-models {:optional true} [:or [:= :all] [:set :string]]]
   [:extra-headers {:optional true} Headers]
   [:response-extras {:optional true} :keyword]
   [:reasoning-style {:optional true} [:enum :openrouter :effort-field]]
   [:reasoning-fields {:optional true} [:set :keyword]]])

(def Registry [:map-of :keyword RegistryEntry])

(def ChatParams
  [:map
   [:messages [:sequential [:map [:role :string]]]]
   [:model {:optional true} [:maybe :string]]
   [:provider {:optional true} [:or :keyword :string]]
   [:api-key {:optional true} :string]
   [:access-token {:optional true} :string]
   [:stream {:optional true} :boolean]
   [:tools {:optional true} [:sequential :map]]
   [:reasoning {:optional true} [:map [:effort [:enum :low :medium :high]]]]
   [:on-delta {:optional true} fn?]])

;; =============================================================================
;; Helpers
;; =============================================================================

(defn validate
  "True when VALUE conforms to SCHEMA."
  [schema value]
  (m/validate schema value))

(defn explain
  "Humanized reason VALUE does not conform to SCHEMA, or nil when it does."
  [schema value]
  (some-> (m/explain schema value) me/humanize))
