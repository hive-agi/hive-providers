(ns hive-providers.openai.request
  "Chat-completions request building for OpenAI-compatible providers.

   Input PARAMS use the OpenAI wire vocabulary with keyword keys:

     :messages     [{:role \"user\" :content \"..\"} ...]   (required)
     :tools        [{:type \"function\" :function {...}}]
     :tool_choice, :temperature, :max_tokens, :top_p, :stop, :stream,
     :response_format, :seed, :n, :user  passed through when present
     :reasoning    {:effort :low|:medium|:high} mapped per provider
     :body-extra   map merged into the body last (provider-specific keys)
     :extra-headers  headers for this request only
     :timeouts     {:connect-ms n :read-ms n}

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.codec.json :as json]
            [hive-providers.openai.registry :as reg]))

(def passthrough-keys
  "Body keys copied from params when present."
  [:tools :tool_choice :temperature :max_tokens :max_completion_tokens :top_p
   :stop :response_format :seed :n :user :frequency_penalty :presence_penalty
   :parallel_tool_calls :stream_options])

(def reasoning-keys
  "Reasoning keys that may ride on an assistant message."
  #{:reasoning :reasoning_content :reasoning_details})

(defn outbound-message
  "Message M prepared for ENTRY: on an assistant message, reasoning keys the
   provider does not accept back are removed, and :reasoning_content is
   restored from :reasoning for a provider that wants it under that name."
  [entry m]
  (if (not= "assistant" (:role m))
    m
    (let [allowed (or (:reasoning-fields entry) #{})
          m       (if (and (contains? allowed :reasoning_content)
                           (nil? (:reasoning_content m))
                           (:reasoning m))
                    (assoc m :reasoning_content (:reasoning m))
                    m)]
      (reduce (fn [acc k] (if (contains? allowed k) acc (dissoc acc k)))
              m reasoning-keys))))

(defn- reasoning-entries
  [entry params]
  (when-let [effort (get-in params [:reasoning :effort])]
    (case (:reasoning-style entry)
      :openrouter   {:reasoning {:effort (name effort)}}
      :effort-field {:reasoning_effort (name effort)}
      nil)))

(defn body
  "Request body map for PARAMS sent to ENTRY as MODEL. Drops :tools for a
   tool-incompatible model and applies the entry's :field-allowlist."
  [entry model params]
  (let [base (merge {:model    model
                     :messages (mapv #(outbound-message entry %) (:messages params))}
                    (into {} (keep (fn [k] (when (some? (get params k)) [k (get params k)])))
                          passthrough-keys)
                    (when (some? (:stream params)) {:stream (boolean (:stream params))})
                    (reasoning-entries entry params)
                    (:body-extra params))
        base (if (and (:tools base) (reg/tool-incompatible? entry model))
               (dissoc base :tools :tool_choice :parallel_tool_calls)
               base)
        base (if (seq (:tools base)) base (dissoc base :tools :tool_choice))]
    (if-let [allow (:field-allowlist entry)]
      (select-keys base (map keyword allow))
      base)))

(defn headers
  "Request headers for ENTRY with API-KEY (nil sends no auth header),
   EXTRA headers of the request merged last."
  [entry api-key extra]
  (let [scheme (or (:auth-scheme entry) :bearer)
        hname  (or (:auth-header-name entry) "Authorization")]
    (cond-> {"Content-Type" "application/json"}
      (and api-key (= scheme :bearer)) (assoc "Authorization" (str "Bearer " api-key))
      (and api-key (not= scheme :bearer)) (assoc hname api-key)
      (map? (:extra-headers entry)) (merge (:extra-headers entry))
      (map? extra) (merge extra))))

(defn http-request
  "HTTP request map for a chat completion: ENTRY resolved for MODEL, the
   credential API-KEY (or nil), and PARAMS."
  [entry model api-key params]
  (cond-> {:method  :post
           :url     (reg/api-url entry params)
           :headers (headers entry api-key (:extra-headers params))
           :body    (json/write-str (body entry model params))}
    (:stream params)  (assoc :stream? true)
    (or (:timeouts params) (:timeouts entry))
    (assoc :timeouts (merge (:timeouts entry) (:timeouts params)))))
