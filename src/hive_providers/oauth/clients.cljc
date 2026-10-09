(ns hive-providers.oauth.clients
  "Well-known public OAuth clients of LLM vendors, as data.

   Each entry is a CLIENT map for hive-providers.oauth.token / pkce / device,
   plus :scopes (the default request) and :redirect (how the vendor expects
   the code back). The client ids are the PUBLIC ids the vendors' own CLIs
   ship; they identify an application, not a user, and carry no secret.

   Harnesses add or override entries with `with-clients`; nothing here is
   global state.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.oauth.vendor :as vendor]
            [hive-providers.result :as r]))

(def anthropic-scopes
  "Scope sets of Anthropic's consumer and console flows."
  {:full           ["org:create_api_key" "user:profile" "user:inference"]
   :inference-only ["user:inference"]})

(def registry
  "Client key -> client map."
  {:anthropic/claude-ai
   {:authorize-url "https://claude.ai/oauth/authorize"
    :token-url     "https://platform.claude.com/v1/oauth/token"
    :client-id     "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
    :scopes        (:full anthropic-scopes)
    :redirect      {:kind :loopback :path "/callback"}
    :description   "Claude.ai consumer (Pro/Max), subscription billing"}

   :anthropic/console
   {:authorize-url "https://console.anthropic.com/oauth/authorize"
    :token-url     "https://platform.claude.com/v1/oauth/token"
    :client-id     "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
    :scopes        (:full anthropic-scopes)
    :redirect      {:kind :loopback :path "/callback"}
    :description   "console.anthropic.com developer/API tier"}

   :openai/codex
   {:authorize-url "https://auth.openai.com/oauth/authorize"
    :token-url     "https://auth.openai.com/oauth/token"
    :client-id     "app_EMoamEEZ73f0CkXaXp7hrann"
    :body-encoding :json
    :scopes        ["openid" "profile" "email" "offline_access"]
    :redirect      {:kind :loopback :port 1455 :path "/auth/callback"}
    :description   "ChatGPT subscription through the codex CLI's client"}

   :kimi/code-mainland
   {:token-url     (str (get-in vendor/kimi-regions [:mainland-cn :oauth-host]) "/api/oauth/token")
    :client-id     "17e5f671-d194-4dfb-9706-5516cb48c098"
    :base-url      (get-in vendor/kimi-regions [:mainland-cn :base-url])
    :description   "Kimi Code CLI, mainland deployment"}

   :kimi/code-global
   {:token-url     (str (get-in vendor/kimi-regions [:global :oauth-host]) "/api/oauth/token")
    :client-id     "17e5f671-d194-4dfb-9706-5516cb48c098"
    :base-url      (get-in vendor/kimi-regions [:global :base-url])
    :description   "Kimi Code CLI, global deployment"}})

(defn with-clients
  "REGISTRY with EXTRA merged in: a map value merges into the entry, nil or
   false removes it."
  ([extra] (with-clients registry extra))
  ([base extra]
   (reduce-kv (fn [acc k v]
                (cond
                  (map? v)                 (update acc k merge v)
                  (or (nil? v) (false? v)) (dissoc acc k)
                  :else                    acc))
              base
              extra)))

(defn client
  "Result: the client map for K in REG (default `registry`)."
  ([k] (client registry k))
  ([reg k]
   (if-let [c (get reg k)]
     (r/ok c)
     (r/err :oauth/unknown-client {:client k :available (vec (keys reg))}))))

(defn with-scopes
  "CLIENT requesting SCOPES instead of its default."
  [client scopes]
  (assoc client :scopes (vec scopes)))
