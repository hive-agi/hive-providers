(ns hive-providers.openai.registry
  "OpenAI-compatible providers, as data, and the pure decisions over them.

   A REGISTRY maps a provider keyword to an ENTRY:

     :api-url        chat-completions endpoint (required), may hold {vars}
     :api-key-env    environment variable holding the key; nil = keyless
     :default-model  model used when a request names none
     optional quirks:
     :auth-header-name  header carrying the key (default \"Authorization\")
     :auth-scheme       :bearer (default), :raw or :api-key (key sent bare)
     :url-template      {:path-extra \"?api-version=..\" :vars #{:deployment}}
     :field-allowlist   set of body keys the provider accepts (strict APIs)
     :tool-incompatible-models  set of models, or :all, that reject tools
     :extra-headers     headers sent on every request
     :response-extras   keyword naming a `response/extras` method
     :reasoning-style   :openrouter (reasoning object), :effort-field
                        (reasoning_effort) or nil
     :reasoning-fields  reasoning keys the provider accepts back on an
                        assistant message (e.g. #{:reasoning_content})

   `seed` is a starting point. A harness overlays its own configuration with
   `overlay`; nothing here is global state.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.result :as r]))

(def seed
  "OpenAI-compatible providers known to work, as of 2026."
  {:openrouter    {:api-url          "https://openrouter.ai/api/v1/chat/completions"
                   :api-key-env      "OPENROUTER_API_KEY"
                   :default-model    "deepseek/deepseek-chat"
                   :cache-control    :anthropic-style
                   :reasoning-style  :openrouter
                   :reasoning-fields #{:reasoning :reasoning_details}}
   :deepseek      {:api-url       "https://api.deepseek.com/v1/chat/completions"
                   :api-key-env   "DEEPSEEK_API_KEY"
                   :default-model "deepseek-chat"}
   :venice        {:api-url                  "https://api.venice.ai/api/v1/chat/completions"
                   :api-key-env              "VENICE_API_KEY"
                   :default-model            "z-ai-glm-5-3-flash"
                   :tool-incompatible-models #{"venice-uncensored"}}
   :groq          {:api-url       "https://api.groq.com/openai/v1/chat/completions"
                   :api-key-env   "GROQ_API_KEY"
                   :default-model "llama-3.3-70b-versatile"}
   :together      {:api-url       "https://api.together.xyz/v1/chat/completions"
                   :api-key-env   "TOGETHER_API_KEY"
                   :default-model "meta-llama/Llama-3.3-70B-Instruct-Turbo"}
   :fireworks     {:api-url       "https://api.fireworks.ai/inference/v1/chat/completions"
                   :api-key-env   "FIREWORKS_API_KEY"
                   :default-model "accounts/fireworks/models/llama-v3p3-70b-instruct"}
   :openai        {:api-url       "https://api.openai.com/v1/chat/completions"
                   :api-key-env   "OPENAI_API_KEY"
                   :default-model "gpt-4o-mini"}
   :ollama-compat {:api-url       "http://localhost:11434/v1/chat/completions"
                   :api-key-env   nil
                   :default-model "devstral-small:24b"}
   :ai21          {:api-url       "https://api.ai21.com/studio/v1/chat/completions"
                   :api-key-env   "AI21_API_KEY"
                   :default-model "jamba-large-1.7"}
   :cerebras      {:api-url       "https://api.cerebras.ai/v1/chat/completions"
                   :api-key-env   "CEREBRAS_API_KEY"
                   :default-model "llama-3.3-70b"}
   :codestral     {:api-url       "https://codestral.mistral.ai/v1/chat/completions"
                   :api-key-env   "MISTRAL_API_KEY"
                   :default-model "codestral-latest"}
   :dashscope     {:api-url       "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions"
                   :api-key-env   "DASHSCOPE_API_KEY"
                   :default-model "qwen3-235b-a22b-instruct"}
   :deepinfra     {:api-url       "https://api.deepinfra.com/v1/openai/chat/completions"
                   :api-key-env   "DEEPINFRA_API_KEY"
                   :default-model "meta-llama/Meta-Llama-3.3-70B-Instruct"}
   :friendli      {:api-url       "https://api.friendli.ai/serverless/v1/chat/completions"
                   :api-key-env   "FRIENDLI_TOKEN"
                   :default-model "meta-llama-3.3-70b-instruct"}
   :hyperbolic    {:api-url       "https://api.hyperbolic.xyz/v1/chat/completions"
                   :api-key-env   "HYPERBOLIC_API_KEY"
                   :default-model "meta-llama/Meta-Llama-3.1-405B-Instruct"}
   :lambda        {:api-url       "https://api.lambda.ai/v1/chat/completions"
                   :api-key-env   "LAMBDA_API_KEY"
                   :default-model "llama3.3-70b-instruct-fp8"}
   :lepton        {:api-url       "https://api.lepton.ai/v1/chat/completions"
                   :api-key-env   "LEPTON_API_KEY"
                   :default-model "llama3-1-405b"}
   :mistral       {:api-url         "https://api.mistral.ai/v1/chat/completions"
                   :api-key-env     "MISTRAL_API_KEY"
                   :default-model   "mistral-large-latest"
                   :field-allowlist #{"model" "messages" "tools" "tool_choice"
                                      "temperature" "max_tokens" "top_p" "stop"
                                      "stream" "safe_prompt"}}
   :moonshot      {:api-url          "https://api.moonshot.ai/v1/chat/completions"
                   :api-key-env      "MOONSHOT_API_KEY"
                   :default-model    "kimi-k2-0905-preview"
                   :reasoning-style  :effort-field
                   :reasoning-fields #{:reasoning_content}}
   :kimi          {:api-url          "https://api.kimi.ai/coding/v1/chat/completions"
                   :api-key-env      nil
                   :auth             :oauth
                   :oauth-client     :kimi/code-global
                   :default-model    "kimi-for-coding"
                   :reasoning-style  :effort-field
                   :reasoning-fields #{:reasoning_content}}
   :nvidia        {:api-url       "https://integrate.api.nvidia.com/v1/chat/completions"
                   :api-key-env   "NVIDIA_API_KEY"
                   :default-model "meta/llama-3.3-70b-instruct"}
   :sambanova     {:api-url       "https://api.sambanova.ai/v1/chat/completions"
                   :api-key-env   "SAMBANOVA_API_KEY"
                   :default-model "Meta-Llama-3.3-70B-Instruct"}
   :xai           {:api-url       "https://api.x.ai/v1/chat/completions"
                   :api-key-env   "XAI_API_KEY"
                   :default-model "grok-4-fast"}
   :zai           {:api-url       "https://api.z.ai/api/paas/v4/chat/completions"
                   :api-key-env   "ZAI_API_KEY"
                   :default-model "glm-4.6"}
   :perplexity    {:api-url                  "https://api.perplexity.ai/chat/completions"
                   :api-key-env              "PERPLEXITY_API_KEY"
                   :default-model            "sonar"
                   :tool-incompatible-models :all
                   :response-extras          :perplexity}
   :azure-openai  {:api-url          "https://{resource}.openai.azure.com/openai/deployments/{deployment}/chat/completions"
                   :api-key-env      "AZURE_OPENAI_API_KEY"
                   :default-model    "gpt-5"
                   :auth-header-name "api-key"
                   :auth-scheme      :raw
                   :url-template     {:path-extra "?api-version=2024-10-21"
                                      :vars       #{:resource :deployment}}}
   :gemini-openai-shim {:api-url         "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions"
                        :api-key-env     "GEMINI_API_KEY"
                        :default-model   "gemini-2.5-pro"
                        :field-allowlist #{"model" "messages" "tools" "tool_choice"
                                           "temperature" "max_tokens" "top_p"
                                           "stop" "stream"}}
   :novita        {:api-url       "https://api.novita.ai/v3/openai/chat/completions"
                   :api-key-env   "NOVITA_API_KEY"
                   :default-model "deepseek/deepseek-v3"}
   :baseten       {:api-url       "https://inference.baseten.co/v1/chat/completions"
                   :api-key-env   "BASETEN_API_KEY"
                   :default-model "llama-4-maverick"}
   :runpod        {:api-url       "https://api.runpod.ai/v2/{endpoint_id}/openai/v1/chat/completions"
                   :api-key-env   "RUNPOD_API_KEY"
                   :default-model "llama-3.3-70b"
                   :url-template  {:path-extra "" :vars #{:endpoint_id}}}})

(def default-provider
  "Provider a request falls back to when it names none."
  :openrouter)

;; =============================================================================
;; Predicates
;; =============================================================================

(defn keyless?
  "True when ENTRY needs no API key (a local server, or OAuth-backed)."
  [entry]
  (nil? (:api-key-env entry)))

(defn oauth?
  "True when ENTRY is credentialed by an OAuth token rather than a key."
  [entry]
  (= :oauth (:auth entry)))

(defn templated-url?
  "True when ENTRY's URL carries request-specific segments."
  [entry]
  (boolean (:url-template entry)))

(defn tool-incompatible?
  "True when ENTRY refuses function tools for MODEL."
  [entry model]
  (let [decl (:tool-incompatible-models entry)]
    (boolean (or (= :all decl)
                 (and (coll? decl) (contains? (set decl) model))))))

(defn anthropic-family?
  "True when MODEL names an Anthropic model, however the gateway spells it."
  [model]
  (boolean (and (string? model)
                (or (str/starts-with? model "anthropic/")
                    (str/includes? model "claude")))))

(defn cache-control-style
  "`:anthropic-style` when ENTRY forwards cache_control markers to MODEL, else
   nil."
  [entry model]
  (when (and (= :anthropic-style (:cache-control entry)) (anthropic-family? model))
    :anthropic-style))

;; =============================================================================
;; Overlay and resolution
;; =============================================================================

(defn secret-key->env-var
  "`:axon-api-key` -> \"AXON_API_KEY\"."
  [k]
  (when k (-> (name k) (str/replace "-" "_") str/upper-case)))

(defn- config-entry
  [entry]
  (if (and (contains? entry :secret-key) (not (contains? entry :api-key-env)))
    (assoc entry :api-key-env (secret-key->env-var (:secret-key entry)))
    entry))

(defn overlay
  "BASE with CONFIG overlaid: a map value merges into the entry (config
   wins), nil or false removes the provider, anything else is ignored. An
   entry naming :dispatch belongs to another client and is skipped."
  [base config]
  (if (map? config)
    (reduce-kv (fn [acc k v]
                 (cond
                   (not (keyword? k))             acc
                   (and (map? v) (:dispatch v))   acc
                   (map? v)                       (update acc k merge (config-entry v))
                   (or (nil? v) (false? v))       (dissoc acc k)
                   :else                          acc))
               base config)
    base))

(defn parse-model-prefix
  "{:provider kw-or-nil :model str} for a `<provider>:<model>` MODEL. Only a
   prefix naming a provider in REGISTRY counts; a colon may sit in a model
   name."
  [registry model]
  (if-let [idx (and (string? model) (str/index-of model ":"))]
    (let [prov (keyword (subs model 0 idx))]
      (if (contains? registry prov)
        {:provider prov :model (subs model (inc idx))}
        {:provider nil :model model}))
    {:provider nil :model model}))

(defn resolve-provider
  "Result {:provider :entry :model} for PARAMS (:provider, :model) against
   REGISTRY. Precedence: explicit :provider, model prefix, `default-provider`.
   err :provider/unknown or :provider/no-endpoint."
  [registry {:keys [provider model]}]
  (let [{prefixed :provider clean :model} (parse-model-prefix registry model)
        prov  (or (some-> provider keyword) prefixed default-provider)
        entry (get registry prov)]
    (cond
      (nil? entry)
      (r/err :provider/unknown {:provider prov :available (vec (keys registry))})

      (not (string? (:api-url entry)))
      (r/err :provider/no-endpoint {:provider prov})

      :else
      (r/ok {:provider prov :entry entry :model (or clean (:default-model entry))}))))

(defn api-url
  "Effective URL of ENTRY for PARAMS: {var} segments filled from PARAMS and
   :path-extra appended when the entry has a :url-template."
  [entry params]
  (let [base (:api-url entry)]
    (if-let [{:keys [path-extra vars]} (:url-template entry)]
      (str (reduce (fn [u v] (str/replace u (str "{" (name v) "}") (str (get params v))))
                   base (or vars #{}))
           path-extra)
      base)))

(defn choose-api-key
  "Result: the key for ENTRY from candidates {:explicit :stored :env}, first
   non-blank wins. ok nil for a keyless entry; err :provider/missing-api-key
   otherwise."
  [entry {:keys [explicit stored env]}]
  (let [present #(when (and (string? %) (not (str/blank? %))) %)]
    (if-let [k (or (present explicit) (present stored) (present env))]
      (r/ok k)
      (if (keyless? entry)
        (r/ok nil)
        (r/err :provider/missing-api-key {:env-var (:api-key-env entry)})))))

(defn available
  "Providers of REGISTRY callable given PRESENT-ENV-VARS (names holding a
   value). Keyless entries are always callable."
  [registry present-env-vars]
  (let [present? (set present-env-vars)]
    (vec (filter (fn [k] (let [e (get registry k)]
                           (or (keyless? e) (contains? present? (:api-key-env e)))))
                 (keys registry)))))

(defn env-vars-of
  "Environment variable names REGISTRY consults."
  [registry]
  (vec (distinct (keep (fn [[_ e]] (:api-key-env e)) registry))))
