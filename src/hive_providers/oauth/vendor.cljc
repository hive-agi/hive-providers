(ns hive-providers.oauth.vendor
  "Vendor credential files, mapped to and from the canonical token.

   CLI tools keep their logins in their own shapes. A harness that shares a
   login with one of them reads that file, works with the canonical token
   (hive-providers.oauth.token), and writes the file back in the vendor's
   shape, keeping every key it does not understand. All functions are pure;
   reading and writing the file is the host's job.

     Claude Code  ~/.claude/.credentials.json  {:claudeAiOauth {...}}
     codex CLI    $CODEX_HOME/auth.json        {:tokens {...} :last_refresh ..}
     Kimi Code    ~/.kimi-code/credentials/<slot>.json

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.crypto.sha256 :as sha]
            [hive-providers.oauth.jwt :as jwt]))

;; =============================================================================
;; Claude Code
;; =============================================================================

(defn claude->token
  "Canonical token for a Claude Code credentials map, either the whole file
   ({:claudeAiOauth {...}}) or its inner map. nil when there is no login."
  [creds]
  (let [c (or (:claudeAiOauth creds) creds)]
    (when (string? (:accessToken c))
      {:access-token  (:accessToken c)
       :refresh-token (:refreshToken c)
       :expires-at-ms (:expiresAt c)
       :token-type    "Bearer"
       :scopes        (some-> (:scopes c) vec)
       :extra         (select-keys c [:subscriptionType :rateLimitTier])})))

(defn token->claude
  "The inner claudeAiOauth map for TOKEN, merged over PRIOR (the inner map
   last read) so fields this library does not manage survive."
  ([token] (token->claude token nil))
  ([token prior]
   (let [extra (:extra token)]
     (cond-> (merge prior
                    {:accessToken  (:access-token token)
                     :refreshToken (:refresh-token token)
                     :expiresAt    (:expires-at-ms token)})
       (:scopes token)                 (assoc :scopes (:scopes token))
       (:subscription_type extra)      (assoc :subscriptionType (:subscription_type extra))
       (:rate_limit_tier extra)        (assoc :rateLimitTier (:rate_limit_tier extra))))))

(defn claude-file-encode
  "Whole Claude Code credentials file for TOKEN, keeping PRIOR-FILE's other
   top-level keys."
  [token prior-file]
  (assoc prior-file :claudeAiOauth (token->claude token (:claudeAiOauth prior-file))))

;; =============================================================================
;; codex CLI (ChatGPT subscription)
;; =============================================================================

(defn chatgpt-account-id
  "ChatGPT account id carried by a codex access or id TOKEN's claims, from
   the first of the three places OpenAI puts it."
  [token]
  (let [c (jwt/claims token)]
    (or (:chatgpt_account_id c)
        (get-in c [(keyword "https://api.openai.com/auth") :chatgpt_account_id])
        (get-in c [:organizations 0 :id]))))

(defn codex->token
  "Canonical token for a codex auth.json map, or nil when it holds no
   ChatGPT login. Expiry comes from the access token's `exp` claim."
  [auth]
  (let [{:keys [access_token refresh_token id_token account_id]} (:tokens auth)]
    (when (string? refresh_token)
      {:access-token  access_token
       :refresh-token refresh_token
       :id-token      id_token
       :expires-at-ms (jwt/exp-ms access_token)
       :token-type    "Bearer"
       :extra         {:account-id (or account_id
                                       (chatgpt-account-id id_token)
                                       (chatgpt-account-id access_token))}})))

(defn token->codex
  "auth.json map for TOKEN, merged over PRIOR. NOW-ISO is written to
   :last_refresh (an ISO-8601 string the host formats)."
  [token prior now-iso]
  (let [tokens (:tokens prior)
        access (or (:access-token token) (:access_token tokens))
        id-tok (or (:id-token token) (:id_token tokens))]
    (-> prior
        (assoc :tokens (assoc tokens
                              :access_token  access
                              :id_token      id-tok
                              :refresh_token (or (:refresh-token token) (:refresh_token tokens))
                              :account_id    (or (chatgpt-account-id id-tok)
                                                 (chatgpt-account-id access)
                                                 (:account_id tokens))))
        (cond-> now-iso (assoc :last_refresh now-iso)))))

;; =============================================================================
;; Kimi Code
;; =============================================================================

(def kimi-regions
  "Kimi Code deployments. A login is bound to one; its credential slot follows."
  {:mainland-cn {:oauth-host "https://auth.kimi.com" :base-url "https://api.kimi.com/coding/v1"}
   :global      {:oauth-host "https://auth.kimi.ai"  :base-url "https://api.kimi.ai/coding/v1"}})

(defn kimi-slot
  "Credential slot name the Kimi Code CLI uses for REGION: the shared slot for
   the mainland endpoints, else one scoped by a hash of the endpoint pair."
  [region]
  (let [{:keys [oauth-host base-url]} (get kimi-regions region)]
    (if (= region :mainland-cn)
      "kimi-code"
      (str "kimi-code-env-"
           (subs (sha/hex-digest (str "{\"oauthHost\":\"" oauth-host "\",\"baseUrl\":\"" base-url "\"}"))
                 0 16)))))

(defn kimi->token
  "Canonical token for a Kimi Code credentials map (:expires_at in epoch
   seconds), or nil when it holds no login."
  [creds]
  (when (seq (:refresh_token creds))
    {:access-token  (:access_token creds)
     :refresh-token (:refresh_token creds)
     :expires-at-ms (when (number? (:expires_at creds)) (* 1000 (:expires_at creds)))
     :token-type    (or (:token_type creds) "Bearer")
     :scopes        (when (string? (:scope creds)) [(:scope creds)])}))

(defn token->kimi
  "Kimi Code credentials map for TOKEN, merged over PRIOR."
  [token prior]
  (cond-> (assoc prior
                 :access_token  (:access-token token)
                 :refresh_token (or (:refresh-token token) (:refresh_token prior)))
    (:expires-at-ms token) (assoc :expires_at (quot (:expires-at-ms token) 1000))
    (:expires-at-ms token) (assoc :expires_in (max 0 (quot (- (:expires-at-ms token)
                                                               (or (:obtained-at-ms token)
                                                                   (:expires-at-ms token)))
                                                            1000)))))
