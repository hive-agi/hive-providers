(ns hive-providers.oauth.token
  "OAuth 2.0 token endpoint (RFC 6749 sections 4 to 6), sans I/O.

   A CLIENT is a map describing one registered OAuth client:

     :token-url      token endpoint (required)
     :client-id      client id (required)
     :client-secret  for confidential clients, optional
     :auth-method    :none (default, public client), :client-secret-post or
                     :client-secret-basic
     :body-encoding  :form (default, the RFC) or :json (some vendors, e.g.
                     the OpenAI auth server for refresh)
     :headers        extra request headers, optional

   A TOKEN is the canonical map every flow returns:

     :access-token   string
     :refresh-token  string or nil
     :expires-at-ms  epoch ms or nil (unknown)
     :token-type     string, usually \"Bearer\"
     :scopes         vector of strings or nil
     :id-token       string or nil (OpenID Connect)
     :obtained-at-ms epoch ms the response was parsed
     :extra          every other response field, keyword keys

   Grant builders return an HTTP request map (see hive-providers.ports);
   parsers take the response and return a Result. The `*-program` functions
   compose both into effect programs.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.base64 :as b64]
            [hive-providers.codec.json :as json]
            [hive-providers.codec.url :as url]
            [hive-providers.effect :as fx]
            [hive-providers.result :as r]
            [hive-providers.text :as text]))

;; =============================================================================
;; Requests
;; =============================================================================

(defn- basic-auth
  [{:keys [client-id client-secret]}]
  (str "Basic " (b64/encode-str (str (url/encode client-id) ":" (url/encode client-secret)))))

(defn token-request
  "HTTP request posting the grant PARAMS (a map or ordered pairs, string or
   keyword keys) to the CLIENT's token endpoint, with client authentication
   applied per :auth-method."
  [{:keys [token-url client-id client-secret auth-method body-encoding headers]
    :or   {auth-method :none body-encoding :form}
    :as   client}
   params]
  (let [pairs  (vec (remove (fn [[_ v]] (nil? v)) params))
        pairs  (case auth-method
                 :client-secret-basic pairs
                 :client-secret-post  (conj pairs [:client_id client-id] [:client_secret client-secret])
                 (conj pairs [:client_id client-id]))
        json?  (= body-encoding :json)
        body   (if json?
                 (json/write-str (into {} (map (fn [[k v]] [(name k) v])) pairs))
                 (url/form-encode pairs))]
    {:method  :post
     :url     token-url
     :headers (cond-> {"Content-Type" (if json? "application/json"
                                          "application/x-www-form-urlencoded")
                       "Accept"       "application/json"}
                (= auth-method :client-secret-basic) (assoc "Authorization" (basic-auth client))
                (map? headers) (merge headers))
     :body    body}))

(defn authorization-code-grant
  "Request exchanging an authorization CODE (RFC 6749 4.1.3, with the PKCE
   VERIFIER of RFC 7636 4.5). EXTRA params are appended."
  [client {:keys [code verifier redirect-uri extra]}]
  (token-request client (concat [[:grant_type "authorization_code"]
                                 [:code code]
                                 [:redirect_uri redirect-uri]
                                 [:code_verifier verifier]]
                                extra)))

(defn refresh-grant
  "Request exchanging REFRESH-TOKEN for a new access token (RFC 6749 6).
   SCOPES, when given, narrows the request."
  ([client refresh-token] (refresh-grant client refresh-token nil))
  ([client refresh-token {:keys [scopes extra]}]
   (token-request client (concat [[:grant_type "refresh_token"]
                                  [:refresh_token refresh-token]
                                  [:scope (when (seq scopes) (str/join " " scopes))]]
                                 extra))))

(defn client-credentials-grant
  "Request for a client-credentials token (RFC 6749 4.4)."
  ([client] (client-credentials-grant client nil))
  ([client {:keys [scopes extra]}]
   (token-request client (concat [[:grant_type "client_credentials"]
                                  [:scope (when (seq scopes) (str/join " " scopes))]]
                                 extra))))

(defn device-code-grant
  "Request polling for the token of a device authorization (RFC 8628 3.4)."
  [client device-code]
  (token-request client [[:grant_type "urn:ietf:params:oauth:grant-type:device_code"]
                         [:device_code device-code]]))

;; =============================================================================
;; Responses
;; =============================================================================

(def ^:private known-fields
  #{:access_token :refresh_token :expires_in :token_type :scope :scopes :id_token})

(defn- scopes-of
  [data]
  (cond
    (string? (:scope data))      (text/words (:scope data))
    (sequential? (:scopes data)) (vec (:scopes data))
    :else nil))

(defn body->token
  "Result: the canonical token for the parsed token-endpoint DATA (a map with
   keyword keys). PRIOR, a token or nil, supplies the refresh token when the
   server did not rotate it. NOW-MS stamps expiry."
  [data now-ms prior]
  (cond
    (not (map? data))
    (r/err :oauth/bad-response {:message "token response is not an object"})

    (:error data)
    (r/err (keyword "oauth" (str (:error data)))
           {:description (:error_description data) :uri (:error_uri data)})

    (not (string? (:access_token data)))
    (r/err :oauth/bad-response {:message "token response has no access_token"})

    :else
    (let [expires-in (:expires_in data)
          expires-in (if (string? expires-in) (parse-long expires-in) expires-in)]
      (r/ok {:access-token   (:access_token data)
             :refresh-token  (or (:refresh_token data) (:refresh-token prior))
             :expires-at-ms  (when (number? expires-in) (+ now-ms (* 1000 expires-in)))
             :token-type     (or (:token_type data) "Bearer")
             :scopes         (or (scopes-of data) (:scopes prior))
             :id-token       (or (:id_token data) (:id-token prior))
             :obtained-at-ms now-ms
             :extra          (apply dissoc data known-fields)}))))

(defn parse-body
  "Result: the canonical token for the token-endpoint BODY text."
  [body now-ms prior]
  (r/bind (json/read-str body) #(body->token % now-ms prior)))

(defn parse-response
  "Result: the canonical token for the HTTP response RESP. An OAuth error
   body (any status) becomes err :oauth/<error code> carrying :status; a
   non-2xx without one becomes err :oauth/http-failed."
  [{:keys [status body error]} now-ms prior]
  (cond
    (nil? status)
    (r/err :oauth/network-failed {:message error})

    (<= 200 status 299)
    (parse-body body now-ms prior)

    :else
    (let [parsed (json/read-str! body)]
      (if (and (map? parsed) (:error parsed))
        (assoc (body->token parsed now-ms prior) :status status)
        (r/err :oauth/http-failed {:status status :body body})))))

;; =============================================================================
;; Expiry
;; =============================================================================

(def default-margin-ms
  "Refresh this long before expiry, so a token is not sent as it lapses."
  (* 5 60 1000))

(defn expired?
  "True when TOKEN expires within MARGIN-MS of NOW-MS. A token without a known
   expiry is treated as valid; one without an access token as expired."
  ([token now-ms] (expired? token now-ms default-margin-ms))
  ([token now-ms margin-ms]
   (or (not (string? (:access-token token)))
       (let [exp (:expires-at-ms token)]
         (and (number? exp) (<= exp (+ now-ms margin-ms)))))))

(defn bearer-header
  "{\"Authorization\" \"Bearer <access token>\"} for TOKEN, or nil."
  [token]
  (when-let [t (:access-token token)]
    {"Authorization" (str "Bearer " t)}))

;; =============================================================================
;; Programs
;; =============================================================================

(defn request-program
  "Program: send REQUEST, read the clock, and parse the answer as a token.
   Yields a Result."
  [request prior]
  (fx/bind (fx/perform :http/request request)
           (fn [resp]
             (fx/fmap (fx/perform :clock/now-ms)
                      (fn [now] (parse-response resp now prior))))))

(defn exchange-code-program
  "Program yielding Result<token> for an authorization-code exchange."
  [client params]
  (request-program (authorization-code-grant client params) nil))

(defn refresh-program
  "Program yielding Result<token> for refreshing TOKEN. A token with no
   refresh token yields err :oauth/no-refresh-token."
  ([client token] (refresh-program client token nil))
  ([client token opts]
   (if-let [rt (:refresh-token token)]
     (request-program (refresh-grant client rt opts) token)
     (fx/done (r/err :oauth/no-refresh-token)))))

(defn client-credentials-program
  "Program yielding Result<token> for a client-credentials grant."
  ([client] (client-credentials-program client nil))
  ([client opts] (request-program (client-credentials-grant client opts) nil)))
