(ns hive-providers.oauth.pkce
  "Authorization-code flow with PKCE (RFC 7636), sans I/O.

   The verifier and state are built from random bytes the HOST supplies
   (the :entropy/bytes op, or bytes passed in), so this namespace stays pure
   and its outputs are reproducible in tests.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.base64 :as b64]
            [hive-providers.codec.url :as url]
            [hive-providers.crypto.sha256 :as sha]
            [hive-providers.effect :as fx]
            [hive-providers.result :as r]
            [hive-providers.text :as text]))

(defn verifier
  "Code verifier for 32 or more random BYTES: their unpadded base64url form
   (RFC 7636 4.1; 32 bytes give the minimum 43 characters)."
  [bytes]
  (b64/encode-url bytes))

(defn challenge
  "S256 code challenge for VERIFIER: base64url(sha256(ascii(verifier)))."
  [verifier]
  (b64/encode-url (sha/digest-str verifier)))

(defn bundle
  "{:verifier :challenge :state} from two vectors of random bytes."
  [verifier-bytes state-bytes]
  (let [v (verifier verifier-bytes)]
    {:verifier v :challenge (challenge v) :state (b64/encode-url state-bytes)}))

(defn bundle-program
  "Program yielding a fresh bundle, asking the host for 64 random bytes."
  []
  (fx/fmap (fx/perform :entropy/bytes {:n 64})
           (fn [bs] (bundle (subvec (vec bs) 0 32) (subvec (vec bs) 32 64)))))

(defn loopback-redirect-uri
  "Redirect URI of a loopback listener on PORT (RFC 8252 7.3)."
  ([port] (loopback-redirect-uri port "/callback"))
  ([port path] (str "http://localhost:" port path)))

(defn authorize-url
  "The URL that starts a login.

   Keys of CLIENT: :authorize-url, :client-id.
   Keys of PARAMS: :redirect-uri, :scopes (vector), :state, :challenge,
   :extra (map or pairs of further query parameters, e.g. login_hint).
   The standard parameters come first and cannot be overridden by :extra."
  [{:keys [authorize-url client-id]} {:keys [redirect-uri scopes state challenge extra]}]
  (let [standard [[:response_type "code"]
                  [:client_id client-id]
                  [:redirect_uri redirect-uri]
                  [:scope (when (seq scopes) (str/join " " scopes))]
                  [:state state]
                  [:code_challenge challenge]
                  [:code_challenge_method "S256"]]
        taken    (set (map (comp name first) standard))
        extras   (remove (fn [[k _]] (contains? taken (name k))) extra)]
    (url/with-query authorize-url (concat standard extras))))

(defn parse-callback
  "Result for the redirect a login lands on. CALLBACK is the full URL, its
   query string, or a map of query params. EXPECTED-STATE must match.

   ok  {:code .. :state ..}
   err :oauth/state-mismatch, :oauth/missing-code, or :oauth/<error> when
       the server redirected with an error."
  [callback expected-state]
  (let [params (cond
                 (map? callback) (into {} (map (fn [[k v]] [(name k) v])) callback)
                 (str/includes? (str callback) "?") (url/parse-query (url/query-of callback))
                 :else (url/parse-query callback))
        {:strs [code state error error_description]} params]
    (cond
      (not= state expected-state) (r/err :oauth/state-mismatch)
      error                       (r/err (keyword "oauth" error) {:description error_description})
      (str/blank? code)           (r/err :oauth/missing-code)
      :else                       (r/ok {:code code :state state}))))

(defn manual-code
  "Result for a code pasted by a user in the `code#state` form some vendors
   (Anthropic's console flow) display instead of redirecting."
  [pasted expected-state]
  (let [[code state] (text/split-first (str/trim (str pasted)) "#")]
    (parse-callback {"code" code "state" (or state expected-state)} expected-state)))
