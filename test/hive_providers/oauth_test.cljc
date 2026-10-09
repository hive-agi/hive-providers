(ns hive-providers.oauth-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-providers.codec.base64 :as b64]
            [hive-providers.codec.json :as json]
            [hive-providers.codec.url :as url]
            [hive-providers.effect :as fx]
            [hive-providers.oauth.authority :as auth]
            [hive-providers.oauth.clients :as clients]
            [hive-providers.oauth.device :as device]
            [hive-providers.oauth.jwt :as jwt]
            [hive-providers.oauth.pkce :as pkce]
            [hive-providers.oauth.token :as token]
            [hive-providers.oauth.vendor :as vendor]
            [hive-providers.ports :as ports]
            [hive-providers.result :as r]))

(def client
  {:token-url "https://auth.example/token" :client-id "cid"
   :authorize-url "https://auth.example/authorize"})

(defn- json-resp [status m] {:status status :headers {} :body (json/write-str m)})

(defn- fake-jwt [claims]
  (str (b64/encode-url (vec (map int [123 125])))
       "." (b64/encode-str (json/write-str claims) {:url? true :pad? false})
       ".sig"))

(deftest pkce-rfc7636-appendix-b
  (let [bytes [116 24 223 180 151 153 224 37 79 250 96 125 216 173 187 186
               22 212 37 77 105 214 191 240 91 88 5 88 83 132 141 121]
        v     (pkce/verifier bytes)]
    (is (= 43 (count v)))
    (is (= "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM" (pkce/challenge v)))))

(deftest pkce-authorize-and-callback
  (let [b   (pkce/bundle (vec (range 32)) (vec (range 32 64)))
        u   (pkce/authorize-url client {:redirect-uri "http://localhost:9/callback"
                                        :scopes ["a" "b"] :state (:state b)
                                        :challenge (:challenge b)
                                        :extra {:login_hint "me" :state "evil"}})
        q   (url/parse-query (url/query-of u))]
    (is (str/starts-with? u "https://auth.example/authorize?response_type=code"))
    (is (= "a b" (get q "scope")))
    (is (= (:state b) (get q "state")) "extra cannot override a standard param")
    (is (= "me" (get q "login_hint")))
    (is (= "S256" (get q "code_challenge_method")))
    (is (= {:ok {:code "c1" :state "s"}} (pkce/parse-callback "http://localhost:9/callback?code=c1&state=s" "s")))
    (is (= :oauth/state-mismatch (:error (pkce/parse-callback "?code=c1&state=x" "s"))))
    (is (= :oauth/access_denied (:error (pkce/parse-callback "?error=access_denied&state=s" "s"))))
    (is (= {:ok {:code "abc" :state "s"}} (pkce/manual-code " abc#s\n" "s"))))
  (testing "bundle-program asks the host for entropy"
    (let [b (ports/run {:entropy (ports/->CountingEntropy (atom 0))} (pkce/bundle-program))]
      (is (= 43 (count (:verifier b))))
      (is (= (pkce/challenge (:verifier b)) (:challenge b))))))

(deftest token-requests
  (let [req (token/refresh-grant client "rt/1")]
    (is (= :post (:method req)))
    (is (= "grant_type=refresh_token&refresh_token=rt%2F1&client_id=cid" (:body req)))
    (is (= "application/x-www-form-urlencoded" (get-in req [:headers "Content-Type"]))))
  (let [req (token/refresh-grant (assoc client :body-encoding :json) "rt")]
    (is (= {:grant_type "refresh_token" :refresh_token "rt" :client_id "cid"}
           (json/read-str! (:body req)))))
  (let [req (token/client-credentials-grant (assoc client :client-secret "s3" :auth-method :client-secret-basic))]
    (is (= (str "Basic " (b64/encode-str "cid:s3")) (get-in req [:headers "Authorization"])))
    (is (not (str/includes? (:body req) "client_id"))))
  (let [req (token/authorization-code-grant client {:code "c" :verifier "v" :redirect-uri "http://localhost:1/cb"})]
    (is (= {"grant_type" "authorization_code" "code" "c" "redirect_uri" "http://localhost:1/cb"
            "code_verifier" "v" "client_id" "cid"}
           (url/parse-query (:body req))))))

(deftest token-responses
  (let [t (r/value (token/parse-response (json-resp 200 {:access_token "a" :expires_in 60 :scope "x y" :foo 1})
                                         1000 {:refresh-token "old"}))]
    (is (= "a" (:access-token t)))
    (is (= "old" (:refresh-token t)) "refresh token kept when not rotated")
    (is (= 61000 (:expires-at-ms t)))
    (is (= ["x" "y"] (:scopes t)))
    (is (= {:foo 1} (:extra t))))
  (is (= :oauth/invalid_grant (:error (token/parse-response (json-resp 400 {:error "invalid_grant"}) 0 nil))))
  (is (= 400 (:status (token/parse-response (json-resp 400 {:error "invalid_grant"}) 0 nil))))
  (is (= :oauth/http-failed (:error (token/parse-response {:status 502 :body "<html>"} 0 nil))))
  (is (= :oauth/network-failed (:error (token/parse-response {:status nil :error "refused"} 0 nil))))
  (is (token/expired? {:access-token "a" :expires-at-ms 1000} 900 200))
  (is (not (token/expired? {:access-token "a" :expires-at-ms 1000} 700 200)))
  (is (not (token/expired? {:access-token "a"} 10 0)))
  (is (= {"Authorization" "Bearer a"} (token/bearer-header {:access-token "a"}))))

(deftest refresh-program-with-ports
  (let [http (ports/scripted-http [(json-resp 200 {:access_token "new" :refresh_token "r2" :expires_in 10})])
        res  (ports/run {:http http :clock (ports/->FixedClock 5)}
                        (token/refresh-program client {:refresh-token "r1"}))]
    (is (= "new" (:access-token (r/value res))))
    (is (= "r2" (:refresh-token (r/value res))))
    (is (= 10005 (:expires-at-ms (r/value res))))
    (is (= 1 (count @(:seen http)))))
  (is (= :oauth/no-refresh-token
         (:error (ports/run {} (token/refresh-program client {:access-token "x"}))))))

(deftest authority-policy
  (is (= :absent (auth/decide nil {:now-ms 0})))
  (is (= :reuse (auth/decide {:access-token "a" :expires-at-ms 10000000} {:now-ms 0})))
  (is (= :refresh (auth/decide {:access-token "a" :expires-at-ms 1000} {:now-ms 0})))
  (is (= :refresh (auth/decide {:access-token "a"} {:now-ms 50000 :force? true})))
  (is (= :reuse (auth/decide {:access-token "a" :obtained-at-ms 45000} {:now-ms 50000 :force? true})))
  (testing "refresh saves and returns; reuse makes no request"
    (let [store (ports/atom-store {:k {:access-token "old" :refresh-token "r" :expires-at-ms 100}})
          http  (ports/scripted-http [(json-resp 200 {:access_token "new" :expires_in 3600})])
          p     {:http http :store store :clock (ports/->FixedClock 1000)}
          res   (ports/run p (auth/access-program client :k {}))]
      (is (= "new" (:access-token (r/value res))))
      (is (= "new" (:access-token (get @(:state store) :k))))
      (is (= "r" (:refresh-token (get @(:state store) :k))))
      (ports/run p (auth/access-program client :k {}))
      (is (= 1 (count @(:seen http))))))
  (testing "vendor-shaped store keeps its shape"
    (let [store (ports/atom-store {:claude {:claudeAiOauth {:accessToken "o" :refreshToken "r"
                                                            :expiresAt 1 :subscriptionType "max"}
                                            :other 1}})
          http  (ports/scripted-http [(json-resp 200 {:access_token "n" :expires_in 60})])
          res   (ports/run {:http http :store store :clock (ports/->FixedClock 0)}
                           (auth/access-program client :claude
                                                {:decode vendor/claude->token
                                                 :encode vendor/claude-file-encode}))
          saved (get @(:state store) :claude)]
      (is (r/ok? res))
      (is (= 1 (:other saved)))
      (is (= {:accessToken "n" :refreshToken "r" :expiresAt 60000 :subscriptionType "max"}
             (:claudeAiOauth saved)))))
  (is (= :oauth/no-credentials
         (:error (ports/run {:store (ports/atom-store) :clock (ports/->FixedClock 0)}
                            (auth/access-program client :missing {}))))))

(deftest device-flow
  (let [dclient (assoc client :device-authorization-url "https://auth.example/device")
        http    (ports/scripted-http
                 [(json-resp 200 {:device_code "d" :user_code "U-1" :verification_uri "https://v" :interval 1 :expires_in 60})
                  (json-resp 400 {:error "authorization_pending"})
                  (json-resp 400 {:error "slow_down"})
                  (json-resp 200 {:access_token "tok"})])
        slept   (atom [])
        shown   (atom nil)
        res     (ports/run {:http http :clock (ports/->FixedClock 0) :sleeper (ports/->NoSleep slept)}
                           (device/flow-program dclient {:scopes ["s"]} #(reset! shown %)))]
    (is (= "tok" (:access-token (r/value res))))
    (is (= "U-1" (:user-code @shown)))
    (is (= [1000 1000 6000] @slept) "slow_down adds five seconds")
    (is (= "https://auth.example/device" (:url (first @(:seen http)))))
    (is (str/includes? (:body (second @(:seen http))) "device_code=d")))
  (is (= :failed (:status (device/poll-outcome (json-resp 400 {:error "access_denied"}) 0)))))

(deftest jwt-and-vendors
  (let [tok (fake-jwt {:exp 100 :chatgpt_account_id "acct"})]
    (is (= 100000 (jwt/exp-ms tok)))
    (is (= "acct" (vendor/chatgpt-account-id tok)))
    (is (nil? (jwt/claims "not-a-jwt")))
    (let [auth {:tokens {:access_token tok :refresh_token "r" :id_token nil} :OPENAI_API_KEY nil}
          t    (vendor/codex->token auth)
          back (vendor/token->codex (assoc t :access-token "n2") auth "2026-01-01T00:00:00Z")]
      (is (= 100000 (:expires-at-ms t)))
      (is (= "acct" (get-in t [:extra :account-id])))
      (is (= "n2" (get-in back [:tokens :access_token])))
      (is (= "r" (get-in back [:tokens :refresh_token])))
      (is (contains? back :OPENAI_API_KEY))
      (is (= "2026-01-01T00:00:00Z" (:last_refresh back))))
    (let [nested {:claims {(keyword "https://api.openai.com/auth") {:chatgpt_account_id "n"}}}]
      (is (= "n" (vendor/chatgpt-account-id (fake-jwt (:claims nested)))))))
  (is (= "kimi-code" (vendor/kimi-slot :mainland-cn)))
  (is (str/starts-with? (vendor/kimi-slot :global) "kimi-code-env-"))
  (is (= 16 (- (count (vendor/kimi-slot :global)) (count "kimi-code-env-"))))
  (let [t (vendor/kimi->token {:access_token "a" :refresh_token "r" :expires_at 50})]
    (is (= 50000 (:expires-at-ms t)))
    (is (= 50 (:expires_at (vendor/token->kimi t {:x 1}))))))

(deftest client-registry
  (is (= "app_EMoamEEZ73f0CkXaXp7hrann" (:client-id (r/value (clients/client :openai/codex)))))
  (is (= :oauth/unknown-client (:error (clients/client :nope))))
  (let [reg (clients/with-clients {:openai/codex {:client-id "x"} :kimi/code-global nil})]
    (is (= "x" (:client-id (get reg :openai/codex))))
    (is (= "https://auth.openai.com/oauth/token" (:token-url (get reg :openai/codex))))
    (is (not (contains? reg :kimi/code-global)))))
