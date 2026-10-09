(ns hive-providers.oauth.jwt
  "Reading (not verifying) JSON Web Tokens.

   OAuth providers hand out JWT access and id tokens whose claims carry the
   expiry and account ids a client needs. This namespace decodes them; it
   never checks a signature, so never use it to decide whom to trust.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.base64 :as b64]
            [hive-providers.codec.json :as json]
            [hive-providers.text :as text]))

(defn- segment
  [token i]
  (when (string? token)
    (let [parts (text/split token ".")]
      (when (= 3 (count parts)) (nth parts i)))))

(defn- decode-segment
  [s]
  (some-> s b64/decode-str json/read-str!))

(defn header
  "Decoded header map of TOKEN, or nil when TOKEN is not a JWT."
  [token]
  (decode-segment (segment token 0)))

(defn claims
  "Decoded payload map of TOKEN (keyword keys), or nil when TOKEN is not a JWT."
  [token]
  (let [c (decode-segment (segment token 1))]
    (when (map? c) c)))

(defn exp-ms
  "Expiry of TOKEN in epoch milliseconds, from its `exp` claim, or nil."
  [token]
  (let [exp (:exp (claims token))]
    (when (number? exp) (* 1000 exp))))

(defn claim
  "The claim at PATH (a vector of keys) in TOKEN's payload."
  [token path]
  (get-in (claims token) path))
