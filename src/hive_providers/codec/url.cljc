(ns hive-providers.codec.url
  "Percent-encoding (RFC 3986), form bodies and query strings.

   Pure. Encoding leaves only the unreserved set (ALPHA DIGIT - . _ ~) as
   is; every other byte of the UTF-8 form becomes %XX.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.utf8 :as utf8]
            [hive-providers.platform :as p]
            [hive-providers.text :as text]))

(def ^:private hex-digits
  (mapv (comp p/code->str p/char-code) (p/chars-of "0123456789ABCDEF")))

(defn- unreserved?
  [b]
  (or (<= 65 b 90) (<= 97 b 122) (<= 48 b 57)
      (= b 45) (= b 46) (= b 95) (= b 126)))

(defn encode
  "Percent-encoded form of the string S. (`apply str` over a vector, not a
   lazy seq: ClojureElisp's apply cannot spread a lazy seq.)"
  [s]
  (apply str
         (mapv (fn [b]
                 (if (unreserved? b)
                   (p/code->str b)
                   (str "%" (nth hex-digits (bit-shift-right b 4))
                        (nth hex-digits (bit-and b 15)))))
               (utf8/encode (str s)))))

(defn- hex-value
  [code]
  (cond
    (<= 48 code 57)  (- code 48)
    (<= 65 code 70)  (- code 55)
    (<= 97 code 102) (- code 87)
    :else nil))

(defn- hex-at
  [cs n i]
  (when (< i n) (hex-value (p/char-code (nth cs i)))))

(defn- flush-bytes
  [parts bytes]
  (if (seq bytes) (conj! parts (utf8/decode bytes)) parts))

(defn decode
  "The string S with %XX sequences decoded as UTF-8. With PLUS-AS-SPACE?
   (form and query bodies) a '+' decodes to a space. Characters outside an
   escape pass through untouched."
  ([s] (decode s false))
  ([s plus-as-space?]
   (let [cs (p/chars-of (str s))
         n  (count cs)]
     (loop [i 0 parts (transient []) bytes []]
       (if (>= i n)
         (apply str (persistent! (flush-bytes parts bytes)))
         (let [k (p/char-code (nth cs i))]
           (cond
             (and (= k 37) (hex-at cs n (inc i)) (hex-at cs n (+ i 2)))
             (recur (+ i 3) parts (conj bytes (+ (* 16 (hex-at cs n (inc i))) (hex-at cs n (+ i 2)))))

             (and plus-as-space? (= k 43))
             (recur (inc i) (conj! (flush-bytes parts bytes) " ") [])

             :else
             (recur (inc i) (conj! (flush-bytes parts bytes) (p/code->str k)) []))))))))

(defn- param-name
  [k]
  (if (keyword? k)
    (if-let [ns (namespace k)] (str ns "/" (name k)) (name k))
    (str k)))

(defn form-component
  "S encoded for an application/x-www-form-urlencoded body or query: as
   `encode`, but a space becomes '+' (WHATWG URL standard, and what
   java.net.URLEncoder produces)."
  [s]
  (str/replace (encode s) "%20" "+"))

(defn form-encode
  "application/x-www-form-urlencoded body for PARAMS, a map or a sequence of
   [k v] pairs. Pairs keep their order; a nil value drops the pair."
  [params]
  (->> params
       (remove (fn [[_ v]] (nil? v)))
       (map (fn [[k v]] (str (form-component (param-name k)) "=" (form-component (str v)))))
       (str/join "&")))

(defn parse-query
  "Map of string keys to string values for a query or form body Q. A leading
   '?' is ignored; a repeated key keeps its last value."
  [q]
  (let [q (str q)
        q (if (str/starts-with? q "?") (subs q 1) q)]
    (if (str/blank? q)
      {}
      (into {}
            (keep (fn [part]
                    (when-not (str/blank? part)
                      (let [[k v] (text/split-first part "=")]
                        [(decode k true) (if v (decode v true) "")]))))
            (text/split q "&")))))

(defn with-query
  "URL with PARAMS appended as a query string, after '?' or '&' as fits."
  [url params]
  (let [q (form-encode params)]
    (cond
      (str/blank? q)            url
      (str/includes? url "?")   (str url "&" q)
      :else                     (str url "?" q))))

(defn query-of
  "The query part of URL (without '?'), or nil."
  [url]
  (let [url (str url)
        url (if-let [h (str/index-of url "#")] (subs url 0 h) url)]
    (when-let [i (str/index-of url "?")]
      (subs url (inc i)))))
