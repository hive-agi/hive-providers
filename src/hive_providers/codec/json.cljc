(ns hive-providers.codec.json
  "JSON (RFC 8259) encoder and decoder in portable Clojure.

   The library depends on no JSON package so it reads the same on every
   dialect. `read-str` returns a Result instead of throwing; `write-str` is
   total.

   Decoding: objects become maps with keyword keys (override with :key-fn),
   arrays become vectors, integers stay integers, numbers with a fraction or
   exponent become doubles.

   Encoding: maps, sequential collections and sets, strings, keywords
   (written as their name, namespace kept), symbols, numbers, booleans and
   nil. A non-finite number is written as null. Anything else is written as
   its `str`.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.utf8 :as utf8]
            [hive-providers.platform :as p]
            [hive-providers.result :as r]
            [hive-providers.text :as text]))

;; =============================================================================
;; Encoding
;; =============================================================================

(def ^:private hex-digits (mapv (comp p/code->str p/char-code) (p/chars-of "0123456789abcdef")))

(defn- hex4
  [n]
  (str (nth hex-digits (bit-and (bit-shift-right n 12) 15))
       (nth hex-digits (bit-and (bit-shift-right n 8) 15))
       (nth hex-digits (bit-and (bit-shift-right n 4) 15))
       (nth hex-digits (bit-and n 15))))

(defn- escape-code
  [c]
  (case c
    34 "\\\""
    92 "\\\\"
    10 "\\n"
    13 "\\r"
    9  "\\t"
    8  "\\b"
    12 "\\f"
    (if (or (< c 0x20) (= c 0x2028) (= c 0x2029))
      (str "\\u" (hex4 c))
      nil)))

(defn- write-string
  [s]
  (str "\""
       (apply str (mapv (fn [ch]
                          (let [c (p/char-code ch)]
                            (or (escape-code c) (p/code->str c))))
                        (p/chars-of s)))
       "\""))

(defn- key-name
  "JSON text of a map key. A keyword is written exactly as it was built,
   namespace included: (str k) minus the colon, because splitting it into
   namespace and name differs per host for names holding a slash."
  [k]
  (cond
    (keyword? k) (subs (str k) 1)
    (string? k)  k
    :else        (str k)))

(defn- write-number
  [n]
  (cond
    (not (p/finite? n)) "null"
    (p/integral? n)     (str n)
    :else               (let [s (str n)]
                          (if (str/ends-with? s ".0") s s))))

(defn write-str
  "JSON text for the value X.
   Booleans are tested with `boolean?` then truthiness, not `true?`/`false?`:
   clojurust can produce a `true` that `true?` (an identity check there)
   rejects."
  [x]
  (cond
    (nil? x)        "null"
    (boolean? x)    (if x "true" "false")
    (string? x)     (write-string x)
    (keyword? x)    (write-string (key-name x))
    (symbol? x)     (write-string (str x))
    (number? x)     (write-number x)
    (map? x)        (str "{"
                         (str/join "," (map (fn [[k v]]
                                              (str (write-string (key-name k)) ":" (write-str v)))
                                            x))
                         "}")
    (or (sequential? x) (set? x))
    (str "[" (str/join "," (map write-str x)) "]")
    :else           (write-string (str x))))

;; =============================================================================
;; Decoding
;; =============================================================================

(defn- fail [msg at] {::error msg ::at at})
(defn- failed? [x] (and (map? x) (contains? x ::error)))

(defn- skip-ws
  [cs i]
  (let [n (count cs)]
    (loop [i i]
      (if (and (< i n) (contains? #{32 9 10 13} (nth cs i)))
        (recur (inc i))
        i))))

(defn- hex-value
  [code]
  (cond
    (<= 48 code 57)  (- code 48)
    (<= 65 code 70)  (- code 55)
    (<= 97 code 102) (- code 87)
    :else nil))

(defn- read-hex4
  [cs i]
  (when (<= (+ i 4) (count cs))
    (let [ds (map #(hex-value (nth cs %)) (range i (+ i 4)))]
      (when (every? some? ds)
        (reduce (fn [acc d] (+ (* acc 16) d)) 0 ds)))))

(def ^:private simple-escapes
  {34 "\"" 92 "\\" 47 "/" 98 (p/code->str 8) 102 (p/code->str 12)
   110 "\n" 114 (p/code->str 13) 116 (p/code->str 9)})

(defn- read-unicode-escape
  "cs[j] is the backslash of a \\u escape. Returns [string next-index] or a
   failure; a surrogate pair written as two escapes becomes one code point."
  [cs j]
  (let [u (read-hex4 cs (+ j 2))]
    (cond
      (nil? u) (fail "bad \\u escape" j)

      (and (<= 0xD800 u 0xDBFF)
           (= 92 (get cs (+ j 6)))
           (= 117 (get cs (+ j 7)))
           (some-> (read-hex4 cs (+ j 8)) (#(<= 0xDC00 % 0xDFFF))))
      (let [lo (read-hex4 cs (+ j 8))]
        [(utf8/code-point->str (+ 0x10000 (* (- u 0xD800) 0x400) (- lo 0xDC00))) (+ j 12)])

      :else
      [(utf8/code-point->str u) (+ j 6)])))

(defn- read-string*
  "cs[i] is the opening quote. Returns [string next-index] or a failure.
   No recur sits inside `case`: Basilisp compiles case branches as fns."
  [cs i]
  (let [n (count cs)]
    (loop [j (inc i) parts (transient [])]
      (if (>= j n)
        (fail "unterminated string" i)
        (let [c (nth cs j)
              e (get cs (inc j))]
          (cond
            (= c 34) [(apply str (persistent! parts)) (inc j)]
            (and (= c 92) (nil? e)) (fail "unterminated escape" j)
            (and (= c 92) (contains? simple-escapes e))
            (recur (+ j 2) (conj! parts (get simple-escapes e)))
            (and (= c 92) (= e 117))
            (let [u (read-unicode-escape cs j)]
              (if (failed? u)
                u
                (recur (second u) (conj! parts (first u)))))
            (= c 92) (fail "bad escape" j)
            (< c 0x20) (fail "control character in string" j)
            :else (recur (inc j) (conj! parts (p/code->str c)))))))))

(defn- number-char? [c]
  (or (<= 48 c 57) (= c 45) (= c 43) (= c 46) (= c 101) (= c 69)))

(defn- read-number
  [cs i]
  (let [n (count cs)
        j (loop [j i] (if (and (< j n) (number-char? (nth cs j))) (recur (inc j)) j))
        token (apply str (mapv p/code->str (subvec cs i j)))
        decimal? (some #{46 101 69} (subvec cs i j))]
    (if (text/json-number? token)
      [(if decimal? (p/parse-decimal token) (p/parse-integer token)) j]
      (fail (str "bad number " token) i))))

(defn- literal
  [cs i word value]
  (let [codes (mapv p/char-code (p/chars-of word))
        end   (+ i (count codes))]
    (if (and (<= end (count cs)) (= codes (subvec cs i end)))
      [value end]
      (fail "unexpected token" i))))

(declare read-value)

(defn- read-array
  [cs i opts]
  (let [i (skip-ws cs (inc i))]
    (if (= 93 (get cs i))
      [[] (inc i)]
      (loop [i i acc (transient [])]
        (let [v (read-value cs i opts)]
          (if (failed? v)
            v
            (let [[x j] v
                  j (skip-ws cs j)
                  c (get cs j)]
              (cond
                (= c 44) (recur (skip-ws cs (inc j)) (conj! acc x))
                (= c 93) [(persistent! (conj! acc x)) (inc j)]
                :else (fail "expected , or ]" j)))))))))

(defn- read-object
  [cs i {:keys [key-fn] :as opts}]
  (let [i (skip-ws cs (inc i))]
    (if (= 125 (get cs i))
      [{} (inc i)]
      (loop [i i acc (transient {})]
        (if (not= 34 (get cs i))
          (fail "expected string key" i)
          (let [k (read-string* cs i)]
            (if (failed? k)
              k
              (let [[ks j] k
                    j (skip-ws cs j)]
                (if (not= 58 (get cs j))
                  (fail "expected :" j)
                  (let [v (read-value cs (skip-ws cs (inc j)) opts)]
                    (if (failed? v)
                      v
                      (let [[x m] v
                            m (skip-ws cs m)
                            c (get cs m)
                            acc (assoc! acc (key-fn ks) x)]
                        (cond
                          (= c 44) (recur (skip-ws cs (inc m)) acc)
                          (= c 125) [(persistent! acc) (inc m)]
                          :else (fail "expected , or }" m))))))))))))))

(defn- read-value
  [cs i opts]
  (let [i (skip-ws cs i)
        c (get cs i)]
    (cond
      (nil? c)   (fail "unexpected end of input" i)
      (= c 123)  (read-object cs i opts)
      (= c 91)   (read-array cs i opts)
      (= c 34)   (read-string* cs i)
      (= c 116)  (literal cs i "true" true)
      (= c 102)  (literal cs i "false" false)
      (= c 110)  (literal cs i "null" nil)
      (or (= c 45) (<= 48 c 57)) (read-number cs i)
      :else      (fail "unexpected character" i))))

(defn read-str
  "Result holding the value of the JSON text S.
   OPTS: :key-fn, applied to every object key (default `keyword`).
   A failure is {:error :json/parse-failed :message .. :at index}."
  ([s] (read-str s {}))
  ([s opts]
   (if-not (string? s)
     (r/err :json/parse-failed {:message "input is not a string"})
     (let [cs  (mapv p/char-code (p/chars-of s))
           out (read-value cs 0 {:key-fn (get opts :key-fn keyword)})]
       (if (failed? out)
         (r/err :json/parse-failed {:message (::error out) :at (::at out)})
         (let [[v j] out
               j (skip-ws cs j)]
           (if (< j (count cs))
             (r/err :json/parse-failed {:message "trailing characters" :at j})
             (r/ok v))))))))

(defn read-str!
  "The value of the JSON text S, or nil when S does not parse."
  ([s] (r/value (read-str s)))
  ([s opts] (r/value (read-str s opts))))
