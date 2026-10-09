(ns hive-providers.codec.base64
  "Base64 (RFC 4648 section 4) and base64url (section 5) over byte vectors.

   Pure. `decode` accepts either alphabet, with or without padding, so a
   JWT segment and a padded standard string both decode.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.codec.utf8 :as utf8]
            [hive-providers.platform :as p]))

(def ^:private std-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(def ^:private url-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(def ^:private decode-table
  (let [std (p/chars-of std-alphabet)
        url (p/chars-of url-alphabet)]
    (merge (zipmap (map p/char-code std) (range 64))
           (zipmap (map p/char-code url) (range 64)))))

(def ^:private std-symbols
  (mapv (comp p/code->str p/char-code) (p/chars-of std-alphabet)))

(def ^:private url-symbols
  (mapv (comp p/code->str p/char-code) (p/chars-of url-alphabet)))

(defn encode
  "Base64 text for BYTES (integers 0..255).
   OPTS: :url? selects the URL-safe alphabet, :pad? (default true) adds '='."
  ([bytes] (encode bytes {}))
  ([bytes opts]
   (let [alpha (if (get opts :url?) url-symbols std-symbols)
         pad?  (get opts :pad? true)
         bs    (vec bytes)
         n     (count bs)]
     (loop [i 0 out (transient [])]
       (if (>= i n)
         (apply str (persistent! out))
         (let [b0   (bit-and (nth bs i) 0xFF)
               has1 (< (inc i) n)
               has2 (< (+ i 2) n)
               b1   (if has1 (bit-and (nth bs (inc i)) 0xFF) 0)
               b2   (if has2 (bit-and (nth bs (+ i 2)) 0xFF) 0)
               out  (-> out
                        (conj! (nth alpha (bit-shift-right b0 2)))
                        (conj! (nth alpha (bit-or (bit-shift-left (bit-and b0 3) 4)
                                                  (bit-shift-right b1 4)))))
               out  (cond
                      has1 (conj! out (nth alpha (bit-or (bit-shift-left (bit-and b1 15) 2)
                                                         (bit-shift-right b2 6))))
                      pad? (conj! out "=")
                      :else out)
               out  (cond
                      has2 (conj! out (nth alpha (bit-and b2 63)))
                      pad? (conj! out "=")
                      :else out)]
           (recur (+ i 3) out)))))))

(defn encode-url
  "Unpadded base64url text for BYTES, the form PKCE and JWTs use."
  [bytes]
  (encode bytes {:url? true :pad? false}))

(defn decode
  "Byte vector for base64 or base64url TEXT, or nil when TEXT holds a
   character outside both alphabets. Padding and whitespace are ignored."
  [text]
  (let [codes (keep (fn [c]
                      (let [k (p/char-code c)]
                        (cond
                          (contains? #{61 32 10 13 9} k) nil
                          :else (get decode-table k ::bad))))
                    (p/chars-of (str text)))]
    (when-not (some #{::bad} codes)
      (let [v (vec codes)
            n (count v)]
        (loop [i 0 out (transient [])]
          (if (>= (inc i) n)
            (persistent! out)
            (let [c0 (nth v i)
                  c1 (nth v (inc i))
                  c2 (when (< (+ i 2) n) (nth v (+ i 2)))
                  c3 (when (< (+ i 3) n) (nth v (+ i 3)))
                  out (conj! out (bit-and 0xFF (bit-or (bit-shift-left c0 2)
                                                       (bit-shift-right c1 4))))
                  out (if c2
                        (conj! out (bit-and 0xFF (bit-or (bit-shift-left c1 4)
                                                         (bit-shift-right c2 2))))
                        out)
                  out (if c3
                        (conj! out (bit-and 0xFF (bit-or (bit-shift-left c2 6) c3)))
                        out)]
              (recur (+ i 4) out))))))))

(defn encode-str
  "Base64 text of the UTF-8 bytes of S."
  ([s] (encode (utf8/encode s)))
  ([s opts] (encode (utf8/encode s) opts)))

(defn decode-str
  "UTF-8 string held in base64 TEXT, or nil when TEXT is not base64."
  [text]
  (some-> (decode text) utf8/decode))
