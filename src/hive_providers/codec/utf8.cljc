(ns hive-providers.codec.utf8
  "UTF-8 between strings and byte vectors (integers 0..255).

   Pure. Works on hosts whose strings are UTF-16 code units (surrogate pairs
   are combined on encode and split on decode) and on hosts whose strings
   are code points.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.platform :as p]))

(defn code-points
  "The Unicode code points of S, in order."
  [s]
  (let [cs (p/chars-of s)
        n  (count cs)]
    (loop [i 0 acc (transient [])]
      (if (>= i n)
        (persistent! acc)
        (let [c (p/char-code (nth cs i))]
          (if (and (<= 0xD800 c 0xDBFF) (< (inc i) n))
            (let [d (p/char-code (nth cs (inc i)))]
              (if (<= 0xDC00 d 0xDFFF)
                (recur (+ i 2) (conj! acc (+ 0x10000
                                              (* (- c 0xD800) 0x400)
                                              (- d 0xDC00))))
                (recur (inc i) (conj! acc c))))
            (recur (inc i) (conj! acc c))))))))

(defn code-point->str
  "String holding the single code point CP."
  [cp]
  (if (and p/utf16-strings? (> cp 0xFFFF))
    (let [v (- cp 0x10000)]
      (str (p/code->str (+ 0xD800 (quot v 0x400)))
           (p/code->str (+ 0xDC00 (rem v 0x400)))))
    (p/code->str cp)))

(defn- encode-cp
  [acc cp]
  (cond
    (< cp 0x80)    (conj! acc cp)
    (< cp 0x800)   (-> acc
                       (conj! (bit-or 0xC0 (bit-shift-right cp 6)))
                       (conj! (bit-or 0x80 (bit-and cp 0x3F))))
    (< cp 0x10000) (-> acc
                       (conj! (bit-or 0xE0 (bit-shift-right cp 12)))
                       (conj! (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3F)))
                       (conj! (bit-or 0x80 (bit-and cp 0x3F))))
    :else          (-> acc
                       (conj! (bit-or 0xF0 (bit-shift-right cp 18)))
                       (conj! (bit-or 0x80 (bit-and (bit-shift-right cp 12) 0x3F)))
                       (conj! (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3F)))
                       (conj! (bit-or 0x80 (bit-and cp 0x3F))))))

(defn encode
  "UTF-8 bytes of the string S, as a vector of integers 0..255."
  [s]
  (persistent! (reduce encode-cp (transient []) (code-points s))))

(defn- byte-at
  [bs n i]
  (if (< i n) (bit-and (nth bs i) 0xFF) 0))

(defn- cont-at
  [bs n i]
  (bit-and (byte-at bs n i) 0x3F))

(defn decode
  "String for the UTF-8 BYTES. A malformed sequence decodes to U+FFFD.
   Bitwise calls stay binary (ClojureWasm takes two args only) and helpers
   are top-level fns, not let-bound ones (ClojureElisp cannot call those)."
  [bytes]
  (let [bs (vec bytes)
        n  (count bs)]
    (loop [i 0 parts (transient [])]
      (if (>= i n)
        (apply str (persistent! parts))
        (let [b0 (byte-at bs n i)]
          (cond
            (< b0 0x80)
            (recur (inc i) (conj! parts (p/code->str b0)))

            (and (= 0xC0 (bit-and b0 0xE0)) (< (inc i) n))
            (recur (+ i 2) (conj! parts (code-point->str
                                         (+ (* (bit-and b0 0x1F) 64)
                                            (cont-at bs n (inc i))))))

            (and (= 0xE0 (bit-and b0 0xF0)) (< (+ i 2) n))
            (recur (+ i 3) (conj! parts (code-point->str
                                         (+ (* (bit-and b0 0x0F) 4096)
                                            (* (cont-at bs n (inc i)) 64)
                                            (cont-at bs n (+ i 2))))))

            (and (= 0xF0 (bit-and b0 0xF8)) (< (+ i 3) n))
            (recur (+ i 4) (conj! parts (code-point->str
                                         (+ (* (bit-and b0 0x07) 262144)
                                            (* (cont-at bs n (inc i)) 4096)
                                            (* (cont-at bs n (+ i 2)) 64)
                                            (cont-at bs n (+ i 3))))))

            :else
            (recur (inc i) (conj! parts (p/code->str 0xFFFD)))))))))
