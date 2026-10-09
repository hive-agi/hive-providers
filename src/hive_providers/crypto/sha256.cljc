(ns hive-providers.crypto.sha256
  "SHA-256 (FIPS 180-4) in portable Clojure.

   The digest PKCE needs (S256 code challenge) and the one the Kimi CLI uses
   to name its credential slots. A host with a native digest can be faster,
   but this one gives the same bytes on every dialect, which is what lets the
   PKCE layer stay pure.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.codec.utf8 :as utf8]
            [hive-providers.platform :as p]))

(def ^:private k-constants
  [0x428a2f98 0x71374491 0xb5c0fbcf 0xe9b5dba5 0x3956c25b 0x59f111f1 0x923f82a4 0xab1c5ed5
   0xd807aa98 0x12835b01 0x243185be 0x550c7dc3 0x72be5d74 0x80deb1fe 0x9bdc06a7 0xc19bf174
   0xe49b69c1 0xefbe4786 0x0fc19dc6 0x240ca1cc 0x2de92c6f 0x4a7484aa 0x5cb0a9dc 0x76f988da
   0x983e5152 0xa831c66d 0xb00327c8 0xbf597fc7 0xc6e00bf3 0xd5a79147 0x06ca6351 0x14292967
   0x27b70a85 0x2e1b2138 0x4d2c6dfc 0x53380d13 0x650a7354 0x766a0abb 0x81c2c92e 0x92722c85
   0xa2bfe8a1 0xa81a664b 0xc24b8b70 0xc76c51a3 0xd192e819 0xd6990624 0xf40e3585 0x106aa070
   0x19a4c116 0x1e376c08 0x2748774c 0x34b0bcb5 0x391c0cb3 0x4ed8aa4a 0x5b9cca4f 0x682e6ff3
   0x748f82ee 0x78a5636f 0x84c87814 0x8cc70208 0x90befffa 0xa4506ceb 0xbef9a3f7 0xc67178f2])

(def ^:private h-initial
  [0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19])

(defn- rotr [x n]
  (p/u32 (bit-or (p/shr32 x n) (p/shl32 x (- 32 n)))))

(defn- add32 [& xs]
  (p/u32 (reduce + 0 xs)))

(defn- pad
  "Message BYTES padded to a multiple of 64 bytes, with the bit length."
  [bytes]
  (let [len    (count bytes)
        bitlen (* 8 len)
        zeros  (mod (- 56 (inc len)) 64)
        len-bytes (mapv (fn [shift]
                          (bit-and 0xFF (quot bitlen (reduce * 1 (repeat shift 256)))))
                        (range 7 -1 -1))]
    (-> (vec bytes)
        (conj 0x80)
        (into (repeat zeros 0))
        (into len-bytes))))

(defn- xor3
  "Three-way xor through binary calls (ClojureWasm's bit-xor takes two)."
  [a b c]
  (bit-xor (bit-xor a b) c))

(defn- block-words
  [bs offset]
  (mapv (fn [w]
          (let [o (+ offset (* 4 w))]
            (p/u32 (+ (* (nth bs o) 16777216)
                      (* (nth bs (+ o 1)) 65536)
                      (* (nth bs (+ o 2)) 256)
                      (nth bs (+ o 3))))))
        (range 16)))

(defn- schedule
  [words]
  (loop [w words i 16]
    (if (= i 64)
      w
      (let [w15 (nth w (- i 15))
            w2  (nth w (- i 2))
            s0  (xor3 (rotr w15 7) (rotr w15 18) (p/shr32 w15 3))
            s1  (xor3 (rotr w2 17) (rotr w2 19) (p/shr32 w2 10))]
        (recur (conj w (add32 (nth w (- i 16)) s0 (nth w (- i 7)) s1)) (inc i))))))

(defn- compress
  "One 64-round block. The round counter is `i`, not `t`: `t` is a constant
   in Emacs Lisp, and ClojureElisp passes local names through."
  [h w]
  (loop [i 0 a (nth h 0) b (nth h 1) c (nth h 2) d (nth h 3)
         e (nth h 4) f (nth h 5) g (nth h 6) hh (nth h 7)]
    (if (= i 64)
      (mapv add32 h [a b c d e f g hh])
      (let [s1  (xor3 (rotr e 6) (rotr e 11) (rotr e 25))
            ch  (p/u32 (bit-xor (bit-and e f) (bit-and (p/u32 (bit-not e)) g)))
            t1  (add32 hh s1 ch (nth k-constants i) (nth w i))
            s0  (xor3 (rotr a 2) (rotr a 13) (rotr a 22))
            maj (xor3 (bit-and a b) (bit-and a c) (bit-and b c))
            t2  (add32 s0 maj)]
        (recur (inc i) (add32 t1 t2) a b c (add32 d t1) e f g)))))

(defn digest
  "SHA-256 of BYTES (integers 0..255) as a vector of 32 bytes."
  [bytes]
  (let [bs (pad bytes)
        h  (reduce (fn [h offset] (compress h (schedule (block-words bs offset))))
                   h-initial
                   (range 0 (count bs) 64))]
    (vec (mapcat (fn [word]
                   [(p/shr32 word 24)
                    (bit-and (p/shr32 word 16) 0xFF)
                    (bit-and (p/shr32 word 8) 0xFF)
                    (bit-and word 0xFF)])
                 h))))

(defn digest-str
  "SHA-256 of the UTF-8 bytes of S."
  [s]
  (digest (utf8/encode s)))

(def ^:private hex-digits (mapv (comp p/code->str p/char-code) (p/chars-of "0123456789abcdef")))

(defn hex
  "Lowercase hex text for BYTES."
  [bytes]
  (reduce (fn [acc b] (str acc (nth hex-digits (quot b 16)) (nth hex-digits (rem b 16))))
          ""
          bytes))

(defn hex-digest
  "Lowercase hex SHA-256 of the string S."
  [s]
  (hex (digest-str s)))
