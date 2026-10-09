(ns hive-providers.platform
  "Host primitives: the ONE namespace that branches per dialect.

   Every other namespace in hive-providers is written against these
   functions and plain clojure.core, so porting the library to a new
   dialect means checking this file and nothing else.

   Feature keys, in the order they are tried (first match wins on most
   readers; keep :default last):

     :cljw   ClojureWasm (also claims :clj, so it must come first)
     :rust   clojurust (cljrs)
     :lpy    Basilisp
     :cljel  ClojureElisp (resolved by the emit step; clel has no #?)
     :cljs   ClojureScript
     :cljr   ClojureCLR
     :cljd   ClojureDart
     :clj    JVM Clojure and Babashka

   Strings: JVM, JS, CLR and Dart strings are UTF-16 code units, so a code
   point above U+FFFF is two chars. ClojureWasm, clojurust, Basilisp and
   Emacs index code points. `utf16-strings?` tells the UTF-8 codec which
   world it is in.

   SPDX-License-Identifier: MIT")

(def dialect
  "Keyword naming the dialect this code was read by."
  #?(:cljw :cljw
     :rust :cljrs
     :lpy :lpy
     :cljel :cljel
     :cljs :cljs
     :cljr :cljr
     :cljd :cljd
     :bb :bb
     :clj :clj
     :default :unknown))

(def utf16-strings?
  "True when a string is indexed by UTF-16 code unit."
  #?(:cljw false
     :rust false
     :lpy false
     :cljel false
     :cljs true
     :cljr true
     :cljd true
     :clj true
     :default false))

(defn chars-of
  "The characters of S as a vector, for O(1) indexed access on every host.
   In Emacs a character is its integer code, so the vector holds integers."
  [s]
  #?(:cljel (vconcat s)
     :default (vec s)))

(defn char-code
  "Integer code of the character C (one element of `chars-of`)."
  [c]
  #?(:lpy (python/ord c)
     :cljel c
     :cljs (.charCodeAt c 0)
     :default (int c)))

(defn code->str
  "One-character string for the code N. N must be a single unit on UTF-16
   hosts (callers split astral code points first)."
  [n]
  #?(:lpy (python/chr n)
     :cljel (string n)
     :cljs (.fromCharCode js/String n)
     :default (str (char n))))

(defn u32
  "N reduced to an unsigned 32-bit integer."
  [n]
  #?(:cljs (unsigned-bit-shift-right n 0)
     :default (bit-and n 0xffffffff)))

(defn shr32
  "Logical right shift of the unsigned 32-bit N by K bits."
  [n k]
  #?(:cljs (unsigned-bit-shift-right n k)
     :default (bit-shift-right (bit-and n 0xffffffff) k)))

(defn shl32
  "Left shift of the unsigned 32-bit N by K bits, truncated to 32 bits."
  [n k]
  (u32 (bit-shift-left n k)))

(defn parse-decimal
  "Number for a JSON-style decimal token with a fraction or exponent."
  [s]
  #?(:cljs (js/parseFloat s)
     :cljel (string-to-number s)
     :default (parse-double s)))

(defn parse-integer
  "Integer for a token of decimal digits with an optional leading minus."
  [s]
  #?(:cljs (js/parseInt s 10)
     :cljel (string-to-number s)
     :default (parse-long s)))

(defn integral?
  "True when N is an integer value (not a float)."
  [n]
  #?(:cljs (and (number? n) (== n (js/Math.floor n)) (not= n ##Inf) (not= n ##-Inf))
     :default (integer? n)))

(defn finite?
  "True when N is a finite number."
  [n]
  #?(:cljs (js/isFinite n)
     :cljel (and (number? n) (= n n) (not= n (/ 1.0 0.0)) (not= n (/ -1.0 0.0)))
     :default (and (number? n) (== n n) (not= n ##Inf) (not= n ##-Inf))))
