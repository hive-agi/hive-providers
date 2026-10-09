(ns hive-providers.text
  "String helpers without regular expressions.

   Regex syntax and support differ per dialect (ClojureElisp cannot compile a
   regex literal at all; Java, JS, Python and Rust engines disagree on
   details), so the library splits and validates text with these instead.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.platform :as p]))

(defn split
  "Pieces of S between occurrences of the literal separator SEP, keeping
   empty pieces, including a trailing one: (split \"a,,b,\" \",\") is
   [\"a\" \"\" \"b\" \"\"]."
  [s sep]
  (let [s (str s) step (count sep)]
    (loop [from 0 acc []]
      (let [idx (str/index-of s sep from)]
        (if idx
          (recur (+ idx step) (conj acc (subs s from idx)))
          (conj acc (subs s from)))))))

(defn split-first
  "[before after] around the first SEP in S, or [s nil] when absent."
  [s sep]
  (let [s (str s)]
    (if-let [idx (str/index-of s sep)]
      [(subs s 0 idx) (subs s (+ idx (count sep)))]
      [s nil])))

(defn- ws-code? [c] (contains? #{32 9 10 13 12} c))

(defn words
  "Non-empty runs of non-whitespace in S."
  [s]
  (let [cs (p/chars-of (str s))
        n  (count cs)]
    (loop [i 0 start nil acc []]
      (cond
        (>= i n) (if start (conj acc (subs (str s) start n)) acc)
        (ws-code? (p/char-code (nth cs i)))
        (recur (inc i) nil (if start (conj acc (subs (str s) start i)) acc))
        :else (recur (inc i) (or start i) acc)))))

(defn lines
  "Lines of S split on \\n, a trailing \\r removed from each; keeps a
   trailing empty piece like `split`."
  [s]
  (mapv (fn [l] (if (str/ends-with? l "\r") (subs l 0 (dec (count l))) l))
        (split s "\n")))

(defn digits?
  "True when S is one or more ASCII digits."
  [s]
  (let [cs (p/chars-of (str s))]
    (and (pos? (count cs))
         (every? (fn [c] (<= 48 (p/char-code c) 57)) cs))))

(defn- code-at [cs i] (when (< i (count cs)) (nth cs i)))

(defn- digit-code? [c] (boolean (and c (<= 48 c 57))))

(defn- digits-end
  "Index past the run of digits in CS starting at I."
  [cs i]
  (loop [j i] (if (digit-code? (code-at cs j)) (recur (inc j)) j)))

(defn json-number?
  "True when S is a number in JSON grammar: -?(0|[1-9]d*)(.d+)?([eE][+-]?d+)?"
  [s]
  (let [cs (mapv p/char-code (p/chars-of (str s)))
        i0 (if (= 45 (code-at cs 0)) 1 0)
        i1 (cond
             (= 48 (code-at cs i0))         (inc i0)
             (digit-code? (code-at cs i0))  (digits-end cs i0)
             :else                          nil)
        i2 (when i1
             (if (= 46 (code-at cs i1))
               (let [j (digits-end cs (inc i1))] (when (> j (inc i1)) j))
               i1))
        i3 (when i2
             (if (contains? #{101 69} (code-at cs i2))
               (let [k (if (contains? #{43 45} (code-at cs (inc i2))) (+ i2 2) (inc i2))
                     j (digits-end cs k)]
                 (when (> j k) j))
               i2))]
    (= i3 (count cs))))
