(ns hive-providers.effect
  "Effects as data: the library's only I/O seam.

   Every flow in hive-providers (an OAuth refresh, a device-code poll, a chat
   completion with retries) is a PROGRAM: a value that is either finished or
   asks the host to perform one operation and says how to continue.

     {::done result}                         finished
     {::op op ::args args ::k (fn [value] program)}   asks for op

   The library never performs I/O. A host runs a program with a map of
   handlers, one per op it supports:

     (run-sync handlers program)           handler: (fn [args] value)
     (run-async handlers program on-done)  handler: (fn [args callback])

   Ops used by the library (a host provides the ones its flows need):

     :http/request    args: request map     value: response map
     :clock/now-ms    args: nil             value: epoch milliseconds
     :entropy/bytes   args: {:n count}      value: vector of n random bytes
     :sleep/ms        args: {:ms n}         value: nil
     :env/get         args: {:name s}       value: string or nil
     :store/load      args: {:key k}        value: stored map or nil
     :store/save      args: {:key k :value m}   value: nil

   Hosts may add their own ops; `perform` takes any keyword.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.result :as r]))

(defn done
  "A finished program yielding V."
  [v]
  {::done v})

(defn done?
  "True when PROGRAM is finished."
  [program]
  (contains? program ::done))

(defn perform
  "A program asking for OP with ARGS, yielding the value the host returns."
  ([op] (perform op nil))
  ([op args]
   {::op op ::args args ::k done}))

(defn bind
  "Program running PROGRAM, then (F value) as the rest."
  [program f]
  (if (done? program)
    (f (::done program))
    (let [k (::k program)]
      (assoc program ::k (fn [v] (bind (k v) f))))))

(defn fmap
  "Program yielding (F value) of PROGRAM."
  [program f]
  (bind program (fn [v] (done (f v)))))

(defn then-ok
  "Program over a PROGRAM that yields a Result: on ok, continue with
   (F value), which must be a program yielding a Result; on err, stop with
   the err."
  [program f]
  (bind program (fn [res] (if (r/ok? res) (f (r/value res)) (done res)))))

(defn of-result
  "Program yielding the Result RES."
  [res]
  (done res))

(defn sequence-programs
  "Program yielding a vector of the values of PROGRAMS, run in order."
  [programs]
  (reduce (fn [acc p] (bind acc (fn [vs] (fmap p (fn [v] (conj vs v))))))
          (done [])
          programs))

(defn run-sync
  "Run PROGRAM to its value. HANDLERS maps an op to (fn [args] value).
   An op with no handler stops the run with
   (r/err :effect/unhandled {:op op})."
  [handlers program]
  (loop [p program]
    (if (done? p)
      (::done p)
      (let [op (::op p)
            h  (get handlers op)]
        (if h
          (recur ((::k p) (h (::args p))))
          (r/err :effect/unhandled {:op op}))))))

(defn run-async
  "Run PROGRAM, calling ON-DONE with its value. HANDLERS maps an op to
   (fn [args callback]); the handler calls callback with the op's value.
   An op with no handler finishes with (r/err :effect/unhandled {:op op})."
  [handlers program on-done]
  (letfn [(step [p]
            (if (done? p)
              (on-done (::done p))
              (let [op (::op p)
                    h  (get handlers op)]
                (if h
                  (h (::args p) (fn [v] (step ((::k p) v))))
                  (on-done (r/err :effect/unhandled {:op op}))))))]
    (step program)))

(defn sync->async-handlers
  "Async handlers wrapping synchronous HANDLERS."
  [handlers]
  (into {} (map (fn [[op h]] [op (fn [args cb] (cb (h args)))])) handlers))

(defn ops-of
  "The ops PROGRAM asks for before it needs a host answer: only the first,
   since later ops depend on values. Useful in tests and dry runs."
  [program]
  (when-not (done? program) [(::op program) (::args program)]))
