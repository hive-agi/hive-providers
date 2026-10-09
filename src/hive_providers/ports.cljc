(ns hive-providers.ports
  "Ports: the host capabilities a harness injects, as protocols.

   A harness that prefers objects to handler maps implements these and turns
   them into effect handlers with `handlers`. Tests implement them with stub
   records; no flow in the library names a concretion.

   SPDX-License-Identifier: MIT"
  (:require [hive-providers.effect :as fx]))

(defprotocol IHttpClient
  (request! [this request]
    "Perform REQUEST, a map:
       :method   keyword (:get :post ...)
       :url      string
       :headers  map of string to string
       :body     string or nil
       :timeouts {:connect-ms n :read-ms n}, optional
       :stream?  true to receive the body as a vector of lines
       :on-line  optional fn called with each line as it arrives (stream)
     Return {:status int :headers map-of-lowercase-strings :body string}
     ({:lines [..]} instead of :body when :stream?), or
     {:status nil :error message} when no response arrived.

     Port methods carry no leading dash: Basilisp munges `-name` protocol
     methods inconsistently, so a dashed method is unreachable there."))

(defprotocol IClock
  (now-ms [this] "Current epoch milliseconds."))

(defprotocol IEntropy
  (random-bytes [this n] "Vector of N cryptographically random bytes (0..255)."))

(defprotocol ISleeper
  (sleep! [this ms] "Block for MS milliseconds."))

(defprotocol IEnvironment
  (env-var [this name] "Value of the environment variable NAME, or nil."))

(defprotocol ITokenStore
  (load-token [this key] "The token map stored under KEY, or nil.")
  (save-token! [this key value] "Store the token map VALUE under KEY."))

(defn handlers
  "Effect handlers for whichever ports PORTS (a map) carries:
   {:http IHttpClient :clock IClock :entropy IEntropy :sleeper ISleeper
    :env IEnvironment :store ITokenStore}."
  [{:keys [http clock entropy sleeper env store]}]
  (cond-> {}
    http    (assoc :http/request (fn [req] (request! http req)))
    clock   (assoc :clock/now-ms (fn [_] (now-ms clock)))
    entropy (assoc :entropy/bytes (fn [{:keys [n]}] (random-bytes entropy n)))
    sleeper (assoc :sleep/ms (fn [{:keys [ms]}] (sleep! sleeper ms)))
    env     (assoc :env/get (fn [{:keys [name]}] (env-var env name)))
    store   (assoc :store/load (fn [{:keys [key]}] (load-token store key))
                   :store/save (fn [{:keys [key value]}] (save-token! store key value)))))

(defn run
  "Run PROGRAM synchronously against PORTS (see `handlers`) and EXTRA
   handlers."
  ([ports program] (run ports {} program))
  ([ports extra program]
   (fx/run-sync (merge (handlers ports) extra) program)))

;; =============================================================================
;; Stub ports, for tests and dry runs
;; =============================================================================

(defrecord FixedClock [now]
  IClock
  (now-ms [_] now))

(defrecord CountingEntropy [counter]
  IEntropy
  (random-bytes [_ n]
    (let [start (swap! counter + n)]
      (mapv #(bit-and 0xFF (+ (- start n) %)) (range n)))))

(defrecord NoSleep [slept]
  ISleeper
  (sleep! [_ ms] (swap! slept conj ms) nil))

(defrecord MapEnv [vars]
  IEnvironment
  (env-var [_ name] (get vars name)))

(defrecord AtomStore [state]
  ITokenStore
  (load-token [_ key] (get @state key))
  (save-token! [_ key value] (swap! state assoc key value) nil))

(defrecord ScriptedHttp [responses seen]
  IHttpClient
  (request! [_ request]
    (swap! seen conj request)
    (let [[resp & more] @responses]
      (reset! responses (vec more))
      (or resp {:status nil :error "no scripted response left"}))))

(defn scripted-http
  "An IHttpClient answering with RESPONSES in order, recording each request
   in the atom (:seen client)."
  [responses]
  (->ScriptedHttp (atom (vec responses)) (atom [])))

(defn atom-store
  "An ITokenStore over an atom, seeded with INITIAL."
  ([] (atom-store {}))
  ([initial] (->AtomStore (atom initial))))
