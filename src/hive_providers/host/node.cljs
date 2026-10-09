(ns hive-providers.host.node
  "Host adapter for ClojureScript on Node.js (18+): asynchronous effect
   handlers for hive-providers.effect/run-async.

     (fx/run-async (node/handlers {:stores {:codex (node/home-path \".codex/auth.json\")}})
                   (chat/chat-program registry params)
                   (fn [result] ...))

   HTTP uses the global fetch; a streamed body is read chunk by chunk through
   hive-providers.openai.response/sse-feed, so :on-line sees each line as it
   arrives.

   SPDX-License-Identifier: MIT"
  (:require [clojure.string :as str]
            [hive-providers.codec.json :as json]
            [hive-providers.openai.response :as response]))

(def ^:private fs (js/require "fs"))
(def ^:private os (js/require "os"))
(def ^:private path (js/require "path"))
(def ^:private node-crypto (js/require "crypto"))

(def default-timeouts {:connect-ms 15000 :read-ms 300000})

(defn home-path
  "File under the user's home directory."
  [rel]
  (.join path (.homedir os) rel))

(defn- headers->map
  [^js h]
  (let [acc (atom {})]
    (.forEach h (fn [v k] (swap! acc assoc (str/lower-case k) v)))
    @acc))

(defn- read-lines!
  "Read the web stream BODY to its end, calling ON-LINE per line; then call
   DONE with the vector of lines."
  [^js body on-line done fail]
  (let [reader  (.getReader body)
        decoder (js/TextDecoder. "utf-8")
        lines   (atom [])
        state   (atom {:buffer ""})
        emit    (fn [ls] (doseq [l ls] (when on-line (on-line l)) (swap! lines conj l)))]
    (letfn [(pump []
              (-> (.read reader)
                  (.then (fn [^js chunk]
                           (if (.-done chunk)
                             (do (when (seq (:buffer @state)) (emit [(:buffer @state)]))
                                 (done @lines))
                             (let [[s ls] (response/sse-feed @state (.decode decoder (.-value chunk) #js {:stream true}))]
                               (reset! state s)
                               (emit ls)
                               (pump)))))
                  (.catch fail)))]
      (pump))))

(defn http-request!
  "Perform the request map REQ, calling CB with the response map. Never
   throws: a transport failure answers {:status nil :error message}."
  [{:keys [method url headers body timeouts stream? on-line]} cb]
  (let [{:keys [read-ms]} (merge default-timeouts timeouts)
        ctl   (js/AbortController.)
        timer (js/setTimeout #(.abort ctl) read-ms)
        fail  (fn [e] (js/clearTimeout timer) (cb {:status nil :error (str (.-message e))}))]
    (-> (js/fetch url (clj->js (cond-> {:method (str/upper-case (name (or method :get)))
                                         :headers headers
                                         :signal (.-signal ctl)}
                                  body (assoc :body body))))
        (.then (fn [^js resp]
                 (let [status (.-status resp)
                       hs     (headers->map (.-headers resp))]
                   (if (and stream? (<= 200 status 299))
                     (read-lines! (.-body resp) on-line
                                  (fn [lines] (js/clearTimeout timer)
                                    (cb {:status status :headers hs :lines lines}))
                                  fail)
                     (-> (.text resp)
                         (.then (fn [text] (js/clearTimeout timer)
                                  (cb {:status status :headers hs :body text})))
                         (.catch fail))))))
        (.catch fail))))

(defn random-bytes
  "Vector of N random bytes from the platform CSPRNG."
  [n]
  (vec (array-seq (.randomBytes node-crypto n))))

(defn read-json-file
  "Parsed JSON content of FILE (keyword keys), or nil when absent."
  [file]
  (when (.existsSync fs file)
    (json/read-str! (.readFileSync fs file "utf8"))))

(defn write-json-file!
  "Replace FILE with VALUE as JSON, atomically, mode 0600."
  [file value]
  (.mkdirSync fs (.dirname path file) #js {:recursive true})
  (let [tmp (str file "." (js/Date.now) ".tmp")]
    (.writeFileSync fs tmp (json/write-str value) #js {:mode 0600})
    (.renameSync fs tmp file)
    nil))

(defn handlers
  "Asynchronous effect handlers for every library op.
   OPTS: :stores maps a store key to a file path (a key with no entry is the
   path itself)."
  ([] (handlers {}))
  ([{:keys [stores]}]
   {:http/request  http-request!
    :clock/now-ms  (fn [_ cb] (cb (js/Date.now)))
    :entropy/bytes (fn [{:keys [n]} cb] (cb (random-bytes n)))
    :sleep/ms      (fn [{:keys [ms]} cb] (js/setTimeout #(cb nil) ms))
    :env/get       (fn [{:keys [name]} cb] (cb (aget (.-env js/process) name)))
    :store/load    (fn [{:keys [key]} cb] (cb (read-json-file (get stores key key))))
    :store/save    (fn [{:keys [key value]} cb] (cb (write-json-file! (get stores key key) value)))}))
