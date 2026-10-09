(ns hive-providers.host.jvm
  "Host adapter for the JVM and Babashka: real ports for the effect ops.

   JDK only (java.net.http, java.security, java.nio), no third-party
   dependency, and every class used here ships in Babashka too.

     (def h (jvm/handlers {:stores {:claude (jvm/file-store-path \".claude/.credentials.json\")}}))
     (fx/run-sync h (chat/chat-program registry params))

   or, as protocol values: (jvm/ports opts) for hive-providers.ports/run.

   Concurrency: `single-flight` runs a program under a per-key monitor and,
   with :file-lock, an OS lock beside the credentials file, so N callers
   sharing one login perform at most one refresh.

   SPDX-License-Identifier: MIT"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-providers.codec.json :as json]
            [hive-providers.effect :as fx]
            [hive-providers.ports :as ports])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandlers]
           [java.nio.channels FileChannel]
           [java.nio.file Files OpenOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute PosixFilePermissions]
           [java.security SecureRandom]
           [java.time Duration]))

;; =============================================================================
;; HTTP
;; =============================================================================

(def default-timeouts
  "Connect and read deadlines. Without a read deadline a provider that stops
   writing mid-response hangs the caller forever."
  {:connect-ms 15000 :read-ms 300000})

(defn- build-client
  [connect-ms]
  (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofMillis connect-ms))
      (.followRedirects HttpClient$Redirect/NORMAL)
      (.build)))

(def ^:private clients (atom {}))

(defn- client-for
  [connect-ms]
  (or (get @clients connect-ms)
      (get (swap! clients assoc connect-ms (build-client connect-ms)) connect-ms)))

(defn- response-headers
  [^HttpResponse resp]
  (into {}
        (map (fn [[k vs]] [(str/lower-case k) (str/join "," vs)]))
        (.map (.headers resp))))

(defn http-request!
  "Perform the request map REQ (see hive-providers.ports/IHttpClient).
   Never throws: a transport failure answers {:status nil :error message}."
  [{:keys [method url headers body timeouts stream? on-line]}]
  (let [{:keys [connect-ms read-ms]} (merge default-timeouts timeouts)
        builder (-> (HttpRequest/newBuilder (URI/create url))
                    (.timeout (Duration/ofMillis read-ms))
                    (.method (str/upper-case (name (or method :get)))
                             (if body
                               (HttpRequest$BodyPublishers/ofString body)
                               (HttpRequest$BodyPublishers/noBody))))]
    (doseq [[k v] headers] (.header builder k v))
    (try
      (if stream?
        (let [resp   (.send ^HttpClient (client-for connect-ms) (.build builder)
                            (HttpResponse$BodyHandlers/ofInputStream))
              status (.statusCode resp)]
          (with-open [rdr (io/reader (.body resp) :encoding "UTF-8")]
            (if (<= 200 status 299)
              {:status  status
               :headers (response-headers resp)
               :lines   (loop [acc (transient [])]
                          (if-let [line (.readLine ^java.io.BufferedReader rdr)]
                            (do (when on-line (on-line line))
                                (recur (conj! acc line)))
                            (persistent! acc)))}
              {:status status :headers (response-headers resp) :body (slurp rdr)})))
        (let [resp (.send ^HttpClient (client-for connect-ms) (.build builder)
                          (HttpResponse$BodyHandlers/ofString))]
          {:status (.statusCode resp) :headers (response-headers resp) :body (.body resp)}))
      (catch Exception e
        {:status nil :error (or (ex-message e) (str (class e)))}))))

;; =============================================================================
;; Entropy, clock, sleep, environment
;; =============================================================================

(def ^:private rng (SecureRandom.))

(defn random-bytes
  "Vector of N bytes (0..255) from SecureRandom."
  [n]
  (let [buf (byte-array n)]
    (.nextBytes ^SecureRandom rng buf)
    (mapv #(bit-and % 0xFF) buf)))

;; =============================================================================
;; File token store
;; =============================================================================

(defn home-path
  "File under the user's home directory."
  [& parts]
  (apply io/file (System/getProperty "user.home") parts))

(defn read-json-file
  "Parsed JSON content of FILE (keyword keys), or nil when absent or
   unreadable."
  [file]
  (let [f (io/file file)]
    (when (.isFile f)
      (json/read-str! (slurp f)))))

(defn- owner-only!
  "Restrict PATH to its owner. A filesystem without POSIX permissions
   (Windows) answers UnsupportedOperationException; there the file keeps the
   directory's ACL, which is the platform's own access model, so the
   exception is the expected answer and not a failure."
  [path]
  (try (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-------"))
       true
       (catch UnsupportedOperationException _ false)))

(defn write-json-file!
  "Replace FILE with VALUE as JSON, atomically, readable by the owner only."
  [file value]
  (let [target (.toPath (io/file file))
        parent (.getParent target)
        _      (when parent (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0)))
        tmp    (.resolveSibling target (str "." (.getFileName target) "." (System/nanoTime) ".tmp"))]
    (try
      (spit (.toFile tmp) (json/write-str value))
      (owner-only! tmp)
      (Files/move tmp target (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE
                                                             StandardCopyOption/REPLACE_EXISTING]))
      (finally (Files/deleteIfExists tmp)))
    nil))

(defrecord FileStore [paths]
  ports/ITokenStore
  (load-token [_ key] (some-> (get paths key key) read-json-file))
  (save-token! [_ key value] (write-json-file! (get paths key key) value)))

(defn file-store
  "ITokenStore keeping each key in a JSON file. PATHS maps a key to a file;
   a key with no entry is used as the path itself."
  ([] (file-store {}))
  ([paths] (->FileStore paths)))

;; =============================================================================
;; Ports and handlers
;; =============================================================================

(defrecord JvmHttp []
  ports/IHttpClient
  (request! [_ req] (http-request! req)))

(defrecord JvmSystem []
  ports/IClock
  (now-ms [_] (System/currentTimeMillis))
  ports/IEntropy
  (random-bytes [_ n] (random-bytes n))
  ports/ISleeper
  (sleep! [_ ms] (Thread/sleep (long ms)) nil)
  ports/IEnvironment
  (env-var [_ name] (System/getenv ^String name)))

(defn ports
  "Port map for hive-providers.ports/run. OPTS: :stores (key -> path map for
   the file store), :store (any ITokenStore, wins over :stores)."
  ([] (ports {}))
  ([{:keys [stores store]}]
   (let [sys (->JvmSystem)]
     {:http    (->JvmHttp)
      :clock   sys
      :entropy sys
      :sleeper sys
      :env     sys
      :store   (or store (file-store (or stores {})))})))

(defn handlers
  "Synchronous effect handlers for every library op. OPTS as for `ports`."
  ([] (handlers {}))
  ([opts] (ports/handlers (ports opts))))

;; =============================================================================
;; Single flight
;; =============================================================================

(def ^:private monitors (atom {}))

(defn- monitor-for
  [k]
  (or (get @monitors k)
      (get (swap! monitors update k #(or % (Object.))) k)))

(defn with-file-lock
  "Call F holding an exclusive OS lock on FILE + \".lock\", which coordinates
   with other processes that lock the same file."
  [file f]
  (let [lock-path (.toPath (io/file (str file ".lock")))]
    (with-open [ch (FileChannel/open lock-path
                                     (into-array OpenOption [StandardOpenOption/CREATE
                                                             StandardOpenOption/WRITE]))]
      (with-open [_ (.lock ch)]
        (f)))))

(defn single-flight
  "Run PROGRAM with HANDLERS while holding the monitor for KEY, so concurrent
   callers for one account serialize. With FILE, also hold an OS lock beside
   it. Programs that re-read their store first (oauth.authority does) then
   see the token the previous holder saved instead of refreshing again."
  ([handlers key program] (single-flight handlers key nil program))
  ([handlers key file program]
   (locking (monitor-for key)
     (if file
       (with-file-lock file #(fx/run-sync handlers program))
       (fx/run-sync handlers program)))))
