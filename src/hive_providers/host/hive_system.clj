(ns hive-providers.host.hive-system
  "Host adapter over hive-system, for harnesses that already run it.

   PROVIDED SCOPE: hive-system is not a dependency of hive-providers. This
   namespace loads only where the consumer has hive-system on its classpath
   (hive-agent, hive-mcp); every other namespace loads without it.

   What hive-system supplies here:
     secrets  :secret/get resolves \"pass:Venice/key\" (or any backend
              registered with hive-system.secrets.registry) and \"env:X\";
              the plaintext is exposed only at that call.
     locking  `single-flight` holds hive-system's advisory file lock
              (IFilesystem/fs-lock!) beside the credentials file, plus a
              per-key monitor, so processes and threads sharing one login
              refresh once.

   Credential files are still written by hive-providers.host.jvm (atomic and
   owner-only from the first byte): IFilesystem/fs-atomic-write! has no
   permissions option, and a token file must never exist world-readable,
   even for the instant between a rename and a chmod.

   SPDX-License-Identifier: MIT"
  (:require [hive-dsl.result :as dsl]
            [hive-providers.effect :as fx]
            [hive-providers.host.jvm :as jvm]
            [hive-providers.ports :as ports]
            [hive-system.fs.filesystem :as hs-fs]
            [hive-system.protocols :as hs]
            [hive-system.secrets.core :as secret]
            [hive-system.secrets.registry :as secrets]))

;; =============================================================================
;; Secrets
;; =============================================================================

(defrecord HiveSystemSecrets [env fetch]
  ports/ISecretStore
  (secret [_ ref]
    (when-let [{:keys [backend key] :as parsed} (ports/parse-secret-ref ref)]
      (if (= :env backend)
        (ports/env-var env key)
        (let [res (fetch (select-keys parsed [:backend :key :opts]))]
          (when (dsl/ok? res)
            (secret/expose (:ok res))))))))

(defn hive-system-secrets
  "ISecretStore over hive-system's secret registry. FETCH defaults to
   hive-system.secrets.registry/fetch (inject a stub in tests)."
  ([] (hive-system-secrets {}))
  ([{:keys [env fetch]}]
   (->HiveSystemSecrets (or env (jvm/->JvmSystem)) (or fetch secrets/fetch))))

;; =============================================================================
;; Handlers
;; =============================================================================

(defn ports
  "Port map: hive-providers.host.jvm's, with hive-system secrets.
   OPTS as for jvm/ports, plus :fetch for the secret registry."
  ([] (ports {}))
  ([opts]
   (assoc (jvm/ports opts) :secrets (hive-system-secrets opts))))

(defn handlers
  "Synchronous effect handlers with hive-system secrets."
  ([] (handlers {}))
  ([opts] (ports/handlers (ports opts))))

;; =============================================================================
;; Single flight under hive-system's file lock
;; =============================================================================

(def default-lock-timeout-ms 30000)

(defn single-flight
  "Run PROGRAM with HANDLERS holding the per-KEY monitor and hive-system's
   advisory lock on LOCK-PATH + \".lock\". Returns the program's Result, or
   the lock's err Result (:fs/lock-failed) when it cannot be taken in time.
   FS is a hive-system IFilesystem (default the babashka.fs one)."
  ([handlers key lock-path program]
   (single-flight (hs-fs/->FsFilesystem) handlers key lock-path program default-lock-timeout-ms))
  ([fs handlers key lock-path program timeout-ms]
   (jvm/with-monitor
     key
     (fn []
       (let [locked (hs/fs-lock! fs (str lock-path ".lock") timeout-ms)]
         (if (dsl/err? locked)
           locked
           (let [release (get-in locked [:ok :lock :release])]
             (try
               (fx/run-sync handlers program)
               (finally (release))))))))))
