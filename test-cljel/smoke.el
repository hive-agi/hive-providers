;;; smoke.el --- run hive-providers' ClojureElisp build in a batch Emacs  -*- lexical-binding: t; -*-
;;
;; Driven by `bin/emit-dialect cljel --test', which puts target/cljel-el on the
;; load-path. Needs the clel runtime (clel.el): set CLEL_RUNTIME to its
;; directory, or have clojure-elisp checked out beside this repo's parent.
;;
;; SPDX-License-Identifier: MIT

(let ((rt (or (getenv "CLEL_RUNTIME")
              (expand-file-name "~/PP/clojure-elisp/resources/clojure-elisp"))))
  (add-to-list 'load-path rt))
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'hive-providers-cljel-prelude)

(defvar smoke-failures 0)
(defvar smoke-passes 0)

(defmacro smoke-check (label expected form)
  `(condition-case err
       (let ((actual ,form))
         (if (equal actual ,expected)
             (setq smoke-passes (1+ smoke-passes))
           (setq smoke-failures (1+ smoke-failures))
           (message "FAIL %s\n  expected: %S\n  actual:   %S" ,label ,expected actual)))
     (error (setq smoke-failures (1+ smoke-failures))
            (message "ERROR %s: %S" ,label err))))

(defvar smoke-gaps 0)

(defmacro smoke-gap (label why expected form)
  "A check blocked by a known ClojureElisp gap (WHY). Reported, not failed,
   so the gate still guards what works; it says when the gap closes."
  `(condition-case nil
       (if (equal ,form ,expected)
           (message "GAP CLOSED %s: promote it to smoke-check" ,label)
         (setq smoke-gaps (1+ smoke-gaps))
         (message "GAP %s: %s" ,label ,why))
     (error (setq smoke-gaps (1+ smoke-gaps))
            (message "GAP %s: %s" ,label ,why))))

(defmacro smoke-load (feature)
  `(condition-case err
       (progn (require ',feature) t)
     (error (setq smoke-failures (1+ smoke-failures))
            (message "ERROR loading %s: %S" ',feature err)
            nil)))

(when (smoke-load hive-providers-crypto-sha256)
  (smoke-check "sha256 abc"
               "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
               (hive-providers-crypto-sha256-hex-digest "abc")))

(when (smoke-load hive-providers-codec-base64)
  (smoke-check "base64 foobar" "Zm9vYmFy" (hive-providers-codec-base64-encode-str "foobar"))
  (smoke-check "base64 decode" "foobar" (hive-providers-codec-base64-decode-str "Zm9vYmFy")))

(when (smoke-load hive-providers-codec-url)
  (smoke-check "url encode" "a%20b%2Fc" (hive-providers-codec-url-encode "a b/c")))

(when (smoke-load hive-providers-codec-json)
  (smoke-check "json write" "[1,\"a\",null]" (hive-providers-codec-json-write-str (vector 1 "a" nil)))
  (smoke-check "json write string escapes" "\"q\\\"\\n\""
               (hive-providers-codec-json-write-str "q\"\n"))
  (smoke-gap "json read"
             "clel compiles a core fn passed as a value (`keyword') to a variable reference"
             (vector 1 "x" 2.5)
             (hive-providers-codec-json-read-str-bang "[1, \"x\", 2.5]")))

(when (smoke-load hive-providers-oauth-jwt)
  (smoke-gap "jwt exp"
             "clel emits (:k m) keyword invocation as a function call in some positions"
             100000
             (hive-providers-oauth-jwt-exp-ms
              (concat "e30." (hive-providers-codec-base64-encode-url
                              (hive-providers-codec-utf8-encode "{\"exp\":100}"))
                      ".sig"))))

(smoke-gap "retry category"
           "openai.retry builds its Step ADT with hive-dsl's defadt, which clel cannot compile; clel also drops keyword namespaces"
           :llm/unauthorized
           (progn (require 'hive-providers-openai-retry)
                  (hive-providers-openai-retry-category (clel-hash-map :status 401))))

(when (smoke-load hive-providers-oauth-pkce)
  (smoke-check "pkce rfc7636"
               "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
               (hive-providers-oauth-pkce-challenge
                (hive-providers-oauth-pkce-verifier
                 (list 116 24 223 180 151 153 224 37 79 250 96 125 216 173 187 186
                       22 212 37 77 105 214 191 240 91 88 5 88 83 132 141 121)))))

(message "SUMMARY cljel :pass %d :fail %d :known-gaps %d" smoke-passes smoke-failures smoke-gaps)
(kill-emacs (if (zerop smoke-failures) 0 1))

;;; smoke.el ends here
