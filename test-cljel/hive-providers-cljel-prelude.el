;;; hive-providers-cljel-prelude.el --- clojure.core gaps of the clel runtime  -*- lexical-binding: t; -*-
;;
;; ClojureElisp (clel 0.8.x) compiles hive-providers but its runtime does not
;; yet define every clojure.core function the library calls. This prelude
;; supplies them, in the representation clel uses (vectors are Elisp vectors,
;; lists are lists, maps are clel hash maps), so the compiled library loads.
;; Load it after `clel' and before any hive-providers-* feature.
;;
;; Each definition is guarded with `fboundp', so a clel release that ships the
;; function wins and this file shrinks to nothing.
;;
;; SPDX-License-Identifier: MIT

(require 'clel)

;; clel compilers before the 0.8.0 rename emit (require 'clojure-elisp-runtime).
(unless (featurep 'clojure-elisp-runtime) (provide 'clojure-elisp-runtime))
;; `(:require [clojure.string :as str])' compiles to (require 'clojure-string);
;; the string functions themselves live in clel.
(unless (featurep 'clojure-string) (provide 'clojure-string))

(defmacro hive-providers-cljel--defn (name args &rest body)
  "Define NAME unless the clel runtime already does."
  (declare (indent 2))
  `(unless (fboundp ',name) (defun ,name ,args ,@body)))

(defmacro hive-providers-cljel--alias (name target)
  "Alias NAME to the clel runtime's TARGET unless NAME is already defined."
  `(unless (fboundp ',name) (defalias ',name #',target)))

;; Present in the runtime, not yet in the compiler's name mappings.
(hive-providers-cljel--alias vec clel-vec)
(hive-providers-cljel--alias mapv clel-mapv)

;; Clojure false compiles to nil, so the only boolean distinguishable from
;; nil is t.
(hive-providers-cljel--defn boolean-p (x) (eq x t))
(hive-providers-cljel--alias integer-p integerp)
(hive-providers-cljel--alias sequential-p clel-sequential-p)
(hive-providers-cljel--alias map-p clel-map-p)
(hive-providers-cljel--alias set-p clel-set-p)
(hive-providers-cljel--alias keyword-p keywordp)
(hive-providers-cljel--alias symbol-p symbolp)

;; Bitwise ops, binary as the library calls them.
(hive-providers-cljel--defn bit-and (a b) (logand a b))
(hive-providers-cljel--defn bit-or (a b) (logior a b))
(hive-providers-cljel--defn bit-xor (a b) (logxor a b))
(hive-providers-cljel--defn bit-not (a) (lognot a))
(hive-providers-cljel--defn bit-shift-left (a n) (ash a n))
(hive-providers-cljel--defn bit-shift-right (a n) (ash a (- n)))

;; Transients: clel collections are values already; a transient is the
;; collection itself and the bang ops are the persistent ones.
(hive-providers-cljel--defn transient (coll) coll)
(hive-providers-cljel--defn persistent-bang (coll) coll)
(hive-providers-cljel--defn conj-bang (coll x) (clel-conj coll x))

(provide 'hive-providers-cljel-prelude)

;;; hive-providers-cljel-prelude.el ends here
