;;;; package.lisp

(cl:in-package cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern "=>" 'cl-user))

(defpackage "https://github.com/g000001/srfi-78"
  (:use)
  (:shadowing-import-from cl-user =>)
  (:export check
           check-ec
           check-set-mode!
           check-reset!
           check-report
           check-passed?))


(defpackage "https://github.com/g000001/srfi-78#internals"
  (:use "https://github.com/g000001/srfi-78"
        "https://github.com/g000001/srfi-42"
        cl
        mbe
        fiveam)
  (:shadowing-import-from srfi-23 error))


;;; *EOF*
