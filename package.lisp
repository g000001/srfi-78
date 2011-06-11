;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-78
  (:use)
  (:export :check
           :check-ec
           :check-set-mode!
           :check-reset!
           :check-report
           :check-passed?))

(defpackage :srfi-78-internal
  (:use :srfi-78 :srfi-42 :cl :mbe :fiveam)
  (:shadowing-import-from :srfi-23
                          :error))

