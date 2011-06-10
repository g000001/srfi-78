;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-78
  (:use)

  (:export))

(defpackage :srfi-78-internal
  (:use :srfi-78 :srfi-42 :cl :mbe :fiveam)
  (:shadowing-import-from :srfi-23
                          :error))

