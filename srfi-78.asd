;;;; srfi-78.asd

(cl:in-package :asdf)


(defsystem :srfi-78
  :version "20200205"
  :description "SRFI 78 for CL: Lightweight testing"
  :long-description "SRFI 78 for CL: Lightweight testing
https://srfi.schemers.org/srfi-78"
  :author "Sebastian Egner"
  :maintainer "CHIBA Masaomi"
  :license "MIT"
  :serial t
  :depends-on (:mbe :srfi-23 :srfi-42 :fiveam)
  :components ((:file "package")
               (:file "srfi-78")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-78))))
  (let ((name "https://github.com/g000001/srfi-78")
        (nickname :srfi-78))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-78))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-78#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-78)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*


