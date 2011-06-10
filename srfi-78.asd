;;;; srfi-78.asd

(cl:in-package :asdf)

(defsystem :srfi-78
  :serial t
  :depends-on (:mbe :srfi-23 :srfi-42)
  :components ((:file "package")
               (:file "srfi-78")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-78))))
  (load-system :srfi-78)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-78-internal :srfi-78))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

