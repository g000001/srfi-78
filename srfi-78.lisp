;;;; srfi-78.lisp

(cl:in-package :srfi-78-internal)

(def-suite srfi-78)

(in-suite srfi-78)

; Copyright (c) 2005-2006 Sebastian Egner.
;
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ``Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
; -----------------------------------------------------------------------
;
; Lightweight testing (reference implementation)
; ==============================================
;
; Sebastian.Egner@philips.com
; in R5RS + SRFI 23 (error) + SRFI 42 (comprehensions)
;
; history of this file:
;   SE, 25-Oct-2004: first version based on code used in SRFIs 42 and 67
;   SE, 19-Jan-2006: (arg ...) made optional in check-ec
;
; Naming convention "check:<identifier>" is used only internally.

; -- portability --

; PLT:      (require (lib "23.ss" "srfi") (lib "42.ss" "srfi"))
; Scheme48: ,open srfi-23 srfi-42

; -- utilities --

(declaim (inline |check:write|))
(defun |check:write| (&rest args)
  (declare (dynamic-extent args))
  (apply #'cl:write args))

; You can also use a pretty printer if you have one.
; However, the output might not improve for most cases
; because the pretty printers usually output a trailing
; newline.

; PLT:      (require (lib "pretty.ss")) (define check:write pretty-print)
; Scheme48: ,open pp (define check:write p)

; -- mode --

(defvar |check:mode| nil)

(defun check-set-mode! (mode)
  (setq |check:mode|
        (case mode
          ((:off)           0)
          ((:summary)       1)
          ((:report-failed) 10)
          ((:report)        100)
          (otherwise (error "unrecognized mode" mode)))))

(check-set-mode! :report)

; -- state --

(defvar |check:correct| nil)
(defvar |check:failed| nil)

(defun check-reset! ()
  (setq |check:correct| 0)
  (setq |check:failed|   '()))

(defun |check:add-correct!| ()
  (setq |check:correct| (+ |check:correct| 1)))

(defun |check:add-failed!| (expression actual-result expected-result)
  (setq |check:failed|
        (cons (list expression actual-result expected-result)
              |check:failed|)))

(check-reset!)

; -- reporting --

(defun |check:report-expression| (expression)
  (terpri)
  (|check:write| expression)
  (princ " :=> "))

(defun |check:report-actual-result| (actual-result)
  (|check:write| actual-result)
  (princ " ; "))

(defun |check:report-correct| (cases)
  (princ "correct")
  (if (not (= cases 1))
      (progn (princ " (")
             (princ cases)
             (princ " cases checked)")))
  (terpri))

(defun |check:report-failed| (expected-result)
  (princ "*** failed ***")
  (terpri)
  (princ " ; expected result: ")
  (|check:write| expected-result)
  (terpri))

(defun check-report ()
  (if (>= |check:mode| 1)
      (progn
        (terpri)
        (princ "; *** checks *** : ")
        (princ |check:correct|)
        (princ " correct, ")
        (princ (length |check:failed|))
        (princ " failed.")
        (if (or (null |check:failed|) (<= |check:mode| 1))
            (terpri)
            (let* ((w (car (reverse |check:failed|)))
                   (expression (car w))
                   (actual-result (cadr w))
                   (expected-result (caddr w)))
              (princ " First failed example:")
              (terpri)
              (|check:report-expression| expression)
              (|check:report-actual-result| actual-result)
              (|check:report-failed| expected-result))))))

(defun check-passed? (expected-total-count)
  (and (= (length |check:failed|) 0)
       (= |check:correct| expected-total-count)))

; -- simple checks --

(defun |check:proc| (expression thunk equal expected-result)
  (case |check:mode|
    ((0) nil)
    ((1)
     (let ((actual-result (funcall thunk)))
       (if (funcall equal actual-result expected-result)
           (|check:add-correct!|)
           (|check:add-failed!| expression actual-result expected-result))))
    ((10)
     (let ((actual-result (funcall thunk)))
       (if (funcall equal actual-result expected-result)
           (|check:add-correct!|)
           (progn
             (|check:report-expression| expression)
             (|check:report-actual-result| actual-result)
             (|check:report-failed| expected-result)
             (|check:add-failed!| expression actual-result expected-result)))))
    ((100)
     (|check:report-expression| expression)
     (let ((actual-result (funcall thunk)))
       (|check:report-actual-result| actual-result)
       (if (funcall equal actual-result expected-result)
           (progn (|check:report-correct| 1)
                  (|check:add-correct!|))
           (progn (|check:report-failed| expected-result)
                  (|check:add-failed!| expression
                                       actual-result
                                       expected-result)))))
    (otherwise (error "unrecognized |check:mode|" |check:mode|)))
  (values))

(define-syntax check
  (syntax-rules (:=>)
    ((check expr :=> expected)
     (check expr (:=> #'equalp) expected))
    ((check expr (:=> equal) expected)
     (if (>= |check:mode| 1)
	 (|check:proc| 'expr (lambda () expr) equal expected)))))

; -- parametric checks --

(defun |check:proc-ec| (w)
  (let ((correct? (car w))
        (expression (cadr w))
        (actual-result (caddr w))
        (expected-result (cadddr w))
	(cases (car (cddddr w))))
    (if correct?
        (progn (if (>= |check:mode| 100)
                   (progn (|check:report-expression| expression)
                          (|check:report-actual-result| actual-result)
                          (|check:report-correct| cases)))
               (|check:add-correct!|))
        (progn (if (>= |check:mode| 10)
                   (progn (|check:report-expression| expression)
                          (|check:report-actual-result| actual-result)
                          (|check:report-failed| expected-result)))
               (|check:add-failed!| expression
                                    actual-result
                                    expected-result)))))

(define-syntax |check-ec:make|
  (syntax-rules (:=>)
    ((|check-ec:make| qualifiers expr (:=> equal) expected (arg ***))
     (if (>= |check:mode| 1)
         (|check:proc-ec|
	  (let ((cases 0))
	    (let ((w (first-ec
		      nil
		      qualifiers
		      (:let equal-pred equal)
		      (:let expected-result expected)
		      (:let actual-result
                            (let ((arg arg) ***) ; (*)
                              expr))
		      (progn (setq cases (+ cases 1)))
		      (if (not (funcall equal-pred actual-result expected-result)))
		      (list (list 'let (list (list 'arg arg) ***) 'expr)
			    actual-result
			    expected-result
			    cases))))
	      (if w
		  (cons nil w)
		  (list t
			'(check-ec qualifiers
				   expr (:=> equal)
				   expected (arg ***))
			nil
		        nil
			cases)))))))))

; (*) is a compile-time check that (arg ...) is a list
; of pairwise disjoint bound variables at this point.

(define-syntax check-ec
  (syntax-rules (nested :=>)
    ((check-ec expr :=> expected)
     (|check-ec:make| (nested) expr (:=> #'equalp) expected ()))
    ((check-ec expr (:=> equal) expected)
     (|check-ec:make| (nested) expr (:=> equal) expected ()))
    ((check-ec expr :=> expected (arg ***))
     (|check-ec:make| (nested) expr (:=> #'equalp) expected (arg ***)))
    ((check-ec expr (:=> equal) expected (arg ***))
     (|check-ec:make| (nested) expr (:=> equal) expected (arg ***)))

    ((check-ec qualifiers expr :=> expected)
     (|check-ec:make| qualifiers expr (:=> #'equalp) expected ()))
    ((check-ec qualifiers expr (:=> equal) expected)
     (|check-ec:make| qualifiers expr (:=> equal) expected ()))
    ((check-ec qualifiers expr :=> expected (arg ***))
     (|check-ec:make| qualifiers expr (:=> #'equalp) expected (arg ***)))
    ((check-ec qualifiers expr (:=> equal) expected (arg ***))
     (|check-ec:make| qualifiers expr (:=> equal) expected (arg ***)))

    ((check-ec (nested q1 ***) q etc ***)
     (check-ec (nested q1 *** q) etc ***))
    ((check-ec q1 q2             etc ***)
     (check-ec (nested q1 q2)    etc ***))))


(test check
  (macrolet ((sout (&body body)
               `(with-output-to-string (*standard-output*)
                  ,@body)))
    (is (string= (sout (check (+ 1 1) :=> 2))
                 "
\(+ 1 1) :=> 2 ; correct
"))
    (is (string= (sout (check (+ 1 1) :=> 3))
                 "
\(+ 1 1) :=> 2 ; *** failed ***
 ; expected result: 3
"))
    (is (string= (sout (check (vector 1) :=> (vector 1)))
                 "
\(VECTOR 1) :=> #(1) ; correct
"))
    (is (string= (sout (check (vector 1) (:=> #'eq) (vector 1)))
                 "
\(VECTOR 1) :=> #(1) ; *** failed ***
 ; expected result: #(1)
"))
    (is (string= (sout (check-ec (+ 1 1) :=> 2))
"
\(CHECK-EC (NESTED) (+ 1 1) (:=> #'EQUALP) 2 NIL) :=> NIL ; correct
"))
    (is (string= (sout (check-ec (:- x 10) (+ x 1) :=> (+ x 1) (x)))
                 "
\(CHECK-EC (:- X 10) (+ X 1) (:=> #'EQUALP) (+ X 1) (X)) :=> NIL ; correct (10 cases checked)
"))
    (is (string= (sout (check-ec (:- e 10) (plusp (expt 2 e)) :=> t (e)))
                 "
\(CHECK-EC (:- E 10) (PLUSP (EXPT 2 E)) (:=> #'EQUALP) T (E)) :=> NIL ; correct (10 cases checked)
"))
    (is (string= (sout (check-ec (:- e 100)
                                 (:let x (expt 2.0 e))
                                 (= (+ x 1) x) :=> nil (x)))
                 "
\(LET ((X 1.6777216e7))
  (= (+ X 1) X)) :=> T ; *** failed ***
 ; expected result: NIL
" ))

    (is (string= (sout (check-ec (:- e 100)
                                 (:let x (expt 2.0 e))
                                 (= (+ x 1) x) :=> nil))
                 "
\(LET ()
  (= (+ X 1) X)) :=> T ; *** failed ***
 ; expected result: NIL
"))


    (is (string= (sout (check-ec (:- x 10) (:- y 10) (:- z 10)
                                 (* x (+ y z)) :=> (+ (* x y) (* x z))
                                 (x y z)))
                 "
\(CHECK-EC (NESTED (:- X 10) (:- Y 10) (:- Z 10)) (* X (+ Y Z)) (:=> #'EQUALP) (+ (* X Y) (* X Z)) (X Y Z)) :=> NIL ; correct (1000 cases checked)
"))

    (defun fib (n)
      (if (<= n 2)
          1
          (+ (fib (- n 1))
             (fib (- n 2)))))

    (is (string= (sout (check (fib 1) :=> 1))
                 "
\(FIB 1) :=> 1 ; correct
"))

    (is (string= (sout (check (fib 2) :=> 1))
                 "
\(FIB 2) :=> 1 ; correct
"))
    (is (string= (sout (check-ec (:- n 1 31)
                                 (evenp (fib n)) :=> (= (mod n 3) 0) (n)))
                 "
\(CHECK-EC (:- N 1 31) (EVENP (FIB N)) (:=> #'EQUALP) (= (MOD N 3) 0) (N)) :=> NIL ; correct (30 cases checked)
")) ))
