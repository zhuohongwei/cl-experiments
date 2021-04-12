(defpackage #:testlib
    (:use #:cl))

(in-package #:testlib)

(let ((indent 0)
       (outcomes '()))
    (defun indent+ ()
        (incf indent))
    (defun indent- ()
        (decf indent))
    (defun add-outcome (outcome)
        (push outcome outcomes))
    (defun reset-outcomes ()
        (setq outcomes '()))
    (defun report-outcomes ()
        (format t "~A out of ~A tests passed~%" (count t outcomes) (length outcomes)))
    (defmacro test (name predicate)
        (let ((result (gensym)))
            `(let ((,result ,predicate))
                (add-outcome ,result)
                (format t "~&~ATest `~A` ~:[failed~;passed~].~%" 
                    (make-string ,indent :initial-element #\space) ,name ,result))))
    (defmacro context (name &rest body)
        `(progn 
            (format t "~&~A~A~%" (make-string ,indent :initial-element #\space) ,name)
            (indent+)
            ,@body
            (indent-) 
            (if (zerop ,indent) 
                (progn (report-outcomes)
                         (reset-outcomes))))))

(defmacro run-tests (&rest body)
        `(context "" ,@body))

(defmacro make-test (name predicate)
    `(test ,name ,predicate))
  
(defun test-testlib ()
    (run-tests
        (make-test "1 == 1" (= 1 1))
        (make-test "2 == 1" (= 2 1))
        (make-test "NIL" nil)
        (make-test "t" t)))

(test-testlib)