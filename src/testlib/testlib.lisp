(defpackage #:testlib
    (:use #:cl))

(in-package #:testlib)

(defvar *test-output-stream* t)

(defun make-spaces (n)
    (make-string n :initial-element #\space))

(defun format-outcome (depth name outcome &optional (output-stream *test-output-stream*))
    (format output-stream "~&~ATest `~A` ~:[failed~;passed~].~%" (make-spaces depth) name outcome))

(defun format-context (depth name &optional (output-stream *test-output-stream*))
    (format output-stream "~&~A~A~%" (make-spaces depth) name))

(defun format-outcomes (outcomes &optional (output-stream *test-output-stream*))
    (format output-stream "~&~A out of ~A tests passed~%" (count t outcomes) (length outcomes)))

(defmacro test (name predicate)
    (let ((outcome (gensym)))
        `(lambda (depth)
            (let ((,outcome ,predicate))
                (format-outcome depth ,name ,outcome) (list ,outcome)))))

(defmacro context (name &rest body)
    `(lambda (depth)
        (format-context depth ,name)
        (let ((depth (+ depth 1)))
           (reduce #'append (map 'list #'(lambda (f) (apply f `(,depth))) (list ,@body))))))

(defun run (root-context)
    (format-outcomes (apply root-context `(,0))))

(defmacro run-tests (&rest body)
    `(run (context "" ,@body)))

(defmacro make-test (name predicate)
    `(test ,name ,predicate))
  
(defun test-testlib ()
    (run-tests
        (make-test "1 == 1" (= 1 1))
        (make-test "2 == 1" (= 2 1))
        (make-test "NIL" nil)
        (make-test "t" t)))

;;(test-testlib)