(defpackage #:testlib
    (:use #:cl))

(in-package #:testlib)

(defmacro make-test (name predicate)
    `(lambda () 
        (let ((p ,predicate))
            (if p 
                (format t "Test `~A` passed~%" ,name)
                (format t "Test `~A` failed~%" ,name))
        p)))

(defun run-tests (&rest tests)
       (let*  ((score (lambda (p) (if p 1 0)))
                (run-test (lambda (test) (funcall test)))
                (scores (map 'list score (map 'list run-test tests)))
                (num-tests (length scores))
                (num-passes (reduce #'+ scores)))
            (format t "~A out of ~A tests passed~%" num-passes num-tests)))

(defun test-testlib ()
    (run-tests
        (make-test "1 == 1" (= 1 1))
        (make-test "2 == 1" (= 2 1))
        (make-test "NIL" nil)
        (make-test "t" t)))

;;(test-testlib)