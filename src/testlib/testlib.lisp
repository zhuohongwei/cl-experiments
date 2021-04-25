(defpackage #:testlib
    (:use #:cl))

(in-package #:testlib)

(defvar *test-output-stream* t)

(defun make-spaces (n)
    (make-string n :initial-element #\space))

(defun format-outcome (depth name outcome &optional (output-stream *test-output-stream*))
    (format output-stream "~&~A~A: ~:[failed~;passed~]~%" (make-spaces depth) name outcome))

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
  
(defun test-testlib ()
    (run (context "testlib"
            (context "make-spaces"
                (test "it should return expected string" (string= (make-spaces 5) "     ")))
            (context "format-outcome"
                (context "when outcome is true"
                    (test "it should return expected format" (string= (format-outcome 2 "foo bar" t nil) (format nil "~&  foo bar: passed~%"))))
                (context "when outcome is false"
                    (test "it should return expected format" (string= (format-outcome 2 "foo bar" nil nil) (format nil "~&  foo bar: failed~%")))))
            (context "format-context"
                (test "it should return expected format" (string= (format-context 1 "bar baz" nil) (format nil "~& bar baz~%"))))
            (context "format-outcomes"
                (test "it should return expected format" (string= (format-outcomes (list t nil) nil) (format nil "~&1 out of 2 tests passed~%")))))))

(test-testlib)