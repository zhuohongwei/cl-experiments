(defpackage #:testlib
    (:use #:cl))

(in-package #:testlib)

(defmacro make-test (name predicate)
    `(lambda () 
        (values ,name ,predicate)))

(defun report-test (name passp)
    (format t "Test `~A` ~:[failed~;passed~]~%" name passp))

(defun run-test (test)
        (multiple-value-bind (name p) (funcall test) 
            (apply #'report-test `(,name ,p)) p))

(defun run-tests (&rest tests)
       (let  ((outcomes (map 'list #'run-test tests)))
            (format t "~A out of ~A tests passed~%" (count t outcomes) (length outcomes))))

;; (defmacro make-group (name &rest tests)
;;     `(lambda ()
;;         (format t "~A~%" ,name)
;;         (run-tests ,@tests)))

;; (defun run-groups (&rest groups)
    
;;     )

(defun test-testlib ()
    (run-tests
        (make-test "1 == 1" (= 1 1))
        (make-test "2 == 1" (= 2 1))
        (make-test "NIL" nil)
        (make-test "t" t)))

;;(test-testlib)