(defpackage #:json-helpers
    (:use #:cl))

(in-package #:json-helpers)

(defun property-p (property)
    (funcall #'consp property))

(defun get-property-value (property)
    (funcall #'cdr property))

(defun get-property-key (property)
    (funcall #'car property))

(defun json-object-p (json)
    (and (listp json) (every #'consp json)))

(defun json-array-p (json)
    (and (listp json) (every json-object-p json)))

(defun json-p (json)
    (or (json-object-p json) (json-array-p json)))

(defun get-json-value (keys json)
    (princ (list keys json))
    (terpri)
    (cond 
        ((null keys) json)
        ((json-p json)
            (let ((key (car keys)))
                (if (numberp key)
                    (get-json-value (cdr keys) (nth key json))
                    (let ((property 
                            (find-if 
                                #'(lambda (property) 
                                        (and (property-p property) 
                                            (eq (get-property-key property) (car keys)))) json)))
                        (if property
                            (get-json-value (cdr keys) (get-property-value property)))))))
        (t nil)))

(ql:quickload :testlib)

(use-package :testlib)

(defun test-json-helpers ()
    (run 
        (context "json-helpers"
            (context "property-p"
                (context "if property is a pair"
                    (test "it should return true"
                        (property-p (cons :name "foo"))))
                (context "if property is not a pair"
                    (test "it should return nil"
                        (not (property-p "foo")))))
            (context "get-property-value"
                (test "it should extract value"
                    (string= (get-property-value (cons :name "foo")) "foo")))
            (context "get-property-key"
                (test "it should extract key"
                    (eq (get-property-key (cons :name "foo")) :name)))
            (context "get-json-value"
                (context "when keys are empty"
                    (test "it should return passed payload"
                        (equal (get-json-value `() `(,(cons :foo "bar"))) `(,(cons :foo "bar")))))
                (context "when invalid json"
                    (test "it should return nil"
                        (eq (get-json-value `(:foo) 'invalid-json) nil)))
                (context "when one key"
                    (context "when property found"
                        (test "it should return value of matching property"
                            (equal (get-json-value `(:foo) `(,(cons :foo "bar"))) "bar")))
                    (context "when property not found"
                        (test "it should return nil"
                            (eq (get-json-value `(:baz) `(,(cons :foo "bar"))) nil))))
                (context "when two keys"
                    (test "it should return value of matching property"
                        (equal (get-json-value `(:foo :bar) 
                                `(,(cons :foo `(,(cons :bar "baz"))))) "baz")))
                (context "when key is a number"
                    (test "it should return correct object"
                        (equal (get-json-value `(0) 
                                `((,(cons :bar "baz")))) `(,(cons :bar "baz")))))))))

;; (test-json-helpers)