(defpackage #:rest-helpers
    (:use #:cl))

(in-package #:rest-helpers)

(ql:quickload :drakma)
(ql:quickload :cl-json)

(defun get-json (url)
    (let ((json))
        (ignore-errors   
            (multiple-value-bind 
                (data status) 
                (drakma:http-request url)
                (if (= 200 status) 
                    (let* ((str (flexi-streams:octets-to-string data)))
                    (setq json (json:decode-json-from-string str))))))
        (unless json (format t "~&Unable to fetch from ~A~%" url))    
        json))