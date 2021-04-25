
(ql:quickload :testlib)
(ql:quickload :drakma)
(ql:quickload :cl-json)

(defun people-p (pair)
    (and (consp pair) (eq (car pair) :people)))

(defun get-people (pair)
    (if (consp pair) (cdr pair)))

(defun format-astronaut (astronaut &optional (output-stream t))
    (format output-stream "~&~A (~A)~%" (get-name astronaut) (get-craft astronaut)))

(defmacro get-property-value (object property-name)
    `(if (listp ,object)
       (cdr (find-if #'(lambda (p) (and (consp p) (eq (car p) ,property-name))) ,object))))

(defun get-name (astronaut)
    (get-property-value astronaut :name))

(defun get-craft (astronaut)
    (get-property-value astronaut :craft))

(catch 'trap-errors 
    (handler-bind (
        (error #'(lambda (condition) 
                    (format *error-output* "~&~A~%" condition)
                    (throw 'trap-errors nil))))
        (multiple-value-bind 
            (data http-response-code) 
            (drakma:http-request "http://api.open-notify.org/astros.json")
                (if (= 200 http-response-code) 
                    (let* ((str (flexi-streams:octets-to-string data))
                           (json (json:decode-json-from-string str)))
                             (mapcar #'format-astronaut (get-people (find-if #'people-p json)))
                        )))))

(use-package :testlib)

(run (context "who is in space?"
        (context "people-p"
            (context "when key is people" 
                (test "it should return true" (people-p (cons :people '()))))
            (context "when key is not people" 
                (test "it should return false" (not (people-p (cons :foo '()))))))
        (context "get-name"
            (context "when name is present" 
                (test "it should return name" (string= (get-name `(,(cons :name "foo"))) "foo")))
            (context "when name is not present" 
                (test "it should return nil" (eq nil (get-name `(,(cons :craft "bar")))))))
        (context "get-craft"
            (context "when craft is present" 
                (test "it should return craft" (string= (get-craft `(,(cons :craft "foo"))) "foo")))
            (context "when craft is not present" 
                (test "it should return nil" (eq nil (get-craft `(,(cons :name "bar")))))))
        (context "format-astronaut"
            (test "it should format correctly"
                (string= (format-astronaut `(,(cons :name "foo") ,(cons :craft "iss")) nil) (format nil "~&foo (iss)~%"))))
        (context "get-people"
            (test "it should return people list from property pair"
                    (equal (get-people (cons :people '(1 2 3))) '(1 2 3))))))

