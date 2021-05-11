
(ql:quickload :testlib)
(ql:quickload :json-helpers)
(ql:quickload :rest-helpers)

(use-package :json-helpers)
(use-package :rest-helpers)

(defmacro get-property-value (object property-name)
    `(if (listp ,object)
       (cdr (find-if #'(lambda (p) (and (consp p) (eq (car p) ,property-name))) ,object))))

(defun get-name (astronaut)
    (get-property-value astronaut :name))

(defun get-craft (astronaut)
    (get-property-value astronaut :craft))

(defun format-astronaut (astronaut &optional (output-stream t))
    (format output-stream "~&~A (~A)~%" (get-name astronaut) (get-craft astronaut)))

(defun get-astronauts ()
    (mapcar #'format-astronaut 
        (get-json-value `(:people) 
        (get-json "http://api.open-notify.org/astros.json"))))

(get-astronauts)

(use-package :testlib)

(run (context "astros"
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
                (string= (format-astronaut `(,(cons :name "foo") ,(cons :craft "iss")) nil) (format nil "~&foo (iss)~%"))))))

