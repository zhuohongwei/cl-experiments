(ql:quickload "testlib")
(use-package :testlib)

(defun get-lines (input-stream)
    (let ((lines '()))
        (do ((line (read-line input-stream nil nil) 
                   (read-line input-stream nil nil)))
                   ((null line) (nreverse lines))
                   (push line lines))))

(defun get-columns (line)
        (let ((columns '()))
                (do ((i 0 (+ j 1)) 
                     (j (position #\, line :start 0) (position #\, line :start (+ j 1))))
                        ((null j) (progn (push (subseq line i) columns) (nreverse columns)))
                            (push (subseq line i j) columns))))
(defun get-rows (lines)
    (map 'list #'get-columns lines))

(defun get-fitting-column-width (rows n)
    (let ((get-nth-column-width #'(lambda (row) (length (nth n row)))))
        (+ (apply #'max (map 'list get-nth-column-width rows)) 1)))

(defun get-fitting-column-widths (rows)
    (let ((cols (apply #'max (map 'list #'length rows))))
            (if (zerop cols) 
                '()
                (loop as n from 0 to (- cols 1) collect (get-fitting-column-width rows n)))))

(defun format-rows (rows &optional (output-stream t))
    (let* ((widths (get-fitting-column-widths rows))
           (format-row 
             (lambda (row) 
                (let ((n 0))
                    (map 'list 
                        #'(lambda (col)
                            (let ((w (nth n widths)))
                                (incf n)
                                (format nil (format nil "~~~AA" w) (string-trim '(#\space #\tab) col)))) row)))))   
        (format output-stream "~{~&~{~A~}~%~}" (map 'list format-row rows))))

(defun format-csv (input-stream &optional (output-stream t))
    (format-rows (get-rows (get-lines input-stream)) output-stream))
    
(run (context "csv-parser" 
        (context "get-lines" 
            (test "it should return the correct lines from input stream" 
                (equal 
                    '("one" "two" "three") 
                    (get-lines (make-string-input-stream (format nil "one~%two~%three~%"))))))
        (context "get-columns"
            (test "it should return the correct words"
                (equal '("column1" "column2" "column3")
                    (get-columns "column1,column2,column3"))))
        (context "get-rows"            
            (test "it should return the correct rows with columns in each row"
                (equal '(("first name" "last name") ("foo" "bar") ("foo foo foo foo" "bar bar")) 
                        (get-rows '("first name,last name" "foo,bar" "foo foo foo foo,bar bar")))))
        (context "get-fitting-column-width"
            (test "it should return the correct column width"
                (= (+ (length "foo foo") 1) (get-fitting-column-width '(("zero" "one") ("foo" "bar") ("bar" "foo foo")) 1))))
        (context "get-fitting-column-widths"
            (test "it should return the correct column widths"
                (equal '(16 10) 
                    (get-fitting-column-widths '(("first name" "last name") ("foo" "bar") ("foo foo foo foo" "bar bar"))))))
        (context "format-rows"
            (test "it should format ouput correctly"
                (string= (format nil "~&first name      last name ~%foo             bar       ~%foo foo foo foo bar bar   ~%")
                    (format-rows '(("first name" "last name") ("foo" "bar") ("foo foo foo foo" "bar bar")) nil))))
        (context "format-csv"
            (test "it should read input stream correctly and output formatted csv"
                (string= (format nil "~&first name      last name ~%foo             bar       ~%foo foo foo foo bar bar   ~%")
                    (format-csv (make-string-input-stream (format nil "~&first name,last name~%foo,bar~%foo foo foo foo,bar bar~%") ) nil))))
            ))