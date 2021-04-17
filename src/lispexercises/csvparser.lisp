(ql:quickload "testlib")
(use-package :testlib)

(defun get-lines (input-stream)
    (let ((lines '()))
        (do ((line (read-line input-stream nil nil) 
                   (read-line input-stream nil nil)))
                   ((null line) (nreverse lines))
                   (push line lines))))

(run (context "csv parser" 
        (context "get-lines" 
            (test "it should return the correct lines from input stream" 
                (equal '("one" "two") (get-lines t))))))