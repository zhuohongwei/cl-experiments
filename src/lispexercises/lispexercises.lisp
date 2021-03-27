(ql:quickload "parse-float")
(ql:quickload "testlib")

(use-package :parse-float)
(use-package :testlib)

(defun prompt-read (prompt)
    (format *query-io* "~a " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-read-integer (prompt)
    (let ((input (parse-integer (prompt-read prompt) :junk-allowed t)))
        (if (null input)
            (prompt-read-integer prompt)
            input)))

(defun prompt-read-float (prompt)
    (let ((input (parse-float (prompt-read prompt) :junk-allowed t)))
        (if (null input)
            (prompt-read-float prompt)
            input)))

(defun trim-whitespaces (str)
    (string-trim '(#\Space #\Tab #\Newline) str))

(defmacro begin (&rest args)
    `(let ((ret 
           ((lambda () ,@args))
           )) 
            ret))

;; Problem 1 Saying Hello
(defun say-hello (name)
    (format t "Hello, ~a, nice to meet you!~%" name))

(defun prompt-name ()
    (prompt-read "What is your name?"))

(defun prompt-name-and-say-hello ()
    (say-hello (prompt-name)))

;; Problem 2 Counting the Number of Characters
(defun print-length (str) 
    (format t "~a has ~a characters" str (length str)))

(defun prompt-string ()
    (let ((str (prompt-read "What is the input string?")))
        (if (= (length (trim-whitespaces str)) 0)
            (begin 
             (format t "You need to enter something...~%")
             (prompt-string))
            str)))

(defun prompt-string-and-print-length ()
    (print-length (prompt-string)))

;; Problem 3 Printing Quotes

(defun print-quote (quotation author)
    (let ((space " ")
            (double-quote "\""))
        (format t "~a~%" (concatenate 'string author space "says," space double-quote quotation double-quote))))

(defun prompt-and-print-quote ()
    (print-quote 
        (prompt-read "What is the quote?") 
        (prompt-read "Who said it?")))

(defun make-quote (quotation author)
    (let ((quote (make-hash-table)))
        (setf (gethash 'author quote) author)
        (setf (gethash 'quotation quote) quotation)
        quote))

(defun print-some-quotes ()
    (let* ((one (make-quote "I said this" "I"))
           (two (make-quote "He said that" "He"))
           (quotes (list one two)))
        (dolist (quote quotes)
            (print-quote (gethash 'quotation quote) (gethash 'author quote)))))

;; Problem 4 Mad Lib

(defun print-story (noun verb adjective adverb)
    (format t "Do you ~a your ~a ~a ~a? That's hilarious!~%" verb adjective noun adverb))

(defun prompt-and-print-story ()
    (let ((noun (prompt-read "Enter a noun: "))
            (verb (prompt-read "Enter a verb: "))
            (adjective (prompt-read "Enter an adjective: "))
            (adverb (prompt-read "Enter an adverb: ")))
            (print-story noun verb adjective adverb)))

;; Problem 5 Simple Math

(defun print-operation (operator-name fn)
    (lambda (operand1 operand2)
        (format t "~a ~a ~a = ~a~%" operand1 operator-name operand2 (funcall fn operand1 operand2))))

(defun print-operations (operand1 operand2)
    (funcall (print-operation "+" #'+) operand1 operand2)
    (funcall (print-operation "-" #'-) operand1 operand2)
    (funcall (print-operation "*" #'*) operand1 operand2)
    (funcall (print-operation "/" #'/) operand1 operand2))

(defun prompt-numbers-and-print-operations ()
    (let ((n1 (prompt-read "What is the first number? "))
            (n2 (prompt-read "What is the second number? ")))
            (print-operations (parse-integer n1) (parse-integer n2))))

;; Problem 6 Retirement Calculator

(defun calculate-retirement (current-age retirement-age)
    (defun print-years-till-retirement (years current-year retirement-year)
        (format t "You have ~a years left until you can retire.~%It's ~a, so you can retire in ~a~%" 
            years current-year retirement-year))
    (defun print-can-already-retire ()
        (format t "You can already retire now"))    
    (let ((years (- retirement-age current-age)))
            (if (> years 0)
                    (let* ((current-year (nth-value 5 (get-decoded-time)))
                        (retirement-year (+ current-year years)))
                        (print-years-till-retirement years current-year retirement-year))
                    (print-can-already-retire))))

(defun prompt-ages-and-calculate-retirement ()
    (let ((current-age (prompt-read "What is your current age? "))
            (retirement-age (prompt-read "At what age would you like to retire? ")))
            (calculate-retirement (parse-integer current-age) (parse-integer retirement-age))))

;; Problem 7 Area of a Rectangular Room

(defun print-dimensions-of-room (ostream length-of-room width-of-room)
    (format ostream "You have entered dimensions of ~a feet by ~a feet.~%" length-of-room width-of-room))

(defun test-print-dimensions-of-room ()
    (assert (string= (print-dimensions-of-room nil 1 2) (format nil "You have entered dimensions of 1 feet by 2 feet.~%"))))

(defun print-area-of-room (ostream area-in-sqft area-in-sqm)
    (format ostream "The area is~%~a square feet~%~a square meters~%" area-in-sqft area-in-sqm))

(defun test-print-area-of-room ()
    (assert (string= (print-area-of-room nil 1 2) (format nil "The area is~%1 square feet~%2 square meters~%"))))

(defun calculate-area (length-of-room width-of-room)
    (* length-of-room width-of-room))

(defun test-calculate-area ()
    (assert (= (calculate-area 20 10) 200)))

(defvar *sqft-to-sqm-factor* 0.09290304)

(defun convert-sqft-to-sqm (sqft) 
    (* sqft *sqft-to-sqm-factor*))

(defun test-convert-sqft-to-sqm ()
    (assert (= (convert-sqft-to-sqm 10) 0.9290304)))

(defun prompt-dimensions-and-calculate-area ()
    (let ((length-of-room (prompt-read-integer "What is the length of the room in feet? "))
            (width-of-room (prompt-read-integer "What is the width of the room in feet? ")))
        (print-dimensions-of-room t length-of-room width-of-room)
        (let* ((sqft (calculate-area length-of-room width-of-room))
                (sqm (convert-sqft-to-sqm sqft)))
                    (print-area-of-room t sqft sqm))))

;; Problem 8 Pizza Party

(defun print-number-of-people-and-pizza (output-stream num-people num-pizzas)
    (format output-stream "~a people with ~a pizzas~%" num-people num-pizzas))

(assert (string= (print-number-of-people-and-pizza nil 8 2) (format nil "8 people with 2 pizzas~%")))

(defun print-per-share-and-leftover (output-stream per-share leftover)
    (let ((per-share-suffix (if (eql per-share 1) "piece" "pieces"))
            (leftover-suffix (if (eql leftover 1) "piece" "pieces")))
        (format output-stream "Each people gets ~a ~a of pizza.~%There are ~a leftover ~a.~%" per-share per-share-suffix leftover leftover-suffix)))

(assert (string= (print-per-share-and-leftover nil 2 0) (format nil "Each people gets 2 pieces of pizza.~%There are 0 leftover pieces.~%")))
(assert (string= (print-per-share-and-leftover nil 1 1) (format nil "Each people gets 1 piece of pizza.~%There are 1 leftover piece.~%")))

(defvar *num-slices-per-pizza* 8)

(defun calculate-per-share-and-leftover (num-people num-pizzas)
    (if (< num-pizzas 1) 
        '(0 0)
        (let ((num-of-pieces (* num-pizzas *num-slices-per-pizza*)))
            (if (< num-people 1) 
                (list 0 num-of-pieces)
                (let* ((leftover (mod num-of-pieces num-people))
                        (per-share (/ (- num-of-pieces leftover) num-people)))
                    (list per-share leftover))))))
        
(assert (equal (calculate-per-share-and-leftover 8 2) '(2 0)))
(assert (equal (calculate-per-share-and-leftover 10 3) '(2 4)))
(assert (equal (calculate-per-share-and-leftover 10 0) '(0 0)))
(assert (equal (calculate-per-share-and-leftover 10 -1) '(0 0)))
(assert (equal (calculate-per-share-and-leftover 0 10) '(0 80)))
(assert (equal (calculate-per-share-and-leftover -1 10) '(0 80)))
(assert (equal (calculate-per-share-and-leftover 0 0) '(0 0)))
(assert (equal (calculate-per-share-and-leftover -1 -1) '(0 0)))

(defun pizza-party ()
    (let ((num-people (prompt-read-integer "How many people? "))
            (num-pizzas (prompt-read-integer "How many pizzas do you have? ")))
            (print-number-of-people-and-pizza t num-people num-pizzas)
            (let ((per-share-and-leftover (calculate-per-share-and-leftover num-people num-pizzas)))
                (apply #'print-per-share-and-leftover (cons t per-share-and-leftover)))))

;; Problem 9 Paint Calculator

(defvar *sqft-per-gallon* 350)

(defun print-gallons-and-area (output-stream gallons area) 
    (format output-stream "You will need purchase ~a gallons of~%paint to cover ~a square feet.~%" gallons area))

(assert (string= (print-gallons-and-area nil 2 360) 
    (format nil "You will need purchase 2 gallons of~%paint to cover 360 square feet.~%")))

(defun calculate-gallons (area) 
    (let* ((r (mod area *sqft-per-gallon*))
            (g (/ (- area r ) *sqft-per-gallon*)))
        (if (equal r 0)
            g
            (+ g 1))))

(assert (= (calculate-gallons 0) 0))
(assert (= (calculate-gallons -1) 0))
(assert (= (calculate-gallons 350) 1))
(assert (= (calculate-gallons 700) 2))
(assert (= (calculate-gallons 800) 3))

(defun paint-calculator ()
    (let ((len (prompt-read-integer "What is the length? "))
            (width (prompt-read-integer "What is the width? ")))
        (let* ((area (* len width))
                (gallons (calculate-gallons area)))
                    (print-gallons-and-area t gallons area))))

;; Problem 10 Self Checkout

(defvar *tax-rate* 0.055)

(defun print-subtotal-tax-total (output-stream subtotal tax total)
    (format output-stream "Subtotal: $~,2F~%Tax: $~,2F~%Total: $~,2F~%" subtotal tax total))

(assert (string= (print-subtotal-tax-total nil 1064 3.52 1067.52) (format nil "Subtotal: $1064.00~%Tax: $3.52~%Total: $1067.52~%")))

(defun calculate-tax (subtotal)
    (* *tax-rate* subtotal))

(assert (= (calculate-tax 64) 3.52))

(defun calculate-tax-and-total (subtotal)
    (let* ((tax (calculate-tax subtotal))
            (total (+ subtotal tax)))
        (values tax total)))

(multiple-value-bind (tax total) (calculate-tax-and-total 64)
    (assert (and (eql total 67.52) (eql tax 3.52))))

(defun prompt-for-item ()
    (cons 
        (prompt-read-float "Enter the price of the item: ") 
        (prompt-read-integer "Enter the quantity of the item: ")))

(defun prompt-for-items (&optional (items '()))
        (let ((new-items (cons (prompt-for-item) items)))
            (let ((choice (prompt-read "Next item? (y/n): ")))
                (if (string= choice "y")
                    (prompt-for-items new-items)
                    new-items))))

(defun calculate-item-total (item)
    (* (car item) (cdr item)))

(assert (= (calculate-item-total (cons 3 5)) 15))

(defun self-checkout ()
        (let ((subtotal (reduce #'+ (map 'list #'calculate-item-total (prompt-for-items)))))
            (multiple-value-bind (tax total) (calculate-tax-and-total subtotal)
                (print-subtotal-tax-total t subtotal tax total))))

;; Problem 11 Currency Conversion

(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))

(defun print-conversion-result (output-stream from-amount from-currency rate to-amount to-currency)
    (format output-stream "~,4d ~A at an exchange rate of ~,4d is~%~,4d ~A~%" from-amount from-currency rate to-amount to-currency))

(assert (string= 
    (print-conversion-result nil 81 "euros" 137.51 111.38 "U.S. dollars") 
    (format nil "81 euros at an exchange rate of 137.51 is~%111.38 U.S. dollars~%")))

(defun calculate-to-amount (from-amount rate to-currency-decimal-places)
    (float (round-to (* from-amount (/ rate 100)) to-currency-decimal-places)))

(assert (eql (calculate-to-amount 81 137.51 2) 111.38))

(defun currency-conversion ()
        (let ((conversions (make-hash-table))
                (eur-rates (make-hash-table)))
            (setf (gethash 'usd eur-rates) 137.11)
            (setf (gethash 'eur conversions) eur-rates)
            (let ((from-amount (prompt-read-float "Amount to convert: "))
                    (from-currency (intern (string-upcase (prompt-read "From currency: "))))
                    (to-currency (intern (string-upcase (prompt-read "To currency: ")))))
                (multiple-value-bind (from-currency-table present) (gethash from-currency conversions)
                    (if present
                        (let ((rate (gethash to-currency from-currency-table)))
                            (if (null rate)
                                (format t "Currency pair: ~A, ~A not found~%" from-currency to-currency)
                                (print-conversion-result t from-amount from-currency rate 
                                    (calculate-to-amount from-amount rate 2) to-currency))))))))


;; Problem 12 Computing Simple Interest

(defun print-accrued-amount (output-stream years interest-rate accrued-amount)
    (format output-stream "After ~A years at ~A%, the investment will be worth $~$.~%" years interest-rate accrued-amount))

(assert (string= (print-accrued-amount nil 4 4.3 1758) (format nil "After 4 years at 4.3%, the investment will be worth $1758.00.~%")))

(defun calculate-accrued-amount (principal interest-rate years)
    (+ principal (* years (* principal (/ interest-rate 100)))))

(assert (= (calculate-accrued-amount 1500 4.3 4) 1758))

(defun simple-interest ()
    (let ((principal (prompt-read-float "Enter the principal: "))
            (interest-rate (prompt-read-float "Enter the rate of interest: "))
            (years (prompt-read-float "Enter the number of years: ")))
        (print-accrued-amount t years interest-rate (calculate-accrued-amount principal interest-rate years))))

;; Problem 13 Determining Compound Interest

(defun print-compounded-amount (output-stream principal interest-rate years annual-compound-frequency accrued-amount)
    (format output-stream "$~$ invested at ~A% for ~A years compounded ~A times per year is $~$.~%" principal interest-rate years annual-compound-frequency accrued-amount))

(assert (string= (print-compounded-amount nil 1500 4.3 6 4 1938.84) (format nil "$1500.00 invested at 4.3% for 6 years compounded 4 times per year is $1938.84.~%")))

(defun calculate-compounded-amount (principal interest-rate years annual-compound-frequency)
    (let* ((percentage-rate (/ interest-rate 100))
            (rate-divide-frequency (/ percentage-rate annual-compound-frequency))
            (years-times-frequency (* years annual-compound-frequency)))
        (float (round-to (* principal (expt (+ 1 rate-divide-frequency) years-times-frequency)) 2))))
    
(assert (= (calculate-compounded-amount 1500 4.3 6 4) 1938.84))

(defun compound-interest ()
    (let ((principal (prompt-read-float "Enter the principal: "))
            (interest-rate (prompt-read-float "Enter the rate of interest: "))
            (years (prompt-read-float "Enter the number of years: "))
            (frequency (prompt-read-float "Enter the number of times the interest is compounded: ")))
        (print-compounded-amount t principal interest-rate years frequency (calculate-compounded-amount principal interest-rate years frequency))))

;; Problem 14 Tax Calculator

(defun calculate-tax (order-amount)
    (let ((tax-rate (/ 55 10)))
        (float (* (/ tax-rate 100) order-amount))))

(defun taxablep (state)
    (string= state "WI"))

(defun tax-calculator ()
    (let ((order-amount (prompt-read-float "What is the order amount? "))
            (state (prompt-read "What is the state? ")))
        (if (taxablep state)
            (let* ((tax (calculate-tax order-amount))
                   (total (+ order-amount tax)))
                (format t "The subtotal is $~$.~%The tax is $~$.~%The total is $~$.~%" order-amount tax total))
            (format t "The total is $~$.~%" order-amount))))

(run-tests 
    (make-test "it should return 0.55 if order amount is 10" (= (calculate-tax 10) 0.55))
    (make-test "it should return 1.1 if order amount is 20" (= (calculate-tax 20) 1.1))
    (make-test "it should be taxable if state is WI" (taxablep "WI"))
    (make-test "it should be not be taxable if state is not WI" (not (taxablep "TX"))))

;; Problem 15 Password Validation

(defun passwordp (password)
    (string= password "abc$123"))

(defun password-validation ()
    (let ((password (prompt-read "What is the password? ")))
            (if (passwordp password)
                (princ "Welcome!")
                (princ "I don't know you."))))

(run-tests
    (make-test "it should return true for the correct password" (passwordp "abc$123"))
    (make-test "it should return false for the wrong password" (not (passwordp "wrongpassword"))))

;; Problem 16 Legal Driving Age

(defun legalp (age) 
    (>= age 16))

(run-tests
    (make-test "it should return true if age == 16" (legalp 16))
    (make-test "it should return true if age > 16" (legalp 17))
    (make-test "it should return false if age < 16" (not (legalp 15))))

(defun validp (age)
    (>= age 1))

(run-tests
    (make-test "it should return true if age > 0" (validp 1))
    (make-test "it should return false if age == 0" (not (validp 0)))
    (make-test "it should return false if age < 0" (not (validp -1))))

(defun legal-driving-age ()
    (let ((age (prompt-read-integer "What is you age? ")))
        (if (not (validp age))
            (legal-driving-age)
            (if (legalp age)
                (princ "You are old enough to legally drive.")
                (princ "You are not old enough to legally drive.")))))

;; Problem 24 Anagram Checker

(defun anagramp (str1 str2)
    (cond 
        ((not (= (length str1) (length str2))) nil)
        (t 
            (let ((h (make-hash-table)))
                (loop for c across str1 do
                    (setf (gethash c h) (+ (gethash c h 0) 1)))
                (loop for c across str2 do
                    (cond 
                        ((zerop (gethash c h 0)) nil)
                        ((= (gethash c h) 1) (remhash c h))
                        (t (setf (gethash c h) (- (gethash c h) 1)))))
                (zerop (hash-table-count h))))))

(run-tests 
    (make-test "it should return true for blank strings" (anagramp "" ""))
    (make-test "it should return true for identical strings" (anagramp "a" "a"))
    (make-test "it should return true for  strings of same length and characters" (anagramp "abc" "bca"))
    (make-test "it should return false for strings of different length" (not (anagramp "abc" "abbc")))
    (make-test "it should return false for strings of same length but different characters " (not (anagramp "abc" "bcd")))
    )

;; Problem 25 Password Strength Indicator
