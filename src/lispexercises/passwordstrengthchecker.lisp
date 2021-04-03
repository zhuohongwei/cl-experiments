(ql:quickload "testlib")
(use-package :testlib)

;; A password is said to be strong if it satisfies the following criteria: 

;; It contains at least one lowercase English character.
;; It contains at least one uppercase English character.
;; It contains at least one special character. The special characters are: !@#$%^&*()-+
;; Its length is at least 8.
;; It contains at least one digit.

;; Given a string, find its strength. 
;; Let a strong password is one that satisfies all above conditions. 
;; A moderate password is one that satisfies first three conditions and has length at least 6. 
;; Otherwise password is weak.

(defun password-strength (password)
    (let ((uc-alpha-char-p  (lambda (c) (and (alpha-char-p c) (lower-case-p c))))
          (lc-alpha-char-p  (lambda (c) (and (alpha-char-p c) (upper-case-p c))))
          (special-char-p (lambda (c) (some #'(lambda (x) (eql c x)) "!@#$%^&*()-+"))))
        (cond 
            ((>= (length password) 8) 
                    (if (and (some uc-alpha-char-p password) 
                             (some lc-alpha-char-p password) 
                             (some special-char-p password)
                             (some #'digit-char-p password))
                        'strong
                        'weak))
            ((>= (length password) 6)
                (if (and (some lc-alpha-char-p password)
                         (some uc-alpha-char-p password)
                         (some special-char-p password)) 
                    'moderate
                    'weak)
            )
            (t 'weak))))
    
(run-tests
    (make-test "it should return strong if it contains at least 1 lowercase alphabet, at least 1 uppercase alphabet,  1 special character, 1 digit and its length is at least 8" (eq (password-strength "AAbb$$11") 'strong))
    (make-test "it should return weak if it contains no lowercase alphabet even though it has at least 1 uppercase alphabet, 1 special character, 1 digit and its length is at least 8" (eq (password-strength "AABB$$11") 'weak))
    (make-test "it should return weak if it contains no uppcase alphabet even though it has at least 1 lowercase alphabet, 1 special character, 1 digit and its length is at least 8" (eq (password-strength "aabb$$11") 'weak))
    (make-test "it should return weak if it contains no special character even though it has at least 1 lowercase alphabet, 1 uppercase alphabet, 1 digit and its length is at least 8" (eq (password-strength "AAbbcc11") 'weak))
    (make-test "it should return weak if it contains no digit even though it has at least 1 lowercase alphabet, 1 uppercase alphabet, 1 special character and its length is at least 8" (eq (password-strength "AAbbcc$$") 'weak))
    (make-test "it should return moderate if its length is less than 8 but at least 6 even though it has at least 1 lowercase alphabet, 1 uppercase alphabet, 1 special character, 1 digit" (eq (password-strength "AAbb$1") 'moderate))
    (make-test "it should return moderate if it contains at least 1 lowercase alphabet, 1 uppercase alphabet,  1 special character and its length is at least 6" (eq (password-strength "AAbb$$") 'moderate))
    (make-test "it should return weak if it contains no lowercase alphabet, even though it contains at least 1 uppercase alphabet,  1 special character and its length is at least 6" (eq (password-strength "aabb$$") 'weak))
    (make-test "it should return weak if it contains no uppcase alphabet even though it has at least 1 lowercase alphabet, 1 special character and its length is at least 6" (eq (password-strength "AABB$$") 'weak))
    (make-test "it should return weak if it contains no special character even though it has at least 1 lowercase alphabet, 1 uppercase alphabet and its length is at least 6" (eq (password-strength "AAbbcc") 'weak))
    (make-test "it should return weak if its length is less than 6 even though it has at least 1 lowercase alphabet, 1 uppercase alphabet and 1 special character" (eq (password-strength "AAbb$") 'weak))
)