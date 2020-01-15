(display "Please use \"is-valid\" method and pass an integer argument")
(display " to verify that your credit card number is valid.")
(newline)

;Example: (is-valid 5231692658223510) => #t 
;         (is-valid 1111111111111111) => #f 

;declare global variable.
(define acct-num '())

;set method that sets the value of our variable to whatever argument is passed.
;(however, passed number would be converted to string to acces digits at certian index). 
(define set-acct-num (lambda (x) (set! acct-num (number->string x) )))

;get method that gets digit based on the index passed in the argument.
;(string is used to have a random access at any given index and then 
;convert given character to string and then to digit and returns it).
(define get-digit (lambda (str index) (string->number (string (string-ref str index)))))

;if number consists of two digits, they are added together otherwise 0 is added.
(define add-digits (lambda (str)

  ;add first digit to whatever is evaluated in the following "if" statement.
  (+ (get-digit str 0) 

    ;if number consists of multiple digits return second digit to be added to the first.
    (if (> (string-length str) 1) 
      (get-digit str 1) 
      0
    )
  )

))

;method that claculates sum of digits as per Luhn Algorithm.
(define get-luhn-sum (lambda (x)

  ;add passed digit to the next digit.
  (+ (get-digit acct-num x)

    ;proccess next digit (from the right).
    ;since it is at the even spot, we double it.
    ;and if this doubled number consists of two digits: add them with add-digits method. 
    (if (> x 0) 
      (add-digits (number->string (* (get-digit acct-num (- x 1)) 2))) 
      0
    )

    ;recurseve call that does the above but at the next index (starting from the right).
    (if (> x 1) 
      (get-luhn-sum (- x 2)) 
      0
    )
  )

))

;main method that is to be called to validate an account number.
(define is-valid (lambda (x) 

  ;call setter method to set the global variable to be used accross all methods.
  (set-acct-num x) 

  ;BiwaScheme Interpreter does not support "modulo" method
  ;Thus, use get-luhn-sum method to get our sum and devide it by 10.
  ;if it is an integer then it is multiple of 10 and is valid!
  (integer? (/ (get-luhn-sum (- (string-length acct-num) 1)) 10)) 

))
