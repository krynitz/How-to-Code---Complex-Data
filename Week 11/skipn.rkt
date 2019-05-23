;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname skipn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfX Natural -> ListOfX
;; produces a list containing 1st element of lox, then skips next n then include ...
(check-expect (skipn empty 0) empty)
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 0) (list "a" "b" "c" "d" "e" "f"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 1) (list "a" "c" "e"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))

; (define (skipn lox n) empty) ; stub

;; template for ListOfX using accumulator
(define (skipn lox0 n)
  ;; acc: Natural ; the number of elements to skip before including the next one
  ;; (skipn (list "a" "b" "c" "d" "e" "f") 2) ; outer call
  ;;
  ;; (skipn (list     "b" "c" "d" "e" "f") 0) ; include
  ;; (skipn (list         "c" "d" "e" "f") 2) ; don't include
  ;; (skipn (list             "d" "e" "f") 1) ; don't include
  ;; (skipn (list                 "e" "f") 0) ; include
  (local [(define (skipn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                       (cons (first lox)
                             (skipn (rest lox)
                                    n))
                       (skipn (rest lox)
                              (sub1 acc)))]))]
    (skipn lox0 0)))