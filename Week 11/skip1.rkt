;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname skip1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfX -> ListOfX
;; produce list consisting of only the first thrid fifth ... elements of lox
(check-expect (skip1 empty) empty)
(check-expect (skip1 (list "a" "b" "c" "d")) (list "a" "c"))
(check-expect (skip1 (list 1 2 3 4 5 6)) (list 1 3 5))

;(define (skip1 lox) empty) ;stub

(define (skip1 lox)
  ; acc: Natural; position of first lox in lox0
  ;; (skip1 (list "a" "b" "c") 1)
  ;; (skip1 (list     "b" "c") 2
  ;; (skip1 (list         "c") 3)
  (local [(define (skip1 lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (odd? acc)
                       (cons (first lox)
                             (skip1 (rest lox)
                                    (... acc)))
                       (skip1 (rest lox)
                              (add1 acc)))]))]
    (skip1 lox0 1)))

#;
(define (skip1 lox)
  (cond [(empty? lox) empty]
        [else
         (if (odd? POSITION-OF-FIRST-LOX)
             (cons (first lox)
                   (skip1 (rest lox)))
             (skip1 (rest lox)))]))