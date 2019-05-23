;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname tail-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ListOfNumber -> Number
;; produce sum of all elements of lon
(check-expect (sum empty) 0)
(check-expect (sum (list 2 4 5)) 11)
#;
(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))

(define (sum lon0)
  ;; acc: Number; the sum of the elements of lon0 seen so far
  ;; (sum (list 2 4 5))
  ;; (sum (list 2 4 5) 0)
  ;; (sum (list   4 5) 2)
  ;; (sum (list     5) 6)
  ;; (sum (list      ) 11)
  (local [(define (sum lon)
            (cond [(empty? lon) acc]
                  [else
                   (sum (rest lon)
                        (+ acc (first lon)))]))]
    (sum lon0 0)))

(sum (list 1 2 3 4 5))