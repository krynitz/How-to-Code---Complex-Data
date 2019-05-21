;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fractals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define CUTOFF 2)

;; Number -> Image
;; produces a Sierpinski Triangle of the given size
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2))
              (overlay (triangle (* 2 CUTOFF) "outline" "red")
                   (local [(define sub (triangle CUTOFF "outline" "red"))]
                     (above sub
                            (beside sub sub)))))

;(define (stri s)
; (square 0 "solid" "white"))

#;
(define (genrec-fn d)
  (if (trivial? d)
      (trivial-answer d)
      (... d
           (genrec-fn (next-problem d)))))

(define (stri s)
  (if (<= s CUTOFF)
      (triangle s "outline" "red")
      (overlay (triangle s "outline" "red")
               (local [(define sub (stri (/ s 2)))]
                 (above sub
                        (beside sub sub))))))

;; Number -> Image
;; produces a Sierpinski carpet of a given size
(check-expect (scarpet CUTOFF) (square CUTOFF "outline" "red"))
(check-expect (scarpet (* CUTOFF 3))
              (overlay (square (* CUTOFF 3) "outline" "red")
                               (local [(define sub (square CUTOFF "outline" "red"))
                                       (define blk (square CUTOFF "solid" "white"))]
                                 (above (beside sub sub sub)
                                        (beside sub blk sub)
                                        (beside sub sub sub)))))

;(define (scarpet s) (square 0 "solid" "white"))

(define (scarpet s)
  (if (<= s CUTOFF)
      (square s "outline" "red")
      (overlay (square s "outline" "red")
               (local [(define sub (scarpet (/ s 3)))
                       (define blk (square (/ s 3) "solid" "white"))]
                 (above (beside sub sub sub)
                        (beside sub blk sub)
                        (beside sub sub sub))))))
