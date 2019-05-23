;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname worklist-accumulator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Data definitions

(define-struct wiz (name house kids))

;; Wizard is (make-wiz String String (listof Wizard))
;; interp. a wizard with a name, house and list of children

(define Wa (make-wiz "A" "S" empty))
(define Wb (make-wiz "B" "G" empty))
(define Wc (make-wiz "C" "R" empty))
(define Wd (make-wiz "D" "H" empty))
(define We (make-wiz "E" "R" empty))
(define Wf (make-wiz "F" "R" (list Wb)))
(define Wg (make-wiz "G" "S" (list Wa)))
(define Wh (make-wiz "H" "S" (list Wc Wd)))
(define Wi (make-wiz "I" "H" empty))
(define Wj (make-wiz "J" "R" (list We Wf Wg)))
(define Wk (make-wiz "K" "G" (list Wh Wi Wj)))

; template arb-arity-tree, encapsulation w/ local
#;
(define (fn-for-wiz w)
  (local [(define (fn-for-wiz w)
            (... (wiz-name w)
                 (wiz-house w)
                 (fn-for-low (wiz-kids w))))
          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wiz (first low))
                        (fn-for-low (rest low)))]))]
    (fn-for-wiz w)))

;; Functions

;; Wizard -> (listof String)
;; produces the names of every decendant in the same house as their parent
(check-expect (same-house-as-parent Wa) empty)
(check-expect (same-house-as-parent Wh) empty)
(check-expect (same-house-as-parent Wg) (list "A"))
(check-expect (same-house-as-parent Wk) (list "A" "F" "E"))

#;
; template from Wizard plus lost context accumulator
(define (same-house-as-parent w)
  ;; parent-house is String; the house of this wizard's immediate parent ("" for root of tree)
  ;; (same-house-as-parent Wk)
  ;; (fn-for-wiz Wk "")
  ;; (fn-for-wiz Wh "G")
  ;; (fn-for-wiz Wc "S")
  ;; (fn-for-wiz Wd "S")
  ;; (fn-for-wiz Wi "G")
  (local [(define (fn-for-wiz w parent-house)
            (if (string=? (wiz-house w) parent-house)                 
                (cons (wiz-name w)
                      (fn-for-low (wiz-kids w)
                                  (wiz-house w)))
                (fn-for-low (wiz-kids w)
                            (wiz-house w))))
          (define (fn-for-low low parent-house)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-wiz (first low) parent-house)
                           (fn-for-low (rest low) parent-house))]))]
    (fn-for-wiz w "")))

;; template from Wizard (arb-arity-tree,wrapped in local), plus worklist accumulator for tail recursion
;; added result so far accumulator for tail recursion
;; added compound data definition for wish list entries
(define (same-house-as-parent w)
  ;; todo is (listof ...) a worklist accumulator
  ;; rsf is (listof String) a result so far accumulator
  (local [
          (define-struct wle (w ph))
          ;; WLE (worklist entry) is (make-wle Wizard String)
          ;; interp a worklist entry with the wizard to pass to fn-for-wiz and that wizard a parent house
          (define (fn-for-wiz todo w ph rsf)
            ;(... (wiz-name w)(wiz-house w)
            (fn-for-low (append (map (λ (k)
                                       (make-wle k (wiz-house w)))
                                     (wiz-kids w))
                                todo)
                        (if (string=? (wiz-house w) ph)
                            (cons (wiz-name w) rsf)
                            rsf)))
          (define (fn-for-low todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-wiz (rest todo)
                               (wle-w (first todo))
                               (wle-ph (first todo))
                               rsf)]))]
    (fn-for-wiz empty w "" empty)))

;; Wizard -> Natural
;; produces the number of wizards in that tree (including the root)
(check-expect (count Wa) 1)
(check-expect (count Wk) 11)

;(define (count w) 0) ; stub

; template from Wizard, add an accumulator for tail recursion

(define (count w)
  ;; rsf is Natural, the number of wizrds seen so far
  ;; todo is listofWizard, wizards we still need to visit with the function
  ;; (count Wk)
  ;; (fn-for-wiz Wk 0)
  ;; (fn-for-wiz Wh 1)
  ;; (fn-for-wiz Wc 2)
  (local [(define (fn-for-wiz w todo rsf)
            (fn-for-low (append (wiz-kids w) todo)
                        (add1 rsf)))
          (define (fn-for-low todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-wiz (first todo) (rest todo) rsf)]))]
    (fn-for-wiz w empty 0)))

