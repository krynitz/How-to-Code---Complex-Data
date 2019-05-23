;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname constructing-cyclic-data) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct room (name exits))
;; Room is (make-room String (Listof Room))
;; interp the room's name and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-))

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list ))))
    -A-))

;; template structural recursion, encapsulation w/local, tail-recursive w/ worklist, context-preserving accumulator
;; what rooms have we already visited

(define (fn-for-house r0)
  ;; todo is (listof Room) a worklist accumulator
  ;; visited is (listof String) context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited))))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))

;; Room String -> Boolean
;; produce true if it is possible to reach a room named rn
(check-expect (reachable? H1 "A") true)
(check-expect (reachable? H1 "B") true)
(check-expect (reachable? H1 "C") false)
(check-expect (reachable? (first (room-exits H1)) "A") false)
(check-expect (reachable? H4 "F") true)

; (define (reachable? r0 rn) false) ;stub

(define (reachable? r0 rn)
  ;; todo is (listof Room) a worklist accumulator
  ;; visited is (listof String) context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited)
            (cond [(string=? (room-name r) rn) true]
                  [(member (room-name r) visited) (fn-for-lor todo visited)]
                  [else  (fn-for-lor (append (room-exits r) todo)
                                (cons (room-name r) visited))]))
            (define (fn-for-lor todo visited)
              (cond [(empty? todo) false]
                    [else
                     (fn-for-room (first todo)
                                  (rest todo)
                                  visited)]))]
(fn-for-room r0 empty empty)))