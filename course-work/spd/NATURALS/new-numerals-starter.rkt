;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname new-numerals-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; new-numerals-starter.rkt

;; NATURAL is one of:
;; - empty
;; - (cons "!" NATURAL)
;; interp. a natural number, # of !s is the number
(define N0 empty)
(define N1 (cons "!" N0))
(define N2 (cons "!" N1))
(define N3 (cons "!" N2))
(define N4 (cons "!" N3))
(define N5 (cons "!" N4))
(define N6 (cons "!" N5))
(define N7 (cons "!" N6))
(define N8 (cons "!" N7))
(define N9 (cons "!" N8))

;; These are the primitives that operate on NATURAL
(define (ZERO? n) (empty? n))  ;Any         -> Boolean
(define (ADD1 n) (cons "!" n)) ;NATURAL     -> NATURAL
(define (SUB1 n) (rest n))     ;NATURAL[>0] -> NATURAL
#;
(define (fn-for-natural n)
  (cond [(ZERO? n) (...)]
        [else (... n
                   (fn-for-natural (SUB1 n)))]))

;; NATURAL NATURAL -> NATURAL
;; produce sum of two naturals
(check-expect (ADD N0 N0) N0)
(check-expect (ADD N3 N1) N4)
(check-expect (ADD N1 N1) N2)
(check-expect (ADD N3 N2) N5)

(define (ADD a b)
  (cond [(ZERO? b) a]
        [else (ADD (ADD1 a) (SUB1 b))]))

;; NATURAL NATURAL -> NATURAL
;; produce sub of two naturals
(check-expect (SUB N0 N0) N0)
(check-expect (SUB N3 N1) N2)
(check-expect (SUB N2 N1) N1)
(check-expect (SUB N6 N2) N4)

(define (SUB a b)
  (cond [(ZERO? b) a]
        [else (SUB (SUB1 a) (SUB1 b))]))

;; NATURAL NATURAL NATURAL -> NATURAL
;; produce additin of b to a multiple times
(check-expect (ADD-R N0 N0 N0) N0)
(check-expect (ADD-R N3 N2 N2) N7)
(check-expect (ADD-R N2 N3 N2) N8)
(check-expect (ADD-R N3 N3 N2) N9)
(check-expect (ADD-R N1 N1 N4) N5)
(check-expect (ADD-R N1 N4 N1) N5)

(define (ADD-R a b n)
  (cond [(ZERO? n) a]
        [else (ADD-R (ADD a b) b (SUB1 n))]))

;; NATURAL NATURAL -> NATURAL
;; produce mult of two naturals
(check-expect (TIMES N0 N0) N0)
(check-expect (TIMES N3 N1) N3)
(check-expect (TIMES N2 N2) N4)
(check-expect (TIMES N3 N2) N6)
(check-expect (TIMES N2 N3) N6)
(check-expect (TIMES N1 N4) N4)

(define (TIMES a b)
  (ADD-R N0 a b))

;; NATURAL -> NATURAL
;; produce fact of natural
(check-expect (FACT N0) N1)
(check-expect (FACT N1) N1)
(check-expect (FACT N2) N2)
(check-expect (FACT N3) N6)

(define (FACT n)
  (cond [(ZERO? n) N1]
        [else (TIMES n
                     (FACT (SUB1 n)))]))