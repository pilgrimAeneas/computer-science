;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname scratch2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct  pos (x y)) ; declaration of 4 definitions

(define BALL1-POS (make-pos 3 6)) ; 1 constructor
(define CAT1 (make-pos 2 8))

(pos-x CAT1)                 ; 2 selectors
(pos-y CAT1)

(pos? CAT1)                  ; 1 predicate
(pos? "Hello!")