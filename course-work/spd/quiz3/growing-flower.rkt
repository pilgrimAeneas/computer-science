;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname growing-flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Flower Up!

;; =================
;; Constants:
(define WIDTH  400)
(define HEIGHT 400)
(define MTS    (empty-scene WIDTH HEIGHT))
(define GROW-RATE 1)

;; =================
;; Data definitions:
(define-struct flower (x y s c?))
;; Flower is (make-flower Number Number Natural Boolean)
;; interp. (make-flower x y s c?) as a growing flower
;;            x y are the coordinates on screen
;;            s is the size of the flower (side length)
;;            c? is true if the flower is created (at least one mouse click)

(define F0 (make-flower   0   0  0 false))
(define F1 (make-flower 200 200 50 true ))
(define F2 (make-flower 175 130  0 true ))
#;
(define (fn-for-flower f)
  (... (flower-x  f)   ;Number
       (flower-y  f)   ;Number
       (flower-s  f)   ;Natural
       (flower-c? f))) ;Boolean

;; =================
;; Functions:

;; Flower -> Flower
;; start the world with (main (make-flower 0 0 0 false))
;; 
(define (main f)
  (big-bang f
    (on-tick   grow-flower)     ; Flower -> Flower
    (to-draw   render-flower)   ; Flower -> Image
    (on-mouse  reset-flower)))    ; Flower Integer Integer MouseEvent -> Flower

;; Flower -> Flower
;; produce the next flower from given flower with increased size
(check-expect (grow-flower F1) (make-flower 200 200 (+ (flower-s F1) GROW-RATE)  true))
(check-expect (grow-flower F2) (make-flower 175 130 (+ (flower-s F2) GROW-RATE)  true))
(check-expect (grow-flower F0) (make-flower   0   0                           0 false))

(define (grow-flower f)
  (cond [(flower-c? f)
         (make-flower (flower-x f) (flower-y f)
                      (+ (flower-s f) GROW-RATE)
                      true)]
        [else f]))

;; Flower -> Image
;; render the flower in the correct size and coordinates on MTS
(check-expect (render-flower F1) (place-image (create-flower F1)
                                             (flower-x F1) (flower-y F1)
                                             MTS))
(check-expect (render-flower F0) (place-image (create-flower F0)
                                             (flower-x F0) (flower-y F0)
                                             MTS))

(define (render-flower f) (place-image (create-flower f)
                                             (flower-x f) (flower-y f)
                                             MTS))

;; Flower -> Image
;; produce image of flower using the size of a given Flower
(check-expect (create-flower F1) (pulled-regular-polygon (flower-s F1)
                                                         5 1 140 "solid" "purple"))
(check-expect (create-flower F2) (pulled-regular-polygon (flower-s F2)
                                                         5 1 140 "solid" "purple"))
(check-expect (create-flower F0) (pulled-regular-polygon (flower-s F0)
                                                         5 1 140 "solid" "purple"))

(define (create-flower f)
  (pulled-regular-polygon (flower-s f) 5 1 140 "solid" "purple"))

;; Flower Integer Integer MouseEvent -> Flower
;; reset the flower to the given mouse
(check-expect (reset-flower F1 200 200 "button-down")    (make-flower 200 200 0 true))
(check-expect (reset-flower F0  20   0 "button-down")    (make-flower  20   0 0 true))
(check-expect (reset-flower F0  35  20 "drag") F0)

(define (reset-flower f x y me)
  (cond [(mouse=? me "button-down") (make-flower x y 0 true)]
        [else f]))