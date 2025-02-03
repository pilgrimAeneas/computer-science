;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; traffic-light-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

;; My world program  (make this more specific)

;; =================
;; Constants:
(define HEIGHT 200)
(define WIDTH 100)
(define MTS (empty-scene WIDTH HEIGHT))
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define TICK-SECODNS 1)

(define BULB-SIZE 15)
(define MARGIN-SIZE (/ BULB-SIZE 5)) ; fix that changing this ruins design
(define BOX-HEIGHT (+ (* BULB-SIZE 2 3) (* MARGIN-SIZE 4)))
(define BOX-WIDTH (+ (* BULB-SIZE 2) (* MARGIN-SIZE 2)))

(define OFF-BULB-RED (circle BULB-SIZE "outline" "red"))
(define OFF-BULB-YELLOW (circle BULB-SIZE "outline" "yellow"))
(define OFF-BULB-GREEN (circle BULB-SIZE "outline" "green"))

(define BULB-RED (circle BULB-SIZE "solid" "red"))
(define BULB-YELLOW (circle BULB-SIZE "solid" "yellow"))
(define BULB-GREEN (circle BULB-SIZE "solid" "green"))

(define BOX-COLOR "black")
(define MARGIN (square MARGIN-SIZE "solid" BOX-COLOR))
(define BOX (rectangle BOX-WIDTH BOX-HEIGHT "solid" BOX-COLOR))

(define RED-IMG (overlay
                 (above MARGIN
                        BULB-RED
                        MARGIN
                        OFF-BULB-YELLOW
                        MARGIN
                        OFF-BULB-GREEN
                        MARGIN)
                 BOX))

(define YELLOW-IMG (overlay
                    (above MARGIN
                           OFF-BULB-RED
                           MARGIN
                           BULB-YELLOW
                           MARGIN
                           OFF-BULB-GREEN
                           MARGIN)
                    BOX))

(define GREEN-IMG (overlay
                   (above MARGIN
                          OFF-BULB-RED
                          MARGIN
                          OFF-BULB-YELLOW
                          MARGIN
                          BULB-GREEN
                          MARGIN)
                   BOX))

;; =================
;; Data definitions:

;; TrafficLight is one of:
;; - "red"
;; - "yellow"
;; - "green"
;; interp. as the color currently of a traffic light
;; <examples in an enumeration is redundant>
#;
(define (fn-for-traffic-light tl)
  (cond [(string=? tl "red") (...)]
        [(string=? tl "yellow") (...)]
        [(string=? tl "green") (...)]))

;; Template Rules Used:
;; one of: 3 cases
;; atmoic distinct: "red"
;; atmoic distinct: "yellow"
;; atmoic distinct: "green"

;; =================
;; Functions:

;; TrafficLight -> TrafficLight
;; start the world with (main red)
;; no tests for main function
(define (main tl)
  (big-bang tl
    (on-tick   next-color TICK-SECODNS)     ; TrafficLight -> TrafficLight
    (to-draw   render-light)))   ; TrafficLight -> Image

;; TrafficLight -> TrafficLight
;; produce the next correct traffic light given one
(check-expect (next-color "red") "yellow")
(check-expect (next-color "yellow") "green")
(check-expect (next-color "green") "red")

(define (next-color tl)
  (cond [(string=? tl "red") "yellow"]
        [(string=? tl "yellow") "green"]
        [(string=? tl "green") "red"]))

;; TrafficLight -> Image
;; render an image of traffic light on an empty scene
(check-expect (render-light "red") (place-image RED-IMG CTR-X CTR-Y MTS))
(check-expect (render-light "yellow") (place-image YELLOW-IMG CTR-X CTR-Y MTS))
(check-expect (render-light "green") (place-image GREEN-IMG CTR-X CTR-Y MTS))

(define (render-light tl)
  (cond [(string=? tl "red") (place-image RED-IMG CTR-X CTR-Y MTS)]
        [(string=? tl "yellow") (place-image YELLOW-IMG CTR-X CTR-Y MTS)]
        [(string=? tl "green") (place-image GREEN-IMG CTR-X CTR-Y MTS)]))