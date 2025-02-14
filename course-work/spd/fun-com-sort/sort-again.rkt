;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sort-again) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
(define IMG1 (rectangle 10 20 "solid" "red"))
(define IMG2 (rectangle 15 25 "solid" "blue"))
(define IMG3 (rectangle 20 30 "solid" "blue"))

(define SPACE (square 5 "solid" "white"))

;------------------
;; Data Definitions

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images
(define LOI1 empty)
(define LOI2 (cons IMG2 (cons IMG3 (cons IMG1 empty))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;-----------
;; Functions

;; ListOfImage -> Image
;; produce all images laid out left to right increasing by size
(check-expect (arrange-images (cons IMG2 (cons IMG3 (cons IMG1 empty))))
              (beside/align "bottom" IMG1 IMG2 IMG3 empty-image))

(define (arrange-images loi)
  (layout-images (sort-images loi)))

;; ListOfImage -> Image
;; produce an image of all images in a given list laid out left to right
(check-expect (layout-images LOI1) empty-image)
(check-expect (layout-images (cons IMG2 (cons IMG3 (cons IMG1 empty))))
              (beside/align "bottom" IMG2 IMG3 IMG1 empty-image))

(define (layout-images loi)
  (cond [(empty? loi) empty-image]
        [else (beside/align "bottom"
                            (first loi)
                            (layout-images (rest loi)))]))

;; ListOfImage -> ListOfImage
;; produce list of images from sorting given list of images by size (asc)
(check-expect (sort-images LOI1) empty)
(check-expect (sort-images (cons IMG2 (cons IMG3 (cons IMG1 empty))))
              (cons IMG1 (cons IMG2 (cons IMG3 empty))))
(check-expect (sort-images (cons IMG1 (cons IMG2 (cons IMG3 empty))))
              (cons IMG1 (cons IMG2 (cons IMG3 empty))))

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
         (insert (first loi)
                 (sort-images (rest loi)))]))

;; Image ListOfImage -> ListOfImage
;; produce list with image added in correct place
;; ASSUME: loi is sorted by size
(check-expect (insert IMG1 LOI1) (cons IMG1 empty))
(check-expect (insert IMG2 (cons IMG1 (cons IMG3 empty)))
              (cons IMG1 (cons IMG2 (cons IMG3 empty))))

(define (insert i loi)
  (cond [(empty? loi) (cons i empty)]
        [else (if (smaller-than? i (first loi))
                  (cons i loi)
                  (cons (first loi) (insert i (rest loi))))]))

;; Image Image -> Boolean
;; produce true if first image is smaller than second
(check-expect (smaller-than? IMG1 IMG2) true)
(check-expect (smaller-than? IMG2 IMG2) false)
(check-expect (smaller-than? IMG3 IMG2) false)

(define (smaller-than? a b)
  (< (calculate-area a)
     (calculate-area b)))

;; Image -> Natural
;; produce area of given image
(check-expect (calculate-area IMG1) (* (image-width IMG1) (image-height IMG1)))
(check-expect (calculate-area IMG2) (* (image-width IMG2) (image-height IMG2)))
(check-expect (calculate-area IMG3) (* (image-width IMG3) (image-height IMG3)))

(define (calculate-area a)
  (* (image-width a) (image-height a)))