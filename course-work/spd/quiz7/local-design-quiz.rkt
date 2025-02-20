;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local-design-quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; local-design-quiz.rkt

;; Data Definitions ==============================

;; Player is String
;; interp.  the name of a tennis player.
(define P0 "Maria")
(define P2 "Serena")
#;
(define (fn-for-player p)
  (... p))


;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; interp.  a team roster, ordered from best player to worst.
(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))
#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else 
         (... (fn-for-player (first r))
              (fn-for-roster (rest r)))]))


(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp.  a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))
#;
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))


;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria") (make-match "Gabriela" "Nadia")))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))


;; Function Definitions ==========================

;; Roster Roster -> Boolean
;; Produce true if players in both given rosters all play
(check-expect (all-play? R0 R0) true)
(check-expect (all-play? R0 R1) false)
(check-expect (all-play? R1 R0) false)
(check-expect (all-play? R1 R2) false)
(check-expect (all-play? R1 (list "dude1" "dude2" "dude3" "dude4")) true)

(define (all-play? rosa rosb)
  (cond [(and (empty? rosa) (empty? rosb)) true]
        [(or (empty? rosa) (empty? rosb)) false]
        [else (all-play? (rest rosa)
                         (rest rosb))]))


;; Roster Roster -> ListOfMatches
;; Produce list of matched that will be played by the two given teams.
;; ASSUME: Both teams have the same number of players.
(check-expect (list-matches R0 R0) empty)
(check-expect (list-matches R1 R1)
              (list (make-match "Eugenie" "Eugenie")
                    (make-match "Gabriela" "Gabriela")
                    (make-match "Sharon" "Sharon")
                    (make-match "Aleksandra" "Aleksandra")))
(check-expect (list-matches R1 (list "dude1" "dude2" "dude3" "dude4"))
              (list (make-match "Eugenie" "dude1")
                    (make-match "Gabriela" "dude2")
                    (make-match "Sharon" "dude3")
                    (make-match "Aleksandra" "dude4")))

(define (list-matches rosa rosb)
  (cond [(and (empty? rosa) (empty? rosb)) empty]
        [else (append (list (make-match (first rosa)
                                        (first rosb)))
                      (list-matches (rest rosa)
                                    (rest rosb)))]))
