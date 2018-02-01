#lang typed/racket


(require typed/test-engine/racket-tests)
(require "core.rkt")
(require "universe.rkt")
(require "cs151-image.rkt")

(define-type Choice
  (U 'Rock 'Paper 'Scissors 'Null))

(define-struct World
  ([first : String]
   [second : String]
   [firstchoice : Choice]
   [fscore : Integer]
   [sscore : Integer]
   [round : Integer]))

(: draw : World -> Image)
(define (draw w)
  (match w
    [(World first second firstchoice fscore sscore round)
     (above (overlay (if (= round 0)
                         (text
                          (if (= fscore sscore)
                              "Draw!"
                              (string-append
                               (if (> fscore sscore) first second) " wins!"))
                          40 "black")
                         (match firstchoice
                           ['Null (text (string-append first "'s turn")
                                        40 "black")]
                           [_ (text (string-append second "'s turn")
                                    40 "black")]))
                     (rectangle 300 100 "solid" "beige"))
            (beside (overlay (text "Rock!" 20 "black")
                             (rectangle 100 100 "solid" "maroon"))
                    (overlay (text "Paper!" 20 "black")
                             (rectangle 100 100 "solid" "maroon"))
                    (overlay (text "Scissors!" 20 "black")
                             (rectangle 100 100 "solid" "maroon")))
            (beside (overlay (text
                              (string-append first ":" (number->string fscore))
                              40 "black")
                             (rectangle 150 100 "solid" "yellow"))
                    (overlay (text
                              (string-append second ":" (number->string sscore))
                              40 "black")
                             (rectangle 150 100 "solid" "green")))
            (overlay (text
                      (string-append "Rounds left:" (number->string round))
                      40 "black")
                     (rectangle 300 100 "solid" "beige")))]
    [_ empty-image]))
 
(: play : World Integer Integer Mouse-Event -> World)
(define (play w x y me)
  (match w
    [(World first second firstchoice fscore sscore r)
     (match me
       ["button-down"
        (cond
          [(and (< 0 x 100) (< 100 y 200) (not (= r 0)))
           (match firstchoice
             ['Null (World first second 'Rock fscore sscore r)]
             ['Rock (World first second 'Null fscore sscore r)]
             ['Paper (World first second 'Null fscore (add1 sscore) (sub1 r))]
             ['Scissors (World first second 'Null (add1 fscore) sscore (sub1 r))])]
          [(and (< 100 x 200) (< 100 y 200) (not (= r 0)))
           (match firstchoice
             ['Null (World first second 'Paper fscore sscore r)]
             ['Paper (World first second 'Null fscore sscore r)]
             ['Scissors (World first second 'Null fscore (add1 sscore) (sub1 r))]
             ['Rock (World first second 'Null (add1 fscore) sscore (sub1 r))])]
          [(and (< 200 x 300) (< 100 y 200) (not (= r 0)))
           (match firstchoice
             ['Null (World first second 'Scissors fscore sscore r)]
             ['Scissors (World first second 'Null fscore sscore r)]
             ['Rock (World first second 'Null fscore (add1 sscore) (sub1 r))]
             ['Paper (World first second 'Null (add1 fscore) sscore (sub1 r))])]
          [(= r 0)
           (World first second 'Null fscore sscore -1)]
          [else w])]
       [_ w])]))

(: stop : World -> Boolean)
(define (stop w)
  (match w
    [(World _ _ _ _ _ -1) #t]
    [_ #f]))

(: play-new : String String Integer -> World)
(define (play-new f s r)
  (big-bang (World f s 'Null 0 0 r) : World
    [to-draw draw]
    [on-mouse play]
    [name 'RockPaperScissors!]
    [stop-when stop]))

