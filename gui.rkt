#lang racket/gui

(require pict
         racket/draw)
 (require (planet jphelps/guiml))

(define spad (make-object bitmap% "spade.png"))
(define clb (make-object bitmap% "clubs.png"))
(define hrt (make-object bitmap% "heart.png"))
(define dmnd (make-object bitmap% "diamond.png"))
(define welcomepic (make-object bitmap% "openingpic.jpg"))

(define isResetVisible #f)

(define deck-numbers
  '("A" 2 3 4 5 6 7 8 9 10 "J" "Q" "K"))

(define player-deck '())

(define dealer-deck '())


(define suit-types (list spad clb hrt dmnd))

(define (make-deck num types)
  (define (outer-loop t)
    (define (inner-loop n)
      (if (null? n) '()
          (cons (cons  (car t) (car n)) (inner-loop (cdr n)))
          )
      )
      (if (null? t) '() 
    (append (inner-loop num) (outer-loop (cdr t))))
    )
  (outer-loop types)
)


(define (shuffle-list list)
  (define (loop in out n)
    (if (= n 0) (begin (cons (car in) (shuffle-list (append (cdr in) out))))
                (loop (cdr in) (cons (car in) out) (- n 1))))
  (if (null? list)
      '()
      (loop list '() (random (length list)))))



(define deck (shuffle-list (make-deck suit-types deck-numbers)))

(define copy-deck (shuffle-list (make-deck suit-types deck-numbers)))



(define (len dck)
(if (null? dck) 0
(+ 1 (len (cdr dck))))
  )

; Naive calculation of value of a hand
(define (count-val-of-deck dck)
(if (null? dck) 0
  (if (number? (car (car dck))) (+ (car (car dck)) (count-val-of-deck (cdr dck)))
   (if (equal? "A" (car (car dck))) (+ 11 (count-val-of-deck (cdr dck)))     
   (+ 10 (count-val-of-deck (cdr dck)))   
   )
)
  ))

; Counts the number of times 'A shows up from a deck. 
(define (num-of-aces deck)
  (define (count lst result)
    (cond
      ((null? lst) result)
      ((eq? (caar lst) "A") (count (cdr lst) (+ 1 result)))
      (else (count (cdr lst) result))))
  (count deck 0))


;; Function to test whether a hand is a bust
(define (bust? p)
  (> (best-total p) 21))

; Function to find the  best total for a given hand
(define (best-total deck)
  (define aces (num-of-aces deck))
  (define count (count-val-of-deck deck))

  (define (best deck)
  
    (if (> aces 0) (if (and (> count 21) (> (num-of-aces deck) 0))
                       (begin
                         (set! count (- count 10))
                         (set! aces (- aces 1))
                         (best deck)) count)
        count )
    )
  (best deck)
  )
; Sample Hand 
(define temp '(("A" . clubs)
                 ("K" . clubs)))
; Sample Hand
(define temp_new '(("A" . clubs)
  (6 . diamond)))



(define frame (new frame% [label "Blackjack"][width 1096]	 
   	 	[height 1024]))
(define turn-message (new message% [parent frame]
                 [label "Welcome to Blackjack"]))

(define panel
(new horizontal-panel%
[parent frame]
[vert-margin 10]	 
[horiz-margin 10]	 
[border 10]
[alignment '(center center)]
[spacing 1]	 
     ))

(define dealer-panel1
(new vertical-panel%
[parent panel]
[vert-margin 10]	 
[horiz-margin 10]
[style '(auto-vscroll)]
[border 10]
[alignment '(center center)]
[spacing 1]	 
     ))

(new message%	 
   	 	[label "Dealer deck"]	 
   	 	[parent dealer-panel1])

(define dealer-deck-val-message
  (new message%	 	 
   	 	[label "Dealer score: 0"]	 
   	 	[parent dealer-panel1]))


(define dealer-panel
(new vertical-panel%
[parent dealer-panel1]
[vert-margin 10]	 
[horiz-margin 10]
[style '(auto-vscroll)]
[border 10]
[alignment '(center center)]
[spacing 1]	 
     ))


(define panel1
(new vertical-panel%
[parent panel]
[vert-margin 10]
[horiz-margin 10]	 
[border 10]
[alignment '(center center)]
[spacing 1]	 
     ))




 (define hit-button  (new button%
       [parent panel1]
       [label "Hit"]
       [vert-margin 10]	 
       [min-width 200]
       [min-height 60]
       [callback (lambda (button event)(player-play))]))
  
 (define stay-button (new button%
       [parent panel1]
       [label "Stay"]
       [vert-margin 10]	 
       [min-width 200]
       [min-height 60]
       [callback (lambda (button event)(dealer-play))]))


(define user-panel1
(new vertical-panel%
[parent panel]
[vert-margin 10]
[style '(auto-vscroll)]
[horiz-margin 10]	 
[border 10]
[alignment '(center center)]
[spacing 1]	 
     ))

(new message%	 
   	 	[label "Player deck"]	 
   	 	[parent user-panel1])
(define width 600)
(define height 250)
(define (headingFont size)  (make-object font% size 'modern 'normal 'bold))

;Welcome screen Frame
(define openingFrame (new frame% [label "Welcome"]
                         	[width width]	 
   	 	[height height]
                [style (list 'no-resize-border)]))
;welcome message
(define welcomeMsg (new message%[parent openingFrame]
                        [label "Welcome to the BlackJack"]
                        [font (headingFont 20)]))
(new message% [parent openingFrame][label welcomepic ])
;buttons at bottom of the screen
(define welcomePanel (new horizontal-panel%
                          [parent openingFrame]
                          [alignment '(center center)]
                          [spacing 30]))
(define (start-callback)(send openingFrame show #f)(send frame show #t))
(new button% [label "Start"]
     [parent welcomePanel]
     [min-width 90]
     [min-height 40]
     [callback (lambda (button event)(start-callback))])

(new button% [label "Quit"]
     [parent welcomePanel]
     [min-width 90]
     [min-height 40]
     [callback (lambda (button event)(send openingFrame show #f))])

(send openingFrame show #t)
  ;;Add your code here!!!(send frame show #t)

(define player-deck-val-message 
  (new message%	 	 
   	 	[label "Player score: 0"]	 
   	 	[parent user-panel1]))

(define user-panel
(new vertical-panel%
[parent user-panel1]
[vert-margin 10]
[style '(auto-vscroll)]
[horiz-margin 10]	 
[border 10]
[alignment '(center center)]
[spacing 1]	 
     ))


;Code to add an obtained card to the users hand
(define (add-to-user-hand card )
( set! player-deck (append (list card) player-deck)))


;Code to add an obtained card to the dealers hand
(define (add-to-dealer-hand card )
( set! dealer-deck (append (list card) dealer-deck)))


;Code to retrive a card and update the gui 
(define (get-card-both panel-name fn)
  (define card (car deck))
  (fn card)
  (set! deck (cdr deck))
 (append-card (car card)(cdr card) panel-name)
  
  (define player-total (best-total player-deck))
  

    (if (equal? fn add-to-user-hand)

        (if (equal? 21 player-total) (update-turn-message "Player wins!!!!!!")

            (if (> player-total 21) (begin (update-turn-message "Player lost - Dealer wins!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset))
                
            (begin (get-card-both dealer-panel add-to-dealer-hand)
                   (update-turn-message "Dealer played - your turn!")
                    ) ) )

   "")
  (define dealer-total (best-total dealer-deck))

  (if (equal? fn add-to-dealer-hand)

        (if (equal? 21 dealer-total) (update-turn-message "Dealer wins!!!!!!")
                   (update-turn-message "Dealer played - your turn!")
                    ) 

   "")


(if (> dealer-total 21) (begin (update-turn-message "Dealer lost - Player wins!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset) ) ""
    
    )
(update-score-message dealer-deck-val-message dealer-total)
(update-score-message player-deck-val-message player-total)
  )


;Code to update gui when hit is called
(define (append-card lbl1 lbl2 panel-name)
(new message%	 
   	 	[label  (~v lbl1)]	 
   	 	[parent panel-name])
  (new message%	 
   	 	[label lbl2]	 
   	 	[parent panel-name])
  )

(define (update-turn-message lbl)
  (send turn-message set-label lbl))

(define (update-score-message msg lbl)
  (send msg set-label (string-append "Score is "(~v lbl))))


(define (restart)
(set! deck (shuffle-list copy-deck))
(set! player-deck '())
(set! dealer-deck '())
(delete-children dealer-panel)
(delete-children user-panel)
  (send hit-button enable #t)
  (send stay-button enable #t)
 (update-turn-message "Welcome to blackjack")
 (send dealer-deck-val-message set-label "Dealer score is : 0 ")
   (send player-deck-val-message set-label "Player score is : 0 "))



(define (display-reset)
  (if (equal? #f isResetVisible)
      (begin
        (set! isResetVisible #t)
 (new button%
       [parent panel1]
       [label "Play again!"]
       [vert-margin 10]	 
       [min-width 200]
       [min-height 60]
       [callback (lambda (button event)(restart))]))
""
      ))



(define (player-play)
 (define card (car deck))
  (add-to-user-hand card)
  (set! deck (cdr deck))
 (append-card (car card)(cdr card) user-panel)
  (define player-total (best-total player-deck))
 (if (equal? 21 player-total) (begin  (update-turn-message "Player wins!!!!!!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset))
  (if ( > player-total 21 ) (begin (update-turn-message "Player lost - Dealer wins!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset))

      "")
     )
(update-score-message player-deck-val-message player-total)
  )


(define (dealer-play)
 (define card (car deck))
  (add-to-dealer-hand card)
  (set! deck (cdr deck))
 (append-card (car card)(cdr card) dealer-panel)
  (define player-total (best-total player-deck))
    (define dealer-total (best-total dealer-deck))
(update-score-message dealer-deck-val-message dealer-total)
 (if ( > dealer-total 21)(begin (update-turn-message "Dealer lost - Player wins!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset))
     
(if (equal? dealer-total 21) (begin (update-turn-message "Dealer won - Player lost!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset))

 (if (> dealer-total player-total) (begin (update-turn-message "Dealer won - Player lost!")
                               (send hit-button enable #f)
                               (send stay-button enable #f)
                               (display-reset))
     (dealer-play)
    
     )
)
)

  
)