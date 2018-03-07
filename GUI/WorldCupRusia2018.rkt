;RPX=Relative position X
#lang racket

(require (lib "graphics.ss" "graphics"))(open-graphics)
(define window (open-viewport "WCR2018" 1200 512))

;--------------------------------------------------------------------------------------------------------------------------------------------
;Useful functions
;--------------------------------------------------------------------------------------------------------------------------------------------
(define (getRPX x) (+ 102 x))
(define (getRPY y) (+ 52  y))
(define (get l i) (cond ((null? l) null)((equal? i 0) (car l))(else (get (cdr l) (- i 1)) )))
;Randon function,it returns a number bewten cero and the given parameter.  
(define (randInt n) (floor (* n (random))))
(define (delta x1 x2)(- x2 x1))
(define (replace lst d i) (append (take lst i) (list d) (drop lst (+ i 1))))
;--------------------------------------------------------------------------------------------------------------------------------------------
;Drawing fuctions
;--------------------------------------------------------------------------------------------------------------------------------------------
  ;Draw the soccer field.
  (define (drawField)
  ;;Main Window 
  ((draw-solid-rectangle window) (make-posn 0 0) 1200 512 "black")
  ;;Soccer field background
  ((draw-solid-rectangle window) (make-posn 100 50) 800 400 "white")
  ;;Soccer field
  ((draw-solid-rectangle window) (make-posn 102 52) 795 395 "darkgreen")
  ;;little rectangle
  ((draw-rectangle window) (make-posn (+ 70 32) (+ 156 20)) 30 150 "white")
  ((draw-rectangle window) (make-posn (+ 163 704) (+ 178 -3)) 30 150 "white")
  ;;big rectangle
  ((draw-rectangle window) (make-posn (+ 70 32) (+ 156 -20)) 100 230 "white")
  ((draw-rectangle window) (make-posn (+ 93 704) (+ 178 -42)) 100 230 "white")
  ;;center line
  ((draw-line window) (make-posn (+ 148 352) 50) (make-posn (+ 148 352) (+ 97 352)) "white")  
  ;;circle
  ((draw-ellipse window)(make-posn (+ 150 (- 352 66)) (+ 32 150)) 132 132 "white")
  ;;WorldCup label
  ((draw-string window)(make-posn 430 30) "WorldCup Russia 2018" "white")
  ;;Score label
  ((draw-string window)(make-posn 950 120) "HOME" "white")
  ((draw-string window)(make-posn 1100 120) "VISITORS" "white")  
  )
;;Fuction that draws the score from the two teams
(define (drawCounter home visitors)
((draw-solid-rectangle window) (make-posn 1130 135) 20 20 "white")
((draw-solid-rectangle window) (make-posn 965 135) 20 20 "white")
((draw-string window) (make-posn 1135 150) (~a visitors) "blue") 
((draw-string window) (make-posn 970 150) (~a home) "red")
)
;Draw a player on screen
(define (drawPlayer x1 y1 x2 y2 team n t)
  (cond
    ((equal? 0 team)
    ((draw-solid-ellipse window)(make-posn (getRPX (moveX x1 x2 t)) (getRPY (moveY x1 y1 x2 y2 t))) 16 16 "red")
    ((draw-string window) (make-posn (getRPX (moveX x1 x2 t)) (getRPY (+ (moveY x1 y1 x2 y2 t) 16))) (number->string n) "white"))
    (else
    ((draw-solid-ellipse window)(make-posn (getRPX (moveX x1 x2 t)) (getRPY (moveY x1 y1 x2 y2 t))) 16 16 "blue")
    ((draw-string window) (make-posn (getRPX (+ (moveX x1 x2 t) 5)) (getRPY (+ (moveY x1 y1 x2 y2 t) 10))) (number->string n) "white"))
))
;Draw a ball on screen, the ball data list structure is [X Y Speed directionX directionY timeX timeY counter1 counter2]
(define (drawBall b_data)
   ((draw-solid-ellipse window) (make-posn (getRPX (getXB (get b_data 9) (get b_data 0) (get b_data 2) (get b_data 5))) (getRPY  (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)))) 10 10 "black")
   (cond
      ((and
       (> (+ (getXB (get b_data 9) (get b_data 0) (get b_data 2) (get b_data 5)) 10) 795)
       (> (+ (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)) 10) 100)
       (< (+ (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)) 10) 300))
       (drawCounter (+ (get b_data 7) 1) (get b_data 8))
       (replace (replace (replace b_data (* -1 (get b_data 3)) 3) (+ (get b_data 5) (* -1 (get b_data 3))) 5) (+ (get b_data 7) 1) 7)
     )
     ((and
       (< (getXB (get b_data 9) (get b_data 0) (get b_data 2) (get b_data 5)) 0)
       (> (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)) 100)
       (< (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)) 300))
       (drawCounter  (get b_data 7)(+ (get b_data 8) 1))
       (replace (replace (replace b_data (* -1 (get b_data 3)) 3) (+ (get b_data 5) (* -1 (get b_data 3))) 5) (+ (get b_data 8) 1) 8)
     )
     ((or
       (> (+ (getXB (get b_data 9) (get b_data 0) (get b_data 2) (get b_data 5)) 10) 795)
       (< (getXB (get b_data 9) (get b_data 0) (get b_data 2) (get b_data 5)) 0))
       (replace (replace b_data (* -1 (get b_data 3)) 3) (+ (get b_data 5) (* -1 (get b_data 3))) 5)
     )
     ((or
       (> (+ (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)) 10) 395)
       (< (getFB (get b_data 9) (get b_data 10) (get b_data 0) (get b_data 1) (get b_data 2) (get b_data 6)) 0))
       (replace (replace b_data (* -1 (get b_data 4)) 4) (+ (get b_data 6) (* -1 (get b_data 4))) 6)
     )
     (else (replace (replace b_data (+ (get b_data 5) (get b_data 3)) 5) (+ (get b_data 6) (get b_data 4)) 6))
))
;Shows the time on screen
(define (displayTimeInSeconds x y t)
  (cond
    ((integer? (* t (/ 1 60)))
    ((draw-solid-rectangle window) (make-posn x y) 100 20 "white")
    ((draw-string window) (make-posn (+ x 0) (+ y 15)) "Time:" "black")
    ((draw-string window) (make-posn (+ x 50) (+ y 15))
    (number->string (* t (/ 1 60))) "black"))
))
;--------------------------------------------------------------------------------------------------------------------------------------------
;Mathematical and directional functions
;--------------------------------------------------------------------------------------------------------------------------------------------
;Give a one dimention movement direction
(define (getDirection x1 x2) (cond ((equal? (- x2 x1) 0) 0) (else (/ (- x2 x1) (abs (- x2 x1))))))
;Lineal function slope
(define (getM x1 y1 x2 y2) (/ (- y2 y1) (- x2 x1)))
;Lineal directional function
(define (getF x1 y1 x2 y2 t) (cond ((zero? x1 x2) (getY y1 y2 t)) (else (+ y1 (* (getDirection x1 x2)(* (getM x1 y1 x2 y2) t))))))
(define (getY y1 y2 t) (+ y1 (* (getDirection y1 y2) t)))
(define (getX x1 x2 t) (+ x1 (* (getDirection x1 x2) t)))
(define (zero? x1 x2) (equal? 0.0 (-(abs x2) (abs x1))))
;--------------------------ceeee mamuttt------------------------------------
(define (getFB x1 y1 x2 y2 s t) (cond ((zero? x1 x2) (getYB y1 y2 s t)) (else (+ y1 (* (getDirection x1 x2) s t (getM x1 y1 x2 y2))))))
(define (getYB y1 y2 s t) (+ y1 (* s t (getDirection y1 y2))))
(define (getXB x1 x2 s t) (+ x1 (* s t (getDirection x1 x2))))
;--------------------------ceeee mamuttt------------------------------------
;Dimentions X and Y movement functions. They use the previos methods and update them with a time variable.
(define (moveY x1 y1 x2 y2 t)
  (cond
    ((<= (* (getDirection x1 x2) (- y2 (getF x1 y1 x2 y2 t))) 0) y2)
    (else (getF x1 y1 x2 y2 t)) 
))
;It returns a one dimension movement in a given "t" time.
(define (moveX x1 x2 t)
  (cond
    ((< t (abs (- x2 x1))) (getX x1 x2 t)) 
    (else x2)
))
;--------------------------------------------------------------------------------------------------------------------------------------------
;Generation functions
;--------------------------------------------------------------------------------------------------------------------------------------------
;This function generates a player with ramdon variables (X Y Speed Strength ability)
(define (generatePlayer) (list (+(randInt 400)(randInt 395)) (+(randInt 200)(randInt 195)) (randInt 10) (randInt 10) (randInt 10)))
(define (generatePosition n) (cond ((equal? n 0) null) (else (cons (generatePlayer) (generatePosition (- n 1))))))
(define (generateTeam F) (cond ((null? F) (list (list (generatePlayer))))(else (cons (generatePosition (car F)) (generateTeam (cdr F))))))
(define (gameData F1 F2 B) (list (generateTeam F1) (generateTeam F2) B))
(define (getTeam t gd) (get gd t))
(define (getPosition p t gd) (get (getTeam t gd) p))
(define (getPlayer n p t gd) (get (getPosition p t gd) n))
;--------------------------------------------------------------------------------------------------------------------------------------------
;Recursives loops
;--------------------------------------------------------------------------------------------------------------------------------------------
;Updates players positions, created through a list with them information.
(define (updatePlayers n_player n p t p_old_data p_data abs_time time)
  (cond
    ((equal? t 2) time)
    ((equal? p 4) (updatePlayers n_player 0 0 (+ t 1) p_old_data p_data abs_time time))
    ((equal? n (length (getPosition p t p_data))) (updatePlayers n_player 0 (+ p 1) t p_old_data p_data abs_time time))
    ((collide 20 20
     (moveX (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_old_data) 0) t)
     (moveY (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) t)
     (getXB (get (get p_data 2) 9) (get (get p_data 2) 0) (get (get p_data 2) 2) (get (get p_data 2) 5))
     (getFB (get (get p_data 2) 9) (get (get p_data 2) 10) (get (get p_data 2) 0) (get (get p_data 2) 1) (get (get p_data 2) 2) (get (get p_data 2) 6)))
     (display "si sale y no chocó se mamut") 
     (updatePlayers n_player n p t p_old_data
        (replace (list
        (get (route
         (moveX (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_old_data) 0) t)
         (moveY (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) t)
         (getTeam t p_data)
        )0)
        (get (route
         (moveX (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_old_data) 0) t)
         (moveY (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) t)
         (getTeam t p_data)
        )1)
        (get (get p_data 2) 2)
        (get (get p_data 2) 3)
        (get (get p_data 2) 4)
         0
         0
        (get (get p_data 2) 7)
        (get (get p_data 2) 8)
        (moveX (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_old_data) 0) t)
        (moveY (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) t)
        )2)
     abs_time time))
    ((integer? (/ time 15)) (drawPlayer
     (get (getPlayer n p t p_old_data) 0)
     (get (getPlayer n p t p_old_data) 1)
     (get (getPlayer n p t p_data) 0)
     (get (getPlayer n p t p_data) 1)
     t
     n_player
     time) (updatePlayers (+ n_player 1) (+ n 1) p t p_old_data p_data abs_time time))
    (else time)
))
;Updates every movement. This function returns the time when the movement of every element is finished
(define (update p_old_data p_data t_abs t)
  (sleep (/ 1 60))
  (drawCounter (get (get p_data 2) 7) (get (get p_data 2) 8))
  (displayTimeInSeconds 1000 60 t_abs)
  (cond
    ((stop? 0 0 0 p_old_data p_data t) p_data)
    (else (update p_old_data (replace p_data (drawBall (get p_data 2)) 2) (+ t_abs 1) (+ (updatePlayers 1 0 0 0 p_old_data p_data t_abs t) 1)))
  )
)
;If every movement stops, this function returns #t else #f 
(define (stop? n p t ogd gd time)
  (cond
    ((equal? t 2) #t)
    ((equal? p 4) (stop? 0 0 (+ t 1) ogd gd time))
    ((equal? n (length (getPosition p t gd))) (stop? 0 (+ p 1) t ogd gd time))
    ((not (zero? (moveX (get (getPlayer n p t ogd) 0) (get (getPlayer n p t gd) 0) time) (get (getPlayer n p t gd) 0))) #f)
    (else (stop? (+ n 1) p t ogd gd time))
))
;Main program loop. Here occurs all game updates. Involves a time parameter to update all the methods.
;Replace gameData per Genetic Algorithm. DEPURATION <---------------------------
(define (repaint F1 F2 p_old_data p_data t_abs t gi gf)
  (sleep (/ 1 60))
  (drawCounter (get (get p_data 2) 7) (get (get p_data 2) 8))
  (drawField)
  (displayTimeInSeconds 1000 60 t_abs)
  (cond 
    ((< gi gf) (display gi) (newline) (repaint F1 F2 (gameData F1 F2 '(400 180 1 1 1 0 0 0 0 395 170)) (update p_old_data p_data t_abs t) 0 0 (+ gi 1) gf))
    (else "Game over")
  )
) 
;--------------------------------------------------------------------------------------------------------
;Main function
;--------------------------------------------------------------------------------------------------------
;This is the main function. For running the program, invoke it.
;Replace gameData. Depuracion <---------------------------
(define (WCR2018 F1 F2 g)
  (drawField)
  (repaint F1 F2 (gameData F1 F2 '(400 180 1 1 1 0 0 0 0 395 170)) (gameData F1 F2 '(400 180 1 1 1 0 0 0 0 395 170)) 0 0 0 g)
)
;--------------------------------------------------------------------------------------------------------------------------------------------

(define (route pos_X pos_Y players_List)(
                                         cond(
                                              (null? players_List) 0)
                                             (else (route_aux pos_X pos_Y players_List 0 0 0 0))
                                             )
)
;;Function that looks for the closer player so the player with the ball can throw it to the better way.
;-------------------------------------------------------------------------------------------------------------------------------------------

(define (route_aux x y list final_x final_y type player)(
                                          cond(
                                                  (null? list) (list final_x final_y))
                                                 ((equal? player (length(getPosition type 0 list)))
                                                  (route_aux x y (cdr (car list)) final_x final_y (+ type 1) 0 ))
                                                 ((<= (abs (- x (get (getPlayer player type 0 list) 0))) final_x)
                                                  (route_aux x y (cdr list) (abs(- x (get (getPlayer player type 0 list) 0))) final_y type (+ player 1)))
                                                 ((<= (abs (- y (get (getPlayer player type 0 list) 1))) final_y)
                                                  (route_aux x y (cdr list) final_x (abs(- y (get (getPlayer player type 0 list) 1))) type (+ player 1)))
                                                 (else (route_aux x y (cdr(list)) final_x final_y type (+ player 1)))
                                                 )
)
;;Function that controls if one of the two teams win so the game will end.
;;------------------------------------------------------------------------------------------------------------------------------------------
(define(end_game c1 c2)(
                        cond((or (equal? c1 3) (equal? c2 3))
                             ((clear-viewport window))
                             (drawField))
                            (else "Game is not over"))
)
;--------------------------------------------------------------------------------------------------------------------------------------------
;Function that calculates the distance between two points.
(define (distanceBetweenPoints x1 y1 x2 y2)
   (sqrt (+ (expt (abs (- x2 x1)) 2) (expt (abs (- y2 y1)) 2))))
;--------------------------------------------------------------------------------------------------------
;Function that detects if there was a collision between any player and the ball.
;--------------------------------------------------------------------------------------------------------
; pPr: player radius.
; pBr: ball radius.
; x1: player position in X.
; y1: player position in Y.
; x2: ball position in X.
; y2: ball position in Y.
;--------------------------------------------------------------------------------------------------------
(define (collide pPr pBr x1 y1 x2 y2)
  (> (+ pPr pBr) (distanceBetweenPoints x1 y1 x2 y2)))
;Depuration
(WCR2018 '(1 0 0) '(0 1 0) 100)
