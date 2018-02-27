;RPX=Relative position X
#lang racket
(require (lib "graphics.ss" "graphics"))(open-graphics)
(define window (open-viewport "WCR2018" 1200 512))

;--------------------------------------------------------------------------------------------------------
;Draw the soccer field.
;--------------------------------------------------------------------------------------------------------
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

;; Fuction that draws the score from the two teams
(define (drawCounter home visitors)

((draw-string window) (make-posn 1135 150) (~a visitors) "blue")
  
((draw-string window) (make-posn 970 150) (~a home) "red")
  )
;--------------------------------------------------------------------------------------------------------
; Funcion que mueve la bola por el terreno de juego.
;--------------------------------------------------------------------------------------------------------
; pX: posicion en X
; pY: posicion en Y
; pR: radio de la bola
; dx: movimiento en x
; dy: movimiento en y
;--------------------------------------------------------------------------------------------------------
(define (moveBall pX pY pR dx dy c1 c2)
         ; If the ball's position > to the right limit/ If the ball's position > to the left limit
         ; We multiply the direction -1 
  (cond (
         (and (< (+ pX dx) (+ pR 90)) (> (+ pY dy) (+ pR 100)) (< (+ pY dy) (+ pR 150)))          
          (moveBall 500 100 10 2 2 (+ c1 1) c2)
          )

        ((and (> (+ pX dx) 870) (> (+ pY dy) (+ pR 100)) )
         (moveBall 500 100 10 2 2 c1 (+ c2 1)))
         
         ((or (> (+ pX dx) (- 900 pR)) (< (+ pX dx) (+ pR 85)))
          (moveBall pX pY pR (* dx -1) dy c1 c2))
        
         ; If the ball's position = down limit / If the ball's position = up limit
         ; We multiply the direction -1 
        ((or (> (+ pY dy) (- 450 pR)) (< (+ pY dy) (+ pR 24)))
          (moveBall pX pY pR dx (* dy -1) c1 c2))
                       
        (else (begin
                (((draw-solid-ellipse window) (make-posn pX pY) pR pR "white")
                 ((clear-viewport window))
                 (drawField)
                 ((draw-solid-ellipse window) (make-posn (+ pX dx) (+ pY dy)) pR pR "white")
                 (sleep 0.0001)
                 (moveBall (+ pX dx) (+ pY dy) pR dx dy c1 c2))))))
;--------------------------------------------------------------------------------------------------------
;Player methods
(define (getRPX x) (+ 102 x))
(define (getRPY y) (+ 52  y))
(define (get l i) (cond ((null? l) null)((equal? i 0) (car l))(else (get (cdr l) (- i 1)) )))
;Draw a player on screen
(define (drawPlayer x1 y1 x2 y2 team number t)
  (cond
    ((equal? 1 team) ((draw-solid-ellipse window)(make-posn (getRPX (moveX x1 x2 t)) (getRPY (moveY x1 y1 x2 y2 t))) 16 16 "red"))
    (else ((draw-solid-ellipse window)(make-posn (getRPX (moveX x1 x2 t)) (getRPY (moveY x1 y1 x2 y2 t))) 16 16 "blue"))
    ))
;Updates players positions, created through a list with them information.
(define (updatePlayers p_old_data p_data t)
  (cond
    ((null? p_data) null)
    (else
    (drawPlayer (get (car p_old_data) 0) (get (car p_old_data) 1) (get (car p_data) 0) (get (car p_data) 1) (get (car p_data) 2) (get (car p_data) 3) t)
    (updatePlayers (cdr p_old_data) (cdr p_data) t))
))
;Give a one dimention movement direction
(define (getDirection x1 x2) (cond ((equal? (- x2 x1) 0) 0) (else (/ (- x2 x1) (abs (- x2 x1))))))
;Parts of a lineal function
(define (getM x1 y1 x2 y2) (/ (- y2 y1) (- x2 x1)))
(define (getB x1 y1 x2 y2) (- y1 (* (getM x1 y1 x2 y2) x1)))
;Lineal directional function
(define (getF x1 y1 x2 y2 t) (+ y1 (* (getDirection x1 x2)(* (getM x1 y1 x2 y2) t))))
(define (getY y1 y2 t) (+ y1 (* (getDirection y1 y2) t)))
(define (getX x1 x2 t) (+ x1 (* (getDirection x1 x2) t)))
(define (zero? x1 x2)(equal? 0 (- x2 x1)))
;Dimentions X and Y movement functions. They use the previos methods and update them with a time variable.
(define (moveY x1 y1 x2 y2 t)
  (cond
    ((and (zero? x1 x2) (< (abs (- y2 y1)) t)) y2)
    ((zero? x1 x2) (getY y1 y2 t))
    ((< (* (getDirection x1 x2) (- y2 (getF x1 y1 x2 y2 t))) 0) y2)
    (else (getF x1 y1 x2 y2 t))
    ))
(define (moveX x1 x2 t) (cond ((< t (abs (- x2 x1))) (getX x1 x2 t)) (else x2)))
;Main program loop. Here occurs all game updates. Involves a time parameter to update all the methods.
(define (repaint ti tf)
  (updatePlayers '((0 0 1 2) (0 100 1 3) (0 200 2 4)) '((0 100 1 2) (0 200 1 3) (200 300 2 4)) ti)
  (sleep (/ 1 60))
  (display_time_in_seconds 1000 60 ti)
  (drawCounter 0 0)
  (cond
    ((< (* ti (/ 1 60)) tf) (repaint (+ ti 1) tf))
    (else "Game over")
  )
)
;(make-posn 100 100) (number->string (* t (/ 1 60))) 
(define (display_time_in_seconds x y t)
  (cond
    ((integer? (* t (/ 1 60)))
    ((draw-solid-rectangle window) (make-posn x y) 100 20 "white")
    ((draw-string window) (make-posn (+ x 0) (+ y 15)) "Time:" "black")
    ((draw-string window) (make-posn (+ x 50) (+ y 15))
    (number->string (* t (/ 1 60))) "black"))
  ))
;This is the main function. For running the program, invoke it.
(define (WCR2018 F1 F2 t)
  (drawField)
  (repaint 0 t)
)
;Depuration
(WCR2018 '() '() 100)