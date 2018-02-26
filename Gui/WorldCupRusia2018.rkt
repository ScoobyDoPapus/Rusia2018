;RPX=Relative position X
#lang racket
(require (lib "graphics.ss" "graphics"))(open-graphics)
(define window (open-viewport "WCR2018" 1024 512))

;Draw the soccer field.
(define (drawField)
((draw-solid-rectangle window) (make-posn 0 0) 1024 512 "gray")
((draw-solid-rectangle window) (make-posn 155 27) 714 362 "white")
((draw-solid-rectangle window) (make-posn 160 32) 704 352 "darkgreen")
((draw-rectangle window) (make-posn (- 160 32) (+ 132 27)) 32 88 "white")
((draw-rectangle window) (make-posn (+ 160 704) (+ 132 27)) 32 88 "white")
((draw-line window) (make-posn (+ 160 352) 32) (make-posn (+ 160 352) (+ 32 352)) "white")
((draw-ellipse window)(make-posn (+ 160 (- 352 66)) (+ 32 110)) 132 132 "white"))
;Player methods
(define (getRPX x) (+ 160 x))
(define (getRPY y) (+ 32  y))
(define (get l i) (cond ((null? l) null)((equal? i 0) (car l))(else (get (cdr l) (- i 1)) )))
;Draw a player on screen
(define (drawPlayer x1 y1 x2 y2 type number t) ((draw-solid-ellipse window)(make-posn (getRPX (moveX x1 x2 t)) (getRPY (moveY x1 y1 x2 y2 t))) 16 16 "red"))
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
;Arreglar moveY
(define (moveY x1 y1 x2 y2 t) (cond ((zero? x1 x2) (getY y1 y2 t)) (else (getF x1 y1 x2 y2 t))))
(define (moveX x1 x2 t) (getX x1 x2 t))
;Main program loop. Here occurs all game updates. Involves a time parameter to update all the methods.
(define (repaint ti tf)
  (updatePlayers '((704 0 1 1) (0 0)) '((0 352) (704 352)) (/ (* ti (/ 1 0.00017)) 60))
  (cond
    ((< ti tf) (repaint (+ ti 0.00017) tf))
    (else "Game over")
  )
)
;This is the main function. For running the program, invoke it.
(define (WCR2018 F1 F2 t)
  (drawField)
  (repaint 0 t)
)
;Depuration
(WCR2018 '() '() 10)