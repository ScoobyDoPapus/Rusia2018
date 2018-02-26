#lang racket/gui
;Load the "Graphics" library
(require (lib "graphics.ss" "graphics"))

;Initialize the "Graphics" library
(open-graphics)

;Create the main window of the game
(define window (open-viewport "World Cup Prusia 2018" 1200 512))

;--------------------------------------------------------------------------------------------------------
;Draw the soccer field.
;--------------------------------------------------------------------------------------------------------
(define (drawField)

;;Main Window
((draw-solid-rectangle window) (make-posn 0 0) 1200 512 "black")

;;((draw-pixmap window) "C:/Users/REIRA/Downloads/bloggif_5a934f9c898ec.jpeg"(make-posn 100 50))
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
((draw-string window)(make-posn 950 60) "VISITORS" "white")
  
((draw-string window)(make-posn 1100 60) "HOME" "white")  
)

;; Fuction that draws the score from the two teams
(define (drawCounter visitors home)

((draw-string window)(make-posn 980 80) (~a visitors) "white")
  
((draw-string window)(make-posn 1120 80) (~a home) "white")
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
                 (drawCounter c1 c2)
                 ((draw-solid-ellipse window) (make-posn (+ pX dx) (+ pY dy)) pR pR "white")
                 (sleep 0.0001)
                 (moveBall (+ pX dx) (+ pY dy) pR dx dy c1 c2))))))
;--------------------------------------------------------------------------------------------------------

;This is the main function. For running the program, invoke it.
(define (WCR2018 F1 F2 t)
  ((drawField)
  (moveBall 500 300 10 2 2 0 0)
  )
)

;Depuration
(WCR2018 '() '() 10)

 