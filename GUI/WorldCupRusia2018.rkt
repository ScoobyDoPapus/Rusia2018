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
  ;;little rectangleGeneticAlgorithm
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
;Draw a ball on screen, the ball data list structure is [X Y Speed directionX directionY timeX timeY counter1 counter2 collisioned? n_player_collisioned]
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
    ((draw-solid-rectangle window) (make-posn x y) 90 20 "white")
    ((draw-string window) (make-posn (+ x 0) (+ y 15)) "Time:" "black")
    ((draw-string window) (make-posn (+ x 50) (+ y 15))
    (number->string (* t (/ 1 60))) "black"))
))
(define (displayGenerations x y g)
  ((draw-solid-rectangle window) (make-posn x y) 135 20 "white")
  ((draw-string window) (make-posn (+ x 0) (+ y 15)) "Generation:" "black")
  ((draw-string window) (make-posn (+ x 105) (+ y 15))
  (number->string g) "black")
)
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
    ((zero? y1 y2) y1)
    ((and (< x1 x2)(< (- x2 (getX x1 x2 t)) 0)) y2)
    ((and (< x2 x1)(< (- (* (getX x1 x2 t) -1) x2) 0)) y2)
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
    ((and (collide 8 5
     (moveX (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_data) 0) time)
     (moveY (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) time)
     (getXB (get (get p_data 2) 9) (get (get p_data 2) 0) (get (get p_data 2) 2) (get (get p_data 2) 5))
     (getFB (get (get p_data 2) 9) (get (get p_data 2) 10) (get (get p_data 2) 0) (get (get p_data 2) 1) (get (get p_data 2) 2) (get (get p_data 2) 6))) (not (get (get p_data 2) 11)))
        (replace p_data (list
        (get (route
         (moveX (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_data) 0) time)
         (moveY (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) time)
         (getTeam t p_data)
         t
        )0)
        (get (route
         (moveX (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_data) 0) time)
         (moveY (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) time)
         (getTeam t p_data)
         t
        )1)
        (get (get p_data 2) 2)
        (get (get p_data 2) 3)
        (get (get p_data 2) 4)
         0
         0
        (get (get p_data 2) 7)
        (get (get p_data 2) 8)
        (moveX (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_data) 0) time)
        (moveY (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) time)
        #t
        n_player
        )2))
    ((and
     (equal? (get (get p_data 2) 12) n_player)
     (not (collide 20 20
     (moveX (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_data) 0) time)
     (moveY (get (getPlayer n p t p_old_data) 0) (get (getPlayer n p t p_old_data) 1) (get (getPlayer n p t p_data) 0) (get (getPlayer n p t p_data) 1) time)
     (getXB (get (get p_data 2) 9) (get (get p_data 2) 0) (get (get p_data 2) 2) (get (get p_data 2) 5))
     (getFB (get (get p_data 2) 9) (get (get p_data 2) 10) (get (get p_data 2) 0) (get (get p_data 2) 1) (get (get p_data 2) 2) (get (get p_data 2) 6)))))
     (replace p_data (replace (replace (get p_data 2) #f 11) -1 12) 2)
     )
    ((integer? (/ time 1)) (drawPlayer
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
  (displayTimeInSeconds 950 60 t_abs)
  (cond
    ((stop? 0 0 0 p_old_data p_data t) p_data)
    ((list? (updatePlayers 1 0 0 0 p_old_data p_data t_abs t)) (update p_old_data (updatePlayers 1 0 0 0 p_old_data p_data t_abs t) (+ t_abs 1) (+ t 1)))
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
(define (repaint F1 F2 p_old_data p_data t_abs t gi gf)
  (drawField)
  (sleep (/ 1 60))
  (drawCounter (get (get p_data 2) 7) (get (get p_data 2) 8))
  (displayTimeInSeconds 950 60 t_abs)
  (displayGenerations 1040 60 gi)
  (cond
    ((and (end_game (get (get p_data 2) 7) (get (get p_data 2) 8)) (< (get (get p_data 2) 7) (get (get p_data 2) 8)))
     ((draw-string window) (make-posn (getRPX 195) (getRPY 195)) "GAME IS NOT OVER! Visitors wons, for now." "blue"))
    ((and (end_game (get (get p_data 2) 7) (get (get p_data 2) 8)) (> (get (get p_data 2) 7) (get (get p_data 2) 8)))
     ((draw-string window) (make-posn (getRPX 195) (getRPY 195)) "GAME IS NOT OVER! Home wons, for now." "red"))
    ((< gi gf)
     (drawCounter (get (get p_data 2) 7) (get (get p_data 2) 8))
     (repaint F1 F2 p_data (update p_data (EvolucionarEquipos p_data) t_abs t) 0 0 (+ gi 1) gf))
    (else
     (cond
      ((< (get (get p_data 2) 7) (get (get p_data 2) 8))
      ((draw-string window) (make-posn (getRPX 195) (getRPY 195)) "GAME IS NOT OVER! Visitors wons, for now." "blue"))
      ((> (get (get p_data 2) 7) (get (get p_data 2) 8))
      ((draw-string window) (make-posn (getRPX 195) (getRPY 195)) "GAME IS NOT OVER! Home wons, for now." "red"))
      (else ((draw-string window) (make-posn (getRPX 195) (getRPY 195)) "Draw :(. Put a higher number of generations." "white"))
     )
    )
  )
) 
;--------------------------------------------------------------------------------------------------------
;Main function
;--------------------------------------------------------------------------------------------------------
;This is the main function. For running the program, invoke it.
(define (WCR2018 F1 F2 g)
  (drawField)
  (repaint F1 F2 (gameData F1 F2 '(400 180 1 1 1 0 0 0 0 395 170 #f -1)) (gameData F1 F2 '(400 180 1 1 1 0 0 0 0 395 170 #f -1)) 0 0 0 g)
)
;--------------------------------------------------------------------------------------------------------------------------------------------

(define (route pos_X pos_Y players_List t)(
                                         cond(
                                             (null? players_List) 0)
                                             (else (route_aux pos_X pos_Y players_List 2000 2000 0 0 t))
                                             )
)
;;Function that looks for the closer player so the player with the ball can throw it to the better way.
;-------------------------------------------------------------------------------------------------------------------------------------------

(define (route_aux x y l final_x final_y type player t) 
  (cond
                                                 ((null? l) (display final_x) (display " ") (display final_y) (newline) (list final_x final_y))
                                                 
                                                 ((and (equal? x (get (get (car l) player) 0)) (equal? y (get (get (car l) player) 1)))
                                                  
                                                  (route_aux x y l final_x final_y type (+ player 1)t))
                                                 
                                                 ((equal? player (length(car l)))
                                                  
                                                  (route_aux x y (cdr l) final_x final_y (+ type 1) 0 t))
                                                 
                                                 ((and(equal? t 0)(<= 650 (get (get (car l) player) 0)))
                                                  
                                                  (list final_x final_y))

                                                 ((and(equal? t 1)(<= 650 (get (get (car l) player) 0)))
                                                  
                                                  (list final_x final_y))
                                                  
                                                   
                                                 ((<= (abs (- x (get (get (car l) player) 0))) final_x)
                                                  
                                                  (route_aux x y l (get (get (car l) player) 0) (get (get (car l) player) 1) type (+ player 1) t))
                                                 
                                                 (else (route_aux x y l final_x final_y type (+ player 1) t))
                                                 )
)

;;Function that controls if one of the two teams win so the game will end.
;;------------------------------------------------------------------------------------------------------------------------------------------
;;Function that controls if one of the two teams win so the game will end.
;;------------------------------------------------------------------------------------------------------------------------------------------
(define(end_game c1 c2)
  (cond
    ((or (equal? c1 3) (equal? c2 3)) #t)
    (else #f)
))
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
;--------------------------------------------------------------------------------------------------------------------------------------------
;Genetic Algorithm
;--------------------------------------------------------------------------------------------------------------------------------------------
(define largoCromosoma 3)
(define tasaMutacion 30)
(define lista (list 1 2 3 4 5))
(define pobb '((0 1 1 1 1 1 1 0 0) (0 1 1 1 1 0 1 0 0) (0 1 1 1 1 0 1 0 0) (0 0 1 0 0 0 1 1 1) (0 0 0 1 1 0 0 0 0) (0 0 0 1 1 0 0 0 0) (0 0 1 0 1 0 0 0 1)))
(define forma '(
                ((1 1 1 0 1 0 1 1 0) (1 0 1 0 0 1 0 1 0) (1 1 1 0 1 0 1 1 1) (0 0 0 0 1 0 0 0 1))
                ((1 1 0 1 1 0 1 0 0) (0 1 0 1 0 1 0 1 0) (0 1 1 0 1 1 0 1 0) (1 0 0 1 1 0 1 1 0))
                ((1 1 1 1 1 1 1 0 0) (1 0 0 1 1 1 0 1 1) (1 0 0 1 1 1 0 1 0))
                ((1 1 1 1 1 1 1 0 1))
                )
  )
(define poss '((1 2) (3 4) (5 3) (7 5) (7 4) (2 4) (15 4) (13 4) (12 4) (3 4) (9 4) (9 4)))
(define equipos '((
                   ((18.0 75.0 3.0 2.0 6.0) (18.0 75.0 3.0 2.0 7))
                   ((536.0 257.0 1.0 5.0 0.0) (336.0 257.0 4 5.0 0.0) (536.0 257.0 1.0 5.0 0.0))
                   ((392.0 125.0 0.0 9.0 3.0) (536.0 257.0 1.0 5.0 1))
                   ((52.0 108.0 3.0 5.0 4.0))
                                             )
                  (((453.0 196.0 3.0 1.0 2.0)) ((330.0 198.0 3.0 8.0 1.0)) ((526.0 167.0 8.0 5.0 5.0)) ((490.0 114.0 0.0 1.0 5.0)))
  (0 50 0 1 1))
)
(define largoCancha 700)
;795*395
(define (convertirFormacionApoblacion lst)
  (append (car forma) (car (cdr forma)) (car (cdr (cdr forma))))
  )
(define (generarJugador lst largo)
  (cond
    ((equal? (length lst) largo) lst)
    ;(print lst)
    (else (generarJugador (append (list (random 2)) lst) largo))
    )
  )
(define (generarJugadores listaJugadores largoPob largoGenes)
  (cond
    ((equal? (length listaJugadores) (+ largoPob 1)) (cdr listaJugadores))
    
    (else (generarJugadores (append listaJugadores (list (generarJugador '() largoGenes))) largoPob largoGenes))
    )
  )

;Funcion Inicio Poblacion
;Args numeroDePoblacion y Largo de los genes 
(define (poblacionInicial numeroPob largoGenes)
  (generarJugadores '(()) numeroPob largoGenes)
  )
;Funcion cuenta los coindicencias de 1 en los genes
;convierte array de bits en cromosomas
;salida: '(velocidad,fuerza,posicion)
(define (atributosJugador lstJ index vel fuer pos)
  (cond
    ((equal? '() lstJ) (list vel fuer pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 1))) (atributosJugador (cdr lstJ)(+ index 1) (+ vel 1) fuer pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 2))) (atributosJugador (cdr lstJ)(+ index 1) vel (+ fuer 1) pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 3))) (atributosJugador (cdr lstJ)(+ index 1) vel fuer (+ pos 1)))
    
    ((atributosJugador (cdr lstJ)(+ index 1) vel fuer pos))
    )
  )
;Interfaz Pedir Atributos
(define (atributosJugsInter lstJ)
  (atributosJugador lstJ 0 0 0 0)
  )
;Funcion de Fitness a los jugadores
(define (fitnessJugador lst)
  (apply + (atributosJugador lst 0 0 0 0))
  )
;Funcion de Fitnes para poblacion 
(define (fitnesJugadores pob)
  (sorter pob)
  )
;Selecciona los mejores individuos
;Input:lista de individuos ordenada
;Output: lista de 12 mejores individuos 
(define (seleccion pobJugadores)
  (take pobJugadores 99)
  )
;Este metodo de cruce es basado en un punto de cruze dice de manera random que x cromosomas son del papa y z de la mamá
(define (cruzarIndividuos jugA jugB i)
  (append (take jugA i) (drop jugB i))
  )
;Cruza a los mejores jugadores de la poblacion
(define (cruzarPoblacion pob lstP)
  (cond
    ((= (length pob) 1) lstP)
    (else (cruzarPoblacion (cdr pob) (append
                                (list (cruzarIndividuos (car pob) (car(cdr pob)) (random (* largoCromosoma 3)))) lstP) 
                                       ))
          )
    )
;Interfaz para cruzar toda la poblacion tiene que venir seleccionados y ordenados para cruzarlos
(define (cruce pob)
  (cruzarPoblacion pob '())
  )
;Mutacion de un jugador
(define (mutacionJug jug)
  (cond
   ((< (random 100) tasaMutacion) (changeNumber (changeNumber jug (random (length jug))) (random (length jug))))
   (else jug)
   )
 )
;Se seleccionan mimbros random de la poblacion y se les aplica una mutacion
(define (mutacionPob pob)
  (map mutacionJug pob)
    )

(define (GAlgorithm)
  (mutacionPob (cruce (seleccion (fitnesJugadores (poblacionInicial 15 9)))))
  )
;(poblacionInicial 100 9) poner de nuevo
(define (Evolucion jugViejos nGen posBola)
  (mutacionPob (cruce (seleccion (fitnesJugadores (append (convertirFormacionApoblacion jugViejos) (poblacionInicial 100 9))))))
  ;(clasificadorInter (Evolucion pobb 30 poss) poss (list-ref (darFormacion jugViejos) 0) (list-ref (darFormacion jugViejos) 1) (list-ref (darFormacion jugViejos) 2))
  )
;Interfaz de Evolucion con formacion me lo devuelve con formacion 
(define (Evolucionar jugV nGen posBola)
  (clasificadorInter (Evolucion jugV nGen posBola) (list-ref (convertirNaBPob jugV '() '()) 1) (list-ref (darFormacion jugV) 0) (list-ref (darFormacion jugV) 1) (list-ref (darFormacion jugV) 2))
)
(define (EvolucionarEquipos equipos)
  (append (list (Evolucionar (getTeam 0 equipos) 30 (getTeam 2 equipos))) (list (Evolucionar (getTeam 1 equipos) 30 (getTeam 2 equipos))) (list (getTeam 2 equipos)))
)
(define (encontrarJug lst pos)
  (cond
    ((equal? (car (cdr (atributosPos lst 0 0 0 0))) pos) #t)
    (else #f)
  )
)
;;;;

;;

;;;;
(define (atributosPos lstJ index vel fuer pos)
  (cond
    ((equal? '() lstJ) (list vel fuer pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 1))) (atributosPos (cdr lstJ)(+ index 1) (+ vel 1) fuer pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 2))) (atributosPos (cdr lstJ)(+ index 1) vel (+ fuer 1) pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 3))) (atributosPos (cdr lstJ)(+ index 1) vel fuer (+ pos 1)))
    ((atributosPos (cdr lstJ)(+ index 1) vel fuer pos))
    )
  )

(define (devolverJugadores lst defensas medios delanteros lstD lstM lstA listP)
  (cond
  ((equal? lst '()) (list lstD lstM lstA listP))
  
  ((and (equal? (length lstD) defensas) (equal? (length lstM) medios) (equal? (length listP) 1) (equal? (length lstA) delanteros)) (list lstD lstM lstA listP))
  ((and (< (length lstD) defensas) (encontrarJug (car lst) 1))
   (display (length lstD))(newline) (devolverJugadores (cdr lst) defensas medios delanteros (append (list (car lst)) lstD) lstM lstA listP))

  ((and (< (length lstM) medios) (encontrarJug (car lst) 2))
   (display (length lstD))(newline) (devolverJugadores (cdr lst) defensas medios delanteros lstD (append (list (car lst)) lstM) lstA listP))

  ((and (< (length lstA) delanteros) (encontrarJug (car lst) 3))
   (display (length lstD))(newline) (devolverJugadores (cdr lst) defensas medios delanteros lstD lstM (append (list (car lst)) lstA) listP))

  ((and (< (length listP) 1) (encontrarJug (car lst) 0))
   (display (length lstD))(newline) (devolverJugadores (cdr lst) defensas medios delanteros lstD lstM lstA (append (list (car lst)) listP)))
  
  ;(display (length lstD))
  (else (devolverJugadores (cdr lst) defensas medios delanteros lstD lstM lstA listP))
  )
 )
;Interfaz para pedir la formacion 
(define (devolverFormacion lst defensas medio delanteros)
  (devolverJugadores lst defensas medio delanteros '() '() '() '())
  )


;(< (random 100) tasaMutacion)

;Sirve para ver si es 1 o 0 en el array de bits
(define (esUno numero)
  (cond
  ((= numero 1) #t)
  (else #f)
  )
)

(define (changeNumber lst i)
  (append (take lst i) (list (random 2)) (drop lst (+ i 1)))
  )

(define (part lst i)
  (list (take lst i)
        (drop lst i)))

;Codigo Modificado de Rnso oct 16 StackOverflow https://stackoverflow.com/questions/40050677/is-there-a-way-to-sort-a-list-of-lists-using-an-index
(define (sorter ll)
  (sort ll
        (lambda(a b)
          (if (>  (fitnessJugador a) (fitnessJugador b))
              #t #f))))

;;;;Funciones de Posicion
(define (actualizarPos lst i x)
  (append (take lst i) (list x) (drop lst (+ i 1)))
  )
(define (InterfazPos lstPos posBola index)
  1
  )
(define (elMasCerca lst posBola menor)
  (cond
    ((equal? lst '()) menor)
    ((< (restaEntrepares (car lst) posBola) (restaEntrepares menor posBola)) (display menor)(elMasCerca (cdr lst) posBola (car lst)))
    (else (elMasCerca (cdr lst) posBola menor))
    )
  )
;Funcion que devuelve pos Actualizada
(define (devolverPos lst posb)
  (actualizarPos lst (finLi poss (elMasCerca lst posb '(1000000 100000)) 0) (sumarPar (elMasCerca lst posb '(1000000 100000)) 20))
  )
; (2,3,7) and (300,200)
(define (combinarConPosEquip lst lstPos)
  (append (list (car lstPos) (car (cdr lstPos)) (car lst)) (cdr (cdr lst)) '(3))
  )
;;;;;
;KADJLFIHOÑODSIF

;;;;;
(define (combinarJugs jugs pos)
  (map combinarConPosEquip jugs pos)
  )
;(atributosJugsInter lstJ)
;Input:Equipo
;Output :Lista ya armada con la formacion
(define (clasificador lst pos defensas medios delanteros lstD lstM lstA listP)
(cond
  ((equal? lst '()) (list lstD lstM lstM listP))
  
  ((and (equal? (length lstD) defensas) (equal? (length lstM) medios) (equal? (length listP) 1) (equal? (length lstA) delanteros))
   (list lstD lstM lstA listP))
  ((and (< (length lstD) defensas) (encontrarJug (car lst) 1))
   (display (length lstD))(newline) (clasificador (cdr lst) (cdr pos) defensas medios delanteros (append (list (combinarConPosEquip (atributosJugsInter (car lst)) (car pos))) lstD) lstM lstA listP))

  ((and (< (length lstM) medios) (encontrarJug (car lst) 2))
   (clasificador (cdr lst) (cdr pos) defensas medios delanteros lstD (append (list (combinarConPosEquip (atributosJugsInter (car lst)) (car pos))) lstM) lstA listP))

  ((and (< (length lstA) delanteros) (encontrarJug (car lst) 3))
   (clasificador (cdr lst) (cdr pos) defensas medios delanteros lstD lstM (append (list (combinarConPosEquip (atributosJugsInter (car lst)) (car pos))) lstA) listP))

  ((and (< (length listP) 1) (encontrarJug (car lst) 0))
   (clasificador (cdr lst) (cdr pos) defensas medios delanteros lstD lstM lstA (append (list (combinarConPosEquip (atributosJugsInter (car lst)) (car pos))) listP)))
  
  ;(display (length lstD))
  (else (clasificador (cdr lst) pos defensas medios delanteros lstD lstM lstA listP))
  )
;3400
  )
;Interfaz para pasar de array de bits sin formacion a numeron con formacion 
(define (clasificadorInter lst pos def med del)
  (clasificador lst pos def med del '() '() '() '())
  )
;;;

;Input :Un jugador
;;;;;Output:Array de bits
(define (cAbts lst)
  (append (hacerunUno (list-ref lst 2) largoCromosoma '()) (hacerunUno (clasificadorPosXY (take lst 2)) largoCromosoma '()) (hacerunUno (list-ref lst 2) largoCromosoma '()))
  )
;Input una poblacion en numeros
;Output una poblacion en bits y lst pos en XY
(define (convertirNaBPob lst lstJugs lstPos)
  (cond
    ((equal? lst '(() () () ())) (list lstJugs lstPos))
    ((> (length (car lst)) 0)
     (convertirNaBPob (cons (remove (car (car lst)) (car lst)) (cdr lst)) (append (list (cAbts (car (car lst)))) lstJugs) (cPosaXY (car (car lst)) lstPos))
     )
    ((> (length (car (cdr lst))) 0)
     (convertirNaBPob (append (cons '() (list (remove (car (car (cdr lst))) (car (cdr lst))))) (cdr (cdr lst))) (append (list (cAbts (car (car (cdr lst))))) lstJugs) (cPosaXY (car (car (cdr lst))) lstPos))
     )
    ((> (length (list-ref lst 2)) 0)
     (convertirNaBPob (append (cons '() (cons '() (list (remove (car (car (cdr (cdr lst)))) (car (cdr (cdr lst))))))) (cdr (cdr (cdr lst)))) (append (list (cAbts (car (car (cdr (cdr lst)))))) lstJugs) (cPosaXY (car (car (cdr (cdr lst)))) lstPos))
     )
    (
     (convertirNaBPob '(() () () ()) (append (list (cAbts (car (list-ref lst 3)))) lstJugs) (cPosaXY (car (list-ref lst 3)) lstPos))
     )    
    )
  )

(define (cPosaXY pos lstP)
  (cons (take pos 2) lstP)
  )
;
;Aca quede
; 
;Interfaz para convertir Numeros pob a Bits pob
(define (convNaB lst)
  1
  )
(define (clasificadorPosXY pos)
  (cond
    ((< (car pos) (/ largoCancha 7)) 0)
    ((< (car pos) (/ largoCancha 3)) 1)
    ((< (car pos) (/ largoCancha 2)) 2)

    ((< (car pos) (/ largoCancha 1)) 3)
    
    )
  )
(define (hacerunUno cont largoCrom lst)
  (cond
    ;crom-cont= 
    ((equal? (+ (- largoCrom (exact-round cont)) (exact-round cont)) (length lst)) lst)
    ((< (length lst) (exact-round cont))
     (hacerunUno (exact-round cont) largoCrom (cons 1 lst))
     )
    (else
     (hacerunUno (exact-round cont) largoCrom (cons 0 lst))
     )
    )
  )
;Output me devuelve si es 4 4 2
(define (darFormacion lst)
  (list (length (car lst)) (length (car (cdr lst))) (length (car (cdr (cdr lst)))))
  
  )
(define (combinarJugsPosAtri jugs pos)
  (combinarJugs (map atributosJugsInter jugs) pos)
  )

(define (combJugsFormacion jugs pos)
  1
  )
;;;
(define (restaEntrepares pos1 pos2)
  (abs (- (apply + pos1) (apply + pos2)))
  )
(define (finLi lst busq index)
  (cond
    ((equal? lst '()) '())
    ((equal? (car lst) busq) index)
    (else (finLi (cdr lst) busq (+ index 1)))
    )
  )
(define (sumarPar lst suma)
  (list (+ (car lst) suma) (+ (car (cdr lst)) suma))
  )

(define prueba '((0 1 1 0 1 1 0 1 0)
  (1 0 1 0 1 1 1 0 0)
  (1 1 0 1 0 1 0 1 0)
  (1 0 1 1 0 0 1 1 1)
  (1 1 1 0 0 0 0 1 0)
  (0 1 1 0 0 1 1 0 1)
  (1 1 1 0 0 0 1 0 1)
  (0 0 1 0 1 0 0 1 0)
  (1 0 0 1 1 1 1 0 1)
  (1 0 1 0 0 0 0 1 0)
  (1 0 0 0 0 1 1 1 1)
  (0 1 0 0 0 0 0 1 1)
  (0 1 0 1 1 0 0 0 0)
  (0 1 1 1 1 1 0 1 1)
  (1 0 0 0 0 0 0 0 1)
  (1 0 1 0 1 1 0 0 0)
  (1 1 1 0 1 0 1 0 0)
  (0 0 1 0 1 0 0 0 0)
  (0 1 0 1 0 0 0 1 1)
  (1 0 1 1 0 1 1 0 1)
  (0 1 1 1 1 0 1 1 0)
  (1 1 0 0 1 0 0 1 1)
  (0 1 0 0 0 1 0 0 0)
  (1 0 0 1 1 0 1 1 1)
  (1 0 0 0 0 1 1 1 0)
  (0 0 0 1 0 0 1 1 1)
  (1 1 1 0 1 1 0 1 0)
  (1 0 1 1 0 0 1 1 1)
  (1 1 0 1 1 1 1 0 0)
  (0 0 0 1 0 0 0 1 1)
  (0 0 1 1 0 1 0 0 1)
  (1 1 0 0 1 0 0 1 1)
  (1 0 0 1 0 1 0 0 1)
  (0 1 1 1 0 0 0 1 1)
  (0 1 0 1 1 1 0 0 1)
  (1 0 0 0 0 0 0 0 0)
  (1 0 1 0 1 0 1 1 0)
  (0 1 0 1 1 1 1 0 0)
  (1 1 1 1 0 1 1 1 0)
  (1 0 0 0 1 1 1 1 1)
  (0 1 1 1 0 0 1 0 0)
  (1 0 1 0 1 1 1 1 0)
  (1 0 0 1 0 0 0 1 0)
  (0 0 1 1 1 0 1 0 1)
  (1 0 1 1 0 1 1 0 1)
  (0 0 1 1 1 0 0 1 0)
  (1 1 1 0 1 0 0 0 0)
  (1 0 0 1 0 1 0 0 1)
  (0 0 0 1 0 0 1 1 1)
  (1 0 0 0 1 1 0 0 1)
  (1 0 1 1 1 0 1 1 1)
  (1 1 0 0 1 0 0 1 1)
  (0 0 1 1 0 0 0 0 0)
  (1 0 0 0 1 0 0 0 1)
  (0 1 0 1 0 0 0 0 0)
  (0 0 0 1 1 0 1 1 1)
  (0 0 1 1 0 1 1 0 0)
  (1 1 1 0 1 0 1 0 1)
  (1 1 1 0 1 0 0 0 0)
  (0 0 0 0 1 1 1 1 0)
  (0 1 1 1 0 0 0 0 1)
  (0 0 1 1 0 0 0 1 1)
  (1 1 0 0 1 0 1 1 1)
  (0 0 1 1 0 0 0 0 0)
  (0 1 1 1 1 0 0 0 1)
  (0 1 1 1 1 1 0 1 1)
  (0 1 1 1 1 1 1 1 0)
  (1 1 0 0 0 0 0 0 0)
  (0 1 0 0 1 1 0 0 1)
  (0 0 1 1 0 1 0 0 1)
  (0 1 0 0 1 0 0 1 1)
  (1 0 0 1 0 0 0 1 0)
  (0 0 1 0 1 0 0 0 0)
  (0 1 1 0 0 0 1 1 1)
  (0 0 1 1 1 1 0 1 0)
  (1 1 0 1 1 0 0 1 1)
  (0 0 0 1 0 0 1 0 1)
  (1 0 0 1 0 0 0 1 0)
  (0 0 1 0 1 1 0 1 0)
  (0 0 1 0 1 1 0 0 0)
  (1 0 1 0 0 1 1 1 0)
  (1 1 1 0 1 1 0 1 0)
  (1 1 0 1 0 0 1 0 1)
  (1 1 0 1 0 1 1 0 1)
  (1 0 0 0 1 1 1 0 1)
  (0 0 1 1 0 1 1 1 1)
  (0 0 0 0 1 0 1 1 1)
  (1 1 0 1 1 1 0 1 0)
  (0 0 0 1 0 1 1 1 1)
  (0 0 0 0 0 1 1 1 1)
  (0 1 0 0 1 1 0 0 0)
  (0 0 0 0 1 0 0 0 1)
  (1 0 1 1 0 1 0 0 0)
  (0 1 0 1 0 0 1 0 1)
  (1 1 1 0 0 0 1 0 1)
  (1 0 1 0 0 1 0 0 1)
  (0 1 1 0 0 1 0 1 1)
  (1 0 0 1 0 0 1 1 1)
  (0 0 0 1 0 1 1 0 0)
  (1 1 0 0 0 1 0 0 1)))


;Depuration
(WCR2018 '(1 1 1) '(1 1 1) 100)
