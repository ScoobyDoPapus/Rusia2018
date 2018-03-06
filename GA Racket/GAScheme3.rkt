#lang racket

(define largoCromosoma 3)
(define tasaMutacion 30)
(define lista (list 1 2 3 4 5))
(define pobb '((0 1 1 1 1 1 1 0 0) (0 0 1 0 0 0 1 1 1) (0 0 0 1 1 0 0 0 0) (0 0 0 1 1 0 0 0 0) (0 0 1 0 1 0 0 0 1)))
(define forma '(
                ((1 1 1 0 1 0 1 1 0) (1 0 1 0 0 1 0 1 0) (1 1 1 0 1 0 1 1 1) (0 0 0 0 1 0 0 0 1))
                ((1 1 0 1 1 0 1 0 0) (0 1 0 1 0 1 0 1 0) (0 1 1 0 1 1 0 1 0) (1 0 0 1 1 0 1 1 0))
                ((1 1 1 1 1 1 1 0 0) (1 0 0 1 1 1 0 1 1))
                ))
(define poss '((1 2) (3 4) (5 3) (7 5)))
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
  )
;Interfaz de Evolucion con formacion
(define (Evolucionar jugV nGen posBola)
  (devolverFormacion (Evolucion jugV nGen posBola) 3 3 3)
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
'((((18.0 75.0 3.0 2.0 6.0)) ((536.0 257.0 1.0 5.0 0.0)) ((392.0 125.0 0.0 9.0 3.0)) ((52.0 108.0 3.0 5.0 4.0)))
  (((453.0 196.0 3.0 1.0 2.0)) ((330.0 198.0 3.0 8.0 1.0)) ((526.0 167.0 8.0 5.0 5.0)) ((490.0 114.0 0.0 1.0 5.0)))
  (0 50 0 1 1))

(define (get l i) (cond ((null? l) null)((equal? i 0) (car l))(else (get (cdr l) (- i 1)) )))
(define (getTeam t gd) (get gd t))
(define (getPosition p t gd) (get (getTeam t gd) p))
(define (getPlayer n p t gd) (get (getPosition p t gd) n))

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