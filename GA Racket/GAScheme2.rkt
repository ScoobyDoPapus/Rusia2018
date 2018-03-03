#lang racket

(define largoCromosoma 3)
(define tasaMutacion 30)
(define lista (list 1 2 3 4 5))
(define pobb '((0 1 1 1 1 1 1 0 0) (0 0 1 0 0 0 1 1 1) (0 0 0 1 1 0 0 0 0) (0 0 0 1 1 0 0 0 0) (0 0 1 0 1 0 0 0 1)))
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
  (take pobJugadores 10)
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
  (mutacionJug (cruce (seleccion (fitnesJugadores (poblacionInicial 15 9)))))
  )

(define (encontrarJug lst pos)
  (cond
    ((equal? (car lst) pos) #t)
    (else #f)
    )
  )

(define (atributosPos lstJ index vel fuer pos)
  (cond
    ((equal? '() lstJ) (list vel fuer pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 1))) (atributosPos (cdr lstJ)(+ index 1) (+ vel 1) fuer pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 2))) (atributosPos (cdr lstJ)(+ index 1) vel (+ fuer 1) pos))
    ((and (esUno (car lstJ)) (< index (* largoCromosoma 3))) (atributosPos (cdr lstJ)(+ index 1) vel fuer (+ pos 1)))
    ((atributosPos (cdr lstJ)(+ index 1) vel fuer pos))
    )
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

