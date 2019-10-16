#lang racket
(require (lib "graphics.ss" "graphics")) (open-graphics)
(define ventana (open-viewport "ventana" 917 471))
(define ventana2 (open-pixmap "lienzo" 917 471))
(define string "3D")

((draw-pixmap ventana) "mesa.png" (make-posn 0 0))
(define (interfaz lista_jugadores click)
  (graficar (car lista_jugadores) 1 0 (len (car lista_jugadores)))
        ;Si hay 2 jugadores
  (cond ((equal? (len lista_jugadores) 2)
        (graficar (cadr lista_jugadores) 2 0 (len (cadr lista_jugadores))))
        ;Si hay 3 jugadores
        
        ((equal? (len lista_jugadores) 3)
        (graficar (cadr lista_jugadores) 2 0 (len (cadr lista_jugadores)))
        (graficar (caddr lista_jugadores) 3 0 (len (caddr lista_jugadores))))
        
        )
  
  
  
  (interfaz lista_jugadores)
  )
;Funcion que dibuja una carta en el tablero usando el codigo el jugador y la posicion en la mano del jugador
(define (graficar cartas jugador posicion cantidad)
  (cond( (null? cartas)
      #t)
       ((equal? jugador 1)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 124) 182))
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad)
        )
       ((equal? jugador 2)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 425) 312))
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad)
        )
       ((equal? jugador 3)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 740) 175))
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad)
        )       
   )
  )

(define (carta posx posy)
  ((draw-pixmap ventana) (string-append string ".png") (make-posn posx posy)))
(define (len lista)
  (cond( (null? lista)
         0)
       (else
        (+ 1 (len (cdr lista))))))

;(define (control click)
;(posn-x (mouse-click-posn click))
;(posn-y (mouse-click-posn click)) 


(interfaz '(("9H" "4S") ("6C" "9D") ("8C" "JS")))
