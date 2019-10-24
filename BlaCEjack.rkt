#lang racket
(require (lib "graphics.ss" "graphics")) (open-graphics)
(define ventana (open-viewport "ventana" 917 471)) 
(require "Logica.rkt")
((draw-pixmap ventana) "mesa.png" (make-posn 0 0))
(define (interfaz lista_jugadores turno deck)
  ;(write lista_jugadores)
  ;(write "/n")
  ;(write deck)
  ((draw-pixmap ventana) "mesa.png" (make-posn 0 0))
  ;si hay 1 jugador
  (cond
    ((and (equal? turno 10) (equal? (len lista_jugadores) 2)) ; Si solo hay un jugador y termino el juego
          (define ventana2 (open-viewport "ventana2" 398 265)) ;se abre la segunda ventana
          ((draw-pixmap ventana2) "fondov2.jpg" (make-posn 0 0))  ;se coloca la imagen de fondo
          ((draw-string ventana2) (make-posn 180 80) (caar lista_jugadores)) ;se grafica el puntaje del primer lugar
          ((draw-string ventana2) (make-posn 325 80) (cadar lista_jugadores)) ;se grafica el puntaje del segundo lugar

          ((draw-string ventana2) (make-posn 180 120) (caadr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 120) (cadadr lista_jugadores))
          (boton2 (mouse-click-posn (get-mouse-click ventana2)) ventana2) )

        
    ((and (equal? turno 10) (equal? (len lista_jugadores) 3))
          (define ventana2 (open-viewport "ventana2" 398 265))  ;se abre la segunda ventana
          ((draw-pixmap ventana2) "fondov2.jpg" (make-posn 0 0))  ;se coloca la imagen de fondo
          ((draw-string ventana2) (make-posn 180 80) (caar lista_jugadores))
          ((draw-string ventana2) (make-posn 325 80) (cadar lista_jugadores)) ;puntaje del primer lugar

          ((draw-string ventana2) (make-posn 180 120) (caadr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 120) (cadadr lista_jugadores)) ;Puntaje del segundo 

          ((draw-string ventana2) (make-posn 180 160) (caaddr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 160) (car (cdaddr lista_jugadores))) ;puntaje del tercero
          (boton2 (mouse-click-posn (get-mouse-click ventana2)) ventana2)
          )
    ((and (equal? turno 10) (equal? (len lista_jugadores) 4))
          (define ventana2 (open-viewport "ventana2" 398 265))  ;se abre la segunda ventana
          ((draw-pixmap ventana2) "fondov2.jpg" (make-posn 0 0))  ;se coloca la imagen de fondo
          ((draw-string ventana2) (make-posn 180 80) (caar lista_jugadores))
          ((draw-string ventana2) (make-posn 325 80) (cadar lista_jugadores))

          ((draw-string ventana2) (make-posn 180 120) (caadr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 120) (cadadr lista_jugadores))

          ((draw-string ventana2) (make-posn 180 160) (caaddr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 160) (car (cdaddr lista_jugadores)))

          ((draw-string ventana2) (make-posn 180 200) (car (cadddr lista_jugadores)))
          ((draw-string ventana2) (make-posn 325 200) (cadr (cadddr lista_jugadores)))
          (boton2 (mouse-click-posn (get-mouse-click ventana2)))
          )
          
    ((equal? (len lista_jugadores) 2)   ;Condiciones que grafican las cartas segun cuantos jugadores esten
         (graficar (car lista_jugadores) 0 0 (len (car lista_jugadores)) turno 2 deck)   ;envia a graficar al crupier
         (graficar (cadr lista_jugadores) 1 0 (len (cadr lista_jugadores)) turno 2 deck) ;envia a graficar al primer jugador
         (botones (mouse-click-posn (get-mouse-click ventana)) turno (len lista_jugadores)))
        ;Si hay 2 jugadores
         ((equal? (len lista_jugadores) 3)
         (graficar (car lista_jugadores) 0 0 (len (car lista_jugadores)) turno 3 deck)     ;envia a grafica al crupier
         (graficar (cadr lista_jugadores) 1 0 (len (cadr lista_jugadores)) turno 3 deck)   ;envia a graficar al primer jugador
         (graficar (caddr lista_jugadores) 2 0 (len (caddr lista_jugadores)) turno 3 deck) ;envia a graficar al segundo jugador
         (botones (mouse-click-posn (get-mouse-click ventana)) turno  (len lista_jugadores) deck))
        ;Si hay 3 jugadores  
         ((equal? (len lista_jugadores) 4)
         (graficar (car lista_jugadores) 0 0 (len (car lista_jugadores)) turno 4 deck)      ;envia a graficar al crupier
         (graficar (cadr lista_jugadores) 1 0 (len (cadr lista_jugadores)) turno 4 deck)    ;envia a graficar al primer jugador
         (graficar (caddr lista_jugadores) 2 0 (len (caddr lista_jugadores)) turno 4 deck)  ;envia a graficar al segundo jugador
         (graficar (cadddr lista_jugadores) 3 0 (len (cadddr lista_jugadores)) turno 4 deck)  ;envia a graficar al tercer jugador
         (botones (mouse-click-posn (get-mouse-click ventana)) turno (len lista_jugadores) lista_jugadores deck))

        )
  )
;Funcion que dibuja una carta en el tablero usando el codigo el jugador y la posicion en la mano del jugador
(define (graficar cartas jugador posicion cantidad turno cant_jugadores deck)
  
  (cond( (null? cartas)
      #t)
       ((and (not (equal? turno 0)) (and (equal? posicion 0) (equal? jugador 0))) ;si es la mano del crupier y no es su turno
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 439) 33)) ;grafica la carta oculta 
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck) ;llama recursivamente a para que se grafique la segunda carta del crupier
        )       
       ((equal? jugador 0) ;si es la mano del crupier
        (write cartas)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 439) 33)) ;grafica la cartas del crupier
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck) ;llama recursivamente a para que se grafique la siguiente carta del crupier
        )
       ((and (not (equal? turno 1)) (and (equal? posicion 0) (equal? jugador 1))) ;si es la mano del primer jugador y no es su turno
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 124) 182)) ;grafica la carta oculta del primer jugador
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck) ;llama recursivamente a para que se grafique la segunda carta del jugador
        )        
       ((equal? jugador 1);si es la mano del primer jugador
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 124) 182)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck)  ;llama recursivamente a para que se grafique la segunda carta del J1
        )
       ((and (not (equal? turno 2)) (and (equal? posicion 0) (equal? jugador 2))) ;si es la mano del segundo jugador y no es su turno
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 425) 312)) ;grafica la carta oculta 
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck) ;llama recursivamente a para que se grafique la segunda carta del jugador
        )        
       ((equal? jugador 2) ;si es la mano del segundo jugador
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 425) 312)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck)  ;llama recursivamente a para que se grafique la segunda carta del J2
        )

       ((and (not (equal? turno 3)) (and (equal? posicion 0) (equal? jugador 3))) ;si es la mano del tercer jugador y no es su turno 
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 740) 175)) ;grafica la carta oculta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck) ;llama recursivamente a para que se grafique la segunda carta del jugador
        )        
       ((equal? jugador 3) ;si es la mano del tercer jugador
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 740) 175))
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno cant_jugadores deck)  ;llama recursivamente a para que se grafique la segunda carta del J3
        )     
   )
  )
;Funcion LEN
(define (len lista)
  (cond( (null? lista)
         0)
       (else
        (+ 1 (len (cdr lista))))))

;Funcion que maneja los botones
(define (botones click turno len lista_jugadores deck)
  (cond  ((and (and (> (posn-x click) 800) (> (posn-y click) 382)) (equal? turno (- len 1)));se definen las posiciones del boton PLANTARSE y la condicion
;-------------------BOTON PLANTARSE SI ES EL CRUPIER -------
  (write " turno 0")
  (begin (interfaz (game '() len (list lista_jugadores (+ turno 1)) deck)  0)))
         
  ((and (> (posn-x click) 800) (> (posn-y click) 382));se definen las posiciones del boton PLANTARSE y la condicion
;-------------------BOTON PLANTARSE -------
   (write "siguiente turno")
  (begin (interfaz (game '() len (list lista_jugadores (+ turno 1)) deck)  (+ turno 1) deck))) ;aqui se prueba agregando un as
       
  
  ((and (and (> (posn-x click) 7) (> (posn-y click) 361)) (< (posn-x click) 917)) ;se definen las posiciones del boton PEDIR y la condicion
;-------------------BOTON PEDIR CARTA ------------
   (write "jugadores    ")
   (write (caar (game '() len  '(lista_jugadores turno) deck)))
   ((interfaz (caar (game '() len  '(lista_jugadores turno) deck)) turno deck))); se prueba agregango una k 
  
  
  (else (botones (mouse-click-posn (get-mouse-click ventana)) turno deck)) ;Si se toca en otro lado llama otra vez a botones
       )
 )
(define (boton2 click ventana2)
  (cond((> (posn-x click) 0)
    ((close-viewport ventana2)
     (interfaz '(("9H" "4S" "9H") ("6C" "9D")) 0)))

  ))

(define (vuelta lista)
  (append-element  (cdr lista) (car lista))
  )
(define (append-element lst elem)
  (append lst (list elem)))
(define juego (bCEj '("j1" "j2" "j3")))
(write juego)
(interfaz (caar juego) 1 (cadr juego))
