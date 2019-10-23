#lang racket
(require (lib "graphics.ss" "graphics")) (open-graphics)
(define ventana (open-viewport "ventana" 917 471)) 

((draw-pixmap ventana) "mesa.png" (make-posn 0 0))
(define (interfaz lista_jugadores turno)
  ;si hay 1 jugador
  (cond  ((equal? turno 5)
          (define ventana2 (open-viewport "ventana2" 398 265))
          ((draw-pixmap ventana2) "fondov2.jpg" (make-posn 0 0))
          ((draw-string ventana2) (make-posn 180 80) (caar lista_jugadores))
          ((draw-string ventana2) (make-posn 325 80) (cadar lista_jugadores))

          ((draw-string ventana2) (make-posn 180 120) (caadr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 120) (cadadr lista_jugadores))

          ((draw-string ventana2) (make-posn 180 160) (caaddr lista_jugadores))
          ((draw-string ventana2) (make-posn 325 160) (car (cdaddr lista_jugadores)))

          ((draw-string ventana2) (make-posn 180 200) (car (cadddr lista_jugadores)))
          ((draw-string ventana2) (make-posn 325 200) (cadr (cadddr lista_jugadores)))
          )
          
    ((equal? (len lista_jugadores) 2)   ;Condiciones que grafican las cartas segun cuantos jugadores esten
         (graficar (car lista_jugadores) 0 0 (len (car lista_jugadores)) turno)   ;envia a graficar al crupier
         (graficar (cadr lista_jugadores) 1 0 (len (cadr lista_jugadores)) turno) ;envia a graficar al primer jugador
         (botones (mouse-click-posn (get-mouse-click ventana))) turno)
        ;Si hay 2 jugadores
         ((equal? (len lista_jugadores) 3)
         (graficar (car lista_jugadores) 0 0 (len (car lista_jugadores)) turno)     ;envia a grafica al crupier
         (graficar (cadr lista_jugadores) 1 0 (len (cadr lista_jugadores)) turno)   ;envia a graficar al primer jugador
         (graficar (caddr lista_jugadores) 2 0 (len (caddr lista_jugadores)) turno) ;envia a graficar al segundo jugador
         (botones (mouse-click-posn (get-mouse-click ventana)) turno))
        ;Si hay 3 jugadores  
         ((equal? (len lista_jugadores) 4)
         (graficar (car lista_jugadores) 0 0 (len (car lista_jugadores)) turno)      ;envia a graficar al crupier
         (graficar (cadr lista_jugadores) 1 0 (len (cadr lista_jugadores)) turno)    ;envia a graficar al primer jugador
         (graficar (caddr lista_jugadores) 2 0 (len (caddr lista_jugadores)) turno)  ;envia a graficar al segundo jugador
         (graficar (cadddr lista_jugadores) 3 0 (len (cadddr lista_jugadores)) turno)  ;envia a graficar al tercer jugador
         (botones (mouse-click-posn (get-mouse-click ventana)) turno))

        )
  )
;Funcion que dibuja una carta en el tablero usando el codigo el jugador y la posicion en la mano del jugador
(define (graficar cartas jugador posicion cantidad turno)
  
  (cond( (null? cartas)
      #t)
       ((and (not (equal? turno 4)) (and (equal? posicion 0) (equal? jugador 0))) ;si es la mano del crupier
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 439) 33)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno) ;llama recursivamente a para que se grafique la segunda carta del crupier
        )       
       ((equal? jugador 0) ;si es la mano del crupier
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 439) 33)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno) ;llama recursivamente a para que se grafique la segunda carta del crupier
        )
       ((and (not (equal? turno 1)) (and (equal? posicion 0) (equal? jugador 1))) ;si es la mano del crupier
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 124) 182)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno) ;llama recursivamente a para que se grafique la segunda carta del crupier
        )        
       ((equal? jugador 1)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 124) 182)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno)  ;llama recursivamente a para que se grafique la segunda carta del J1
        )
       ((and (not (equal? turno 2)) (and (equal? posicion 0) (equal? jugador 2))) ;si es la mano del crupier
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 425) 312)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno) ;llama recursivamente a para que se grafique la segunda carta del crupier
        )        
       ((equal? jugador 2)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 425) 312)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno)  ;llama recursivamente a para que se grafique la segunda carta del J2
        )

       ((and (not (equal? turno 3)) (and (equal? posicion 0) (equal? jugador 3))) ;si es la mano del crupier
        ((draw-pixmap ventana) "carta_atras.png" (make-posn (+ (* posicion 25) 740) 175)) ;grafica la carta
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno) ;llama recursivamente a para que se grafique la segunda carta del crupier
        )        
       ((equal? jugador 3)
        ((draw-pixmap ventana) (string-append (car cartas) ".png") (make-posn (+ (* posicion 25) 740) 175))
        (graficar (cdr cartas) jugador (+ 1 posicion) cantidad turno)  ;llama recursivamente a para que se grafique la segunda carta del J3
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
(define (botones click turno)
  (cond(
  (and (> (posn-x click) 800) (> (posn-y click) 382));se definen las posiciones del boton PLANTARSE y la condicion
;-------------------BOTON PLANTARSE -------
  (begin (interfaz '(("9H" "4S") ("6C" "9D") ("8C" "JS") ("8C" "KD"))  (+ turno 1)))) ;aqui se prueba agregando un as
       
  
  ((and (and (> (posn-x click) 7) (> (posn-y click) 361)) (< (posn-x click) 917)) ;se definen las posiciones del boton PEDIR y la condicion
;-------------------BOTON PEDIR CARTA ------------
   ((interfaz '(("9H" "4S" "9H") ("6C" "9D") ("8C" "JS") ("8C" "KD")) turno))); se prueba agregango una k 
  
  
  (else (botones (mouse-click-posn (get-mouse-click ventana)) turno)) ;Si se toca en otro lado llama otra vez a botones
       )
 )
(interfaz '(("9H" "4S" "9H") ("6C" "9D") ("8C" "JS") ("8C" "KD")) 0)
