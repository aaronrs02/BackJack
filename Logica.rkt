#lang racket
(provide (all-defined-out))


;Funcion principal, recibe como parametros la lista de los jugadores, luego manda a la funcion auxiliar, el numero de jugadores y
;el mazo de cartas.
(define (bCEj x)
  (auxbCEj x (len x) '("2H" "3H" "4H" "5H" "6H" "7H" "8H" "9H" "10H" "JH" "QH" "KH" "AH"
                       "2S" "3S" "4S" "5S" "6S" "7S" "8S" "9S" "10S" "JS" "QS" "KS" "AS"
                       "2C" "3C" "4C" "5C" "6C" "7C" "8C" "9C" "10C" "JC" "QC" "KC" "AC"
                       "2D" "3D" "4D" "5D" "6D" "7D" "8D" "9D" "10D" "JD" "QD" "KD" "AD")))

;Funcion auxiliar de la principal. Esta funcion recibe como parametros las lista de jugadores,
;el numero de jugadores y el mazo de cartas, dependiendo del numero de jugadores
;esta funcion llama al caso correspondiente, si el numero de jugadores es mayor a tres
;mandara una advertencia que se ha sobrepasado el numero de jugadores.
(define(auxbCEj players numPlayers deck)
  (cond 
        ((= numPlayers 1)
         (case1 players numPlayers 0 '() '() '() '() deck))
        ((= numPlayers 2)
         (case2 players numPlayers 0 '() '() '() '() deck))
        ((= numPlayers 3)
         (case3 players numPlayers 0 '() '() '() '() deck))
        ((= numPlayers 4) "Ha sobrepasado la cantidad maxima de jugadores")))

;Esta funcion se utilizara si solo esta jugando un jugador contra el crupier,
;recibira como parametros la lista de jugadores, el numero de jugadores, un contador,
;y las listas de cada jugador y el crupier y el mazo de cartas
;esta ira llenando la lista del cruppier y del jugador1 con una carta al azar
;luego llamara a su auxiliar para eliminar esta carta del mazo, asi hasta poner dos cartas en el crupier
;y el jugador1
(define(case1 players numPlayers count crupier player1 player2 player3 deck)
  (cond ((= count 0)
         (auxCase1 players numPlayers count (cons(car (shuffle deck)) crupier) player1 player2 player3 deck))
        ((= count 1)
         (auxCase1 players numPlayers count (cons(car (shuffle deck)) crupier) player1 player2 player3 deck))
        ((= count 2)
         (auxCase1 players numPlayers count crupier (cons(car (shuffle deck)) player1) player2 player3 deck))
        ((= count 3)
         (auxCase1 players numPlayers count crupier (cons(car (shuffle deck)) player1) player2 player3 deck))
        ((= count 4)
         (game players numPlayers (list (list crupier player1 player2 player3) 0 ) deck))))

;Esta funcion se utilizara si solo esta jugando dos jugadores contra el crupier,
;recibira como parametros la lista de jugadores, el numero de jugadores, un contador,
;y las listas de cada jugador y el crupier y el mazo de cartas
;esta ira llenando la lista del cruppier y del jugador 1 y 2 con una carta al azar
;luego llamara a su auxiliar para eliminar esta carta del mazo, asi hasta poner dos cartas en el crupier,
;jugador1, jugador2.
(define(case2 players numPlayers count crupier player1 player2 player3 deck)
    (cond ((= count 0)
         (auxCase2 players numPlayers count (cons(car (shuffle deck)) crupier) player1 player2 player3 deck))
        ((= count 1)
         (auxCase2 players numPlayers count (cons(car (shuffle deck)) crupier) player1 player2 player3 deck))
        ((= count 2)
         (auxCase2 players numPlayers count crupier (cons(car (shuffle deck)) player1) player2 player3 deck))
        ((= count 3)
         (auxCase2 players numPlayers count crupier (cons(car (shuffle deck)) player1) player2 player3 deck))
        ((= count 4)
         (auxCase2 players numPlayers count crupier player1 (cons(car (shuffle deck)) player2) player3 deck))
        ((= count 5)
         (auxCase2 players numPlayers count crupier player1 (cons(car (shuffle deck)) player2) player3 deck))
        ((= count 6)
         (game players numPlayers (list (list crupier player1 player2 player3) 0 ) deck))))

;Esta funcion se utilizara si estan jugando todos los jugadores contra el crupier,
;recibira como parametros la lista de jugadores, el numero de jugadores, un contador,
;y las listas de cada jugador y el crupier y el mazo de cartas
;esta ira llenando la lista del cruppier y del jugador 1,2 y 3 con una carta al azar
;luego llamara a su auxiliar para eliminar esta carta del mazo, asi hasta poner dos cartas en el crupier,
;jugador 1, jugador 2 y jugador 3.
(define(case3 players numPlayers count crupier player1 player2 player3 deck)
   (cond ((= count 0)
         (auxCase3 players numPlayers count (cons(car (shuffle deck)) crupier) player1 player2 player3 deck))
        ((= count 1)
         (auxCase3 players numPlayers count (cons(car (shuffle deck)) crupier) player1 player2 player3 deck))
        ((= count 2)
         (auxCase3 players numPlayers count crupier (cons(car (shuffle deck)) player1) player2 player3 deck))
        ((= count 3)
         (auxCase3 players numPlayers count crupier (cons(car (shuffle deck)) player1) player2 player3 deck))
        ((= count 4)
         (auxCase3 players numPlayers count crupier player1 (cons(car (shuffle deck)) player2) player3 deck))
        ((= count 5)
         (auxCase3 players numPlayers count crupier player1 (cons(car (shuffle deck)) player2) player3 deck))
        ((= count 6)
         (auxCase3 players numPlayers count crupier player1 player2 (cons(car (shuffle deck)) player3) deck))
        ((= count 7)
         (auxCase3 players numPlayers count crupier player1 player2 (cons(car (shuffle deck)) player3) deck))
        ((= count 8)
         (game players numPlayers (list (list crupier player1 player2 player3) 0 ) deck))))

;;Funcion auxiliar para eliminar del mazo en el primer caso
;Recibe como parametros lalista de jugadores, el numero de jugadores, un contador,
;la lista de cada jugador  y el mazo de cartas;
;Esta le devolvera a la funcion case1 los mismos parametros pero con la carta eliminada en el mazo.
(define (auxCase1 players numPlayers count crupier player1 player2 player3 deck)
  (cond ((= count 0)
        (case1 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car crupier) deck)))
        ((= count 1)
        (case1 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car crupier) deck)))
        ((= count 2)
        (case1 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player1) deck)))
        ((= count 3)
        (case1 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player1) deck)))))

;;Funcion auxiliar para eliminar del mazo en el segundo caso
;Recibe como parametros la lista de jugadores, el numero de jugadores, un contador,
;la lista de cada jugador  y el mazo de cartas;
;Esta le devolvera a la funcion case1 los mismos parametros pero con la carta eliminada en el mazo
(define (auxCase2 players numPlayers count crupier player1 player2 player3 deck)
   (cond ((= count 0)
        (case2 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car crupier) deck)))
        ((= count 1)
        (case2 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car crupier) deck)))
        ((= count 2)
        (case2 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player1) deck)))
        ((= count 3)
        (case2 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player1) deck)))
        ((= count 4)
        (case2 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player2) deck)))
        ((= count 5)
        (case2 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player2) deck)))))

;;Funcion auxiliar para eliminar del mazo en el tercer caso
;Recibe como parametros la lista de jugadores, el numero de jugadores, un contador,
;la lista de cada jugador  y el mazo de cartas;
;Esta le devolvera a la funcion case1 los mismos parametros pero con la carta eliminada en el mazo
(define (auxCase3 players numPlayers count crupier player1 player2 player3 deck)
     (cond ((= count 0)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car crupier) deck)))
        ((= count 1)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car crupier) deck)))
        ((= count 2)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player1) deck)))
        ((= count 3)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player1) deck)))
        ((= count 4)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player2) deck)))
        ((= count 5)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player2) deck)))
        ((= count 6)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player3) deck)))
        ((= count 7)
        (case3 players numPlayers (+ 1 count) crupier player1 player2 player3 (eliminarDelDeck (car player3) deck)))))
 
;; FUNCION ELIMINAR DEL DECK
;Recibe como parametros el elemento a eliminar y el mazo de cartas.
(define (eliminarDelDeck elemento deck)
   (cond 
          ((null? deck)'())                                                    ;Verifica si la lista es nul
          ((equal? elemento (car deck)) (eliminarDelDeck elemento (cdr deck)) );Compara si el numero es el primero de la lista,
                                                                               ;sino llama a la funcion con la lista sin el primer elemento
          (else
              (cons(car deck) (eliminarDelDeck elemento (cdr deck)));Aqui va creando una nueva lista, si en la linea anterior
                                                                    ;encontro el numero se elimina
           )        
    )
  )


(define(randomCard deck)
  (car (shuffle deck)))

(define (appendEle lst elem)
  (append lst (list elem)))

(define (addCard newList players player deck)

  (cond
     ((null? players) newList )
     ((equal? player (car players) )  (addCard (appendEle newList (cons (randomCard deck) player ) ) (cdr players) player deck)  )
     ((not(null? players))  (addCard (appendEle newList (car players) ) (cdr players) player deck) )
     

    )
  
 
  )
 
(define (len lista)
  (cond( (null? lista)
         0)
       (else
        (+ 1 (len (cdr lista))))))

;FUNCION PARA SUMAR LAS CARTAS DE UNA LISTA
(define (sumarDeck deck sumatoria); AL DEFINIR ESTA FUNCION SE TOMA SUMATORIA COMO CERO '0'
                                  ;PARA QUE LUEGO S EMPIEZEN A SUMAR RECURSIVAMENTE LOS VALORES DE LAS CARTAS
  (cond
    ((null? deck)  sumatoria )
    ((equal? (car deck) "dead" ) "dead" )

    (else
      (cond
        ((equal? "2" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 2))  )
        ((equal? "3" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 3))  )
        ((equal? "4" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 4))  )
        ((equal? "5" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 5))  )
        ((equal? "6" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 6))  )
        ((equal? "7" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 7))  )
        ((equal? "8" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 8))  )
        ((equal? "9" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 9))  )
        ((equal? "10"(substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 10)) )
        ((equal? "J" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 10)) )
        ((equal? "Q" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 10)) )
        ((equal? "K" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 10)) )
        ((equal? "A" (substring (car deck)  0  (- (string-length (car deck) ) 1)  ) )  (sumarDeck (cdr deck) (+ sumatoria 1))  )
       )
     )

    )

  )


;;dar elemento por posición
(define (givePos pos list iter); iter se pone siempre uno (1), la liste empieza a contarse desde cero (0, 1, 2, 3) no (1, 2, 3)

          
              (cond
                   ((< iter pos )

                    (cond
                       ((null? (cdr list)) "oob" )
                       (else
                         (givePos pos (cdr list) (+ iter 1) )
                        )
                      )


                    )

                   ((= iter pos)  (car list)  )


                   )
         
  

  )


(define (nextInLine numPlayers turn)

  (cond
      ((< turn numPlayers) (+ turn 1) )
      ((= turn numPlayers) 0 )
    )

  )


(define (game players numPlayers playerStats deck)



  (cond
     ((equal? "oob" (givePos (cadr playerStats)  (car playerStats) 0 ) ) "Fatal error: list in playerStats [game function] doesnt have requested items."  )
     (else

         (cond
           ((> (sumarDeck (givePos (cadr playerStats) (car playerStats) 0 )  0 )   21 )   (list playerStats deck) )
           ((and (= (cadr playerStats) 0) (>= (sumarDeck (givePos (cadr playerStats) (car playerStats) 0 ) 0) 17 ) )    (list playerStats deck)   )
           (else
              (list (list  (addCard (list ) (car playerStats) (givePos (cadr playerStats) (car playerStats) 0 ) deck)   (cadr playerStats)   )  deck  )
              
            )
           )
          

      )
    )



  
  )


(bCEj (list "Car" "Cal" "Cam") )
  


