#lang eopl

;Carolain Jiménez Bedoya--> 202071368
;Gabriel Franco Betancourth--> 202024200
;Natalia López Rincón--> 202025618
;Juan Steban Diaz Ramirez--> 202024147
;Hernando López Rincón--> 202022318
;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 1:
;;invert:
;;Proposito:
;;List -> List : Procedimiento que recibe una lista de listas de tamaño 2 (x,y)
;;y devuelte otra lista con las parejas en orden invertido (y,x)
;;
;;<lista> := ()
;;        := (<list-valores> <lista>)

;;<list-valores>:= <valor valor>


(define invert
  (lambda (l)
    (cond
      [(eqv? l '()) empty]
      [else (cons(cons(cadar l)(cons(caar l)empty)) (invert (cdr l)))]
      )
    )
  )


;; Pruebas
;;(invert '((a 1) (a 2) (1 b) (2 b)))
;;(invert '((5 9) (10 91) (82 7) (a e)))
;;(invert '(('a 'b) ('w 9) (45 78)))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 2:
;;down:
;;Proposito:
;;List -> List : Procedimiento que devuelve una lista con cada elemento de la lista original
;;asociado a un nivel mas de parentesis.
;;
;;<lista> := ()
;;        := ('(<valor>) <lista>)
;;<valor> := <int> <symbol> <list>


(define down
  (lambda (lista)
    (if (eqv? lista empty)
        empty
        (cons (cons (car lista) empty)(down (cdr lista)))
    )
  )
)


;; Pruebas
;;(down '(1 2 3))
;;(down '((una) (buena) (idea)))
;;(down '(un (objeto (mas)) complicado))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 3:
;;list-set:
;;Proposito:
;;List, numero, elemento -> List : Procedimiento que ingresa en la posicion indicada por el numero
;;un elemento cualquiera X en una lista.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

;;<valor-de-scheme>:=  <int> <symbol> <list>

(define list-set
  (lambda (lst n x)
    (letrec
        (
         (list-aux
          (lambda (lstaux naux xaux acc)
            (cond
              [(eqv? lstaux '()) empty]
              [(eqv? acc naux) (cons xaux (list-aux (cdr lstaux) naux xaux (+ 1 acc)))]
              [else (cons (car lstaux) (list-aux (cdr lstaux) naux xaux (+ 1 acc)))]
              )
            )
          )
         )
      (list-aux lst n x 0))
    )
  )

;;Pruebas
;;(list-set '(a b c d) 2 '(1 2))
;;(list-set '(a b c d) 3 '(1 5 10))
;;(list-set '(d 2 3 44 ch) 3 'ab)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 4:
;;filter-in:
;;Proposito:
;;Predicado, List -> List : La funcion retorna una lista que contiene los elementos que pertenecen
;;a la lista y que satisfacen el predicado
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)


(define filter-in
  (lambda (P L)
    (cond
      [(eqv? L '()) empty]
      [(eqv? (P (car L)) #T) (cons (car L) (filter-in P (cdr L)))]
      [else (filter-in P (cdr L))]
      )
    )
 )

;;Pruebas
;;(filter-in number? '(a 2 (1 3) b 7))
;;(filter-in symbol? '(a (b c) 17 foo))
;;(filter-in list? '( c d er (a b) (1 3)))
;;(filter-in symbol? '( 1 2 3 (a v) b gg))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 5:
;;list-index:
;;Proposito:
;;Predicado, List -> Elemento : La funcion retorna el primer elemento que pertenece
;;a la lista y que satisface el predicado propuesto, si no se encuentra ninguno, devuelve #f.
;;
;;<lista> := <false> 
;;        := (<int> <lista>)


(define list-index
  (lambda (p lst)
    (letrec
        (
         (list-aux
          (lambda (paux lstaux acc)
            (cond
              [(eqv? lstaux '()) #false]
              [(p (car lstaux)) acc]
              [else (list-aux paux (cdr lstaux) (+ 1 acc))]
              )
            )
          )
         )
      (list-aux p lst 0)
      )
    )
  )


;;Pruebas
;;(list-index number? '(a 2 (1 3) b 7))
;;(list-index symbol? '(a (b c) 17 foo))
;;(list-index symbol? '(1 2 (a b) 3))
;;(list-index number? '(a b dw '(1 2 6) g 6))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 6:
;;swapper:
;;Proposito:
;;Elemento, Elemento, List -> List: El procedimiento intercambia cada coincidencia del primer elemento por el
;;segundo y del segundo por el primero. Los elementos deben pertenecer a la lista.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define swapper
  (lambda (e1 e2 lista)
    (if (eqv? lista empty)
        empty
        (cond
          [(eqv? e1 (car lista)) (cons e2(swapper e1 e2 (cdr lista)))]
          [(eqv? e2 (car lista)) (cons e1(swapper e1 e2 (cdr lista)))]
          [else  (cons (car lista)(swapper e1 e2 (cdr lista))) ]
          
        )
    ) 
    
  )
)


;;Pruebas
;;(swapper 'a 'd '(a b c d))
;;(swapper 'a 'd '(a d () c d))
;;(swapper 'x 'y '(y y x y x y x x y))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 7:
;;cartesian-product:
;;Proposito:
;;List, List -> List:La funcion debe retornar una lista de tuplas que representen el producto
;;cartesiano entre las listas ingresadas
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

;;<valor-de-scheme>:= <int> <simbolo>

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        empty
        (myAppend (lista (car L1) L2) (cartesian-product (cdr L1) L2))
      )
    )
  )

;;Pruebas
;;(cartesian-product '(a b c) '(x y))
;;(cartesian-product '(p q r) '(5 6 7))
;;(cartesian-product '(2 a 4 tr) '(1 2))


;;lista:
;;Proposito:
;;List, List -> List:La funcion debe retornar la concatenacion de dos listas para su posterior distribucion en tuplas de producto cartesiano

(define lista
  (lambda (L1 L2)
    (if (null? L2)
        empty
        (cons (cons L1 (cons (car L2) empty))
        (lista L1 (cdr L2)))
        )
    )
  )



;;Pruebas
;;(lista '(2 3 4) '(a s r))


;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 8:
;;mapping:
;;Proposito:
;;Funcion Unaria, List, List -> List: La funcion retorna una lista de pares (a,b) siendo a elemento de la primera lista
;;y b elemento de la segunda, cumpliendose la propiedad que al aplicar la funcion unaria con el argumento a, debe arrojar
;;el numero b y considerando que las listas son de igual tamaño
;;
;;<lista> := ()
;;        := (<Int> <lista>)

(define mapping
  (lambda (F L1 L2)
    (cond
      [(null? L1) '()]
      [(eqv? (car L2) (F (car L1)))
       (cons (cons (car L1) (cons (car L2) '())) (mapping F (cdr L1) (cdr L2)))]
      [else (mapping F (cdr L1) (cdr L2))]
      )
    )
  )


;;Pruebas
;;(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;;(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;;(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 9:
;;inversions: Función que devuelve la cantidad de combinaciones posibles de una lista que cumplan las condiciones de que
;;la posición i sea < j, y el valor de la posición i > j.
                      
;;
;<lista-de-enteros> ::= <int>
;                   ::= (<int> <lista-de-enteros>)

(define inversions
  (lambda (L)
    (letrec
        (
         (logic
          (lambda (L i j acc)
            (cond
              [(< i j)
               (cond
                  [(eqv? i (length L 0)) acc ]
                  [(eqv? j (length L 0)) (logic L (+ i 1) 0 acc)]
                  [(> (posicion-valor1 L i 0) (posicion-valor1 L j 0)) (logic L i (+ j 1) (+ acc 1))]
                  [else (logic L i (+ j 1) acc)])

                         ]
              [else (logic L i (+ j 1) acc)]
              )
            )
          )  
         )
      (logic L 0 1 0)
      )
    )
  )

;;Pruebas
;;(inversions '(2 3 4 6 7))
;;(inversions '(8 6 5 3 2 1))
;;(inversions '(3 8 2 4 10 7))



;;Se usaron funciones auxiliares para el desarrollo del problema.
;;posicion-valor1
;;Proposito:
;;List -> symbol : Funcion que a partir de una posicion en una lista, devuelve el valor que se encuentra en la posición. 
;;
;;<lista> := ()
;;        := (<Int> <lista>)

(define posicion-valor1
  (lambda (l posicion acc)
    (cond
      [(eqv? l '()) "Error"]
      [(eqv? acc posicion) (car l)]
      [else (posicion-valor1 (cdr l) posicion (+ acc 1))]
      ) 
    )
  )

;;Pruebas
;;(posicion-valor1 '(a b c 3 4 55) 2 0)
;;(posicion-valor1 '( t y u ccd 3 4 99) 4 0)
;;(posicion-valor1 '(4 5 63 v) 0 0)


;;length
;;Proposito:
;;List -> Integer : Funcion que determina el tamaño de una lista. 

(define length
  (lambda (l acc)
    (cond
      [(eqv? l '()) acc]      
      [else (length (cdr l) (+ acc 1))]
      ) 
    )
  )

;;Pruebas
;;(length '(o gg c 6 4 5) 0)
;;(length '( t y u ccd 9) 0)
;;(length '(n mt (4 5) z) 0)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 10:
;;up:
;;List -> List: Procedimiento que devuelve una lista con cada elemento de la lista original
;;asociado a un nivel menos de parentesis. Si un elemento no tiene parentesis se incluye así.
;;
;;<lista> := <list>
;;        := (<valor-de-scheme> <lista>)


(define up
  (lambda (lst)
    (cond
      [(eqv? lst '()) lst]
      [(list? (car lst)) (append (car lst) (up(cdr lst)))]
      [else (cons(car lst) (up (cdr lst)))]
      ) 
    )
  )


;;Pruebas
;;(up '((1 2) (3 4)))
;;(up '((x (y)) z))
;;(up '((((d g r))) 4 5 ((89)) m))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 11:
;;zip:
;;funcion binaria, List, List -> List: Procedimiento que debe retornar una lista donde
;;la posicion n-esima corresponde al resultado de aplicar la funcion sobre los elementos en
;;la posicion n-esima en L1 y L2
;;
;;<lista> := ()
;;        := (<int> <lista>)


(define zip
  (lambda (funcion l1 l2)
    (if (eqv? l1 empty)
        empty
        (cons (funcion (car l1) (car l2))(zip funcion (cdr l1) (cdr l2)))
    ) 
  )
)



;;Pruebas
;;(zip + '(1 4) '(6 2))
;;(zip * '(11 5 6) '(10 9 8))
;;(zip + '(3 2 1) '(8 7 6))
;;(zip * '(12 5 8) '(3 6 8))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 12:
;;filter-acum:
;;Integer, Integer, funcionBinaria, modulo, filtro -> Integer
;;Próposito: El procedimiento filter-acum aplica la función binaria que se requiere a todos los elementos que se encuentran en el
;;intervalo y que además cumplan con el predicado o filtro propuesto.

;;<expression> := <int>
;;             := (<valor-de-scheme> <expression>)
;;<valor-de-scheme> := <valor>

(define filter-acum
  (lambda (a b F acum filter)
    (letrec
        (
         (list-made
          (lambda (aaux baux acc)
            (cond
              [(eqv? acc (+ (- b a) 1)) empty]
              [else (cons (+ a acc) (list-made aaux baux (+ 1 acc)))]
              )
            )
          )
      
         )
      (funcion-val (list-made a b 0) filter acum F)
      )
           
    
    )
  )

;;Pruebas
;;(filter-acum 1 10 * 0 even?)
;;(filter-acum 2 7 + 0 odd?)
;;(filter-acum 8 15 + 0 odd?)

;;Se necesita una función auxiliar, para el correcto funcionamiento.
;;funcion-val:
;;próposito: Valida los valores existentes de una lista que cumplen una determinada condición que se requiera y retorna la operación que se debe
;;realizar con estos.   

(define funcion-val
  (lambda (l filtro acum f)
    (cond
      [(eqv? l '()) (if (or (eqv? f *) (eqv? f /)) (+ acum 1) acum)]
      [(filtro (car l)) ( + acum (f (car l) (funcion-val (cdr l) filtro acum f)))]
      [else (funcion-val (cdr l) filtro acum f)]
      ) 
    )
  )


;;Pruebas
;;(funcion-val '(1 2 3 4 5 6 7 8) odd? 0 +)
;;(funcion-val '(1 2 3 4 5 6 7 8) even? 0 +)
;;(funcion-val '(a w 3 r 4) number? 0 *)
;;(funcion-val '(dd 20 5 lk) number? 0 /)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 13:
;;operate:
;;List, List -> Int: La funcion recibe una primera lista de operaciones binarias y una segunda lista de
;;numeros y retorna el resultado de aplicar sucesivamente las operaciones en la lista 1 a los valores en la lista 2.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)


(define operate
  (lambda (lrators lrands)
  (aux (invert2 lrators) (invert2 lrands))
 ))

;;Pruebas
;;(operate (list + * + - *) '(1 2 8 4 11 6))
;;(operate (list *) '(4 5))
;;(operate (list + + +) '(3 4 5 6))

;;Funcion auxiliar
;;aux:
;;List, List -> Int:La funcion recibe 2 listas y realiza las operaciones marcadas en la primera lista entre los elementos de la segunda lista

(define aux
  (lambda (lrators lrands)
    (cond
      [(eqv? lrators '()) (car lrands)]
      [else ((car lrators) (aux (cdr lrators) (cdr lrands)) (car lrands))]
      )
    )
  )


;;Funcion auxiliar
;;invert:
;;List -> List: La funcion recibe una lista y la invierte

(define (invert2 lista)
  (cond
    [(eqv? lista '()) empty]
    [else (myAppend (invert2 (cdr lista)) (list (car lista)))]
    )
  )

;;Funcion auxiliar
;;myAppend:
;;List, List -> List: La funcion recibe 2 listas y las concatena en una sola

(define myAppend
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (myAppend (cdr L1) L2 ))
      )
    )
  )



;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 14:
;;operate:
;;Integer, arbol -> List: La funcion debe recorrer el árbol binario y retornar con simbolos la ruta que se debe recorrer
;;para encontrar el número requerido.
;;
;;<arbol-binario> ::= <list>
;;                ::= (<simbolo> <arbol-binario> <arbol-binario>)


(define path
  (lambda (n arbol)
    (cond
        [(eqv? n (car arbol)) empty]
        [(null? arbol) empty]
        [(> n (car arbol)) (cons 'right (path n (caddr arbol)))]
        [else (cons 'left (path n (cadr arbol)))]
        )
    )
  )

;;Pruebas
;;(path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
;;(path 40 '(12 (5 () (10 () ()))(24 (19 (16 () ())())(40 () ()))))
;;(path 24 '(12 (5 () (10 () ()))(24 (19 (16 () ())())(40 () ()))))


;-----------------------------------------------------------------------------------------------------------------------------------------------------

;Ejercicio 15:
;;count-odd-and-even:
;;Arbol -> List: La funcion recibe un arbol binario y devuelve una lista con la cantidad de numeros pares y numeros impares en el arbol.
;;
;;<arbol> := ()
;;        := (<int> <arbol>)


(define count-odd-and-even
  (lambda (arbol)
    (cons (esPar arbol) (cons (esImpar arbol) '()))
    )
  )

;;Pruebas
;;(count-odd-and-even '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
;;

;;Función auxiliar
;;;;esPar:
;;Arbol -> Int: La funcion recibe un arbol binario y devuelve la cantidad de numeros pares en el arbol.

(define esPar
  (lambda (arbol)
    (cond
     [(null? arbol) 0]
     [(even? (car arbol)) (+ 1 (esPar (cadr arbol)) (esPar (caddr arbol)))]
     [else (+ 0 (esPar (cadr arbol)) (esPar (caddr arbol)))]
     )
    )
  )

;;Funcion auxiliar
;;esImpar:
;;Arbol -> Int: La funcion recibe un arbol binario y devuelve la cantidad de numeros impares en el arbol.

(define esImpar
  (lambda (arbol)
    (cond
     [(null? arbol) 0]
     [(even? (car arbol)) (+ 0 (esPar (cadr arbol)) (esPar (caddr arbol)))]
     [else (+ 1 (esPar (cadr arbol)) (esPar (caddr arbol)))]
     )
    )
  )

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 16:
;;simpson-rule:
;;Funcion, int, int -> int: Calcula la integral de una funcion f entre los valores a y b mediante la regla de Simpson. n debe ser par y mayor que 0 
;;
;


(define simpson-rule
  (lambda (funcion lim-inf lim-sup n)
    (* (/ (- lim-sup lim-inf) (* 3 n)) (sumatoria funcion lim-inf lim-sup (- n 1) (/ (- lim-sup lim-inf) n) ))
  )
)



;;sumatoria:
;;Funcion, int, int -> int: Calcula la parte de la integral que se encarga de sumar los terminos de la integral. 
;;


(define sumatoria
  (lambda (funcion lim-inf lim-sup k h)
    (if (< k 0)
        0
        (if (= k 0)
            (+ (funcion lim-inf) (funcion lim-sup))
            (if (even? k)
                (+ (* 2 (funcion (+ lim-inf (* k h)) )) (sumatoria funcion lim-inf lim-sup (- k 1) h )  )
                (+ (* 4 (funcion (+ lim-inf (* k h)) )) (sumatoria funcion lim-inf lim-sup (- k 1) h )  )          
             )

        )      
    )
  )
)

;;Pruebas
;;(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
;;(simpson-rule (lambda (x) x) 1 5 12)

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;;Ejercicio 17:
;;prod-scalar-matriz:
;;Matriz, Lista -> Matriz: retorna el resultado de realizar la multiplicacion matriz por vector
;;
;;<lista> := ()
;;        := (<Int> <lista>)
;;
;;<Matriz> := ()
;;         := (<lista> <Matriz>)

(define prod-scalar-matriz
  (lambda (matriz vector)
    (if (eqv? matriz empty)
        empty
        (cons (prod-vector-vector (car matriz) vector) (prod-scalar-matriz (cdr matriz) vector)) 
      
    )     
  )
)

;;Pruebas
;;(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;;(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))

;;prod-vector-vector:
;;Listas, Lista -> Lista: retorna el resultado de realizar la multiplicacion de dos vectores
;;
;;<lista> := ()
;;        := (<Int> <lista>)
;;


(define prod-vector-vector
  (lambda (vector1 vector2)
    (if (eqv? vector1 empty)
        empty
        (cons (* (car vector1) (car vector2)) (prod-vector-vector (cdr vector1) (cdr vector2)))
    ) 
    
    
  )
)

;;Pruebas
;;(prod-vector-vector '(1 1) '(2 3))
;;(prod-vector-vector '(1 1 3) '(2 3 6))

;-----------------------------------------------------------------------------------------------------------------------------------------------------

;Ejercicio 18:
;;pascal:
;;Int -> List: La funcion recibe un entero que representa el numero de la columna en el triangulo de pascal y devuelve una lista que representa a los valores en la columna n del triangulo de pascal.
;;
;;<lista> := ()
;;        := (<int> <lista>)
(define pascal
  (lambda (n)
    (cond
      [(= n 1) (cons 1 '())]
      [else (zip (lambda (n1 n2) (+ n1 n2))
            (cons 0 (pascal (- n 1)))
            (invert2 (cons 0 (pascal (- n 1)))))]
        )
      )
    )

;;Pruebas:
;;(pascal 5)
;;(pascal 1)
;;(pascal 3)
;;(pascal 6)
;;(pascal 9)






