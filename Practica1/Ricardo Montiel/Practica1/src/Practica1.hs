{-
- Lógica computacional 2020-2
- Practica 1
- Alumno: 			Escamilla Soto Cristopher Alejandro
- Número de cuenta: 314309253
-Correo: 			crisopher@ciencias.unam.mx
- Alumno:			Montiel Manriquez Ricardo
- Número de cuenta: 314332662
-Correo: 			moma92@ciencias.unam.mx
-}

module Practica1 where

-- Tipos definidos

--La expresion deriving show solicit Haskell que calcule de forma
--automatica la forma de mostrar valores de tipo COMPLEJO. De
--esta forma, sera posible evaluar y mostrar valores de tipo COMPLEJO.

--Definir el tipo Complejo
data Complejo = Comp Float Float
  deriving Show

--Forma para definir el tipo de los números naturales
--de manera recursiva iniciando desde el cero.
data Nat = Cero | Suc Nat deriving(Show,Eq)


--Definición recursiva de listas, la lista más
--pequeña es la lista vacía representada por la palabra Nula
data Lista a = Nula | Cons a (Lista a) deriving(Show,Eq)


--Definición recursiva de árboles, el árbol más pequeño es
--el árbol vacío.
data Arbol = Vacio | Nodo Arbol Int Arbol deriving(Show,Eq)


-- Funciones principales

--Funcion puntoMedio que dados dos puntos en el plano encuentra el punto medio entre los dos.
--Ejemplo
--Prelude>puntoMedio (-1,2) (7,6)
--(3.0,4.0)
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x1,y1) (x2,y2)= ((x1+x2)/2,(y1+y2)/2)

--Función que dada una ecuación de segundo grado encuentra las raices de esta en una
--pareja ordenada
--Definire la funcion raices de tal forma que (raices a b c) devuelve
--las raices  de la ecuacion ax^2 + by + c
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c
  |discriminante a b c == True = (Comp ((-b+(sqrt(d)))/t) 0, Comp ((-b-(sqrt(d)))/t) 0)
  |discriminante a b c == False = (Comp (-b/t) (sqrt(-d)/t), Comp (-b/t) (-(sqrt(-d)/t)))
   where d = (b^2)-(4*a*c)
         t = 2*a

--Definir la función segmento tal que (segmento m n xs) es la lista de los
--elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
--segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
--segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
--segmento 5 3 [3,4,1,2,7,9,0] == []
segmento :: Int -> Int -> [a] -> [a]
segmento m n xs = drop (m-1) (take n xs)

--Definir la función extremos tal que (extremos n xs) es la lista formada
--por los n primeros elementos de xs y los n finales elementos de xs. Por ejemplo,
--extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
extremos :: Int -> [a] -> [a]
extremos n xs = take n xs ++ drop(length xs- n) xs


--Funcion que elimina un intervalo de una lista; dados dos números y una lista,
--elimina los elementos que se encuentren en el intervalo de esos dos numeros.
--Por ejemplo,
--dIntervalos 2 4 [1,2,3,4,5,6,7] == [1,5,6,7]
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos m n xs =take (m-1) xs ++ drop n xs

--Un número natural n se denomina abundante si es menor que la suma de sus divisores
--propios, sin el mismo. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
--Definir la función numerosAbundantes tal que (numerosAbundantes n)
--es la lista de números abundantes menores o iguales que n. Por ejemplo,
--numerosAbundantes 50 == [12,18,20,24,30,36,40,42,48]
numerosAbundantes :: Int -> [Int]
numerosAbundantes x = [y | y <- [1..x], abundante y, x /= y]

--Funcion discriminante que nos da precisamente el discriminante de
--una ecuacion de segundo grado, regresando True si es >0
-- y False si < 0
discriminante :: Float -> Float -> Float -> Bool
discriminante a b c
  | d>0 = True
  | d<0 = False
  | otherwise = True
   where d = b^2-4*a*c

--Definir la función que recibe una lista y regrese una lista tal que
--la lista resultante contiene los mismos elementos que la original pero
--sin duplicados.
--Ejemplo:
--eliminaDuplicados [1,3,1,2,3,2,1] ; [1,3,2]
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [x] = [x]
eliminaDuplicados (x:xs) | elem x xs = eliminaDuplicados xs
                         | otherwise = x:eliminaDuplicados xs

--Se define el primitivo de un número como sigue:
--Dado un número natural n, multiplicamos todos sus dígitos,
--repetimos este procedimiento hasta que quede un solo dígito al
--cual llamamos primitivo de n. Por ejemplo, para 327
--327 : 3 X 2 X 7 = 42 y 4 X 2 = 8.
--Por lo tanto, el primitivo de 327 es 8.
--Definir la función dado un número nos regrese el primitivo.
--Ejemplo:
--primitivo 327 ; 8
primitivo :: Integer -> Integer
primitivo n
       | n < 10    = n
       | otherwise = primitivo (productoDigitos n)

--Función que dadas dos listas y un Natural j, regresa una lista tal que, se encuentran
--concatenados el i-ésimo elemento de la primer Lista con el i-ésimo elemento de la
--segunda Lista; a partir del elemento j de cada una de las listas.
--Ejemplo:
--sipLis (Suc (Suc Cero)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nula))))) (Cons 7 (Cons 8 (Cons 9 Nula))) ==
--(Cons 2 (Cons 8 (Cons 3 (Cons 9 Nula))))
sipLis :: Nat -> Lista a -> Lista a -> Lista a
sipLis Cero a b = Nula
sipLis n Nula b = Nula
sipLis n a Nula = Nula
sipLis n a b = concatena(presipLis n a) (presipLis n b)

--Definir una funcion la cual convierte un  ́arbol en una lista haciendo el recorrido en preorden.
--Firma de la funcion:
--Ejemplo: aplanaArbolPre arbol1 [2,0,-1,1,4,3,5].
--Donde arbol1 es:
--(Nodo (Nodo (Nodo Vacio (-1) Vacio) 0 (Nodo Vacio 1 Vacio)) 2 (Nodo (Nodo Vacio 3 Vacio) 4 (Nodo Vacio 5 Vacio)))
aplanaArbolPre :: Arbol -> [Int]
aplanaArbolPre Vacio =[]
aplanaArbolPre (Nodo Vacio n Vacio) = [n]
aplanaArbolPre (Nodo x n y) = [n] ++ (aplanaArbolPre x) ++ (aplanaArbolPre y)



-- ///////////////////(Funciones Aux)///////////////////////////////////
--Funciones auxiliares en caso de tenerlas van aquí

--Funcion Drop  y funcion Take

--Definidas como
--drop :: Int -> [a] -> [a]
--Que elimina un nomero dado de elementos desde el comienzo de la lista

--take :: Int -> [a] -> [a]
--Crea una lista. El primer argumento determina cuantos elementos deben
--tomarse de la lista  pasada como segundo argumento

--elem ::Eq a => a -> [a]-> Bool
--no dice si dado un elemento, este esta contenido en la lista

--rem :: Integral a => a-> a-> a
--el resto de x entre y

--Funcion que nos dice  si un numero x es divisile entre un numero y
divisible :: Int -> Int -> Bool
divisible x y = x `rem` y == 0

--Funcion que nos dice si un numero es abundante
abundante :: Int -> Bool
abundante x
   | d>x  = True
   | d==x = False
   | d<x  = False
   | otherwise = False
   where d = sumaPositivos [y | y <- [1..x], divisible x y, x /= y]

--Funcio que suma los elementos dentro de una lista xs
-- sum se usa para sumar todos los elementos, dentro de una lista
sumaPositivos :: [Int] -> Int
sumaPositivos xs = sum [x | x <- xs, x > 0]

--Nos regresa el resultado de multiplicar los digitos de un numero
productoDigitos :: Integer -> Integer
productoDigitos n
       | n < 10    = n
       | otherwise = n `rem` 10 * primitivo(n `div` 10)



-- Funciones Aux creada solo para entender de mejor manera
-- /////////////////////////////////////////////////////////

dameCabeza :: Lista a -> Lista a
dameCabeza Nula = Nula
dameCabeza (Cons (m)g) = Cons ( m) Nula

cuenta ::Nat -> Nat
cuenta Cero = Cero
cuenta (Suc m) = cuenta(m)
--Funcion que suma TDA Natural
suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (Suc m) n = Suc (suma m n)

-- /////////////////////////////////////////////////////////
--Funcion Aux que apartir de un Nat elimina en  una lista
--la cantidad de sucesores de ese natural a los elementos de la lista
presipLis :: Nat -> Lista a -> Lista a
presipLis Cero a  = a
presipLis n Nula = Nula
presipLis (Suc Cero) (Cons (a)f)= (Cons a f)
presipLis (Suc m) (Cons(a)f) = presipLis(m) f


--Funcion Aux que concatena cadenas en el orden
-- [1x, 1y, 2x, 2y......nX,nY] donde X Y son las listas a concatenar
concatena :: Lista a -> Lista a -> Lista a
concatena (Cons (m)Nula) (Cons (n)f)= Cons m (Cons n Nula)
concatena (Cons (m)g)(Cons (n)Nula)=Cons(m)(Cons n Nula)
concatena Nula a = Nula
concatena b Nula = Nula
concatena (Cons (m)g)(Cons (n)f) =Cons m (Cons n (concatena g f))
