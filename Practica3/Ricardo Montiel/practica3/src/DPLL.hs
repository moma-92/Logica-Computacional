{-
- Estructuras discretas 2020-2
- Practica: Practica 03
- Alumno: 	Escamilla Soto Cristopher Alejandro
- Numero de cuenta: 314309253
- Correo: cristopher@ciencias.unam.mx
- Alumno: Montiel Manriquez Ricardo
- Número de cuenta: 314332662
- Correo: moma92@ciencias.unam.mx
-}
module DPLL where

import LProp
import Data.List

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- Seccion de funciones para la regla de la clausula unitaria

unit :: Solucion -> Solucion
unit ([], p)  
      |sacaUnitaria p == [] = ([],p)
	  |otherwise = (sacaUnitaria p,p)
	  
--  Seccion de funciones para la regla de eliminacion

elim :: Solucion -> Solucion
elim (m,f) = (m,[x | x <- f, notElem (head m) x ])
      
-- Seccion de funciones para la regla de reduccion

red :: Solucion -> Solucion
red ([],f) = ([],f)
red (m,f)  =  (m,[delete(complemento(head m)) x | x <-f, elem (complemento(head m)) x] ++ clausulas)	
	   where
	   clausulas = [x | x <- f, notElem (complemento(head m)) x]
-- Seccion de funciones para la regla de separacion

split :: Solucion -> [Solucion]
split ([],f) = [  ( [head(literales(f))] ,  f )  ,  ( [complemento(head(literales(f)))], f ) ]
split (m,f)  
     | igualdadConjuntos m  (literalesAtom (literales(f))) = [(m,f)]
     | otherwise = [ (meteElem m (  daLista m (literales(f))     ) , f),
                     (meteElemNeg m (  daLista m (literales(f))  ) , f)]
                                       
-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict (m,f) 
          | elem [] f = True
          |otherwise = False  
          
-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m ,[]) = True
success (m,f) = False        



-- ///////////////////// Funciones Auxiliares ///////////////////////////	  

-- Es Unitaria: es una funcion que verifica si una clausula
-- es unitaria o no 
esUnitaria :: Clausula -> Bool
esUnitaria [_] = True
esUnitaria _   = False

-- Elimina elemento de una lista 
-- no regresa una lista sin el elemento
eliminaElemList :: Literal -> [Literal] -> [Literal]
eliminaElemList a [] = []
eliminaElemList a (x:xs)
           | a == x = eliminaElemList a xs
           |otherwise = x:(eliminaElemList  a xs)
-- Genera el complemento de una literal y nos 
-- la regresa           
complemento :: Literal -> Literal
complemento (V p) = (Neg (V p)) 
complemento (Neg (V p))  = (V p)

-- Union listas es una funcion que una las listas 
-- que estan dentro de una lista  y nos regresa una sola lista
-- con todos los elementos
unionListas :: Eq a =>[[a]]->[a]
unionListas []   = []
unionListas (x:xs) = x `union` unionListas xs

-- Literales nos regresa las literales de una lista de clausulas 
-- cabe mencionar que hay literales negadas
literales :: [Clausula] -> [Literal]
literales m = unionListas m

-- Concatena es la tipica funcion para concatenar cadenas
concatena :: [Literal] -> [Literal] -> [Literal]
concatena (x:xs) ys = x : nub (concatena xs ys)
 
-- daLista  es una funcion que dadas dos listas, elimina los elementos 
-- de la primera en la segunda y nos regresa su resultado
daLista :: [Literal] -> [Literal] -> [Literal]
daLista x y = [l | l <- y,  notElem l x ]

-- Mete elemento, mete el primer elemento de una lista a otra lista
-- y nos regresa el resultado en otra lista
meteElem :: [Literal] -> [Literal] -> [Literal]
meteElem x (y:ys) = y:x

-- Hace lo mismo que mete elemento,pero en esta funcion metemos el 
-- elemento negado dentro de la lista
meteElemNeg :: [Literal] -> [Literal] -> [Literal]
meteElemNeg x (y:ys) = (Neg (y)):x

--Saca Unitaria: Es una funcion que saca la clausula unitaria si es que
--esta y si no hay ni una clausula unitaria regresa la lista vacia  
sacaUnitaria :: [Clausula] -> [Literal]
sacaUnitaria [] = []
sacaUnitaria (x:xs) 
     |esUnitaria x = x
      |otherwise = sacaUnitaria xs

-- Igualdad de Conjuntos  nos dice si un conjunto es igual a otro     
igualdadConjuntos :: Eq a => [a] -> [a] -> Bool
igualdadConjuntos xs ys = aux (nub xs) (nub ys)
                where 
                   aux []    []   = True
                   aux (x:_) []   = False
                   aux []    (y:_)= False
                   aux (x:xs) ys = x `elem` ys && aux xs (delete x ys)        

-- Literales Atomicas es una funcion que se encarga de hacer lo mismo que 
-- literales,sin embargo la diferencia es que dejamos fuera las literales 
-- negadas     
literalesAtom :: [Literal] -> [Literal]
literalesAtom l = [x | x<-l,peso x == 0] 




