{-
- Logica Conmputacional 2020-2 
- Práctica 4, Implementación del algoritmo dpll.
- Creador: Pedro Juan Salvador Sánchez Pérez
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
unit s@(m,f) = error "Funcion a implementar"

--  Seccion de funciones para la regla de eliminacion

elim :: Solucion -> Solucion
elim s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de reduccion

red :: Solucion -> Solucion
red s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de separacion

split :: Solucion -> Solucion
split s@(m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict (m,f) = error "Funcion a implementar"

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m,f) = error "Funcion a implementar"

-- Seccion de las funciones principales de DPLL

dpllsearch :: Solucion -> Solucion
dpllsearch (m,f) = error "Funcion a implementar"

dpll :: Solucion -> Solucion
dpll (m,f) = error "Funcion a implementar"

main :: Solucion -> Solucion
main s = error "Funcion a implementar"

-- Ejemplos

bueno = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exe1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exe2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exe3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exe4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exe5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exe6 = [[V "p"], [Neg (V "p")]]        

ejemplo1 = main ([], exe1)
ejemplo2 = main ([], exe2)
ejemplo3 = main ([], exe3)
ejemplo4 = main ([], exe4)
ejemplo5 = main ([], exe5)   
ejemplo6 = main ([], bueno)   
ejemplo7 = main ([], exe6)
