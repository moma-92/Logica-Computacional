{-
- Lógica computacional 2020-2
- Practica 2
- Alumno: Escamilla Soto Cristopher Alejandro
- Número de cuenta: 314309253
-Correo: cristopher@ciencias.unam.mx
- Alumno: Montiel Manriquez Ricardo
- Número de cuenta: 314332662
-Correo: moma92@ciencias.unam.mx
-}

module Practica2 where

-- ---------------------------------------------------------------------
-- Definimos los siguientes tipos de datos:
-- Prop para representar las fórmulas proposicionales usando los
-- constructores T, F, Var, Neg, Conj, Disy, Impl y Equi para las fórmulas
-- atómicas, negaciones, conjunciones, implicaciones y equivalencias,
-- respectivamente.
-- ---------------------------------------------------------------------

data Prop = T | F | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Equi Prop Prop deriving Eq

type Estado = [String]

instance Show Prop where
         show T = "Verdadero"
         show F = "Falso"
         show (Var p) = p
         show (Neg p) = "¬" ++ show p
         show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
         show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
         show (Impl p q) = "(" ++ show p ++ " ⟶  " ++ show q ++ ")"
         show (Equi p q) = "(" ++ show p ++ " ⟷  " ++ show q ++ ")"


-- ---------------------------------------------------------------------
-- Definimos las siguientes fórmulas proposicionales
-- como variables atómicas: p, q, r, s, t, u.
-- ---------------------------------------------------------------------
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


-- ---------------------------------------------------------------------
-- Símbolos proposicionales de una fórmula --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
-- variables :: Prop -> [String]
-- tal que (variables f) es el conjunto formado por todos los
-- símbolos proposicionales que aparecen en f. Por ejemplo,
-- >variables (Impl (Conj (Var "p") (Var "q")) (Var "p"))
-- ["p","q"]
-- >variables (Conj (Var "q") (Disy (Var "r") (Var "p")) 
-- ["q","r","p"]
-- ---------------------------------------------------------------------

variables :: Prop -> [String]
variables x =  repetidos (variablesRec x)
    where
      variablesRec T = []
      variablesRec F = []
      variablesRec (Var p) = [p]
      variablesRec (Neg p) = variablesRec p  
      variablesRec (Disy p q) = (variablesRec p ++ variablesRec q)
      variablesRec (Conj p q) = (variablesRec p ++ variablesRec q)
      variablesRec (Impl p q) = (variablesRec p ++ variablesRec q)
      variablesRec (Equi p q) = (variablesRec p ++ variablesRec q) 
       
-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
-- conjPotencia :: [a] -> [[a]]
-- tal que (conjPotencia x) es la lista de todos los subconjuntos de x.
-- Por ejmplo,
-- >conjPotencia [1,2]
-- [[]; [2]; [1]; [1; 2]]
-- >conjPotencia []
-- [[]]
-- >conjPotencia "abc"
-- ["abc","ab","ac","a","bc","b","c",""]
-- ---------------------------------------------------------------------

conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs)= (conjPotencia xs) ++ [x:t | t <- conjPotencia xs] 

-- ---------------------------------------------------------------------
-- Interpretaciones --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la 	
-- interpretacion :: Prop -> Estado -> Bool
-- tal que (interpretacion f e) es la interpretación de f en e. Por ejemplo,
-- interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p"]
-- False 
-- >interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p","q"]
-- True
-- ---------------------------------------------------------------------

interpretacion :: Prop -> Estado -> Bool
interpretacion  T _ = True
interpretacion  F _ = False
interpretacion (Var p) e = elem p e
interpretacion (Neg p) e = not(interpretacion p e)
interpretacion (Disy p q) e = (interpretacion p e) || (interpretacion q e)	
interpretacion (Conj p q) e = (interpretacion p e) && (interpretacion q e)	
interpretacion (Impl p q) e = not(interpretacion p e) || (interpretacion q e)	
interpretacion (Equi p q) e = (interpretacion p e) == (interpretacion q e)	


-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir una función que dada una fórmula proposicional,
-- la función devuelve todos los estados con los que podemos evaluar
-- la fórmula. Por ejemplo,
-- >estadosPosibles Disy (Var "q") (Conj (Var "r") (Var "q"))
-- [[],["q"]; ["r"]; ["q","r"]]
-- ---------------------------------------------------------------------

estadosPosibles :: Prop -> [Estado]
estadosPosibles p = conjPotencia $ variables p

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir una función que dada una fórmula proposicional,
-- nos diga si es una tautología. Por ejemplo,
-- >tautologia Disy (Var "p") (Neg (Var "p"))
-- True
-- >tautologia Disy (Var "q") (Var "r")
-- False
-- ---------------------------------------------------------------------
tautologia :: Prop -> Bool
tautologia p = elementosIguales(estadosPosibles p) (modelos p)

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir una función que dada una fórmula proposicional,
-- nos diga si es una contradicción. Por ejemplo,
-- >contradiccion Disy (Var "p") (Neg (Var "p"))
-- False
-- >contradiccion Disy (Var "q") (Var "r")
-- True
-- ---------------------------------------------------------------------

contradiccion :: Prop -> Bool
contradiccion p | tautologia p == True = False
	            | otherwise = True

-- ---------------------------------------------------------------------
-- Modelos --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir una función que dada una interpretación y una 
-- fórmula proposicional, verifique si esta interpretación es un modelo.
-- Por ejemplo,
-- >esModelo ["r"] (Conj (Disy p q) (Disy (Neg q) r))
-- False
-- >esModelo ["p","r"] (Conj (Disy p q) (Disy (Neg q) r)) 
-- True
-- ---------------------------------------------------------------------

esModelo :: Estado -> Prop -> Bool
esModelo p q = interpretacion  q p

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir una función que dada una fórmula proposicional
-- devuelve la lista de todos sus modelos; tal que (modelos f) es la 
-- lista de todas las interpretaciones de f que son modelo. Por ejemplo,
-- >modelos (Conj (Disy p q) (Disy (Neg q) r))
-- [["p","q","r"],["p","r"],["p"],["q","r"]]
-- ---------------------------------------------------------------------

modelos :: Prop -> [Estado]
modelos p = [x | x <- estadosPosibles p, interpretacion p x]           	 

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir una función que dada una fórmula proposicional f
-- verifica si f es válida. Por ejemplo,
-- esValida (Impl p p)
-- True
-- esValida (Impl p q) 
-- False
-- esValida (Disy (Impl p q) (Impl q p))
-- True
-- ---------------------------------------------------------------------

esValida :: Prop -> Bool
esValida p = modelos p == estadosPosibles p 

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir una función que dada una fórmula proposicional f
-- verifica si f es insatisfacible. Por ejemplo,
-- esInsatisfacible (Conj p (Neg p)) 
-- True
-- esInsatisfacible (Conj (Impl p q) (Impl q r)) 
-- False
-- ---------------------------------------------------------------------

esInsatisfacible :: Prop -> Bool
esInsatisfacible p = modelos p == [] 


-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir una función que dada una fórmula proposicional f
-- verifica si f es satisfacible. Por ejemplo,
-- esSatisfacible (Conj p (Neg p)) 
-- False
-- esSatisfacible (Conj (Impl p q) (Impl q r)) 
-- True
-- ---------------------------------------------------------------------

esSatisfacible :: Prop -> Bool
esSatisfacible p | esInsatisfacible p = False
                 | otherwise = True
               
                                
-- ///////////// Funciones Auxiliares  /////////////////////////////////

--repetidos :: Eq a => [a] -> [a]  es una funcion que apartir de una lista
--nos regresa otra lista con los mismo eslementos pero sin repeticiones,
--utilizamos la definicion de cabeza y cola de una Lista
repetidos :: Eq a => [a] -> [a] 
repetidos [] = []
repetidos (x:xs) |elem x xs = (repetidos xs)
	             |otherwise = [x] ++ (repetidos xs)
				
-- elem Eq a => [a] -> Bool
-- regresa true si la lista contiene dicho elemento
-- regresa falso si el elemento no es esta en la lista

elementosIguales :: Eq a => [a] -> [a] -> Bool
elementosIguales [] b = True
elementosIguales(x:xs) b | elem x b && elementosIguales xs b = True
                         |otherwise = False
 

 

