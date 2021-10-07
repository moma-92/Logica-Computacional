{-
- Lógica computacional 2020-2
- Practica 3
- Alumno: Escamilla Soto Cristopher Alejandro
- Número de cuenta: 314309253
- Correo: cristopher@ciencias.unam.mx
- Alumno: Montiel Manriquez Ricardo
- Número de cuenta: 314332662
- Correo: moma92@ciencias.unam.mx
-}

module LProp where

-- | VarP. Tipo que representa el conjunto de variables proposicionales.
type VarP = String

-- | Prop. Tipo que representa el conjunto de fórmulas de la lógica
-- proposicional.
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop 

-- / se intancia la clase Show para poder especificar como queremos que se impriman la logica proposicional 
-- / TTrue en terminal se vera como "T", Neg (V "p") se vera como ~(p)
instance Show Prop where
    show TTrue = "T"
    show FFalse = "F"
    show (V x) = show x
    show (Neg p) = "~("++ show p ++")"
    show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
    show (Disy p q) = "(" ++ show p ++ " v " ++ show q ++")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++")"
    show (Equiv p q) = "(" ++  show p ++ " <-> " ++ show q ++")"

-- / se insatancia la clase Eq para poder comparar formulas proposicionales
-- TTrue == False = False
instance Eq Prop where
    (==) TTrue TTrue = True
    (==) FFalse FFalse = True
    (==) (V x) (V y) = x == y
    (==) (Neg p) (Neg q) = (==) p q
    (==) (Conj p q) (Conj r s) = (==) p r && (==) q s
    (==) (Disy p q) (Disy r s) = (==) p r && (==) q s
    (==) (Imp p q) (Imp r s) = (==) p r && (==) q s
    (==) (Equiv p q) (Equiv r s) = ((==) p r && (==) q s) || ((==) p s && (==) q r)
    (==) p q = False

    
-- / se instancia la clase Ord para poder dar orden a las formulas proposicionales 
-- ~p > p = False
instance Ord Prop where
    (<) p q = peso p < peso q
    (>) p q = peso p > peso q 
    (<=) p q = peso p <= peso q 
    (>=) p q = peso p >= peso q 
    min p q = if peso p <= peso q then p else q
    max p q = if peso p >= peso q then p else q

-- | peso. Función que dada una fórmula devuelve el número de sus conectivos.
--
-- --> peso (Conj (V 1) (Disy (V 2) (FFalse))) = 2
-- --> peso (Conj (V 1) (Disy (V 2) (Neg (V 3)))) = 3
peso :: Prop -> Int
peso (Neg p) = 1 + peso p
peso (Conj p q) = 1 + peso p + peso q
peso (Disy p q) = 1 + peso p + peso q
peso (Imp p q) = 1 + peso p + peso q
peso (Equiv p q) = 1 + peso p + peso q
peso _ = 0 

-- --> elimEquiv (Equiv (V p) (V q)) = (Conj (Imp (V p) (V q)) (Imp (V q) (V p)))
elimEquiv :: Prop -> Prop
elimEquiv (V p) = (V  p)
elimEquiv (TTrue) = (TTrue)
elimEquiv (FFalse) = (FFalse)
elimEquiv (Neg phi)    = Neg (elimEquiv phi) 
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q) 
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q) 
elimEquiv (Imp p q) = Imp (elimEquiv p) (elimEquiv q) 
elimEquiv (Equiv p q) = Conj (Imp (elimEquiv p) (elimEquiv q)) (Imp (elimEquiv q) (elimEquiv p))
         
-- | elimImp. Funció que dada una fórmula devuelve su equivalente que no contiene implicaciones.
--
-- --> elimEquiv (Imp (V p) (Disy (V q) (FFalse))) = Disy (Neg (V p)) (Disy (V q) FFalse)
elimImp :: Prop -> Prop
elimImp (V f)      = (V f)
elimImp (TTrue) = (TTrue)
elimImp (FFalse) = (FFalse)
elimImp (Neg p)    = Neg (elimImp p) 
elimImp (Conj p q) = Conj (elimImp p) (elimImp q) 
elimImp (Disy p q) = Disy (elimImp p) (elimImp q) 
elimImp (Imp p q) = Disy (Neg (elimImp p)) (elimImp q) 

-- | elimIE. Función que dada una fórmula devuelve su equivalente que no contiene implicaciones,
-- ni equivalencias.
--
elimIE :: Prop -> Prop
elimIE p = elimImp $ elimEquiv p

-- / funcion que recibe una formula de la logica proposicional y que devuelve otra formula de la logica proposicional que es logicamente
-- equivalente pero las negaciones que existen solo aplican a formulas atomicas. tambien elimina la doble negacion. la funcion supone
-- que la fomula ya no tiene implicaciones ni equivalencias.
meteNeg :: Prop -> Prop
meteNeg (Neg TTrue) = (FFalse)
meteNeg (Neg FFalse) = (TTrue)
meteNeg (TTrue) = (TTrue)
meteNeg (FFalse) = (FFalse)
meteNeg (V p)   = (V p)
meteNeg (Neg p)    = aux p
meteNeg (Conj p q) = Conj (meteNeg p) (meteNeg q) 
meteNeg (Disy p q) = Disy (meteNeg p) (meteNeg q) 

-- / Metodo aux, credado para el caso donde meteNeg recibe la negacion de p
-- pues como no sabemos si p es atomica con este metodo nos aseguramos de que lo sea
-- si no es asi la transformamos hasta que lo sea 
aux :: Prop -> Prop
aux (V p)   = Neg (V p)
aux (Neg p)    = meteNeg p  
aux (Conj p q) = Disy (aux p) (aux q) 
aux (Disy p q) = Conj (aux p) (aux q) 
 

-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal negativa,
fnn :: Prop -> Prop 
fnn p = meteNeg (elimIE p)

-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente pero que distribuye la disyuncion sobre la conjuncion (Ej. p v (q ^ r) = (p v q) ^ (p v r)). la funcion supone que la formula 
-- esta en forma normal negativa
dist :: Prop -> Prop
dist (Disy (Conj p q) r) = dist (Conj (Disy (dist p) (dist r))
                                      (Disy (dist q) (dist r)))
dist (Disy p (Conj q r)) = dist (Conj (Disy (dist p) (dist q))                                                                         
                                      (Disy (dist p) (dist r)))
dist (Conj p q) =Conj (dist p) (dist q)
dist  p = p


-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal conjuntiva,
cnf :: Prop -> Prop
cnf p = dist $ fnn p 
