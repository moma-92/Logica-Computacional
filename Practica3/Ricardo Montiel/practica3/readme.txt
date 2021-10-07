Practica 03 

Creado por:
Escamilla Soto Cristopher Alejandro
Montiel Manriquez Ricardo
//////////////////////////////////////////////////////////////////////////////////////////
								DESCRIPCION DEL 
								
Para elaborar la primera parte:
elimEquiv :: Prop -> Prop 
Lo unico  que hacemos es pasar por todos los casos en los que puede
haber una equivalencia y lo reestructuramos por p -> q && q -> p para
eliminar el conectivo principal que es el <->

elimImp :: Prop -> Prop
Es excatamente el mismo procedimiento que la funcion anterior solo 
que ahora reestructuramos la implicacion, es decir p -> q  por 
¬p || q

meteNeg :: Prop -> Prop
Mete negacion es una funcion en donde se complica el caso en que 
tenemos la ¬p, donde p es una formula, por lo tanto tenemos que crear  una
funcion que modele cada caso para poder meter apropiadamente la negacion  
A dicha funcion  le llamaremos:

fnn es una funcion simple, pues solo manda a llamar a las funciones anteriores
por lo tanto el resultado es la forma normal negativa

dist :: Prop -> Prop
Para esta funcion solo tenemos que simular el comportamiento de la propiedad de
distributividad de una proposicion  es practicamente la funcion que 
nos distribure las disyunciones y nos separa clausulas

cnf p = dist $ fnn p 
Unicamente aplica dist a un conjunto de formulas pasadas en forma normal
negativa, como resultado tenemos la forma normal conjuntiva


unit :: Solucion -> Solucio
Para Unit tenemos dos casos,cuando esta vacio nuestro conjunto de literales es vacio
utilizamos una funcion auxiliar para saber si tenemos  conjunto de literal unitaria
y es la que usamos

elim :: Solucion -> Solucion
Lo conseguimos mediante listas por comprencion pues regresamos el mismo modelo que 
recibimos y nuesta la lista  de clausulas  tales que si la literal en m es literal de una
clausula entonces la elimina completamente 

red :: Solucion -> Solucion
Similar a elimina, pero nos vemos con la complicacion de que solo tenemos que eliminar la 
literal negada en cada clausula si es que esta contenida  por lo tanto usamos la funcion delete 
con la funcion complemento lo que elimina las literales complemento en todas las clausulas 

split :: Solucion -> [Solucion]
Seperacion es facil cuando lo que nos pasan en los modelos son en si todas las literales pues 
solo hacemos uso de funcion auxiliares para sacar las literales del conjunto de formulas luego 
verificiar que no haya literales complemento y finalmente si este conjunto es igual al modelo entonces
regresamos lo mismo que nos dan si es vacia unicamente tomamos algun elemento de la lista de literales
y si ya nos dan un modelo pero aun quedan literales libres usamos alguna de estas

Meramente la definicion de conflict
conflict :: Solucion -> Bool

Meramente la definicion de conflict
success :: Solucion -> Bool

/////////////////////////////////////////////////////////////////////////////////////////////////////////
                        FORMA DE EJECUCION
                        
- Compilamos 
- ejecutamos con ghci 
- Cada Funcion se invoca con su nombre identificador 
  y con los parametros especificados en el .hs 
  
  elimEquiv Prop   Nos Regresa Prop
  elimImp   Prop
  elimIE    Prop
  meteNeg   Prop
  meteNeg   Prop
  dist      Prop
  cnf       Prop
  
  unit        Solucion Nos Regresa Solucion
  elim        Solucion
  red         Solucion
  split       Solucion
  conflict    Solucion Nos Regresa Bool
  success     Solucion 
  
///////////////////////////////////////////////////////////////////////////////////////////////////////////  
				CONCLUSION
A forma de conclusion este programa es la implementacion del algoritmo DPLL y tambien 
es la implementacion de los algoritmos para llevar un conjunto de formulas a un conjunto 
de formulas en forma normal conjuntiva
