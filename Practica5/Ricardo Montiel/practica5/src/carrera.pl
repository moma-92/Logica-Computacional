% Practica: Practica 05

% Alumno:   Escamilla Soto Cristopher Alejandro
% Numero de cuenta: 314309253
% Correo: cristopher@ciencias.unam.mx

% Alumno: Montiel Manriquez Ricardo
% Número de cuenta: 314332662
% Correo: moma92@ciencias.unam.mx

estaAdelante([Adelante |[Atras|_] ],Adelante,Atras).
estaAdelante([_|Resto],Adelante,Atras) :- estaAdelante(Resto,Adelante,Atras).

estaTras(Tabla,X,Y) :- estaAdelante(Tabla,Y,X).

%[Nombre del piloto,Color del auto,Marca del auto ,Pais del Conductor]%
respuesta(Tabla) :- Tabla = [[1,_,_,_,_],[2,_,_,_,_],[3,jorge,_,mazda,alemania],[4,_,_,trooper,_],[5,_,rojo,_,italia]],
           
				   member([_,_,rojo,_,italia],Tabla),
				   
				   
                   member([_,pablo,amarillo,monza,francia],Tabla),
				   member([_,mario,_,ferrari,_],Tabla),
				   member([_,_,blanco,_,argentina],Tabla),	                                                         
				   

				   member([_,_,negro,_,_],Tabla),	
				   member([_,_,azul,_,_],Tabla),	                   
                   member([_,_,rojo,_,italia],Tabla),
                   member([_,_,_,_,brasil],Tabla),
                   
                   member([_,raul,_,_,_],Tabla),
				   member([_,carlos,_,_,_],Tabla),	
				   
					nonmember([3,jorge,azul,mazda,alemania],Tabla),
                    nonmember([1,_,negro,_,_],Tabla),
                    nonmember([5,_,negro,_,_],Tabla),
					
				   estaAdelante(Tabla,[_,pablo,amarillo,monza,fracia],[_,_,blanco,_,argentina]),             	
				   estaAdelante(Tabla,[_,pablo,amarillo,monza,fracia],[_,mario,_,ferrari,_]),              
                   estaAdelante(Tabla,[_,_,_,trooper,_],[_,_,rojo,_,_]),              
					
                   estaAdelante(Tabla,[_,raul,_,_,_],[_,_,_,susuzi,_]),
				   estaAdelante(Tabla,[_,_,_,_,brasil],[_,carlos,_,_,_]).
                   
					

                   
                   
                   
                   
                  

resultados(Po,Nom,Color,Carro,Pais) :- respuesta(Tabla), member([Po,Nom,Color,Carro,Pais],Tabla).
