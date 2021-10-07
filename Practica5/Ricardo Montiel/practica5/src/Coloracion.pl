% Practica: Practica 05

% Alumno:   Escamilla Soto Cristopher Alejandro
% Numero de cuenta: 314309253
% Correo: cristopher@ciencias.unam.mx

% Alumno: Montiel Manriquez Ricardo
% Número de cuenta: 314332662
% Correo: moma92@ciencias.unam.mx

color(negro).
color(azul).
color(rojo).
color(verde).

adjacent(State1Color, State2Color) :-
    color(State1Color), color(State2Color),
    State1Color \= State2Color.