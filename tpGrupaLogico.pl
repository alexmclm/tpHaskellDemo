%1
%Predicado (Nombre,Partido)
candidato(frank,rojo).
candidato(claire,rojo).
candidato(garret,azul).
candidato(jackie,amarillo).
candidato(linda,azul).
candidato(catherine,rojo).
candidato(seth,amarillo).
candidato(heather,amarillo).


%Predicado (Nombre,Edad)
edad(frank,50).
edad(claure,52).
edad(garret,64).
edad(peter,26).
edad(jackie,38).
edad(linda,30).
edad(catherine,59).
edad(heather,51).

%Predicado (Provincia,Partido)
sePostulanEn(buenosAires,azul).
sePostulanEn(chaco,azul).
sePostulanEn(tierraDelFuego,azul).
sePostulanEn(sanLuis,azul).
sePostulanEn(neuquen,azul).
sePostulanEn(buenosAires,rojo).
sePostulanEn(santaFe,rojo).
sePostulanEn(cordoba,rojo).
sePostulanEn(chubut,rojo).
sePostulanEn(tierraDelFuego,rojo).
sePostulanEn(sanLuis,rojo).
sePostulanEn(chaco,amarillo).
sePostulanEn(formosa,amarillo).
sePostulanEn(tucuman,amarillo).
sePostulanEn(salta,amarillo).
sePostulanEn(santaCruz,amarillo).
sePostulanEn(laPampa,amarillo).
sePostulanEn(corrientes,amarillo).
sePostulanEn(misiones,amarillo).
sePostulanEn(buenosAires,amarillo).

%Predicado (Provincia, NumeroHabitantes)
habitantes(buenosAires,15355000).
habitantes(chaco,1143201).
habitantes(tierraDelFuego,160720).
habitantes(sanLuis,489255).
habitantes(neuquen,637913).
habitantes(santaFe,3397532).
habitantes(cordoba,3567654).
habitantes(chubut,577466).
habitantes(formosa,527895).
habitantes(tucuman,1687305).
habitantes(salta,1333365).
habitantes(santaCruz,273964).
habitantes(laPampa,349299).
habitantes(corrientes,992595).
habitantes(misiones,11894669).

/*hay ciertos datos que aclara en el enunciado, en el cual , debido a que prolog trabaja con datos certeros
todo lo que no pertenece a la base de conocimiento se lo considera como falso
*/

%2 
esPicante(Provincia):-
    sePresentaEnMismaProvincia(Provincia),
    habitantes(Provincia,Habitantes),
    Habitantes > 1000000.

sePresentaEnMismaProvincia(Provincia):-
    sePostulanEn(Provincia,Candidato1),
    sePostulanEn(Provincia,Candidato2),
    Candidato1 \= Candidato2.