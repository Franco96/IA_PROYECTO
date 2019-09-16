

:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3

	  ]).

:- [module_beliefs_update,extras_for_agents].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
%


buscar_plan_desplazamiento(Metas,Plan,Destino):-at([agent, me], MyNode),node(MyNode,_Vec,_Adj),
                                                busqueda([nodo(MyNode,0,[])],[],Metas,[Destino|Camino]),
                                                acomodarPlan([Destino|Camino],[_X|Plan]).



acomodarPlan([],[]).
acomodarPlan([H|T],L):- acomodarPlan(T,R), append(R,[move(H)],L).



%        | Frontera |Visitados|Metas |Caminosolucion
busqueda([nodo(Id,_Costo,CaminoNodo)|_Frontera],_Vis,Metas,[Id|CaminoNodo]):-member(nodoM(Id, _Vector), Metas).


busqueda([Nodo|Frontera],Vis,Metas,Camino):-generarVecinos(Nodo,Metas,Vecinos),agregar(Frontera,Vis,Vecinos,FTemp,NuevoVis),
                                            ordenar_por_f(FTemp,NuevaF),
                                            busqueda(NuevaF,[Nodo|NuevoVis],Metas,Camino).





generarVecinos(nodo(Id,Costo,Camino),Metas,Vecinos):-node(Id,_,Adyacentes),

                                                   findall(nodo(IdAdy,C,[Id|Camino]),

                                                            (member([IdAdy,Cadyi],Adyacentes),
                                                             node(IdAdy,Vector,_Adya),
                                                             heuristicas(Vector,Metas,[H|_Hs]),
                                                             C is H + Cadyi + Costo),

                                                              Vecinos).



agregar(Frontera,Vis,[],Frontera,Vis).

%vecino no está en frontera ni visitado.

agregar(Frontera,Vis,[nodo(Id,Costo,Camino)|Vecinos],NuevaF,NuevoVis):-not(member(nodo(Id,Cost,_Cam),Frontera)),
                                                                      not(member(nodo(Id,Cost,_Cam),Vis)),
                                                   agregar([nodo(Id,Costo,Camino)|Frontera],Vis,Vecinos,NuevaF,NuevoVis).


%vecino ya está en frontera y es mejor que el nuevo encontrado.
agregar(Frontera,Vis,[nodo(Id,Costo,_Camino)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,_Cam),Frontera),
                                                                Costo>=Cost,
                                                          agregar(Frontera,Vis,Vecinos,NuevaF,NuevoVis).

%vecino ya fue visitado por un mejor camino.
agregar(Frontera,Vis,[nodo(Id,Costo,_Camino)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,_Cam),Vis),
                                                                Costo>=Cost,
                                                             agregar(Frontera,Vis,Vecinos,NuevaF,NuevoVis).


% vecino estaba en la frontera pero ahora tiene mejor costo. borrar
% viejo y agregar nuevo.
agregar(Frontera,Vis,[nodo(Id,Costo,Camino)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,Cam),Frontera),
                                                                Costo<Cost,
                                                                 delete_if_exists(nodo(Id,Cost,Cam),Frontera,Ftemp),
                                                               agregar([nodo(Id,Costo,Camino)|Ftemp],Vis,Vecinos,NuevaF,NuevoVis).


%vecino ya habia sido visitado pero ahora se encuentra nuevamente con
% costo menor.
agregar(Frontera,Vis,[nodo(Id,Costo,Camino)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,Cam),Vis),
                                                                Costo<Cost,
                                                                 delete_if_exists(nodo(Id,Cost,Cam),Vis,VisTemp),
                                                       agregar([nodo(Id,Costo,Camino)|Frontera],VisTemp,Vecinos,NuevaF,NuevoVis).



ordenar_por_f([],[]).
ordenar_por_f([M|CL],Lord) :-
           particion(M,CL,L1,L2),
           ordenar_por_f(L1,Lord1),
           ordenar_por_f(L2,Lord2),
           append(Lord1,[M|Lord2],Lord).


 particion(_X,[],[],[]).

 particion(nodo(Id2,Cos2,Cam2),[nodo(Id1,Cos1,Cam1)|L],[nodo(Id1,Cos1,Cam1)|L1],L2) :- Cos1<Cos2,
                                                                         particion(nodo(Id2,Cos2,Cam2),L,L1,L2).

 particion(nodo(Id2,Cos2,Cam2),[nodo(Id1,Cos1,Cam1)|L],L1,[nodo(Id1,Cos1,Cam1)|L2]) :-Cos2=<Cos1,
                                                                                       particion(nodo(Id2,Cos2,Cam2),L,L1,L2).


%menorf(nodo(_Id1,Cos1,_Cam1),nodo(_Id2,Cos2,_Cam2)):-Cos1<Cos2.
%menorigf(nodo(_Id1,Cos1,_Cam1),nodo(_Id2,Cos2,_Cam2)):-Cos1=<Cos2.


heuristicas(_Vec,[],[]).

heuristicas(Vec,[nodoM(_IdMeta,VecM)|Metas],Lista):-heuristicas(Vec,Metas,Lista2),
                                                    distance(Vec,VecM,Dist),
                                                    inserta(Dist,Lista2,Lista).


%Inserta Ordenado usa la heuristica

inserta(X,[],[X]).

inserta(X,[Y|L],[Y|XenL]) :-
          Y<X, inserta(X,L,XenL).

inserta(X,[Y|L],[X,Y|L]) :- X=<Y.





































