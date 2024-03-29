

:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4


	  ]).

:- [module_beliefs_update,extras_for_agents].






%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
%


buscar_plan_desplazamiento(Metas,Plan,Destino,CostoReal):-at([agent, me], MyNode),node(MyNode,_Vec,_Adj),

                                          findall(nodoM(NodoMeta,Vec),(member(NodoMeta,Metas),node(NodoMeta,Vec,_)),MetasModif),

                                                busqueda([nodo(MyNode,0,[],0)],[],MetasModif,[Destino|Camino],CostoReal),
                                                acomodarPlan([Destino|Camino],[_X|Plan]).





acomodarPlan([],[]).
acomodarPlan([H|T],L):- acomodarPlan(T,R), append(R,[move(H)],L).



%     Busqueda(+Frontera,+Visitados,+Metas,-Caminosolucion,-CostoReal).
busqueda([nodo(Id,_Costo,CaminoNodo,CostoReal)|_Frontera],_Vis,Metas,[Id|CaminoNodo],CostoReal):-member(nodoM(Id, _Vector), Metas).


busqueda([Nodo|Frontera],Vis,Metas,Camino,CostoReal):-generarVecinos(Nodo,Metas,Vecinos),
                                                     agregar(Frontera,Vis,Vecinos,FTemp,NuevoVis),
                                            ordenar_por_f(FTemp,NuevaF),
                                            busqueda(NuevaF,[Nodo|NuevoVis],Metas,Camino,CostoReal).




%Costo = Heurica + costoReal y CostoReal = Costo solo
generarVecinos(nodo(Id,Costo,Camino,CostoReal),Metas,Vecinos):-node(Id,_,Adyacentes),

                                                   findall(nodo(IdAdy,C,[Id|Camino],CostRel),

                                                            (member([IdAdy,Cadyi],Adyacentes),
                                                             node(IdAdy,Vector,_Adya),
                                                             heuristicas(Vector,Metas,[H|_Hs]),
                                                             C is H + Cadyi + Costo,
                                                               CostRel is Cadyi+CostoReal),

                                                              Vecinos).



agregar(Frontera,Vis,[],Frontera,Vis).

%vecino no est� en frontera ni visitado.

agregar(Frontera,Vis,[nodo(Id,Costo,Camino,CostoReal)|Vecinos],NuevaF,NuevoVis):-not(member(nodo(Id,Cost,Cam,C),Frontera)),
                                                                      not(member(nodo(Id,Cost,Cam,C),Vis)),
                                                   agregar([nodo(Id,Costo,Camino,CostoReal)|Frontera],Vis,Vecinos,NuevaF,NuevoVis).


%vecino ya est� en frontera y es mejor que el nuevo encontrado.
agregar(Frontera,Vis,[nodo(Id,Costo,_Camino,_CostoReal)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,_Cam,_),Frontera),
                                                                Costo>=Cost,
                                                          agregar(Frontera,Vis,Vecinos,NuevaF,NuevoVis).

%vecino ya fue visitado por un mejor camino.
agregar(Frontera,Vis,[nodo(Id,Costo,_Camino,_CostoReal)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,_Cam,_),Vis),
                                                                Costo>=Cost,
                                                             agregar(Frontera,Vis,Vecinos,NuevaF,NuevoVis).


% vecino estaba en la frontera pero ahora tiene mejor costo. borrar
% viejo y agregar nuevo.
agregar(Frontera,Vis,[nodo(Id,Costo,Camino,CostoReal)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,Cam,C),Frontera),
                                                                Costo<Cost,
                                                                 delete_if_exists(nodo(Id,Cost,Cam,C),Frontera,Ftemp),
                                                    agregar([nodo(Id,Costo,Camino,CostoReal)|Ftemp],Vis,Vecinos,NuevaF,NuevoVis).


%vecino ya habia sido visitado pero ahora se encuentra nuevamente con
% costo menor.
agregar(Frontera,Vis,[nodo(Id,Costo,Camino,CostoReal)|Vecinos],NuevaF,NuevoVis):-member(nodo(Id,Cost,Cam,C),Vis),
                                                                Costo<Cost,
                                                                 delete_if_exists(nodo(Id,Cost,Cam,C),Vis,VisTemp),
                                                agregar([nodo(Id,Costo,Camino,CostoReal)|Frontera],VisTemp,Vecinos,NuevaF,NuevoVis).



ordenar_por_f([],[]).
ordenar_por_f([M|CL],Lord) :-
           particion(M,CL,L1,L2),
           ordenar_por_f(L1,Lord1),
           ordenar_por_f(L2,Lord2),
           append(Lord1,[M|Lord2],Lord).


 particion(_X,[],[],[]).

 particion(nodo(Id2,Cos2,Cam2,CostoReal2),[nodo(Id1,Cos1,Cam1,CostoReal1)|L],[nodo(Id1,Cos1,Cam1,CostoReal1)|L1],L2) :- Cos1<Cos2,
                                                                         particion(nodo(Id2,Cos2,Cam2,CostoReal2),L,L1,L2).

 particion(nodo(Id2,Cos2,Cam2,CostoReal2),[nodo(Id1,Cos1,Cam1,CostoReal1)|L],L1,[nodo(Id1,Cos1,Cam1,CostoReal1)|L2]) :-Cos2=<Cos1,
                                                                              particion(nodo(Id2,Cos2,Cam2,CostoReal2),L,L1,L2).



heuristicas(_Vec,[],[]).

heuristicas(Vec,[nodoM(_IdMeta,VecM)|Metas],Lista):-heuristicas(Vec,Metas,Lista2),
                                                    distance(Vec,VecM,Dist),
                                                    inserta(Dist,Lista2,Lista).


%Inserta Ordenado usa la heuristica

inserta(X,[],[X]).

inserta(X,[Y|L],[Y|XenL]) :-
          Y<X, inserta(X,L,XenL).

inserta(X,[Y|L],[X,Y|L]) :- X=<Y.





































