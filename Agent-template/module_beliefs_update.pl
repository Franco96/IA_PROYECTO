:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2 .
:- [extras_for_agents].
:- encoding('iso_latin_1').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado din�mico (creencia)
% manipulado por la actualizaci�n de creencias, para que puedan ser
% consultadon por el resto del c�digo del agente.
%

update_beliefs(Perc):-



	% y recuerda lo que percibi�
   actualizarTerreno(Perc),
   forall(member(X, Perc), rev(X)).


%


%----------------REVISION--------------------------------%
%

%

%---time/1---------------------------------------------%
 rev(time(X)):-	retractall(time(_)),assert(time(X)).
%------------------------------------------------------%


%---node/3----------------------------------------------%
 rev(node(X,Y,Z)):- node(X,Y,Z).
 rev(node(X,Y,Z)):-assert(node(X,Y,Z)).

%----------------------------------------------------%



%----at/2-----------------------------------------------%
rev(at(X,Y)):-at(X,Y).
rev(at(X,Y1)):-at(X,Y2),Y1\=Y2,retractall(at(X,Y2)),assert(at(X,Y1)).%veo la misma entidad en otra pos
rev(at(X,Y)):-has(Entity, X),retractall(has(Entity,X)),assert(at(X,Y)).%si veo un objeto tirado que tenia una entidad borro el has
rev(at(X,Y)):-assert(at(X,Y)).
rev(atPos(X,Y)):- atPos(X,Y).
rev(atPos(X,Y1)):-atPos(X,Y2),Y1\=Y2,retractall(atPos(X,Y2)),assert(atPos(X,Y1)).
rev(atPos(X,Y)):-has(Entity, X),retractall(has(Entity,X)),assert(atPos(X,Y)).
rev(atPos(X,Y)):-assert(atPos(X,Y)).
%--------------------------------------------------------%







% ------has/2--------------------------------------------------------------%

rev(has(X,Y)):-has(X,Y).

rev(has([agent,X],[Obj,Y])):-at([Obj,Y],_),retractall(at([Obj,Y],_)),assert(has([agent,X],[Obj,Y])).

rev(has([agent,AgentName],[Obj,NameObj])):-has([Building,NameBuil],[Obj,NameObj]),
                    retractall(has([Building,NameBuil],[Obj,NameObj])),assert(has([agent,AgentName],[Obj,NameObj])).

rev(has(X,Y)):-assert(has(X,Y)).



%-----------------------------------------------------------------------------%




% ----entity_descr/2-----------------------------------------------%

 rev(entity_descr(X,Y1)):- retractall(entity_descr(X,_)),assert(entity_descr(X,Y1)).


% ------------------------------------------------------------------------%





% actualizarTerreno(Perc):- forall(member(node(X,_Y,_Z), Perc),
% chequearAt(X,Perc)).
% chequearAt(NodoId,Perc):-at(Entidad,NodoId),not(member(at(Entidad,NodoId),Perc)),
%
  %                         retractall(at(Entidad,NodoId)),retractall(atPos(Entidad,_Vec)).
%chequearAt(_,_).





actualizarTerreno(Perc):- findall(at(X,Nodo),(at(X,Nodo)),ListaDeAt),

                          forall(member(Ati,ListaDeAt),chequearAt(Ati,Perc)),

                          findall(has([Entidad1,Name1],[Entidad2,Name2]),(has([Entidad1,Name1],[Entidad2,Name2])),ListaDeHas),

                          forall(member(Hasi,ListaDeHas),chequearHas(Hasi,Perc)).



chequearAt(at(Entidad,Nodo),Perc):-member(node(Nodo,_Y,_Z),Perc),

                                    not(member(at(Entidad,Nodo),Perc)),

                                    retractall(at(Entidad,Nodo)),retractall(atPos(Entidad,_Vec)).
chequearAt(_,_).



chequearHas(has([Entidad1,Name1],[Entidad2,Name2]),Perc):-member(at([Entidad1,Name1],_),Perc),

                                                          not(member(has([Entidad1,Name1],[Entidad2,Name2]),Perc)),

                                                          retractall(has([Entidad1,Name1],[Entidad2,Name2])).

chequearHas(_,_).









