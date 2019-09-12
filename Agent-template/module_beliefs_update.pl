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
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultadon por el resto del código del agente.
%

update_beliefs(Perc):-


	% El agente olvida
	retractall(time(_)),
	
   



	% y recuerda lo que percibió

    forall(member(X, Perc), rev(X)),
   actualizarTerreno(Perc).


%
% para cada at(Entidad, ID) en la percepcion:
%		si no tenias este at en la base de creencias:
%			si tenias Entidad en otro at, borrar la vieja y agregar la nueva.
%			si no tenias Entidad en otro at, agregar.
%
%		si ya tenias este at(ENtidad, ID)
%			queda igual


%	recorrer at(Entidad, Nodo) de tu base de creencias.
%	si tenes at(Entidad, Nodo) y en la percepcion viene el Nodo pero no viene el at:
%		borrar el at.
%	si tenes at(Entidad, Nodo) y en la percepcion no viene el nodo:
%		lo dejo tal cual está
%
%
%
%
%
%
%
%


%----------------REVISION--------------------------------%
%

%

%---time/1---------------------------------------------%
 rev(time(X)):-assert(time(X)).
%------------------------------------------------------%


%---node/3----------------------------------------------%
 rev(node(X,Y,Z)):- node(X,Y,Z).
 rev(node(X,Y,Z)):-assert(node(X,Y,Z)).

%----------------------------------------------------%



%----at/2-----------------------------------------------%
rev(at(X,Y)):-at(X,Y).
rev(at(X,Y1)):-at(X,Y2),Y1\=Y2,retractall(at(X,Y2)),assert(at(X,Y1)).%veo la misma entidad en otra pos
rev(at(X,Y)):-retractall(has(_Entity,X)),assert(at(X,Y)).%si veo un objeto tirado que tenia una entidad borro el has
rev(at(X,Y)):-assert(at(X,Y)).
rev(atPos(X,Y)):- atPos(X,Y).
rev(atPos(X,Y1)):-atPos(X,Y2),Y1\=Y2,retractall(atPos(X,Y2)),assert(atPos(X,Y1)).
rev(atPos(X,Y)):-retractall(has(_Entity,X)),assert(atPos(X,Y)).
rev(atPos(X,Y)):-assert(atPos(X,Y)).
%--------------------------------------------------------%







% ------has/2--------------------------------------------------------------%

 rev(has(X,Y)):-has(X,Y).

 rev(has([agent,X],[Obj,Y])):-
     retractall(at([Obj,Y],_N)),assert(has([agent,X],[Obj,Y])).

rev(has([agent,AgentName],[Obj,NameObj])):-retractall(has([_Building,_NameBuil],[Obj,NameObj])),
                                            assert(has([agent,AgentName],[Obj,NameObj])).

rev(has(X,Y)):-assert(has(X,Y)).



%-----------------------------------------------------------------------------%




% ----entity_descr/2-----------------------------------------------%

 rev(entity_descr(X,Y)):- entity_descr(X,Y).

 rev(entity_descr(X,Y1)):-entity_descr(X,Y2),Y1\=Y2,
                           retractall(entity_descr(X,Y2)),assert(entity_descr(X,Y1)).


 rev(entity_descr(X,Y)):-assert(entity_descr(X,Y)).

% ------------------------------------------------------------------------%





actualizarTerreno(Perc):- forall(member(X, Perc), chekearAt(X,Perc)).




chekearAt(node(X,Y,Z),Perc):-at(Entidad,X),not(member(at(Entidad,X),Perc)),                           
                           retractall(at(Entidad,X)),retractall(atPos(Entidad,_Vec)).
chekearAt(X,Y).










