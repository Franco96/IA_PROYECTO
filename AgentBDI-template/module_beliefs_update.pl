:- encoding('iso_latin_1').
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

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultadon por el resto del código del agente.
%

update_beliefs(Perc):-

	% El agente olvida todo lo que recordaba
	retractall(time(_)),
	retractall(at(_,_)),
	retractall(atPos(_,_)),
	retractall(has(_,_)),
	retractall(entity_descr(_,_)),
	retractall(node(_,_,_)),

	% y recuerda lo que percibió
	forall(member(Rel, Perc), assert(Rel)).

































