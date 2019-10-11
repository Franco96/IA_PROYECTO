:- encoding('iso_latin_1').
:- module(strips,
	  [
	    strips/2
	  ]).

:- [module_actions_rep_and_projection].

:- dynamic last_action/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Esta implementación corresponde a la estrategia de planificación STRIPS básica,
% es decir, sin los refinamientos "proteger y reordenar" ni "realcanzar".
% Luego resulta fundamental, al especificar las acciones (en
% module_actions_rep_and_projection), ordenar sensiblemente las
% precondiciones para evitar la necesidad de reordenamientos en la
% planifiación.
%
% Aclaración: las metas alcanzadas se protegen con el propósito de
% provocar la falla del planificador ante un intento de deshacerlas
% (lo que conduciría a un plan inválido en caso de no controlarse).


last_action(none).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% strips(+Metas, -Plan)
%
%
% Predicado implementando el algoritmo de planificación STRIPS.
% Versión exportada.


strips(Metas, Plan):-
	dynamic_state_rels(EInicial), % Obtener la lista Init de creencias relevantes a la planificación.
	strips(Metas, EInicial, Plan).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% strips(+Metas, +Estado_Inicial, -Plan)
%
% Predicado implementando el algoritmo de planificación STRIPS.
% Versión oculta, en la que se basa la versión exportada.
% Recibe como argumento el estado inicial, codificado como una lista
% de relaciones dinámicas primitivas válidas en el estado.

strips(Metas, EInicial, Plan):-
              alcanzar_todas(Metas, EInicial, [], [], Plan, _),
	      !. % Para forzar una única solución

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% alcanzar_todas(+Metas, +Estado_Actual, +Protegidas, -Plan, EstadoLuegoPlan)
%
%

alcanzar_todas([], EstadoActual, _, _ActionsHeap, [], EstadoActual). % Si no hay metas que alcanzar, entonces el plan es []

alcanzar_todas(Metas, EActual, Prot, ActionsHeap, Plan, ELuegoPlan):-
           select(Meta, Metas, RestoMetas),
	   !, % Para evita reordenamiento de metas.
           alcanzar(Meta, EActual, Prot, ActionsHeap, PlanMeta, EstadoLuegoPlanMeta),
           alcanzar_todas(RestoMetas, EstadoLuegoPlanMeta, [Meta | Prot], ActionsHeap, RestoPlan, ELuegoPlan),
           append(PlanMeta, RestoPlan, Plan).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% alcanzar(+Meta, +EActual, +Protegidas, +ActionsHeap, -PlanMeta, -NuevoEActual)
%
% ActionsHeap permite llevar cuenta de la pila de acciones seleccionadas
% previamente y cuyas precondiciones están pendientes de ser logradas.
% Se emplea para controlar que la acción seleccionada en este paso no
% pertenezca a la pila, en cuyo caso se estaría entrando en un
% ciclo.

alcanzar(Meta, EActual, _Prot, _ActionsHeap, [], EActual):-
               holds(Meta, EActual),
	       !.



alcanzar(Meta, EActual, Prot, ActionsHeap, PlanMeta, ELuegoPlanMeta):-
               alcanza(Accion, Meta),
	       Accion = [NomAcc, Pre, _Add, _Del],
	       not(member(NomAcc, ActionsHeap)),
               not((member(MetaProt, Prot), deshace(Accion, MetaProt))),
	       not(ciclo(NomAcc)), % Control de ciclo específico para este dominio.
	       retract(last_action(_)), assert(last_action(NomAcc)),
               alcanzar_todas(Pre, EActual, Prot, [NomAcc|ActionsHeap], PlanPre, ELuegoPlanPre),
               ejecutar(Accion, ELuegoPlanPre, ELuegoPlanMeta),
               append(PlanPre, [NomAcc], PlanMeta).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ciclo(+Relation)
%
%

ciclo(drop(Obj)):-
	last_action(pickup(Obj)).

%ciclo(pickup(Obj)):-
%	 last_action(drop(Obj)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% alcanza(-Accion, +Meta)
%
% Retorna una acción que alcanza la meta.

alcanza(Accion, Meta):-
                action_descr(Accion),
		Accion = [NomAcc,_,Add_List, _],
		strips_considers(NomAcc),
                member(Meta, Add_List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% strips_considers(+Action)
%
% Establece qué acciones (de las descriptas en
% module_actions_rep_and_projection.pl) son consideradas por el
% planificador STRIPS.
%
% Notar que STRIPS no planifica al nivel de acciones de moviemiento
% primitivas (move_fwd y turn/2), sino que asume la existencia de una
% acción de "alto nivel" goto(PosDest), donde PosDest es una posición
% destino. Ver especificación de goto/1 en
% module_actions_rep_and_projection.pl.


strips_considers(goto(_)).

strips_considers(pickup(_)).

strips_considers(drop(_)).

strips_considers(cast_spell(_)).

strips_considers(noop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% deshace(+Accion, +MetaProt)
%
%

deshace(Accion, Meta):-
                Accion = [_, _, _, Del_list],
                member(Meta, Del_list).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ejecutar(+Accion, +EActual, -NuevoEActual)
%
%

ejecutar(Accion, EActual, NuevoEActual):-
                 Accion = [_NomAcc, _Pre, Add_list, Del_list],
                 % No tengo que verificar que EActual contenga Pre, ya que
                 % en esta instancia del algoritmo esto siempre va a ser así.
                 union(EActual, Add_list, Aux),
                 subtract(Aux, Del_list, NuevoEActual).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS (actualmente son inválidos)
%
%
%test(1, Plan):-
%          Metas= [has([agent, me], [treasure, t1])],
%          EInicial= [at([treasure, t1], [5,10]), at([agent, me], [7,6]), dir([agent, me], n)],
%          strips(Metas, EInicial, Plan).
%
%test(2, Plan):-
%          Metas= [at([treasure, t1], [8,8])],
%          EInicial= [at([treasure, t1], [5,10]), at([agent, me], [7,6]), dir([agent, me], n)],
%          strips(Metas, EInicial, Plan).
%
%
%test(3, Plan):-
%          Metas= [has([agent, me], [treasure, t1])],
%          EInicial= [has([grave, g], [treasure, t1]), at([grave, g], [20, 20]), at([agent, me], [7,6]), closed([grave, g]), has([agent, me], [opening_potion, op])],
%          strips(Metas, EInicial, Plan).
%
%
%test(4, Plan):-
%          Metas= [at([treasure, t1], [2,8])],
%          EInicial= [has([dragon, d], [treasure, t1]), at([dragon, d], [5,10]), at([agent, me], [7,6]), at([sleep_potion, sp], [7,6])],
%          strips(Metas, EInicial, Plan).
%
%
%test(5, Plan):-
%          Metas= [has([agent, me], [treasure, t1])],
%          EInicial= [has([grave, g], [treasure, t1]), at([grave, g], [20, 20]), at([agent, me], [7,6]), closed([grave, g]), has([dragon, d], [opening_potion, op]), at([dragon, d], [5,10]), at([sleep_potion, sp], [7,6])],
%          strips(Metas, EInicial, Plan).





























