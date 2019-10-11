:- encoding('iso_latin_1').
:- module(actions_rep_and_projection,
	  [
	    dynamic_state_rels/1,
	    project/3,
	    action_descr/1,
	    holds/2
	  ]).

:- [extras_for_agents].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% action(+ActionName, -Preconditions, -AddList, -DelList).
%
% Representación STRIPS de las acciones del agente.
% Preconditions, AddList y DelList son listas de relaciones.
%
% Además de las acciones primitivas, debe especificarse mínimamente la
% acción goto(PosDest).
%
% Aclaración: en las precondiciones de las acciones puede hacerse uso de
% los predicados =/2, \=/2 y not/1, además de las relaciones dinámicas
% primitivas consideradas en el predicado dyn_state_rel/1 y las
% estáticas especificadas en el predicado static_relation.
%


action(move(To), [at([agent, me], From), ady(From, To)], [at([agent, me], To)], [at([agent, me], From)]).


action(goto(PosDest), [at([agent, me], PosOrig)], [at([agent, me], PosDest)], [at([agent, me], PosOrig)]).


action(pickup(Obj), [at(Obj, PosX), at([agent, me], PosX)], [has([agent, me], Obj)], [at(Obj, PosX)]).


action(stay, [at([agent, me], Pos), at([inn, _Inn], Pos)], [], []). %'stay' should be 'stay_at_inn'.

% << TODO: DEFINIR OTRAS ACCIONES >>




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% action_descr(-ActionDescr).
%
% Retorna la descripción completa de una acción. Concretamente,
% ActionDescr es una lista
%
%          [ActionName, Preconditions, AddList, DelList],
%
% tal que vale action(ActionName, Preconditions, AddList, DelList).
%

action_descr([ActionName, Preconditions, AddList, DelList]):-
	action(ActionName, Preconditions, AddList, DelList).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dynamic_state_rels(-InitState).
%
% Extrae la lista InitState de todas las relaciones dinámicas primitivas
% validas de acuerdo a las creencias actuales del agente
%
% Para poder proyectar (o simular) la ejecución de una secuencia de acciones a partir del estado actual,
% sin destruir la representación interna que se tiene actualmente del mundo, resulta necesario
% replicar (parcialmente) dicha representación para poder realizar la simulación o proyección sobre la
% réplica, que se irá modificando de acuerdo a los efectos de las acciones proyectadas.
% Concretamente, para realizar una proyección, solo resulta necesario replicar las relaciones dinámicas primitivas.
%
% Luego, InitState es una réplica de las creencias dinámicas del agente,
% a partir de la cual se pueda proyectar la ejecución de una secuencia
% de acciones.

dynamic_state_rels(InitState):-
	findall(Rel, dyn_state_rel(Rel), InitState).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dyn_state_rel(-Relation)
%
% Retorna una instancia, válida actualmente, de una relación dinámica
% primitiva. Es empleado por el predicado dynamic_state_rels/1 para
% obtener todas las instancias de relaciones dinámicas primitivas que
% valen actualmente.
%
% <<<PUEDE AGREGAR OTRAS REGLAS SI DESEA CONSIDERAR CREENCIAS DINÁMICAS
% PRIMITIVAS ADICIONALES>>>


dyn_state_rel(at(ThingID, Pos)):-
	at(ThingID, Pos).

dyn_state_rel(has(ThingID, Thing1ID)):-
	has(ThingID, Thing1ID).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% static_relation(+Relation, +Arity)
%
% Establece que ciertas creencias del agente son constantes, es decir,
% que no cambian a lo largo del tiempo. Esto permite excluir estas
% relaciones de la representación del estado actual del mundo que se
% lleva al realizar una proyección, por ejemplo, en el contexto de
% una planificación.
% Luego, cuando se desee saber si una cierta relación estática se
% satisface en el estado actual alcanzado en una proyección, se
% consultará directamente a la base de conocimiento del agente.
%
% Tambien puede emplearse para permitir el uso de ciertos predicados
% predefinidos (como en =/2, \=2 o el is/2) o definidos por el usuario,
% (como is_a/2 y ady_at_cardinal/3). Estos últimos pueden pensarse como
% relaciones estáticas predefinidas.


static_relation(node, 3).
static_relation(last_seen_at, 2).

static_relation(=, 2).    % Predicados predefinidos
static_relation(\=, 2).
static_relation(not, 1).

static_relation(ady, 2). % Predicados definido en extras_for_agents.pl






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% holds(+Relation, +State)
%
% Establece si Relation vale en el estado denotado por State
% Dado que State solo lista relaciones dinámicas primitivas,
% si Relation es estática entonces se consultará directamente a la base
% de conocimiento del agete (regla 2), como fue explicado anteriormente.


holds(Relation, State):-
	member(Relation, State),
	!.

holds(Relation, _State):-
	functor(Relation, Func, Arity),
	static_relation(Func, Arity),   % relación derivada y constante
	call(Relation).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% holds_all(+Relations, +State)
%
% Versión de holds/2 para una lista de relaciones.


holds_all([], _State).

holds_all([Rel | Rels], State):-
	holds(Rel, State),
	holds_all(Rels, State).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% execute(+Action, +SBeforeAction, -SAfterAction)
%
% Se satisface cuando la acción Action puede ejecutarse con éxito a
% partir del estado actual (ie, se satisfacen sus precondiciones).
% SAfterAction representa el estado resultante de ejecutar la acción a
% partir del estado previo SBeforeAction.
%
% También se satisface cuando la acción no se encuentra especificada,
% retornando como estado resultante al estado original (es decir, se
% asume que nada cambia al ejecutar la acción).

execute(ActName, SBeforeAction, SAfterAction):-
	action_descr([ActName, Pre, Add, Del]),
	!,
	holds_all(Pre, SBeforeAction),
	union(SBeforeAction, Add, Aux),
	subtract(Aux, Del, SAfterAction).

execute(_HLAction, SBeforeAction, SBeforeAction).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% project(+Plan, +Init, -Finish)
%
% Se satisface cuando el plan Plan puede ejecutarse con
% éxito a partir del estado actual (ie, se satisfacen las precondiciones
% de cada acción del plan en el momento que les toca ejecutarse). Además
% retorna el estado (proyectado) resultado de ejecutar el plan.

project([], CurrState, CurrState).


project([Action | RestOfPlan], CurrState, StateAfterPlan):-
	execute(Action, CurrState, StateAfterAction),
	!,
	project(RestOfPlan, StateAfterAction, StateAfterPlan).

project(Action, _EActual, _Finish):-
	write('Plan no factible. Fallarán las PRE de '), writeln(Action),nl,
	fail.

















