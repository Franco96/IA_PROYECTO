:- encoding('iso_latin_1').
%% Player-Agent BDI
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_actions_rep_and_projection, module_strips, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT		   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      ag_name(_AgName),

      %tell('log.txt'),

      update_beliefs(Percept),

      display_ag, nl,!,

      deliberate,  % FUE IMPLEMENTADO DE MANERA QUE SI POSTERIORMENTE FALLA LA OBTENCI�N DE UN PLAN PARA LA INTENCI�N
		   % EN PRINCIPIO SELECCIONADA, VUELVE A RECONSIDERAR INTENCI�N.

      planning_and_execution(Action),

      do_action(Action),

      run,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     2. DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% deliberate
%
% El agente analiza si continuar� con su intenci�n actual, considerando
% deseos de alta prioridad, la factibilidad del plan
% para la intencion actual, si la intenci�n actual fue lograda, etc.
% En caso de no continuar con la intenci�n corriente, establece cual
% ser� la nueva intenci�n analizando los deseos existentes y
% seleccionando uno de ellos.

deliberate:-

	once(high_priority(HPDesire, Explanation)),	 % Si existe un deseo HPDesire de alta prioridad:
						                         % (NO es un <<<CHOICE POINT>>>: una falla en la
						                         % siguiente l�nea no deber�a buscar alternativas).

	not(intention(HPDesire)),        % y no es el caso que coincide con la intenci�n actual,

	write('High-priority Desire: '), write(HPDesire), write(', since '), writeln(Explanation), nl,

	retractall(intention(_)),
	retractall(plan(_)),

	assert(intention(HPDesire)),     % se establece HPDesire como intenci�n actual.
	assert(plan([HPDesire])).

deliberate:-       % Si

	(   not(intention(_)),                     % actualmente no hay intenci�n
	    writeln('There is no intention '), nl
	;                                          % o
	    intention(Int),
	    achieved(Int),                         % la intenci�n corriente fue lograda
	    write('Intention '), write(Int), writeln(' achieved.')
	;					   % o

	    plan([]),                              % el plan para para la intenci�n actual fue consumido
	    writeln('Plan consumed.')
	;                                          % o
	    (

	        plan(Plan), Plan \= [], not(feasible(Plan))   % el plan corriente se torn� no factible, o

		;

	        not(plan(_))                                  % no hay plan. Esto ocurre si se descubre que el plan actual es no
	                                                      % factible al intentar obtener, sin �xito, el (sub) plan para la
	                                                      % siguiente acci�n de alto nivel (falla el next_primitive_action).
	    ),
	    writeln('Current plan became infeasible.'), nl
	),

	!,

	findall(Desire, desire(Desire, _Explanation), Desires),  % Calcular el conjunto de deseos
	write('Desires: '), writeln(Desires),nl,
	select_intention(NewInt, NewExpl, Desires),   % Seleccionar una intenci�n
	                                              % (deseo que el agente se compromete a lograr)
	                                              % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	write('New Intention: '), write(NewInt), write(', since '), writeln(NewExpl), nl,

	retractall(intention(_)),  % (Estrat�gicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	   % ante la b�squeda de una intenci�n alternativa (por no haberse encontrado un plan
	                           % para la anterior), la intenci�n anterior se elimina y se asserta la nueva.)

	assert(intention(NewInt)),                    % Recordar la nueva intenci�n seleccionada.
	assert(plan([NewInt])).


deliberate:-
	intention(Int),
	write('Current Intention: '), writeln(Int), nl.
	% Caso contrario, se contin�a con la intenci�n y plan corrientes


deliberate:-            % Si llega ac� significa que fall� el next_primitive_action al planificar la siguiente acci�n
	                % de alto nivel de un plan factible. Ejecuto nuevamente el delibarate.
	deliberate.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%  2.1. COMPUTING DESIRES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Metas / Deseos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% desire(-Desire, -Explanation)
%
% Retorna un deseo Desire (actualmente activo de acuerdo a las creencias
% del agente). Asociado al deseo se retorna una explicaci�n,
% especificando las razones por las cuales Desire se considera
% actualmente un deseo. En su forma m�s b�sica Explanation puede ser un
% string con una descripci�n narrada (breve) de dichas razones,
% que luego puede ser impresa por pantalla.



%_____________________________________________________________________
%
% Get treasure at position
%
% Si recuerdo que un tesoro dado se encuentra tirado en el piso, tener
% ese tesoro es una meta.

desire(get([relic, TrName]), 'quiero apoderarme de muchos tesoros!'):-
	at([relic, TrName], _PosTr).

%_____________________________________________________________________
%
% Get potion at position
%
% Si recuerdo que una pocion dado se encuentra tirado en el piso, tener
% esa pocion es una meta.

desire(get([potion, PName]), 'quiero apoderarme de muchas pociones!'):-
	at([potion, PName], _PosP).

%_____________________________________________________________________
%
% Abrir tumba
%
% si recuerdo que una tumba tiene reliquias ,abrir tumba es una meta.
%
desire(get([grave, PName]), 'quiero abrir una tumba!'):-
	   has([grave,PName],_),once(has([agent,me],[potion,_])).




%_____________________________________________________________________
%
% Deseo explorar un nodo no conocido
%
%si recuerdo un nodo que tiene al menos un adyacente que no conozco
%deceo explorar ese nodo.
%

desire(goto(NodoId), 'quiero explorar algun nodo desconocido '):-
                     property([agent, me], life, St),
	                                    St > 150,
                    node(NodoId,_,Ady),member([Desconocido,_],Ady),once(not(node(Desconocido,_,_))).


%_____________________________________________________________________
%
% descansar si no tengo otra cosa que hacer
%

desire(rest, 'necesito descansar').


%_____________________________________________________________________
%
% Move at Random
%

desire(move_at_random, 'quiero estar siempre en movimiento!'):-
                     property([agent, me], life, St),
	                                    St > 150.




%_____________________________________________________________________
%
% atacar a otro agente (PROXIMA ETAPA)
%
%
%
/*desire(attack([agent,Target]), 'quiero atacar a otro agente!'):-
   atPos([agent, me], MyPos),
	atPos([agent, Target], TPos),
	Target \= me,
	property([agent, Target], life, TLife),
	TLife > 0,
	pos_in_attack_range(MyPos, TPos).

*/






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% high_priority(-HPDesire, Explanation).
%
% Determina si existe un deseo HPDesire de alta prioridad, es
% decir, tal que implica abandonar la intenci�n actual. En caso de
% existir tal deseo de alta prioridad, lo retorna.
%
% An�logamente al predicado desire/2, asociado al deseo HPDesire de alta
% prioridad se retorna una explicaci�n, especificando las
% razones por las cuales HPDesire se considera un deseo que
% debe adoptarse inmediatamente como intenci�n.
% En su forma m�s b�sica Explanation puede ser un string con una
% descripci�n narrada (breve) de dichas razones, que luego puede ser
% impresa por pantalla.
:- dynamic high_priority/2.




high_priority(rest, 'necesito descansar'):- property([agent, me], life, St),
	                                    St < 50.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  2.2. SELECTING INTENTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% select_intention(-Intention, -Explanation, +CurrentDesires).
%
% Selecciona uno de los deseos corrientes del agente como intenci�n.
%
% En su forma m�s b�sica, define un orden de prioridad entre los deseos
% del agente, seleccionando como intenci�n al primero en este orden
% de prioridad.
%
% Tiene m�ltiples soluciones. Esto es adrede: si
% posteriormente a la selecci�n de una intenci�n (por parte de este
% predicado) no se encuentra plan para alcanzarla, entonces se obtendr�
% la siguiente intenci�n (mediante backtracking), hasta encontrar una
% viable, o agotarlas a todas.











%_____________________________________________________________________
%
% Conseguir un objeto que se halla tirado en el suelo
%
% De todos los posibles objetos tirados en el suelo o en las tumbas que
% el agente desea tener, selecciono como intenci�n obtener aquel que se
% encuentra m�s cerca.
%
select_intention(get(Obj), 'es el objeto m�s cercano de los que deseo obtener', Desires):-
                  property([agent, me], life, St),

                 findall(ObjPos, (member(get(Obj), Desires),
				 at(Obj, ObjPos)),
                  Metas), % Obtengo posiciones de todos los objetos meta tirados en el suelo y de las tumbas.

                once(buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos,CostoMeta)),
                write('Costo Meta es :'),writeln(CostoMeta),
                       (St - CostoMeta)>50,
		member(get(Obj), Desires),
                at(Obj, CloserObjPos).



%_____________________________________________________________________
%
% rest before commiting to any other desire

select_intention(rest, 'no tengo otra cosa m�s interesante que hacer', Desires):-
	member(rest, Desires),
        property([agent, me], life, St),
             St < 170.



%_____________________________________________________________________
%
% explorar de todos los nodos desconocidos el mas cercano al agente

select_intention(goto(NodoId), 'Nodo desconocido mas cercados de los que deseo explorar', Desires):-
                            findall(Nodos, (member(goto(Nodos), Desires)),
		           Metas), % Obtengo posiciones de todos los nodos desconocidos.
                            buscar_plan_desplazamiento(Metas, _Plan, CloserNodo,_),
				         NodoId=CloserNodo.




%_____________________________________________________________________
%
% Move at random

select_intention(move_at_random, 'no tengo otra cosa m�s interesante que hacer', Desires):-
	member(move_at_random, Desires).




%_____________________________________________________________________
%
% atacarrr (PROXIMA ETAPA)
%
%
/*select_intention(attack(Obj), 'Ataco a otro agente', Desires):-

	member(attack(Obj), Desires).

*/





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% achieved(+Intention).
%
% Determina si la intenci�n Intention fue alcanzada. Esto es, si
% se verifica de acuerdo a las creencias del agente.


achieved(rest):-
	property([agent, me], life, St),
	property([agent, me], lifeTotal, MaxSt),
	AlmostMaxSt is MaxSt - 10,
	St > AlmostMaxSt.

achieved(get(Obj)):-
	has([agent, me], Obj).

achieved(goto(Pos)):-
	at([agent, me], Pos).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      3. PLANNING         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       & EXECUTION        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planning_and_execution(-Action)
%
% Obtiene la siguiente acci�n primitiva Action correspondiente al plan
% actual, removi�ndola del plan. N�tese que la siguiente acci�n
% del plan podr�a ser de alto nivel (no primitiva), en cuyo caso debe
% planificarse hasta llegar al nivel de acciones primitivas.
% (Ver next_primitive_action/3).

planning_and_execution(Action):-

	retract(plan(Plan)),      % Ejecutar siguiente acci�n del plan.

	write('Following plan: '), writeln(Plan), nl,
	next_primitive_action(Plan, Action, RestOfPlan),
	write('Next action: '), writeln(Action),
	assert(plan(RestOfPlan)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planify(+HLAction, -Plan)
%
% Define una librer�a de Planes, es decir, un mapeo de acciones de alto
% nivel (en particular, intenciones) a planes involucrando acciones de
% menor nivel.
%
% Dada una acci�n HLAction de alto nivel, retorna un plan (involucrando
% acciones de menor nivel) cuya ejecuci�n equivalga al efecto de la
% acci�n HLAction.
%
% Debe definirse una regla de este predicado por cada acci�n de alto
% nivel considerada por el agente (inclu�da las intenciones, que
% constituyen las acciones de m�s alto nivel).
%
% La planificaci�n de HLAction puede realizarse,
% seg�n el tipo de la acci�n, de las siguientes maneras:
%
%   a) simplemente especificando en forma "est�tica" la secuencia
%      (lista) de acciones de menor nivel cuyo efecto equivale al de
%       HLAction.
%
%   b) empleando un algoritmo de b�squeda (por ejemplo el implementado
%      para la etapa 2, permitiendo planificar el desplazamiento a una
%      posici�n determinada)
%
%   c) empleando el algoritmo de planeamiento STRIPS (cuya
%      implementaci�n es provista por la c�tedra), adecuado para
%      realizar planificaciones de metas m�s complejas, como obtener un
%      tesoro que se encuentra en una tumba.
%
%
% La opci�n a admite la especificaci�n de planes recursivos, donde en
% particular, la �ltima acci�n del plan para HLAction es la propia
% HLAction. Esto permite, por ejemplo, especificar que la
% planificaci�n de HLAction es [Action, HLAction], donde Action es
% una acci�n que acerca al agente al cumplimiento de HLAction. Cuando
% esa siguiente acci�n sea ejecutada y consumida, el agente vuelve a
% considerar y planificar HLAction, obteniendo la siguiente acci�n, y
% as� siguiendo.
%
% Esta estrategia es ideal para intenciones/acciones que no pueden
% planificarse por completo en un dado instante, resultando apropiado
% establecer en cada turno cual es la siguiente acci�n para lograrlas
% (por ejemplo, perseguir a un agente para saquearlo).
%
% Este plan recursivo se corta cuando los efectos de HLAction se logran
% (achieved(HLAction)), o cuando falla la planificaci�n de HLAction,
% reflejando que ya no existe plan para la misma.
%
% IMPORTANTE: Este predicado entregar� soluciones alternativas (por
% demanda), correspondientes a planes alternativos para la meta
% considerada. Analizar seg�n la acci�n que se est� planificando,
% si es deseable brindar soluciones alternativas.



planify(get(Obj),Plan):-Obj = [grave,_],   %si el objeto es de tipo tumba el plan es diferente
            at(Obj,Pos),has([agent,me],[potion,P]),
            Plan = [goto(Pos),cast_spell(open(Obj,[potion,P]))],!.


planify(get(Obj), Plan):- % Planificaci�n para obtener de un objeto que yace en el suelo
	at(Obj, Pos),
	Plan = [goto(Pos), pickup(Obj)].




/*(PROXIMA ETAPA)
planify(attack(Obj),Plan):-
         Plan = [attack(Obj)].
*/

% -----------------------------------------------------------------------%


planify(goto(PosDest), Plan):- % Planificaci�n para desplazarse a un destino dado
	buscar_plan_desplazamiento([PosDest], Plan, _MetaLograda,_),
		!. % Evita la b�squeda de soluciones alternativas para un plan de desplazamiento.


planify(rest, Plan):- % Planificaci�n para descansar


          findall(PosH, (at([inn, _H], PosH)),
		Metas), % Obtengo todas las pos de las posadas.

         buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos,_),

                Plan = [goto(CloserObjPos), stay].


planify(stay, [noop , stay]).                     % Planificaci�n recursiva. En este caso permite repetir indefinidamente
                                                  % una acci�n (noop) hasta que la intenci�n de alto nivel corriente
                                                  % (rest) sea lograda (achieved/1). Esto se hizo as� dado que resulta
                                                  % m�s simple que predecir de antemano cuantos turnos exactamente debe
                                                  % permanecer el agente para recargarse por completo (n�tese que el agente
						  % podr�a incluso sufrir ataques mientras est� en la posada, siendo imposible
						  % planificar de antemano cuantos turnos debe permanecer en la posada para
						  % reponerse por completo)


planify(move_at_random, Plan):- % Planificaci�n para moverse aleatoriamente

	findall(Node, node(Node, _, _), PossibleDestPos),

	random_member(DestPos, PossibleDestPos), % Selecciona aleatoriamente una posici�n destino.
				                 % <<<CHOICE POINT>>> (Posibilidad de backtracking)
	Plan = [goto(DestPos)].





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_primitive_action(+Plan, -NextAction, -RemainingPlan)
%
% Selecciona y retorna la siguiente acci�n primitiva del plan de alto
% nivel, adem�s del devolver el plan restante.
%
% Planteo Recursivo:
%
% Sea Plan = [A_1, A_2, ..., A_n]
%
% CB: Si A_1 es primitiva, entonces NextAction = A_1 y RemainingPlan =
% [A_2, ..., A_n].
%
% CR: Si A_1 es de alto nivel, se hallar� mediante planify/2 una
% secuencia de acciones de menor nivel [A_1.1, A_1.2, ..., A_1.k], o
% (sub)plan, para A_1, dando lugar a una versi�n refinada de Plan:
%
%          PlanRef = [A_1.1, A_1.2, ..., A_1.k, A_2, ..., A_n]
%
% Luego se obtiene recursivamente la siguinte acci�n, pero esta vez para
% PlanRef.
%
% CR 2: Los efectos de A_1 se cumplen en el estado actual del mundo.
% Luego se obtiene recursivamente la siguinte acci�n, pero esta vez para
% [A_2, ..., A_n].
%
% Observaci�n: A modo de mantener registro de la descomposici�n de
% acciones de alto nivel en subplanes, la estructura empleada por este
% predicado para representar planes incolucra el uso de "marcadores".
% Concretamente, en CR, PranRef = [A_1.1, A_1.2, ..., A_1.k, [A_1], A_2,
% ..., A_n] donde [A_1] es un marcador indiciando que las acciones que
% se encuentran a la izquierda corresponden al sub-plan para lograr A_1.
% Luego, el prop�sito del predicado remove_executed_ancestors/2 empleado
% en CB y CR 2 es eliminar los marcadores de acciones de alto nivel
% cuyo sub-plan asociado fue ejecutado por completo.


% CR 2:

next_primitive_action([Action | RestOfPlan], NextAction, RemainingPlan):-
	% Este caso permite, por ejemplo, terminar exitosamente un programa para una HLAction
	% cuando �sta ya fue lograda (debe especificarse achieved/1 para HLAction).

	clause(achieved(Action), _), % Existe especificaci�n de cu�ndo Action se considera lograda.

	achieved(Action), % Action fue lograda.
	!,
	write('Action '), write(Action), write(' achieved.'),nl,
	remove_executed_ancestors(RestOfPlan, CleanRestOfPlan),
	next_primitive_action(CleanRestOfPlan, NextAction, RemainingPlan).


% CB:

next_primitive_action([Action | RemainingPlan], Action, CleanRemainingPlan):-
	primitive(Action),
	remove_executed_ancestors(RemainingPlan, CleanRemainingPlan),
	!.

% CR:

next_primitive_action([HLAction | RestOfPlan], Action, RemainingPlan):-


        if_fails_do(

	clause(planify(HLAction, _SubPlan), _), % Planificaci�n definida para HLAction.

		    throw_exception((
			  write(HLAction),
			  write(' is undefined. Declare it as primitive or planify it.')
			 ))
		   ),
        !,

	(

	     planify(HLAction, SubPlan)	 % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	     ;

	     (write('Planning for '), write(HLAction), write(' failed.'), nl, fail)
	                                 % Si definitivamente no encuentra plan para la intenci�n seleccionada,
	                                 % luego de buscar planes alternativos (backtracking sobre planify(HLAction, SubPlan)),
				         % selecciona otra intenci�n mediante backtracking en deliberate/0.

	),

	(   last_element(HLAction, SubPlan),
	    append(SubPlan, RestOfPlan, LowerLevelPlan) % Se evita introducir el marcador de super-accion
							% si la acci�n de alto nivel coincide con la �ltima del subplan.
	;
	    append(SubPlan, [[HLAction]|RestOfPlan], LowerLevelPlan)
	),

	%'High-level action ' HLAction ' expanded into '
	%write(HLAction), write(' -- expanded into -> '), write(SubPlan),nl,
	writeln('          -- expanded into -> '), nl,
	write(LowerLevelPlan), nl, nl,

	next_primitive_action(LowerLevelPlan, Action, RemainingPlan).

% Observaci�n: si en particular Subplan es [] (esto puede
% ocurrir cuando los efectos de HLAction ya valen en
% el estado actual del mundo) entonces ejecuta la siguiente acci�n de
% RestOfPlan.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove_executed_ancestors(+Plan, -CleanPlan)
%
%

remove_executed_ancestors([[_]| Rest], Clean):-
	!,
	remove_executed_ancestors(Rest, Clean).

remove_executed_ancestors(Clean, Clean).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% primitive(ActionName).
%
% Especifica las acciones primitivas del agente, es decir, aquellas que
% no pueden descomponerse en acciones m�s b�sicas.

primitive(move(_)).
primitive(pickup(_)).
primitive(drop(_)).
primitive(attack(_)).
primitive(cast_spell(_)).
primitive(buy(_,_)).
primitive(sell(_,_)).
primitive(noop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% feasible(+Plan).
%
% Determina si el plan jer�rquico Plan es factible de acuerdo a las
% creencias actuales del agente.

feasible(Plan):-
	dynamic_state_rels(Init),
	project(Plan, Init, _Finish).
	% El plan puede ejecutarse con �xito a partir del estado actual. Si alguna de las precondiciones de las
        % acciones del plan ya no se satisfacen (por ejemplo, el tesoro que voy a juntar ya no se encuentra m�s
        % en la posici�n que recordaba), entonces project/3 fallar�, reflejando que el plan no es factible.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registraci�n al juego, y recuerda su nombre.


start_ag:- AgName = agentBDI,
           agent_init(AgName),
           assert(ag_name(AgName)),
		   agent_reset,
           connect,
           run,
           disconnect.

s:- start_ag.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)
%
% Solicita la registraci�n al juego de una instancia, y recuerda su
% nombre, que ser� el de la versi�n original seguido del InstanceID
% entre par�ntesis.


start_ag_instance(InstanceID):-
                    AgClassName = agentBDI,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).













































































