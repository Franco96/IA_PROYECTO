:- encoding('iso_latin_1').
%% Agent-Environment interaction primitives
% Agent-primitives for interaction with the environment


:- module(ag_primitives,
	  [
	    agent_init/1,
	    agent_reset/0,
	    connect/0,
	    disconnect/0,
	    get_percept/1,
	    do_action/1,
	    do_actionF/2
	  ]).



:- dynamic action/2, lastAction/1.

%--------------------------------------------------------------------%
connect :-
    IP = '127.0.0.1',
    Port = 8888,
    connect(IP, Port).

connect(IP, Port) :-
    tcp_socket(Socket),
    tcp_connect(Socket, IP:Port),
    tcp_open_socket(Socket, InStream, OutStream),
    retractall(connection(_, _, _)),
    assert(connection(Socket, InStream, OutStream)),
    !,
    writef('CONNECTION: Connected to %w on port %w.\n', [IP, Port]),
    name(Name),
    authenticate(Name, success),
    write('CONNECTION: Successfully authenticated. Ready to receive percepts.'), nl,
    !.
connect(IP, Port) :-
    writef('CONNECTION ERROR: Connection to %w:%w failure!\n', [IP, Port]),
    fail.

%--------------------------------------------------------------------%
authenticate(Name, Result) :-
    connection(_, InStream, OutStream),

    % Send agent name.
    swritef(Authentication, '<authentication><name>%w</name></authentication>', [Name]),
    write_term(OutStream, Authentication, []),
    nl(OutStream),
    flush_output(OutStream),

    % Receive result.
    read(InStream, Result),
    write('CONNECTION: authentication result: '), write(Result), nl.

%--------------------------------------------------------------------%
disconnect :-
    % Flush the percept message from the stream.
    recv_percept(_),

    connection(Socket, InStream, OutStream),

    % Send goodbye message.
    send_action(action(goodbye, []), success),

    tcp_close_socket(Socket),
    retractall(connection(Socket, InStream, OutStream)),
    write('CONNECTION: Disconnected.\n'),
    !.
disconnect :-
    write('CONNECTION: Disconnect failure!\n'),
    fail.

%--------------------------------------------------------------------%
recv_percept(Percept) :-
    % Write percept request to socket.
    connection(_, InStream, OutStream),
    write_term(OutStream, 'perc_req', []),
    nl(OutStream),
    flush_output(OutStream),
    read(InStream, Percept),
    Percept \= end_of_file,
    !.
recv_percept(_) :-
    write('CONNECTION: failure to recieve percept!'), nl,
    fail.

%--------------------------------------------------------------------%
%% Valid actions:
%%     action(ID, goodbye, [])
%%     action(ID, noop,    [])
%%     action(ID, move,    [Position])
%%     action(ID, attack,  [Agent])
%%     action(ID, pickup,  [Object])
%%     action(ID, drop,    [Object])
%%
%% where
%%     Position = [Integer, Integer]
%%     Object   = Integer
%%     Agent    = Atom
%%     Result   = success | failure
send_action(Action, Result) :-
    %Ver para que lo necesito
    retractall(lastAction(_)),
    assert(lastAction(Action)),
    action_id(ID),
    action_to_xml(Action, ID, XML),
    % Write action to socket.
    connection(_, InStream, OutStream),
    write_term(OutStream, XML, []),
    nl(OutStream),
    flush_output(OutStream),
    % Receive action result.
    read(InStream, Response),
    writeln(Response),
    action_result(Response, Result),!.

action_result(unknown, unknown).
action_result(success, success):-
	lastAction(LastAction),
	assert(LastAction).
action_result(failure, failure) :-
    write('CONNECTION: action failed!'), nl.
action_result(end_of_file, failure) :-
    write('CONNECTION ERROR: connection closed!'), nl,
    fail.

%--------------------------------------------------------------------%
action_to_xml(action(goodbye, []), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>goodbye</type></action>', [ID]),
    !.
action_to_xml(action(noop, []), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>noop</type></action>', [ID]),
    !.
action_to_xml(action(move, [Position]), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>move</type><position>%w</position></action>', [ID, Position]),
    !.
action_to_xml(action(attack, [[_Type, Agent]]), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>attack</type><agent><id>%w</id></agent></action>', [ID, Agent]),
    !.
action_to_xml(action(pickup, [[_Type, Object]]), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>pickup</type><object><id>%w</id></object></action>', [ID, Object]),
    !.
action_to_xml(action(drop, [[_Type, Object]]), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>drop</type><object><id>%w</id></object></action>', [ID, Object]),
    !.
action_to_xml(action(cast_spell, [Spell]), ID, XML) :-
    Spell =.. [SpellName, [_TType, TargetId], [potion, PotionId]],
    swritef(XML, '<action><id>%w</id><type>cast_spell</type><spell>%w</spell><target><id>%w</id></target><potion><id>%w</id></potion></action>', [ID, SpellName, TargetId, PotionId]),
    !.
action_to_xml(action(buy, [[inn, TargetId], [_ObjectType, ObjectId]]), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>buy</type><target><id>%w</id></target><object><id>%w</id></object></action>', [ID, TargetId,ObjectId]),
    !.    
action_to_xml(action(sell, [[inn, TargetId], [_ObjectType, ObjectId]]), ID, XML) :-
    swritef(XML, '<action><id>%w</id><type>sell</type><target><id>%w</id></target><object><id>%w</id></object></action>', [ID, TargetId,ObjectId]),
    !.        
action_to_xml(_, _, _) :-
    write('CONNECTION ERROR: Invalid action.'), nl,
    fail.

%--------------------------------------------------------------------%
action_id(ID) :-
    current_action_id(ID),
    NextID is ID + 1,
    retractall(current_action_id(_)),
    assert(current_action_id(NextID)).

%--------------------------------------------------------------------%
agent_reset :-
    retractall( current_action_id(_) ),
    assert(     current_action_id(0) ).


%--------------------------------------------------------------------%
%% This predicate must be implemented by the students
agent_init(Name) :-
    retractall( name(_)              ),
    assert(     name(Name)           ).



get_percept(Perc):- recv_percept(PercUnity),
	            transformAll(PercUnity, Perc).

transformAll([], []).
transformAll([P|Ps], Perc):-
	transform(P, PT),
	transformAll(Ps, PsT),
	append(PT, PsT, Perc).

do_action(Action):- do_actionF(Action, _).

do_actionF(Action, Result):- Action =.. [Name | Args],
			     send_action(action(Name, Args), Result).


entityId(Type, Name, [Type, me]):- name(Name),!.
entityId(Type, Name, [Type, Name]).


transform(node(Name, Vector, Connections), [node(Name, Vector, Connections)]).

transform(entity(Name, Type, Node, Position, Properties), Result):-
	entityId(Type, Name, EntityId),
	extractHas(EntityId, Properties, Has, RemainingProperties),
	Result = [at(EntityId, Node),
		  atPos(EntityId, Position),
		  entity_descr(EntityId, RemainingProperties)|Has].

/*
transform(selfProperties(Name, _ResultadoUltimaAccion, _Vida, _VidaTotal, Backpack, _InBuilding), Result):-
	entityId(agent, Name, EntityId),
	hasInBackpack(EntityId, Backpack, HasInBackpack),
	Result = HasInBackpack.
*/

transform(time(T), [time(T)]).


hasInBackpack(_AgId, [], []).

hasInBackpack(AgId, [E|Es], [has(AgId, EntityId), entity_descr(EntityId, Properties) | Hs]):-
	E = entity(Name, Type, _Node, _Position, Properties),
	entityId(Type, Name, EntityId),
	hasInBackpack(AgId, Es, Hs).


extractHas(EntityId, Properties, Has, RemainingProperties):-
	delete_if_exists([has, PropHas], Properties, RemainingProperties),!,
	hasInBackpack(EntityId, PropHas, Has).












