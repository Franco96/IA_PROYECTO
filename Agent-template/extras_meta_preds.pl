% Meta - Predicados auxiliares





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% if_fails_do(+Goal, +ExceptionHandler)
%

if_fails_do(Goal, _ExceptionHandler):- call(Goal), !.

if_fails_do(_Goal, ExceptionHandler):- call(ExceptionHandler), fail.


try_catch(Goal, _ExceptionHandler):- call(Goal), !.

try_catch(_Goal, ExceptionHandler):- call(ExceptionHandler), fail.



dont_fail(X):-call(X).
dont_fail(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% implies(+Ant, +Cons)
%
% Tiene éxito si no se satisface Ant, o se satisfacen tanto Ant como
% Cons.


implies(Ant, Cons):- call(Ant), !,
                     call(Cons).

implies(_Ant, _Cons).



throw_exception(CallBeforeBreak):-
	write('EXCEPTION: '),
	dont_fail(CallBeforeBreak),
	break.













