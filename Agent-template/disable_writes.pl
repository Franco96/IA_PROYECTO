% Uncomment the last line of this file to disable writes.


:- dynamic writes_disabled/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	disable_writes/0
%
%	Call this predicate to disable writes.
%

disable_writes:-
	redefine_system_predicate(write(_)),
	assert(write(_)),
	redefine_system_predicate(writeln(_)),
	assert(writeln(_)),
	redefine_system_predicate(nl),
	assert(nl),
	assert(writes_disabled).

%:- disable_writes.










