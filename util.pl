:- module(util, [codes_lower/2, take/3]).


codes_lower([N|Ns], [NL|NLs]) :-
	to_lower(N, NL),
	codes_lower(Ns, NLs).
codes_lower([], []).

take(0, _, _).
take(N, List0, Beg0) :-
	integer(N), N > 0,
	length(List0, LN), LN >= N,
	length(Beg0, N),

	List0 = [H|List], Beg0 = [H|Beg],
	M is N - 1,
	!,
	take(M, List, Beg).


