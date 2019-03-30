:- module(http, [request//4, response//3, update/3]).
:- use_module(library(dcg/basics)).
:- use_module(util).

method('GET').
method('POST').
method('PUT').
method('DELETE').

method('OPTION').
method('HEAD').


status(404, 'Not Found').
status(200, 'OK').



delete(_, [], []).
delete(Name, [Name-_|Headers0], Headers) :-
	delete(Name, Headers0, Headers).
delete(Name, [Name0-Val0|Headers0], [Name0-Val0|Headers]) :-
	Name \= Name0, !,
	delete(Name, Headers0, Headers). 

update(Name-Value, Headers0, Headers) :-
	atom(Name), string(Value), !,
	delete(Name, Headers0, Headers1),
	append(Headers1, [Name-Value], Headers).

guess_content_length(Headers, CLen) :-
	memberchk('content-length'-CLenStr, Headers),
	number_string(CLen, CLenStr).
guess_content_length(_, 0) :-
	writeln("No content-length header, assuming 0").


request(Method, Path, Headers, Body) -->
	request_pre(Method, Path, _Version, Headers),
	{ guess_content_length(Headers, CLen) },
	body(CLen, Body).

/* doing a recursive def on N, because I want to avoid the state where
there's no more buffered chars in the stream, so it waits on the client
to send more. */
body(N, Body) -->
	{ var(Body), ! }, body_parse(N, Body)
;	{ nonvar(Body), ! }, body_gen(N, Body).
body_parse(0, []) --> [].
body_parse(N, [B|Body]) -->
	{ integer(N), N > 0, M is N - 1 },
	([B] ; { format("Couldn't read body: ~d~n", N) }),
	{ ! },
	body_parse(M, Body).
body_gen(N, Body) --> { take(N, Body, BodyGen) }, string(BodyGen).

request_pre(Method, Path, Version, Headers) -->
	string(M), " ", string(P), " ", string(V), "\r\n",
	headers(Headers), "\r\n",
	{ atom_codes(Method, M), string_codes(Path, P), atom_codes(Version, V) }.

response(Status, Headers, Body) -->
	response_pre(_Version, Status), "\r\n",
	headers(Headers), "\r\n",
	{ guess_content_length(Headers, CLen) },
	body(CLen, Body).

response_pre('HTTP/1.1', Status) -->
	"HTTP/1.1 ", status(Status).
status(Status) -->
	{ var(Status), ! }, status_parse(Status)
;	{ nonvar(Status), ! }, status_gen(Status).
status_parse(status(Code, Msg)) -->
	integer(Code), " ", string_without("\r\n", Codes),
	{ string_codes(Msg, Codes) }.
status_gen(Status) -->
	{ Status = status(Code, Msg)
	; integer(Status), status(Status, Msg),  Code = Status
	; atom(Status),  status(Code, Status), atom_string(Status, Msg) },
	{ string_codes(Msg, Message) },
	integer(Code), " ", Message.

header(Name-Val) -->
	{ ( var(Name); var(Val) ), ! }, header_parse(Name-Val)
;	{ ( nonvar(Name), nonvar(Val) ), ! }, header_gen(Name-Val).
header_parse(Name-Val) -->
	nonblank(N), string(Ns), ":", whites, !, string_without("\r\n", V),
	{
		codes_lower([N|Ns], NameLower),
		atom_codes(Name, NameLower),
		string_codes(Val, V)
	}.
header_gen(Name-Val) -->
	{
		atom_codes(Name, NameCodes),
		string_codes(Val, ValCodes)
	},
	string(NameCodes), ": ", string(ValCodes).

headers([H|Hs]) -->
	header(H), "\r\n",
	headers(Hs).
headers([]) --> [].


