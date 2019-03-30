:- use_module(server).
:- use_module(http).

:- op(700, xfx, in).

in(E, L) :- memberchk(E, L).


% routing
route(_, P, _, _, shutdown) :- P in ["/shutdown", "/kill"].
route(_, "/ping", _, _, respondBody(`pong\n`)).
route(_, "/echo", _, Body, respondBody(Body)).
route(M, "/konami", H, B, respondBody(RB)) :-
	M = 'PUT',
	'content-type'-"cheat/code" in H,
	B = `^^vv<><>ba`,
	RB = `achievement unlocked\n`
;	RB = `nothing to see here\n`.

route(_, P, _, _, respond404(P)).


% request handlers
shutdown(Response) :-
	respondBody(`goodbye\n`, Response),
	server:shutdown.

respond404(Path, response(404, ['content-type'-"text/plain"], Body)) :-
	string_codes(Path, PathCodes),
	format(codes(Body), 'The requested path was not found: ~s~n', [ PathCodes ]).

respondBody(Body, response(200, ['content-type'-"text/plain"], Body)).

% main app
app :- app(_).
app(Port) :- server:run(Port, handle_service).

handle_service(Stream) :-
	stream_to_lazy_list(Stream, List),
	phrase(http:request(Method, Path, ReqHeaders, ReqBody), List, _Rest),
	writeln([Method, Path, ReqHeaders, ReqBody]),

	Response = response(Status, ResHeaders, ResBody),
	route(Method, Path, ReqHeaders, ReqBody, Handler),
	call(Handler, Response),

	length(ResBody, CLen),
	number_string(CLen, CLenStr),
	http:update('content-length'-CLenStr, ResHeaders, Headers),
	phrase(http:response(Status, Headers, ResBody), Resp, []),

	format(Stream, '~s', [ Resp ]),
	flush_output(Stream).

