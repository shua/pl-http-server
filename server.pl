:- module(server, [ run/2, running/0, shutdown/0 ]).
:- use_module(library(socket)).
:- dynamic running/0.
running.

server_opts(default).

% Address can be an integer(Port), or Hostname:Port
run(Address, Handler) :- run(Address, Handler, []).
run(Address, Handler, Options) :- assertz(running), setup_call_cleanup(
	(	tcp_socket(SocketId),
		tcp_bind(SocketId, Address),
		format('Listening on: ~w~n', Address),
		tcp_listen(SocketId, 5),
		tcp_open_socket(SocketId, Socket)),
	(	(memberchk(serial(true), Options); server_opts(serial(true)) ),
		dispatch(Socket, Handler)
	;	dispatch_parallel(Socket, Handler)),
	close(Socket)).

shutdown :-
	retractall(running),
	thread_send_message(server, shutdown, []).

dispatch(Socket, Handler) :-
	running,
	tcp_accept(Socket, SlaveId, Peer),
	process_client(SlaveId, Peer, Handler),
	dispatch(Socket, Handler)
;	writeln("Shutting down...").

dispatch_parallel(Socket, Handler) :- running, setup_call_cleanup(
	message_queue_create(ServerQ, [ alias(server) ]),
	(	thread_create(listen_connection(ServerQ, Socket), ListenId, []),
		dispatch_queue(ServerQ, Handler),
		writeln("Shutting down..."),
		thread_signal(ListenId, throw(shutdown)),
		thread_join(ListenId, exception(shutdown))),
	(	message_queue_destroy(ServerQ)
	;	writeln("Couldn't destroy server message queue"))).

listen_connection(Queue, Socket) :-
	tcp_accept(Socket, SlaveId, Peer),
	thread_send_message(Queue, connection(SlaveId, Peer)),
	listen_connection(Queue, Socket).

dispatch_queue(Queue, Handler) :-
	thread_get_message(Queue, Msg),
	(	Msg = shutdown,
		retractall(running)
	;	Msg = connection(SlaveId, Peer),
		thread_create(process_client(SlaveId, Peer, Handler), _, [ detached(true) ]),
		dispatch_queue(Queue, Handler)).

process_client(SocketId, _Peer, Handler) :-
	setup_call_cleanup(
		tcp_open_socket(SocketId, Socket),
		call(Handler, Socket),
		close(Socket)).


