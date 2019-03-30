:- use_module(library(socket)).

get_page(Host, Resp) :-
	atom(Host),
	get_page(Host:80, Resp).

get_page(Host:Port, Resp) :-
	setup_call_cleanup(
		tcp_connect(Host:Port, Stream, []),
		(
			format(Stream,
				'GET / HTTP/1.1~n\c
				Host: ~s~n\c
				Connection: close~n~n',
				[ Host ]),
			flush_output(Stream),

			read_stream_to_codes(Stream, RespCodes),
			phrase(http_response(S, H, B), RespCodes, []),
			Resp = response(S, H, B)
		),
		close(Stream)).

