:- consult('resolution.pl').

:- use_module(library(socket)).

:- dynamic input_stream/1.
:- dynamic output_stream/1.

init_socket(Port) :-
    tcp_socket(Socket),
    tcp_connect(Socket, localhost:Port),
    tcp_open_socket(Socket, InStream, OutStream),
    assertz(input_stream(InStream)),
    assertz(output_stream(OutStream)),
    process_commands(InStream, OutStream).

process_commands(InStream, OutStream) :-
    read(InStream, Command),
    process_command(InStream, OutStream, Command).

process_command(_, OutStream, exit) :- !,
    close(OutStream),
    halt.

process_command(_, OutStream, halt) :- !,
    close(OutStream),
    halt.

process_command(InStream, OutStream, TestName) :-
    atom(TestName),
    !,
    ( atom_concat('test_', BaseName, TestName) ->
        true
    ;
        BaseName = TestName
    ),
    atom_concat(BaseName, '.txt', FileName),
    format(OutStream, 'Starting ~w...~n', [TestName]),
    flush_output(OutStream),
    ( sub_atom(BaseName, 0, 4, _, 'prop') ->
        prove_prop_from_file(FileName)
    ;
        prove_fol_from_file(FileName)
    ),
    process_commands(InStream, OutStream).

process_command(InStream, OutStream, _) :-
    process_commands(InStream, OutStream).

send_to_gui(Message) :-
    output_stream(Stream),
    !,
    format(Stream, '~w~n', [Message]),
    flush_output(Stream).

send_to_gui(Message) :-
    format('~w~n', [Message]).

prove_fol_from_file(File) :-
    read_clauses_from_file(File, Clauses),
    ( prove_fol(Clauses) ->
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!')
    ;
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ).

prove_prop_from_file(File) :-
    read_clauses_from_file(File, Clauses),
    ( prove_prop(Clauses) ->
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!')
    ;
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ).

:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [PortAtom|_]
    ->  atom_number(PortAtom, Port),
        init_socket(Port)
    ;   true
    ).
