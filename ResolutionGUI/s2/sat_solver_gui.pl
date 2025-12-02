:- consult('sat_solver.pl').

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

process_command(InStream, OutStream, solve(TestName, Strategy)) :-
    atom(TestName),
    atom(Strategy),
    !,
    atom_concat(TestName, '.txt', FileName),
    format(OutStream, 'Running ~w with strategy: ~w~n', [FileName, Strategy]),
    flush_output(OutStream),
    solve_file_gui(FileName, Strategy),
    process_commands(InStream, OutStream).

process_command(InStream, OutStream, _) :-
    process_commands(InStream, OutStream).

solve_file_gui(File, Strategy) :-
    output_stream(OutStream),
    !,
    parse_text_file_to_clauses(File, Clauses),
    ( execute_sat_solver(Clauses, Strategy, Assignment) ->
        format(OutStream, 'Result: YES (SATISFIABLE)~n', []),
        format(OutStream, 'Assignment: {', []),
        print_assignment_to_stream(OutStream, Assignment),
        format(OutStream, '}~n', []),
        flush_output(OutStream)
    ; 
        format(OutStream, 'Result: NO (UNSATISFIABLE)~n', []),
        flush_output(OutStream)
    ).

print_assignment_to_stream(_, []).
print_assignment_to_stream(Stream, [true(Atom)]) :-
    format(Stream, '~w/true', [Atom]).
print_assignment_to_stream(Stream, [false(Atom)]) :-
    format(Stream, '~w/false', [Atom]).
print_assignment_to_stream(Stream, [true(Atom)|Rest]) :-
    format(Stream, '~w/true; ', [Atom]),
    print_assignment_to_stream(Stream, Rest).
print_assignment_to_stream(Stream, [false(Atom)|Rest]) :-
    format(Stream, '~w/false; ', [Atom]),
    print_assignment_to_stream(Stream, Rest).

:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [PortAtom|_]
    ->  atom_number(PortAtom, Port),
        init_socket(Port)
    ;   format('No port specified in arguments~n', [])
    ).
