:- consult('davis_putnam_solver.pl').

% Socket communication setup
:- use_module(library(socket)).

% Store the socket streams
:- dynamic input_stream/1.
:- dynamic output_stream/1.

% Initialize socket connection
init_socket(Port) :-
    tcp_socket(Socket),
    tcp_connect(Socket, localhost:Port),
    tcp_open_socket(Socket, InStream, OutStream),
    assertz(input_stream(InStream)),
    assertz(output_stream(OutStream)),
    % Start processing commands from Java
    process_commands(InStream, OutStream).

% Process commands in a loop
process_commands(InStream, OutStream) :-
    read(InStream, Command),
    process_command(InStream, OutStream, Command).

% Handle exit command
process_command(_, OutStream, exit) :- !,
    close(OutStream),
    halt.

process_command(_, OutStream, halt) :- !,
    close(OutStream),
    halt.

% Handle solve command: solve(test1, first_atom) or solve(test1, shortest_clause)
process_command(InStream, OutStream, solve(TestName, Strategy)) :-
    atom(TestName),
    atom(Strategy),
    !,
    atom_concat(TestName, '.txt', FileName),
    format(OutStream, 'Running Davis-Putnam on ~w with strategy: ~w~n', [FileName, Strategy]),
    flush_output(OutStream),
    catch(
        (
            solve_file_gui(FileName, Strategy),
            format(OutStream, 'Test ~w completed.~n', [TestName]),
            flush_output(OutStream)
        ),
        Error,
        (
            format(OutStream, 'ERROR: ~w~n', [Error]),
            flush_output(OutStream)
        )
    ),
    process_commands(InStream, OutStream).

% Handle unknown commands
process_command(InStream, OutStream, Unknown) :-
    format(OutStream, 'Unknown command: ~w~n', [Unknown]),
    flush_output(OutStream),
    process_commands(InStream, OutStream).

% GUI version of solve_file that sends output to GUI
solve_file_gui(File, Strategy) :-
    output_stream(OutStream),
    !,
    format(OutStream, 'Reading clauses from ~w...~n', [File]),
    flush_output(OutStream),
    read_file_to_clauses(File, Clauses),
    format(OutStream, 'Clauses loaded. Running Davis-Putnam algorithm...~n', []),
    flush_output(OutStream),
    ( dp(Clauses, Strategy, Assignment) ->
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

% Main initialization when started from Java
:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [PortAtom|_]
    ->  atom_number(PortAtom, Port),
        init_socket(Port)
    ;   format('No port specified in arguments~n', [])
    ).
