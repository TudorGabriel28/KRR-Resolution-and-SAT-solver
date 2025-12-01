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
    format('Exiting...~n'),
    close(OutStream),
    halt.

process_command(_, OutStream, halt) :- !,
    format('Halting...~n'),
    close(OutStream),
    halt.

process_command(InStream, OutStream, TestName) :-
    atom(TestName),
    current_predicate(TestName/0),
    !,
    format(OutStream, 'Executing ~w...~n', [TestName]),
    flush_output(OutStream),
    call(TestName),
    process_commands(InStream, OutStream).

process_command(InStream, OutStream, Unknown) :-
    format(OutStream, 'Unknown command: ~w~n', [Unknown]),
    flush_output(OutStream),
    process_commands(InStream, OutStream).

send_to_gui(Message) :-
    output_stream(Stream),
    !,
    format(Stream, '~w~n', [Message]),
    flush_output(Stream).

send_to_gui(Message) :-
    format('~w~n', [Message]).

test_fmi :-
    send_to_gui('Starting test_fmi (FOL - Student/Course)...'),
    KB = [
        [neg(studentAt(X1, fmi)), smart(X1)],
        [neg(inCourse(X2, ai)), studentAt(X2, fmi)],
        [neg(smart(X3)), neg(studiesHard(X3)), passes(X3, krr)],
        [inCourse(ana, ai)],
        [studiesHard(ana)],
        [neg(passes(ana, krr))]
    ],
    send_to_gui('Knowledge Base loaded. Running resolution...'),
    (prove_fol(KB) -> 
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!') 
    ; 
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ),
    send_to_gui('test_fmi completed.').

test_blocks :-
    send_to_gui('Starting test_blocks (FOL - Blocks World)...'),
    KB = [
        [on(a,b)],
        [on(b,c)],
        [green(a)],
        [neg(green(c))],
        [neg(green(X)), green(Y), neg(on(X,Y))]
    ],
    send_to_gui('Knowledge Base loaded. Running resolution...'),
    (prove_fol(KB) -> 
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!') 
    ; 
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ),
    send_to_gui('test_blocks completed.').

test_plus :-
    send_to_gui('Starting test_plus (FOL - Arithmetic)...'),
    KB = [
        [plus(zero, X1, X1)],
        [neg(plus(X2, Y2, Z2)), plus(succ(X2), Y2, succ(Z2))],
        [neg(plus(succ(succ(zero)), succ(succ(succ(zero))), _U))]
    ],
    send_to_gui('Knowledge Base loaded. Running resolution...'),
    (prove_fol(KB) -> 
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!') 
    ; 
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ),
    send_to_gui('test_plus completed.').

test_john_mice :-
    send_to_gui('Starting test_john_mice (FOL - John & Mice)...'),
    KB = [
        [neg(hound(X1)), howl(X1)],
        [neg(have(X2,Y2)), neg(cat(Y2)), neg(have(X2,Z2)), neg(mouse(Z2))],
        [neg(ls(X3)), neg(have(X3,Y3)), neg(howl(Y3))],
        [have(john, sk1)],
        [cat(sk1), hound(sk1)],
        [ls(john)],
        [have(john, sk2)],
        [mouse(sk2)]
    ],
    send_to_gui('Knowledge Base loaded. Running resolution...'),
    (prove_fol(KB) -> 
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!') 
    ; 
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ),
    send_to_gui('test_john_mice completed.').

prove_prop_from_file(File) :-
    send_to_gui('Reading clauses from file...'),
    read_clauses_from_file(File, Clauses),
    format(atom(Msg), 'Loaded ~w clauses. Running resolution...', [Clauses]),
    send_to_gui(Msg),
    (prove_prop(Clauses) ->
        send_to_gui('Result: UNSATISFIABLE - Contradiction found!')
    ;
        send_to_gui('Result: SATISFIABLE - No contradiction')
    ).

test_prop1 :- 
    send_to_gui('Starting test_prop1 (Propositional Logic)...'),
    prove_prop_from_file('prop1.txt'),
    send_to_gui('test_prop1 completed.').

test_prop2 :- 
    send_to_gui('Starting test_prop2 (Propositional Logic)...'),
    prove_prop_from_file('prop2.txt'),
    send_to_gui('test_prop2 completed.').

test_prop3 :- 
    send_to_gui('Starting test_prop3 (Propositional Logic)...'),
    prove_prop_from_file('prop3.txt'),
    send_to_gui('test_prop3 completed.').

test_prop4 :- 
    send_to_gui('Starting test_prop4 (Propositional Logic)...'),
    prove_prop_from_file('prop4.txt'),
    send_to_gui('test_prop4 completed.').

:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    (   Argv = [PortAtom|_]
    ->  atom_number(PortAtom, Port),
        init_socket(Port)
    ;   format('No port specified in arguments~n', [])
    ).
