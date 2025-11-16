:- use_module(library(lists)).

% --- Main entry point ---

solve_file(File, Strategy) :-
    read_file_to_clauses(File, Clauses),
    ( dp(Clauses, Strategy, Assignment) ->
        format_solution(Assignment)
    ; write('NO.'), nl
    ).

% --- Core DP Algorithm (Brachman & Levesque) ---

dp([], _, []).

dp(Clauses, _, _) :-
    member([], Clauses), !, fail.

dp(Clauses, Strategy, [true(Atom)|Assignment]) :-
    select_atom(Strategy, Clauses, Atom),
    simplify(Clauses, Atom, SimplifiedClauses),
    dp(SimplifiedClauses, Strategy, Assignment).

dp(Clauses, Strategy, [false(Atom)|Assignment]) :-
    select_atom(Strategy, Clauses, Atom),
    get_complement(Atom, NotAtom),
    simplify(Clauses, NotAtom, SimplifiedClauses),
    dp(SimplifiedClauses, Strategy, Assignment).

% --- C•m (Simplify) Operation ---

simplify([], _, []).

simplify([Clause|Rest], Lit, NewClauses) :-
    member(Lit, Clause), !,
    simplify(Rest, Lit, NewClauses).

simplify([Clause|Rest], Lit, [NewClause|NewClauses]) :-
    get_complement(Lit, Comp),
    delete(Clause, Comp, NewClause), !,
    simplify(Rest, Lit, NewClauses).

simplify([Clause|Rest], Lit, [Clause|NewClauses]) :-
    simplify(Rest, Lit, NewClauses).

% --- Atom Selection Strategies ---

select_atom(first_atom, Clauses, Atom) :-
    Clauses = [[Lit|_]|_],
    get_atom(Lit, Atom).

select_atom(shortest_clause, Clauses, Atom) :-
    find_shortest_clause(Clauses, ShortestClause),
    member(Lit, ShortestClause),
    get_atom(Lit, Atom), !.

find_shortest_clause(Clauses, Shortest) :-
    predsort(compare_length, Clauses, [Shortest|_]).

compare_length(R, L1, L2) :-
    length(L1, Len1),
    length(L2, Len2),
    compare(R, Len1, Len2).

% --- File Parsing ---

read_file_to_clauses(File, Clauses) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    parse_lines(Lines, Clauses).


read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, Lines) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    ( Line == end_of_file ->
        Lines = []
    ; Line = "" ->
        read_lines(Stream, Lines)         % skip blank line
    ;
        Lines = [Line|Rest],
        read_lines(Stream, Rest)
    ).

parse_lines([], []).
parse_lines([Line|Rest], [Clause|Clauses]) :-
    split_string(Line, " ", "", Words),
    parse_words(Words, Clause),
    parse_lines(Rest, Clauses).

parse_words([], []).
parse_words([Word|Rest], [Lit|Lits]) :-
    ( string_concat("-", AtomName, Word) ->
        atom_string(Atom, AtomName),
        Lit = not(Atom)
    ;
        atom_string(Atom, Word),
        Lit = Atom
    ),
    parse_words(Rest, Lits).

% --- Output Formatting ---

format_solution(Assignment) :-
    write('YES.'), nl,
    write('{'),
    print_assignment(Assignment),
    write('}'), nl.

print_assignment([]).
print_assignment([true(Atom)]) :-
    format('~w/true', [Atom]).
print_assignment([false(Atom)]) :-
    format('~w/false', [Atom]).
print_assignment([true(Atom)|Rest]) :-
    format('~w/true; ', [Atom]),
    print_assignment(Rest).
print_assignment([false(Atom)|Rest]) :-
    format('~w/false; ', [Atom]),
    print_assignment(Rest).

% --- Utility Predicates ---

get_atom(not(P), P).
get_atom(P, P) :- \+ P = not(_).

get_complement(not(P), P).
get_complement(P, not(P)) :- \+ P = not(_).