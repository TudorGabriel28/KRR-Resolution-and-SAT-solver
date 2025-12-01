:- use_module(library(lists)).

solve_file(File, Strategy) :-
    read_file_to_clauses(File, Clauses),
    ( dp(Clauses, Strategy, Assignment) ->
        format_solution(Assignment)
    ; write('NO.'), nl
    ).

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

select_atom(most_frequent, Clauses, BestAtom) :-
    findall(A, (member(C, Clauses), member(L, C), get_atom(L, A)), AllAtoms),
    sort(AllAtoms, UniqueAtoms),
    map_counts(UniqueAtoms, AllAtoms, Counts),
    sort(Counts, SortedCounts),
    last(SortedCounts, _-BestAtom).

select_atom(shortest_clause, Clauses, Atom) :-
    find_shortest_clause(Clauses, ShortestClause),
    member(Lit, ShortestClause),
    get_atom(Lit, Atom), !.

map_counts([], _, []).
map_counts([A|Rest], All, [Count-A|Counts]) :-
    count_occurrences(A, All, Count),
    map_counts(Rest, All, Counts).

count_occurrences(_, [], 0).
count_occurrences(X, [X|T], N) :-
    count_occurrences(X, T, N1),
    N is N1 + 1.
count_occurrences(X, [Y|T], N) :-
    X \= Y,
    count_occurrences(X, T, N).

find_shortest_clause(Clauses, Shortest) :-
    predsort(compare_length, Clauses, [Shortest|_]).

compare_length(R, L1, L2) :-
    length(L1, Len1),
    length(L2, Len2),
    compare(R, Len1, Len2).

get_atom(not(P), P).
get_atom(P, P) :- \+ P = not(_).

get_complement(not(P), P).
get_complement(P, not(P)) :- \+ P = not(_).

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
        read_lines(Stream, Lines)
    ;
        Lines = [Line|Rest],
        read_lines(Stream, Rest)
    ).

parse_lines([], []).
parse_lines([Line|Rest], [Clause|Clauses]) :-
    split_string(Line, " ", "\s\t\n", Words),
    Words \= [],
    parse_words(Words, Clause),
    parse_lines(Rest, Clauses).
parse_lines([_|Rest], Clauses) :-
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