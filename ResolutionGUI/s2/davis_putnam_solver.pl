:- use_module(library(lists)).

solve_sat_from_file(FilePath, SelectionMethod) :-
    parse_text_file_to_clauses(FilePath, CurrentClauses),
    ( execute_davis_putnam(CurrentClauses, SelectionMethod, TruthValues) ->
        display_sat_result(TruthValues)
    ; write('NO.'), nl
    ).

execute_davis_putnam([], _, []).

execute_davis_putnam(CurrentClauses, _, _) :-
    member([], CurrentClauses), !, fail.

execute_davis_putnam(CurrentClauses, SelectionMethod, [true(VariableAtom)|TruthValues]) :-
    pick_next_variable(SelectionMethod, CurrentClauses, VariableAtom),
    simplify_clause_set(CurrentClauses, VariableAtom, SimplifiedClauses),
    execute_davis_putnam(SimplifiedClauses, SelectionMethod, TruthValues).

execute_davis_putnam(CurrentClauses, SelectionMethod, [false(VariableAtom)|TruthValues]) :-
    pick_next_variable(SelectionMethod, CurrentClauses, VariableAtom),
    find_opposite_literal(VariableAtom, NotAtom),
    simplify_clause_set(CurrentClauses, NotAtom, SimplifiedClauses),
    execute_davis_putnam(SimplifiedClauses, SelectionMethod, TruthValues).

simplify_clause_set([], _, []).

simplify_clause_set([Clause|Rest], Literal, NewClauses) :-
    member(Literal, Clause), !,
    simplify_clause_set(Rest, Literal, NewClauses).

simplify_clause_set([Clause|Rest], Literal, [NewClause|NewClauses]) :-
    find_opposite_literal(Literal, Complement),
    delete(Clause, Complement, NewClause), !,
    simplify_clause_set(Rest, Literal, NewClauses).

simplify_clause_set([Clause|Rest], Literal, [Clause|NewClauses]) :-
    simplify_clause_set(Rest, Literal, NewClauses).

pick_next_variable(most_frequent, CurrentClauses, BestAtom) :-
    findall(A, (member(C, CurrentClauses), member(L, C), extract_atom_from_literal(L, A)), AllAtoms),
    sort(AllAtoms, UniqueAtoms),
    associate_counts_with_atoms(UniqueAtoms, AllAtoms, Counts),
    sort(Counts, SortedCounts),
    last(SortedCounts, _-BestAtom).

pick_next_variable(shortest_clause, CurrentClauses, Atom) :-
    locate_smallest_clause(CurrentClauses, ShortestClause),
    member(Lit, ShortestClause),
    extract_atom_from_literal(Lit, Atom), !.

associate_counts_with_atoms([], _, []).
associate_counts_with_atoms([A|Rest], All, [Count-A|Counts]) :-
    count_times_atom_appears(A, All, Count),
    associate_counts_with_atoms(Rest, All, Counts).

count_times_atom_appears(_, [], 0).
count_times_atom_appears(X, [X|T], N) :-
    count_times_atom_appears(X, T, N1),
    N is N1 + 1.
count_times_atom_appears(X, [Y|T], N) :-
    X \= Y,
    count_times_atom_appears(X, T, N).

locate_smallest_clause(CurrentClauses, Shortest) :-
    predsort(compare_clause_lengths, CurrentClauses, [Shortest|_]).

compare_clause_lengths(R, L1, L2) :-
    length(L1, Len1),
    length(L2, Len2),
    compare(R, Len1, Len2).

extract_atom_from_literal(not(P), P).
extract_atom_from_literal(P, P) :- \+ P = not(_).

find_opposite_literal(not(P), P).
find_opposite_literal(P, not(P)) :- \+ P = not(_).

parse_text_file_to_clauses(FilePath, CurrentClauses) :-
    open(FilePath, read, Stream),
    read_all_lines(Stream, Lines),
    close(Stream),
    convert_lines_to_clauses(Lines, CurrentClauses).

read_all_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_all_lines(Stream, Lines) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    ( Line == end_of_file ->
        Lines = []
    ; Line = "" ->
        read_all_lines(Stream, Lines)
    ;
        Lines = [Line|Rest],
        read_all_lines(Stream, Rest)
    ).

convert_lines_to_clauses([], []).
convert_lines_to_clauses([Line|Rest], [Clause|Clauses]) :-
    split_string(Line, " ", "\s\t\n", Words),
    Words \= [],
    convert_words_to_literals(Words, Clause),
    convert_lines_to_clauses(Rest, Clauses).
convert_lines_to_clauses([_|Rest], Clauses) :-
    convert_lines_to_clauses(Rest, Clauses).

convert_words_to_literals([], []).
convert_words_to_literals([Word|Rest], [Lit|Lits]) :-
    ( string_concat("-", AtomName, Word) ->
        atom_string(Atom, AtomName),
        Lit = not(Atom)
    ;
        atom_string(Atom, Word),
        Lit = Atom
    ),
    convert_words_to_literals(Rest, Lits).

display_sat_result(TruthValues) :-
    write('YES.'), nl,
    write('{'),
    show_variable_values(TruthValues),
    write('}'), nl.

show_variable_values([]).
show_variable_values([true(Atom)]) :-
    format('~w/true', [Atom]).
show_variable_values([false(Atom)]) :-
    format('~w/false', [Atom]).
show_variable_values([true(Atom)|Rest]) :-
    format('~w/true; ', [Atom]),
    show_variable_values(Rest).
show_variable_values([false(Atom)|Rest]) :-
    format('~w/false; ', [Atom]),
    show_variable_values(Rest).
