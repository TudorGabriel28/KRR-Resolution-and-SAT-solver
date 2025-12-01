:- module(resolution,
    [
        check_fol_consistency/1,
        check_propositional_consistency/1,
        load_logic_clauses/2
    ]).

:- use_module(library(lists)).

get_negated_literal(neg(X), X) :- !.
get_negated_literal(X, neg(X)).

check_fol_consistency(ListOfClauses) :-
    process_fol_resolution_step(ListOfClauses).

process_fol_resolution_step(ListOfClauses) :-
    member([], ListOfClauses), !,
    true.
process_fol_resolution_step(ListOfClauses) :-
    findall(NewDerivedClause, (
        member(FirstClause, ListOfClauses),
        member(SecondClause, ListOfClauses),
        try_resolve_two_clauses_fol(FirstClause, SecondClause, NewDerivedClause)
    ), Resolvents),
    list_to_set(Resolvents, NewResolvents),
    subtract(NewResolvents, ListOfClauses, TrulyNew),
    (
        TrulyNew = [], !,
        false
    ;
        append(TrulyNew, ListOfClauses, NextClauses),
        process_fol_resolution_step(NextClauses)
    ).

try_resolve_two_clauses_fol(FirstClause_in, SecondClause_in, NewDerivedClause) :-
    copy_term((FirstClause_in, SecondClause_in), (FirstClause, SecondClause)),
    select(Literal1, FirstClause, RestFirstClause),
    get_negated_literal(Literal1, NegatedLiteral1),
    select(Literal2, SecondClause, RestSecondClause),
    unify_with_occurs_check(Literal2, NegatedLiteral1),
    append(RestFirstClause, RestSecondClause, Combined),
    sort(Combined, NewDerivedClause).

check_propositional_consistency(ListOfClauses) :-
    maplist(sort, ListOfClauses, SortedClauses),
    list_to_set(SortedClauses, UniqueClauses),
    exclude(clause_is_always_true, UniqueClauses, NonTautClauses),
    process_prop_resolution_step(NonTautClauses).

process_prop_resolution_step(ListOfClauses) :-
    member([], ListOfClauses), !,
    true.
process_prop_resolution_step(ListOfClauses) :-
    findall(NewDerivedClause, (
        member(FirstClause, ListOfClauses),
        member(SecondClause, ListOfClauses),
        FirstClause @< SecondClause,
        try_resolve_two_clauses_prop(FirstClause, SecondClause, NewDerivedClause_Unclean),
        \+ clause_is_always_true(NewDerivedClause_Unclean),
        NewDerivedClause = NewDerivedClause_Unclean
    ), Resolvents),
    list_to_set(Resolvents, NewResolvents),
    remove_redundant_clauses(NewResolvents, ListOfClauses, TrulyNew),
    (
        TrulyNew = [], !,
        false
    ;
        append(TrulyNew, ListOfClauses, TempNextClauses),
        cleanup_clause_list(TempNextClauses, NextClauses),
        process_prop_resolution_step(NextClauses)
    ).

try_resolve_two_clauses_prop(FirstClause, SecondClause, NewDerivedClause) :-
    select(Literal, FirstClause, RestFirstClause),
    get_negated_literal(Literal, NegatedLiteral),
    member(NegatedLiteral, SecondClause),
    select(NegatedLiteral, SecondClause, RestSecondClause),
    append(RestFirstClause, RestSecondClause, Combined),
    sort(Combined, NewDerivedClause).

clause_is_always_true(Clause) :-
    member(Literal, Clause),
    get_negated_literal(Literal, NegatedLiteral),
    member(NegatedLiteral, Clause).

clause_contains_another(FirstClause, SecondClause) :-
    subset(FirstClause, SecondClause).

remove_redundant_clauses([], _, []).
remove_redundant_clauses([NewClause|RestNew], OldClauses, FilteredNew) :-
    check_if_clause_is_redundant(OldClauses, NewClause), !,
    remove_redundant_clauses(RestNew, OldClauses, FilteredNew).
remove_redundant_clauses([NewClause|RestNew], OldClauses, [NewClause|FilteredNew]) :-
    remove_redundant_clauses(RestNew, OldClauses, FilteredNew).

check_if_clause_is_redundant(ClauseSet, Clause) :-
    member(ExistingClause, ClauseSet),
    clause_contains_another(ExistingClause, Clause).

cleanup_clause_list(ListOfClauses, OutputClauses) :-
    sort(ListOfClauses, Sorted),
    recursive_cleanup_helper(Sorted, [], OutputClauses).

recursive_cleanup_helper([], Accumulator, Accumulator).
recursive_cleanup_helper([Clause|Rest], Accumulator, OutputClauses) :-
    check_if_clause_is_redundant(Accumulator, Clause), !,
    recursive_cleanup_helper(Rest, Accumulator, OutputClauses).
recursive_cleanup_helper([Clause|Rest], Accumulator, OutputClauses) :-
    exclude(clause_contains_another(Clause), Rest, PrunedRest),
    exclude(clause_contains_another(Clause), Accumulator, PrunedAccumulator),
    recursive_cleanup_helper(PrunedRest, [Clause|PrunedAccumulator], OutputClauses).

load_logic_clauses(FilePath, ListOfClauses) :-
    setup_call_cleanup(
        open(FilePath, read, Stream),
        read_all_terms_from_stream(Stream, ListOfClauses),
        close(Stream)
    ).

read_all_terms_from_stream(Stream, []) :-
    at_end_of_stream(Stream), !.
read_all_terms_from_stream(Stream, [Term|Rest]) :-
    read_term(Stream, Term, []),
    ( Term == end_of_file ->
        Rest = []
    ;
        Rest = [Term | More],
        read_all_terms_from_stream(Stream, More)
    ).
