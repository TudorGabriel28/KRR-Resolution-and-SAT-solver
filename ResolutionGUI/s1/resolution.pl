:- module(resolution,
    [
        prove_fol/1,
        prove_prop/1,
        read_clauses_from_file/2
    ]).

:- use_module(library(lists)).

negate(neg(X), X) :- !.
negate(X, neg(X)).

prove_fol(Clauses) :-
    fol_loop(Clauses).

fol_loop(Clauses) :-
    member([], Clauses), !,
    true.
fol_loop(Clauses) :-
    findall(R, (
        member(C1, Clauses),
        member(C2, Clauses),
        resolve_pair_fol(C1, C2, R)
    ), Resolvents),
    list_to_set(Resolvents, NewResolvents),
    subtract(NewResolvents, Clauses, TrulyNew),
    (
        TrulyNew = [], !,
        false
    ;
        append(TrulyNew, Clauses, NextClauses),
        fol_loop(NextClauses)
    ).

resolve_pair_fol(C1_in, C2_in, Resolvent) :-
    copy_term((C1_in, C2_in), (C1, C2)),
    select(L1, C1, RestC1),
    negate(L1, NegL1),
    select(L2, C2, RestC2),
    unify_with_occurs_check(L2, NegL1),
    append(RestC1, RestC2, Combined),
    sort(Combined, Resolvent).

prove_prop(Clauses) :-
    maplist(sort, Clauses, SortedClauses),
    list_to_set(SortedClauses, UniqueClauses),
    exclude(is_tautology, UniqueClauses, NonTautClauses),
    prop_loop(NonTautClauses).

prop_loop(Clauses) :-
    member([], Clauses), !,
    true.
prop_loop(Clauses) :-
    findall(R, (
        member(C1, Clauses),
        member(C2, Clauses),
        C1 @< C2,
        resolve_pair_prop(C1, C2, R_Unclean),
        \+ is_tautology(R_Unclean),
        R = R_Unclean
    ), Resolvents),
    list_to_set(Resolvents, NewResolvents),
    filter_subsumed(NewResolvents, Clauses, TrulyNew),
    (
        TrulyNew = [], !,
        false
    ;
        append(TrulyNew, Clauses, TempNextClauses),
        filter_master_subsumption(TempNextClauses, NextClauses),
        prop_loop(NextClauses)
    ).

resolve_pair_prop(C1, C2, Resolvent) :-
    select(L, C1, RestC1),
    negate(L, NegL),
    member(NegL, C2),
    select(NegL, C2, RestC2),
    append(RestC1, RestC2, Combined),
    sort(Combined, Resolvent).

is_tautology(Clause) :-
    member(L, Clause),
    negate(L, NegL),
    member(NegL, Clause).

subsumes(C1, C2) :-
    subset(C1, C2).

filter_subsumed([], _, []).
filter_subsumed([NewC|RestNew], OldClauses, FilteredNew) :-
    is_subsumed_by(OldClauses, NewC), !,
    filter_subsumed(RestNew, OldClauses, FilteredNew).
filter_subsumed([NewC|RestNew], OldClauses, [NewC|FilteredNew]) :-
    filter_subsumed(RestNew, OldClauses, FilteredNew).

is_subsumed_by(ClauseSet, C) :-
    member(S, ClauseSet),
    subsumes(S, C).

filter_master_subsumption(Clauses, Out) :-
    sort(Clauses, Sorted),
    sub_filter_helper(Sorted, [], Out).

sub_filter_helper([], Acc, Acc).
sub_filter_helper([C|Rest], Acc, Out) :-
    is_subsumed_by(Acc, C), !,
    sub_filter_helper(Rest, Acc, Out).
sub_filter_helper([C|Rest], Acc, Out) :-
    exclude(subsumes(C), Rest, PrunedRest),
    exclude(subsumes(C), Acc, PrunedAcc),
    sub_filter_helper(PrunedRest, [C|PrunedAcc], Out).

read_clauses_from_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_terms(Stream, Clauses),
        close(Stream)
    ).

read_terms(Stream, []) :-
    at_end_of_stream(Stream), !.
read_terms(Stream, [T|Rest]) :-
    read_term(Stream, T, []),
    ( T == end_of_file ->
        Rest = []
    ;
        Rest = [T | More],
        read_terms(Stream, More)
    ).
