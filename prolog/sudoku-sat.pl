:- use_module(library(clpb)).
:- use_module(library(clpfd)).
:- dynamic(cell_bool/4).

% set every cell with false value on dynamic database
set_cell_var :-
    A = [1,2,3,4,5,6,7,8,9],
    member(X, A),
    member(Y, A),
    member(Z, A),
    assert(cell_bool(X, Y, Z, 0)),
    fail.

solve_cnf(CNF, Result) :-
    extract_variables(CNF, Vars),
    Assignment = [],
    generate_assignment(CNF, Vars, Assignment, Result).

not_form(not(X), X).
not_form(X, X).

yes_form(yes(X), X).
yes_form(X, X).

remove_not([], []).

remove_not([H|T], ResultList) :-
    not_form(H, X), !,
    remove_not(T, ResultNext),
    append([X], ResultNext, ResultList).

remove_yes([], []).

remove_yes([H|T], ResultList) :-
    yes_form(H, X), !,
    remove_yes(T, ResultNext),
    append([X], ResultNext, ResultList).

extract_variables(CNF, Vars) :-
    flatten(CNF, Flat),
    remove_not(Flat, RemovedNot),
    remove_yes(RemovedNot, RemovedYes),
    sort(RemovedYes, Vars).
    

generate_assignment(CNF, [], Assignment, Result) :-
    satisfies_cnf(CNF, Assignment),
    append([], Assignment, Result).
generate_assignment(CNF, [Var|Vars], Assignment, Result) :-
    append([Var], Assignment, Temp1),
    generate_assignment(CNF, Vars, Temp1, Result).

generate_assignment(CNF, [_|Vars], Assignment, Result) :-
    generate_assignment(CNF, Vars, Assignment, Result).

satisfies_cnf([], _).
satisfies_cnf([Clause|Tail], Assignment) :-
    satisfies_clause(Clause, Assignment), satisfies_cnf(Tail, Assignment).

satisfies_clause([], _) :- fail.
satisfies_clause([Literal|Tail], Assignment) :-
    (   check_satisfy(Literal, Assignment) ; 
    satisfies_clause(Tail, Assignment)).

satisfies_clause(Literal, Assignment) :-
    check_satisfy(Literal, Assignment).

check_satisfy(not(X), Assignment) :-
    !,
    \+ member(X, Assignment).
check_satisfy(yes(X), Assignment) :-
    !,
    member(X, Assignment).
check_satisfy(X, Assignment) :-
    member(X, Assignment).

