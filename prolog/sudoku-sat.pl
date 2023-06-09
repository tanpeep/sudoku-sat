:- use_module(library(clpb)).
:- use_module(library(clpfd)).

append2(A1-Z1, Z1-Z2, A1-Z2).

solve_puzzle(Puzzle, Result) :-
    length(Puzzle, N),
    extract_clues_to_cnf(Puzzle, 1, CluesCNF ),
    numlist(1, N, LL),
    generate_least_row_col(LL, LL, LL, LeastCNF, RowCNF, ColumnCNF, Vars),
    generate_most(LL, LL, LL, MostCNF),
    generate_blocks(BlocksCNF),
    append(LeastCNF, RowCNF, Temp),
    append(Temp, ColumnCNF, Temp2),
    append(Temp2, MostCNF, Temp3),
    append(Temp3, BlocksCNF, CNF),
    solve_cnf(CNF, CluesCNF, Vars, Assignment),
    sort(Assignment, SortedAssignment),
    convert_cnf(SortedAssignment, Result).

generate_least_row_col(_, [], [], [], [], [], []).
generate_least_row_col(_, [], _, [], [], [], []).
generate_least_row_col(_, _, [], [], [], [], []).
generate_least_row_col(LL, [Num1|Tail1],[Num2|Tail2],Least, Row, Col, Vars) :-
    generate_least_clause(Num1, Num2, LL, Least1, Row1, Col1, Vars1),
    generate_least_row_col(LL, [Num1], Tail2, Least2, Row2, Col2, Vars2),
    generate_least_row_col(LL, Tail1, [Num2|Tail2], Least3, Row3, Col3, Vars3),
    append(Least2, Least3, TempLeast),
    append([Least1], TempLeast, Least),
    append(Row2, Row3, TempRow),
    append([Row1], TempRow, Row),
    append(Col2, Col3, TempCol),
    append([Col1], TempCol, Col),
    append(Vars2, Vars3, TempVars),
    append(TempVars, Vars1, Vars).

generate_least_clause(_, _, [], [], [], [], []).
generate_least_clause(Num1, Num2, [Num3|Tail], Least, Row, Col, Vars) :-
    generate_least_clause(Num1, Num2, Tail, Least1, Row1, Col1, Vars1),
    append([yes([Num1,Num2,Num3])], Least1, Least),
    append([yes([Num1,Num3,Num2])], Row1, Row),
    append([yes([Num3,Num1,Num2])], Col1, Col),
    append([[Num1,Num2,Num3]], Vars1, Vars).

generate_most(_, [], [], []).
generate_most(_, [], _, []).
generate_most(_, _, [], []).

generate_most(LL, [Num1|Tail1],[Num2|Tail2],Result) :-
    generate_most_clause(Num1, Num2, LL, Res1),
    generate_most(LL, [Num1], Tail2, Res2),
    generate_most(LL, Tail1, [Num2|Tail2], Res3),
    append(Res2, Res3, TempRes),
    append(Res1, TempRes, Result).

generate_most_clause(_, _, [], []).
generate_most_clause(_, _, [_], []).
generate_most_clause(Num1, Num2, [Num3|Tail], Result) :-
    generate_most_clause(Num1, Num2, Tail, Result1),
    generate_most_mini(Num1, Num2, Num3, Tail, Result2),
    append(Result2,Result1,Result).

generate_most_mini(_, _, _, [], []).
generate_most_mini(Num1, Num2, Num3, [Num4|Tail], Result) :-
    generate_most_mini(Num1, Num2, Num3, Tail, Result1),
    append([[not([Num1,Num2,Num3]), not([Num1, Num2, Num4])]], Result1, Result).

generate_blocks(Result) :-
    numlist(1,9,LL),
    numlist(1,3,L1),
    numlist(4,6,L2),
    numlist(7,9,L3),
    generate_block(LL, L1, L1, Part11),
    generate_block(LL, L1, L2, Part12),
    generate_block(LL, L1, L3, Part13),
    generate_block(LL, L2, L1, Part21),
    generate_block(LL, L2, L2, Part22),
    generate_block(LL, L2, L3, Part23),
    generate_block(LL, L3, L1, Part31),
    generate_block(LL, L3, L2, Part32),
    generate_block(LL, L3, L3, Part33),
    append(Part11, Part12, Temp1),
    append(Temp1, Part13, Part1),
    append(Part21, Part22, Temp2),
    append(Temp2, Part23, Part2),
    append(Part31, Part32, Temp3),
    append(Temp3, Part33, Part3),
    append(Part2, Part3, Temp),
    append(Part1, Temp, Result).

generate_block([], _, _, []).
generate_block([Num0|Tail], Row,Col, Result ) :-
    generate_block(Tail, Row, Col, Res2),
    generate_block_clauses(Num0, Row, Col, Res1 ),
    append(Res2, [Res1], Result).

generate_block_clauses(_, [], _, []).
generate_block_clauses(_, _, [], []).
generate_block_clauses(Num, [Row|Tail1], [Col|Tail2], Res) :-
    generate_block_clauses(Num, [Row], Tail2, Res1),
    generate_block_clauses(Num, Tail1, [Col|Tail2], Res2),
    append([yes([Row,Col,Num])], Res1, Temp),
    append(Temp, Res2, Res).
    

extract_clues_to_cnf([], _, []).
extract_clues_to_cnf([Row|Tail], Counter, Result) :-
    Nextcnt is Counter + 1,
    extract_clues_to_cnf(Tail, Nextcnt, Result1),
    extract_row(Row, Counter, 1, Result2),
    append(Result2, Result1, Result).

extract_row([], _, _, []).
extract_row([Val|Tail], RowCnt, Counter, Result) :-
    Val > 0,
    !,
    Nextcnt is Counter + 1,
    extract_row(Tail, RowCnt, Nextcnt, Result1),
    append([[yes([RowCnt, Counter, Val])]], Result1, Result).
extract_row([_|Tail], RowCnt, Counter, Result) :-
    Nextcnt is Counter + 1,
    extract_row(Tail, RowCnt, Nextcnt, Result1),
    append([], Result1, Result).


extract_clues([], []).
extract_clues([[yes(X)]|Tail], Res) :-
    extract_clues(Tail, Resnext),
    append([X], Resnext, Res).

solve_cnf(CNF, Clues, Vars, Result) :-
    extract_clues(Clues, Cluelist),
    subtract(Vars, Cluelist, RemovedVars),
    remove_1_literal(CNF, NewCNF, Clues),
    append([], Cluelist, Assignment),
    generate_assignment(NewCNF, RemovedVars, Assignment, Result).

remove_1_literal(_, [], []).
remove_1_literal(Clauses, NewCNF, [[Clue]|Tail]) :-
    remove_1_literal(Clauses, NextCNF, Tail),
    check_literal(Clauses, ThisCNF, Clue),
    append(ThisCNF, NextCNF, NewCNF).

check_literal([], [], _).
check_literal([Clause|Rest], NewCNF, Clue) :-
    check_literal(Rest, NextCNF, Clue),
    (
        member(Clue, Clause),
        append([], NextCNF, NewCNF) ;
        append(Clause, NextCNF, NewCNF)).

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
    satisfies_cnf(CNF, Assignment), !,
    append([], Assignment, Result).
generate_assignment(CNF, [[X,Y,Z]|Vars], Assignment, Result) :-
    \+ member([X,Y,_], Assignment),
    append([[X,Y,Z]], Assignment, Temp1),
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

convert_cnf(CNF, Result) :-
    convert_cnf(CNF, [], 1, [], Result),
    !.

convert_cnf([], Current, _, _, Current) :- !.
convert_cnf([H|T], Current, Counter, Row, Result) :-
    read_column(H, ColumnResult),
    append(Row, [ColumnResult], Row1),
    (
        Counter < 9,
        Counter1 is Counter + 1, 
        convert_cnf(T, Current, Counter1, Row1, Result);

        Counter1 is 1,
        append(Current, [Row1], Current1),
        convert_cnf(T, Current1, Counter1, [], Result)
    ).

read_column([X], X).
read_column([_|T], Result) :-
    read_column(T, Result).

