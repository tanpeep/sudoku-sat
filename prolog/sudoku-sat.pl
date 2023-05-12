:- use_module(library(clpb)).
:- use_module(library(clpfd)).
:- dynamic(cell_bool/4).

% set every cell with false value on dynamic database
set_cell_var :-
    A = [1,2,3,4,5,6,7,8,9],
    member(X, A),
    member(Y, A),
    append(cell_bool(X,Y,0)).

solve_cnf(CNF) :-
    extract_variables(CNF, Vars),
    Assignment = [],
    generate_assignment(CNF, Vars, Assignment).

not_form(not(X), X).
not_form(X, X).

remove_not([], []).

remove_not([H|T], ResultList) :-
    not_form(H, X), !,
    remove_not(T, ResultNext),
    append([X], ResultNext, ResultList).

extract_variables(CNF, Vars) :-
    flatten(CNF, Flat),
    remove_not(Flat, RemovedNot),
    sort(RemovedNot, Vars).
    

generate_assignment(CNF, [], Assignment) :-
    satisfies_cnf(CNF, Assignment), 
    write(Assignment).
generate_assignment(CNF, [Var|Vars], Assignment) :-
    append([Var], Assignment, Temp1),
    generate_assignment(CNF, Vars, Temp1).

generate_assignment(CNF, [_|Vars], Assignment) :-
    generate_assignment(CNF, Vars, Assignment).

satisfies_cnf([], _).
satisfies_cnf([Clause|Tail], Assignment) :-
    satisfies_clause(Clause, Assignment), satisfies_cnf(Tail, Assignment).

satisfies_clause([], _).
satisfies_clause([Literal|Tail], Assignment) :-
    (   check_satisfy(Literal, Assignment) ; 
    satisfies_clause(Tail, Assignment)).

satisfies_clause(Literal, Assignment) :-
    check_satisfy(Literal, Assignment).

check_satisfy(not(X), Assignment) :-
    \+ member(X, Assignment).
check_satisfy(X, Assignment) :-
    member(X, Assignment).


% gpt
solve_sudoku(Board) :-
    % Convert the board to a list of variables and their possible values
    extract_variables(Board, Variables),
    generate_cnf(Board, Variables, CNF),
    % Solve the CNF formula using the SAT solver
    solve_cnf(CNF),
    % Convert the assignment of variables to a completed Sudoku board
    extract_board(Board).

generate_cnf(Board, Variables, CNF) :-
    % Generate CNF clauses for each cell and its constraints
    findall(Clause, (
        member(Variable, Variables),
        variable(Var, X, Y) = Variable,
        between(1, 9, Val),
        (cell_value(Board, X, Y, Val) -> Clause = [Var-Val] ; Clause = [not(Var-Val)])
    ), Clauses),
    % Combine the clauses into a single CNF formula
    flatten(Clauses, CNF).

cell_value(Board, X, Y, Val) :-
    nth1(X, Board, Row),
    nth1(Y, Row, Val).

generate_sudoku(Board) :-
    % Generate a random starting board with 25-35 clues
    random_between(25, 35, Clues),
    generate_board(Clues, Board1),
    % Solve the board to create a unique solution
    solve_sudoku(Board1),
    % Remove some values to create a puzzle
    random_puzzle(Board1, Board).

generate_board(Clues, Board) :-
    % Generate a list of variables and values for the given number of clues
    findall(variable(Var, X, Y)-Val, (
        between(1, 81, N),
        random_between(1, 81, N),
        variable(Var, X, Y),
        (N =< Clues -> random_between(1, 9, Val) ; Val = 0)
    ), Assignment),
    % Convert the variable-value pairs to a Sudoku board
    extract_board(Board, Assignment).

random_puzzle(Board1, Board) :-
    % Convert the board to a list of variables and their assignments
    extract_variables(Board1, Variables),
    generate_puzzle(Variables, Board1, Board).

generate_puzzle([], Board, Board).
generate_puzzle([Variable|Variables], Board1, Board) :-
    % Try removing the value of the variable from the board
    variable(Var, X, Y) = Variable,
    nth1(X, Board1, Row),
    nth1(Y, Row, Val),
    select(variable(Var, X, Y)-Val, Assignment1, Assignment2),
    % Check if the resulting board still has a unique solution
    once((solve_sudoku(extract_board(Assignment2)), \+ solve_sudoku(extract_board(Assignment1)))),
    % Recurse on the remaining variables
    generate_puzzle(Variables, Assignment2, Board).

board_clauses(Board, Clauses) :-
    findall(Literal, (
        nth1(X, Board, Row),
        nth1(Y, Row, Val),
        variable(Var, X, Y),
        (Val == 0 -> Literal = not(Var) ; Literal = Var)
    ), Literals),
    Clauses = [or|Literals].

extract_board(Board) :-
    findall(Row, (
        between(1, 9, X),
        findall(Val, (
            between(1, 9, Y),
            variable(Var, X, Y),
            member(Var-Val, Assignment)
        ), Row)
    ), Board),
    print_board(Board).

print_board(Board) :-
    maplist(writeln, Board).

% Define the variables for a 9x9 Sudoku grid
variable(Var, X, Y) :-
    between(1, 9, X),
    between(1, 9, Y),
    atomic_list_concat([Var, X, Y], '_', VarName),
    term_string(Var, VarName).

% Define the constraints for Sudoku
sudoku_constraints(Clauses) :-
    findall(Var, variable(Var, _, _), Vars),
    % Each cell must have a value between 1 and 9.
    maplist(between(1, 9), Vars),
    % Each row must contain every digit from 1 to 9 exactly once.
    maplist(row_constraint, Vars, Clauses1),
    % Each column must contain every digit from 1 to 9 exactly once.
    maplist(column_constraint, Vars, Clauses2),
    % Each 3x3 sub-grid must contain every digit from 1 to 9 exactly once.
    maplist(square_constraint, Vars, Clauses3),
    append(Clauses1, Clauses2, Clauses12),
    append(Clauses12, Clauses3, Clauses).

% Define the row constraint
row_constraint(Var, Clause) :-
    variable(Var, X, _),
    findall(Var, variable(Var, X, _), Vars),
    all_different(Vars),
    Clause =.. [or|Vars].

% Define the column constraint
column_constraint(Var, Clause) :-
    variable(Var, _, Y),
    findall(Var, variable(Var, _, Y), Vars),
    all_different(Vars),
    Clause =.. [or|Vars].

% Define the 3x3 square constraint
square_constraint(Var, Clause) :-
    variable(Var, X, Y),
    SquareX is (X - 1) // 3 * 3 + 1,
    SquareY is (Y - 1) // 3 * 3 + 1,
    findall(Var, (
        between(SquareX, SquareX + 2, SX),
        between(SquareY, SquareY + 2, SY),
        variable(Var, SX, SY)
    ), Vars),
    all_different(Vars),
    Clause =.. [or|Vars].

