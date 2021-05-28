% Carson Kim 
% Homework #4 - KenKen Solver 
% CS 131 - Winter 2021




%%%%%%%% Problem 1. %%%%%%%%%
%%%%%%% KenKen Solver %%%%%%%%


% misc. predicates / constraints. 
is_unique([]).
is_unique([H|T]):- fd_all_different(H), is_unique(T).

is_columnSizeN(T, N):- 
    length(T, N).

is_rowSizeN([],_).
is_rowSizeN([Head|Tail], N):- length(Head, N), is_rowSizeN(Tail, N).

is_domainN([],_).
is_domainN([Head | Tail], N):- fd_domain(Head, 1, N), is_domainN(Tail, N).

check_all_constraints(_, []).
check_all_constraints(T, [Head | Tail]):-
    kk_constraint_single(T, Head),
    check_all_constraints(T, Tail).


% retrieves the necessary cell / square %
get_value(T, [Row|Col], Value):- 
    nth(Row, T, R),
    nth(Col, R, Value).

%  multiplication constraint predicates %
kk_constraint_single(T, *(P, L)):-
    prod(T, L, P).

% division constraint predicates %
kk_constraint_single(T, /(Q, J, K)):-
    div(T, J, K, Q).

% sum constraint predicates %   
kk_constraint_single(T, +(S, L)):-
    mysum(T, L, S).

 % subtraction constraint predicates %
kk_constraint_single(T, -(D, J, K)):-
    sub(T, J, K, D).

prod(_, [], 1).
prod(T, [Head | Tail], P0):-
    get_value(T, Head, Value),
    prod(T, Tail, TailValue),
    P0 #= Value * TailValue.

sub(T, J, K, D0):-
   get_value(T, J, JValue),
   get_value(T, K, KValue),
   (D0 #= JValue - KValue; D0 #= KValue - JValue).

div(T, J, K, Q):-
    get_value(T, J, Jvalue),
    get_value(T, K, Kvalue),
    (Q #= Jvalue / Kvalue; Q #= Kvalue / Jvalue).

mysum(_, [], 0).
mysum(T, [Head | Tail], S):-
    get_value(T, Head, Value), 
    mysum(T, Tail, TailValue),
    S #= Value + TailValue.

% transpose to help with unique and other rules
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% main predicate
kenken(N, C, T):-
    is_columnSizeN(T, N),
    is_rowSizeN(T, N), 
    is_domainN(T, N),
    is_unique(T),
    transpose(T, Transposed_T),
    is_unique(Transposed_T),
    check_all_constraints(T, C),
    maplist(fd_labeling, T).

% test case from spec 
kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

% my own test case, just to help me test one operation. 

kenken_testcase(
    2,
    [
    +(3, [[1|1], [1|2]]), 
    +(1, [[2|1]]),
    +(2, [2|2])
    ]
    ).

kenken_testcase2(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% Problem 2: %%%%%%%%%%%%%%%%%%%%
%%%%%%%% plain ken-ken solver %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain_prod(_, [], 1).
plain_prod(T, [Head | Tail], P0):-
    get_value(T, Head, Value),
    plain_prod(T, Tail, TailValue),
    P0 is Value * TailValue.

plain_sub(T, J, K, D0):-
   get_value(T, J, JValue),
   get_value(T, K, KValue),
   (D0 is JValue - KValue; D0 is KValue - JValue).

plain_div(T, J, K, Q):-
    get_value(T, J, Jvalue),
    get_value(T, K, Kvalue),
    (Q is Jvalue / Kvalue; Q is Kvalue / Jvalue).

plain_mysum(_, [], 0).
plain_mysum(T, [Head | Tail], S):-
    get_value(T, Head, Value), 
    plain_mysum(T, Tail, TailValue),
    S is Value + TailValue.

plain_check_all_constraints(_, []).
plain_check_all_constraints(T, [Head | Tail]):-
    plainkk_constraint_single(T, Head),
    plain_check_all_constraints(T, Tail).


% multiplication constraint predicates % 
plainkk_constraint_single(T, *(P, L)):-
    plain_prod(T, L, P).

% division constraint predicates %
plainkk_constraint_single(T, /(Q, J, K)):-
    plain_div(T, J, K, Q).

% sum constraint predicates %   
plainkk_constraint_single(T, +(S, L)):-
    plain_mysum(T, L, S).

 % subtraction constraint predicates %
plainkk_constraint_single(T, -(D, J, K)):-
   plain_sub(T, J, K, D).


% correct grid size and each element is unique % 


within_domain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

fill_2d([], _).
fill_2d([Head | Tail], N) :-
    within_domain(N, Domain),
    permutation(Domain, Head),
    fill_2d(Tail, N).

create_grid(Grid, N) :-
    length(Grid, N),
    fill_2d(Grid, N).

unique_list1(List, N) :-
	length(List, N),
	all_unique(List).

all_unique([]).
all_unique( [Head | Rest]) :- 
    check_list(Head), 
    all_unique(Rest).

check_list([]).
check_list([Head | Tail]) :-
    member(Head, Tail), !, fail.
check_list([Head | Tail]) :- check_list(Tail).

% plain kenken implementation %
plain_kenken(N, C, T):-
    create_grid(T, N),
    unique_list1(T, N),
    transpose(T, Transposed_T),
    unique_list1(Transposed_T, N),
    plain_check_all_constraints(T, C).
   

/* ##### TEST RESULTS #####
kenken is noticeably faster than plain_kenken. For the example test, I utilized the 4x4 kenken grid utilized in the homework spec (included in this file under the name kenkentestcase2).
Here are some of the stats for the finite domain solver version of kenken.

Cputime = 0
End = 25
Start = 25

plain_kenken fared much worse. Here are the stats for the example ran with my implementation:

Cputime = 94
End = 3235
Start = 3141

plain_kenken is practically useless for grids larger than 4x4 and even 5x5. It certainly would take too long for a 6x6 grid, such as the one included initially in the project spec.


##### No-Op KenKen API #####

The API for a No-Op KenKen would likely be similar to the vanilla kenken implementation, with the exception of a new operations argument / parameter.
It could look something like this:

no_op_kenken(N, C, T, O)

where O is a list of operations that correspond to the various cage constraints. Of course, the length of this list would be the same as the list of constraints
that is passed into C. 

The implementation of no-op kenken would be rather time consuming, but an API would still maintain the idea of having a constraint_checker that checks all the constraints. There would certainly be a predicate
that iterated through all the cage constraints and tried one of the 4 operations until one succeeded, and then it would add that to the list of operations in O. 
