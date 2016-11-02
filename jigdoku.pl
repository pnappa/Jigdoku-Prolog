% Patrick Nappa
% 440243449
:- use_module(library(clpfd)).
:- use_module(library(lists)).

%
% QUESTION 1
%

corlength(List) :- length(List, 9).
%convert the flattened list into a 2dig number representing coords
superlist([], []).
superlist([X,Y|TList], Output) :- superlist(TList, NewOutput), Output = [A|NewOutput], A is 10*X+Y.
%rows = 9, cols = 9, make sure each coord in range, then construct numbers representing cart coords, and make sure all distinct
completegrid(X) :- length(X, 9), maplist(corlength, X), flatten(X, FlatX), FlatX ins 1..9, superlist(FlatX, Suplist), all_distinct(Suplist).


%
% QUESTION 2

nextto([A,B], [C,B]) :- A is C+1.
nextto([A,B], [C,B]) :- C is A+1.
nextto([A,B], [A,D]) :- B is D+1.
nextto([A,B], [A,D]) :- D is B+1.

%succeed if any are nextto the coord
nexttolist(A, [H|_]) :- nextto(A, H).
nexttolist(A, [_|T]) :- nexttolist(A, T).

%the head of the unseen list next to any of the seen list
closure(Seen, [HUnseen|TUnseen], Remainder) :- 
	nexttolist(HUnseen, Seen),
	append(TUnseen, Remainder, NewUnseen),
	closure([HUnseen|Seen], NewUnseen, []).
%the head is not next to anything, add to the remainder list	
closure(Seen, [HUnseen|TUnseen], Remainder) :- closure(Seen, TUnseen, [HUnseen|Remainder]).
	
%the Unseen and remainder list is empty therefore done
closure(_, [], []).

%continuous if the closure of the first cell equals the rest of the set
%also, a grid is contiguous over the empty set (fight me in real life if you disagree)
contiguousgrid([]).
contiguousgrid([H|T]) :- closure([H], T, []).

%
% QUESTION 3
%

%for a given row, construct the jigsaw list
jiggo(_, [], []).
jiggo(Solution, [HJig|TJig], [[Row,Col]|T]) :- nth1(Row, Solution, SlickRow), nth1(Col, SlickRow, HJig), jiggo(Solution, TJig, T).
%jiggo(Solution, GridRow, Jigsawlist) :-
recjiggo(_, [], []).
recjiggo(Solution, [H|T], [Jighead|Jigtail]) :- jiggo(Solution, H, Jighead), recjiggo(Solution, T, Jigtail).

solve(Grid, Sudoku, X) :-
	%validate the arguments
	maplist(corlength, Sudoku),
	corlength(Sudoku),

	X = Sudoku,
	
	%all numbers in range 1..9
	flatten(X, FlatSol),
	FlatSol ins 1..9,

	%generate a list of the jigsaw pieces
	recjiggo(X, JigsawList, Grid),
	transpose(X, ColList), %columns are simply transpose of rows

	%all numbers unique in each row/col/jigsaw
	maplist(all_different, X),	%note this is a list of each of the rows, so can be used in place instead
	maplist(all_different, ColList),
	maplist(all_different, JigsawList),

	%late check continuity
	maplist(contiguousgrid, Grid),

	%force prolog to actually solve it
	label(FlatSol).


