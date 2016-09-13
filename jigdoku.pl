% Patrick Nappa
% 440243449
:- use_module(library(clpfd)).

%
% QUESTION 1
%
% TODO: alternative way, which is check column length, then check all row lengths, then sort the sum of the rows, then assert that the first is 1,1 and then each cell is one more than the previous
% TODO: test intersect row
% if any of the cells are the same
intersectrow([HR0|_], [HR0|_]).
intersectrow([HR0|TR0], [_|TR1]) :- intersectrow([HR0|TR0], TR1).
intersectrow([_|TR0], [HR1|TR1]) :- intersectrow(TR0, [HR1|TR1]).

% TODO: test intersect grid
% check every combination of rows if they intersect
intersectgrid([H |[TH | _]]) :- intersectrow(H, TH).
intersectgrid([H |[_ | TT]]) :- intersectgrid([H | TT]).
intersectgrid([_ |[TH | TT]]) :- intersectgrid([TH | TT]).


%TODO test rowlengthcheck
%default case
rowlengthcheck([]).
%make sure this row is 9, and check the rest
rowlengthcheck([H|T]) :- length(H, 9), rowlengthcheck(T).

%TODO test cellvalcheck
cellvalcheck([]).
%make sure values within bounds and check next
cellvalcheck([[A,B] | T]) :- A >= 1, A =< 9, B >= 1, B =< 9, cellvalcheck(T).

%TODO test rowvalcheck
rowvalcheck([]).
%check every cell for this row, then check remainder rows
rowvalcheck([H | T]) :- cellvalcheck(H), rowvalcheck(T).

%check that the length of the grid is 9, and each row has length 9, and minimum value is each is 1, and max is 9
makesgrid(X) :- length(X,9), rowlengthcheck(X), rowvalcheck(X).

% check first all subgrids dont intersect
% then all values are within range and 9 rows and 9 columns
completegrid(X) :- not(intersectgrid(X)), makesgrid(X).


%
% QUESTION 2
% TODO: test all these functions, yo

nextto([A,B], [C,B]) :- A is C+1.
nextto([A,B], [C,B]) :- C is A+1.
nextto([A,B], [A,D]) :- B is D+1.
nextto([A,B], [A,D]) :- D is B+1.

%indicates whether any member of the current set is next to this var
setadjacent([H | _], Var) :- nextto(H, Var).
setadjacent([_ | T], Var) :- setadjacent(T, Var).

%closure is complete when we've removed all elements from the Unseen set
closure(_, [], []).
%we can find it! so we add the head to the seen list, and replace the remainder into the unseen list
closure(Seen, [HUnseen|TUnseen], []) :- setadjacent(Seen, HUnseen), closure([HUnseen | Seen], TUnseen, []).
closure(Seen, [HUnseen|TUnseen], Remainder) :- setadjacent(Seen, HUnseen), closure([HUnseen | Seen], [Remainder | TUnseen], []).
%can't find it? add the head of unseen to remainder
closure(Seen, [HUnseen|TUnseen], []) :- closure(Seen, TUnseen, HUnseen).
closure(Seen, [HUnseen|TUnseen], Remainder) :- closure(Seen, TUnseen, [HUnseen | Remainder]).
% Seen = set of nodes we have seen and validated
% Unseen = (Universe \ Seen) \ Attempted
% Attempted = set of nodes we have seen and couldn't fit with the current Seen list
%closure(Seen, Unseen, Attempted) :- .

%continuous if the closure of the first cell equals the rest of the set
contiguousregion([H|T]) :- closure([H], T, []).


%
% QUESTION 3
%

%define this to validate all rows
contiguousgrid([]).
contiguousgrid([H|T]) :- contiguousregion(H), contiguousgrid(T).

%return the row given by the rownum
nth(1, Item, [H|T]) :- Item = H.
nth(Itemnum, Item, [H|T]) :- nth(Nextitem, Item, T), Itemnum is Nextitem+1.

%yield the jigsaw row of the space
jigsawslice(_, [], _).
jigsawslice(Space, [HCoord|[]], Jigsaw) :- query(JigVal, Space, HCoord), Jigsaw=[JigVal].
jigsawslice(Space, [HCoord|TCoords], Jigsaw) :- query(JigVal, Space, HCoord), Jigsaw=[JigVal|RemJig], jigsawslice(Space, TCoords, RemJig).

% predicate to query the grid location
% grab the nth row, then nth cell in that row
query(Value, Space, XPos, YPos) :- nth(XPos, Row, Space), nth(YPos, Value, Row).
query(Value, Space, [H,T]) :- query(Value, Space, H, T).

%checks the list if all are within range of Min..Max
validlist([], _, _).
validlist([H|T], Min, Max) :- H ins Min..Max, validlist(T, Min, Max).  
%check all in each list is unique
uniquelist([]).
uniquelist([H|T]) :- all_different(H), uniquelist(T).

%generate the list of lists given the list of lists of coords of the jigsaw pieces
jigsawlist(_, [], _).
jigsawlist(Space, [HGrid|[]], JigsawList) :- jigsawslice(Space, HGrid, Jigsaw), JigsawList=[Jigsaw].
jigsawlist(Space, [HGrid|TGrid], JigsawList) :- jigsawslice(Space, HGrid, Jigsaw), JigsawList=[Jigsaw|RemainderJigsaw], jigsawlist(Space, TGrid, RemainderJigsaw).

%solve(Solution, Grid, Space) :- length(Space, 9), completegrid(Grid), contiguousgrid(Grid), validstate(Space)
solve(Solution, Grid, Space) :-
	Solution = Space,
	
	%all numbers in range 1..9
	flatten(Solution, FlatSol),
	FlatSol ins 1..9,

	%generate a list of the jigsaw pieces
	jigsawlist(Solution, Grid, JigsawList),
	transpose(Solution, ColList), %columns are simply transpose of rows

	%all numbers unique in each row/col/jigsaw
	uniquelist(Solution),	%note this is a list of each of the rows, so can be used in place instead
	uniquelist(ColList),
	uniquelist(JigsawList),
	%force prolog to actually solve it
	label(FlatSol).


