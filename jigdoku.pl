% Patrick Nappa
% 440243449

%
% QUESTION 1
%
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
%
badfn(X) :- false.

nextto([A,_], [C,_]) :- A=C+1.
nextto([A,_], [C,_]) :- C=A+1.
nextto([_,B], [_,D]) :- B=D+1.
nextto([_,B], [_,D]) :- D=B+1.

%indicates whether any member of the current set is next to this var
setadjacent([H | T], Var) :- nextto(H, Var).
setadjacent([H | T], Var) :- setadjacent(T, Var).

%closure is complete when we've removed all elements from the Unseen set
closure(_, [], []).
%we can find it! so we add the head to the seen list, and replace the remainder into the unseen list
closure(Seen, [HUnseen|TUnseen], Remainder) :- setadjacent(Seen, HUnseen), closure([HUnseen | Seen], [Remainder | TUnseen], []).
%can't find it? add the head of unseen to remainder
closure(Seen, [HUnseen|TUnseen], Remainder) :- closure(Seen, TUnseen, [HUnseen | Remainder]).
% Seen = set of nodes we have seen and validated
% Unseen = (Universe \ Seen) \ Attempted
% Attempted = set of nodes we have seen and couldn't fit with the current Seen list
%closure(Seen, Unseen, Attempted) :- .

%continuous if the closure of the first cell equals the rest of the set
contiguousgrid([H|T]) :- closure([H], T, []).
