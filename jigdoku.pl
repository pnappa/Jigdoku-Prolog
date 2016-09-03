% Patrick Nappa
% 440243449

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

% wh
makesgrid(X) :-


% check first all subgrids dont intersect
% then unioning them generates 9x9 grid
% completegrid(X) :- not(intersectgrid(X)), makesgrid(X).


