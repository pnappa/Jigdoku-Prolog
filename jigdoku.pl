same(X, X).

% if any of the cells are the same
intersectrow([HR0|TR0], [HR0|TR1]).
intersectrow([HR0|TR0], [HR1|TR1]) :- intersectrow([HR0|TR0], TR1).
intersectrow([HR0|TR0], [HR1|TR1]) :- intersectrow(TR0, [HR1|TR1]).

% check every combination of rows if they intersect
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R1).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R2).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R3).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R4).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R5).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R6).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R0, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R2).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R3).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R4).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R5).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R6).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R1, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R2, R3).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R2, R4).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R2, R5).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R2, R6).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R2, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R2, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R3, R4).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R3, R5).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R3, R6).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R3, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R3, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R4, R5).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R4, R6).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R4, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R4, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R5, R6).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R5, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R5, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R6, R7).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R6, R8).
intersectgrid([R0,R1,R2,R3,R4,R5,R6,R7,R8]) :- intersectrow(R7, R8).

% check first all subgrids dont intersect
% then unioning them generates 9x9 grid
completegrid(X) :- not(intersectgrid(X)), makesgrid(X).


