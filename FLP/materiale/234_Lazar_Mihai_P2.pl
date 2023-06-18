% Lazar Mihai
% grupa 234
% varianta 1

mem(X) :- integer(X).

aexp(I, _, []) :- integer(I).
aexp(mem, Mem, []) :- mem(Mem).
aexp(E1 + E2, Mem, L) :- aexp(E1, Mem, Ls), aexp(E2, Mem, Ls), L is [E | Ls].
aexp(save(E), Mem, L) :- aexp(E, Mem, L), Mem is mem(E).


stmt(stop, Mem, L).
stmt(:::(E, C), Mem, L) :- aexp(E, Mem, L), stmt(C, Mem, L).

program(start(C), L) :- stmt(C, 0, L).

% a

