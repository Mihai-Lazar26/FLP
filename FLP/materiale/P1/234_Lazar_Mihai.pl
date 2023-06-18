% Lazar Mihai
% grupa 234
% varianta 1

cate_rasp_corecte([], [], _, 0).
cate_rasp_corecte(Lc, [(Nume, Ls) | _], Nume, N) :- raspunsuri_corecte(Lc, Ls, N).
cate_rasp_corecte(Lc, [(Nume1, _) | Res], Nume2, N) :- 
Nume1 \= Nume2,
cate_rasp_corecte(Lc, Res, Nume2, N).


raspunsuri_corecte([], [], 0).
raspunsuri_corecte([X | Xs], [X | Ys], N) :-
raspunsuri_corecte(Xs, Ys, Ns),
N is Ns + 1.
raspunsuri_corecte([X | Xs], [Y | Ys], N) :-
X \= Y,
raspunsuri_corecte(Xs, Ys, Ns),
N is Ns.