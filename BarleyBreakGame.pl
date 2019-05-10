/*
@author kseniya.perepechina0@gmail.com
@date 04.20.2019
@group 621701
Игра в пятнашки с полем размерностью 3*3 на языке Prolog.
*/

after([Empty | Tiles], [Tile | Tiles1], 1) :- % Стоимость всех дуг = 1
swap(Empty, Tile, Tiles, Tiles1). % Переставив Empty и Tile получаем Tiles
 
swap(Empty, Tile, [Tile | Ts], [Empty | Ts]) :-
mandist(Empty, Tile, 1). 
 
swap(Empty, Tile, [T1 | Ts], [T1 | Ts1]) :-
swap(Empty, Tile, Ts, Ts1).
 
  % Манхеттеновское расстояние между клетками
mandist(X/Y, X1/Y1, D) :-                                                   %клетками
dif(X, X1, Dx),
dif(Y, Y1, Dy),
D is Dx + Dy.

   % где D есть |A-B|
dif(A, B, D) :- 
D is A-B, D >= 0, ! ;
D is B-A.
 
  % Эвристическая оценка h равна сумме расстояний фишек от их "целевых" клеток 
  % плюс "степень упорядоченности"*3
h([Empty | Tiles], H) :-
goal([Empty1 | GoalSquares] ),
totdist(Tiles, GoalSquares, D),
seq(Tiles, S), 
H is D + 3*S.

  % Рассчёт суммарного расстояния
totdist([], [], 0).
totdist([Tile | Tiles], [Square | Squares], D) :-
mandist(Tile, Square, D1),
totdist(Tiles, Squares, D2),
D is D1 + D2.
 
  % Рассчёт степени упорядоченности 
seq([First | OtherTiles], S) :-
seq([First | OtherTiles ], First, S).
 
seq([Tile1, Tile2 | Tiles], First, S) :-
score( Tile1, Tile2, S1),
seq([Tile2 | Tiles], First, S2),
S is S1 + S2.
 
seq([Last], First, S) :-
score(Last, First, S).
 
  /*
     Очки приписываются фишкам по следующим правилам:
     - фишка в центральной позиции — 1 очко;
     - фишка не в центральной позиции, и непосредственно за ней следует (по часовой стрелке) та фишка, какая и должна за ней следовать в целевой позиции — 0 очков.
     - то же самое, но за фишкой следует "не та" фишка — 2 очка.
  */
  % Фишка в центре 1
score(2/2, _, 1) :- !. 
  % Правильная последовательность 0
score(1/3, 2/3, 0) :- !. 
score(2/3, 3/3, 0) :- !.
score(3/3, 3/2, 0) :- !.
score(3/2, 3/1, 0) :- !.
score(3/1, 2/1, 0) :- !.
score(2/1, 1/1, 0) :- !.
score(1/1, 1/2, 0) :- !.
score(1/2, 1/3, 0) :- !.
  % Неправильная последовательность 2
score(_, _, 2). 
 
  /*
    Вершина пространства состояний — это некоторая конфигурация из фишек на игровой доске. В программе она задается списком текущих положений фишек. Каждое положение определяется парой координат X|Y.
    Элементы списка располагаются в следующем порядке:
    (1) текущее положение пустой клетки,
    (2) текущее положение фишки 1,
    (3) текущее положение фишки 2,
     …
    Целевая позиция, где * - маркер:
    1 2 3
    8 * 4
    7 6 5
    Маркер можно перемещать в любую соседнюю клетку, т.е. менять местами с соседней фишкой.
  */
goal([2/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,1/2]). 
 
% Отображение решающего пути списком позиций на доске
showsol([]).
showsol([P|L]) :-
showsol(L),
nl, write('---'),
showpos(P).
 
% Отображение позиции на доске
showpos([S0,S1,S2,S3,S4,S5,S6,S7,S8]) :-
member(Y, [3,2,1] ), % Порядок Y-координаты
nl, member(X, [1,2,3] ), % Порядок X-координаты
member(Tile-X/Y,  % Фишка в клетке X/Y
['*'-S0,1-S1,2-S2,3-S3,4-S4,5-S5,6-S6,7-S7,8-S8] ),
write(Tile),
fail % Возврат с переходом к следующей клетке
;
true. % Обработка всех клеток закончена
 
  % @based_on И.Братко - Программирование на языке Пролог для искусственного интеллекта. Глава 12
  % Поиск с предпочтением bestfirst
  % bestfirst( Start, Solution): Solution это решение от Start до goal
 
bestfirst( Start, Solution) :-
expand([], l( Start, 0/0), 9999, _, yes, Solution).
% 9999 самое большое значение эвристической оценки
 
expand(P, l( N, _), _, _, yes, [N|P]) :-
goal(N).
 
expand(P, l(N,F/G), Bound, Tree1, Solved, Sol) :-
F =< Bound,
( bagof(M/C, (after(N,M,C), not(member(M,P)) ), Succ),
!,
succlist(G, Succ, Ts),
bestf(Ts, F1),
expand(P, t(N,F1/G,Ts), Bound, Tree1, Solved, Sol)
;
Solved = never % Нет приемников-тупик
) .
expand(P, t(N,F/G,[T|Ts]), Bound, Tree1, Solved, Sol) :-
F =< Bound,
bestf(Ts, BF), min( Bound, BF, Bound1), % Bound1 = min(Bound,BF)
expand([N|P], T, Bound1, T1, Solved1, Sol),
continue(P, t(N, F/G, [T1 | Ts]), Bound, Tree1, Solved1, Solved, Sol).
 
expand(_, t(_, _, []), _, _, never, _) :- !.
 
% Тупиковое дерево-нет решений
 
expand(_, Tree, Bound, Tree, no, _) :-
f(Tree, F), F > Bound. % рост остановлен
 
% continue( Path, Tree, Bound, NewTree, SubtreeSolved, TreeSolved, Solution)
 
continue(_, _, _, _, yes, yes, Sol).
 
continue(P, t(N,F/G,[T1|Ts]), Bound, Tree1, no, Solved, Sol) :-
insert(T1, Ts, NTs),
bestf(NTs, F1),
expand(P, t(N,F1/G,NTs), Bound, Tree1, Solved, Sol).
 
continue(P, t(N,F/G,[_|Ts]), Bound, Tree1, never, Solved, Sol) :-
bestf(Ts, F1),
expand(P, t(N,F1/G,Ts), Bound, Tree1, Solved, Sol).
 
% succlist( G0, [ Node1/Cost1, ...], [ l(BestNode,BestF/G), ...]):
succlist(_, [], []).
 
succlist(G0, [N/C | NCs], Ts) :-
G is G0 + C,
h(N, H), % Heuristic term h(N)
F is G + H,
succlist(G0, NCs, Ts1),
insert(l(N,F/G), Ts1, Ts).
 
insert(T, Ts, [T | Ts]) :-
f(T, F), bestf( Ts, F1),
F =< F1, !.
 
insert(T, [T1 | Ts], [T1 | Ts1]) :-
insert(T, Ts, Ts1).
 
 
% Получение эвристической оценки
f(l(_,F/_), F). % f-  эв. оценка листа
f(t(_,F/_,_), F). % f- эв. оценка дерква
bestf([T|_], F) :- % Наилучшая эв-оценка для списка дереьев
f(T, F).
bestf([], 9999). % Нет деревьев, плохая эв-оценка
min(X, Y, X) :-
X =< Y, !.
min(X, Y, Y).

start([_,_,_,_,_,_,_,_,_]).

/* Примеры стартовых позиций
 * start1([2/2,1/3,3/2,2/3,3/3,3/1,2/1,1/1,1/2]). % Требуется для решения 4 шага
 * start2([2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3]). % Требуется для решения 6 шагов
 * start3([2/2,2/3,1/3,3/1,1/2,2/1,3/3,1/1,3/2]). % Требуется для решения 18 шагов
*/

/* Пример запроса: 
 * ?- start([2/2,1/3,3/2,2/3,3/3,3/1,2/1,1/1,1/2]), 
 * bestfirst([2/2,1/3,3/2,2/3,3/3,3/1,2/1,1/1,1/2], Sol), 
 * showsol(Sol).
 * */
