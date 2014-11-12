
edge(a,b).
edge(a,c).
edge(b,d).
edge(a,f).
edge(c,d).

path(X,X).
path(X,Y) :- edge(X,Z), path(Z,Y).

e(1,2).
e(X,Y) :- e(Y,X).

rainy(seattle).
rainy(rochester).
cold(rochester).

snowy(X) :- rainy(X), cold(X).

int(o).
int(s(X)) :- int(X).
succ(X, s(X)).
pred(s(X), X).

add(X,o,X).
add(X,s(Y),s(Z)) :- add(X,Y,Z).

red(a).
black(b).
red(c).
color(P,red) :- red(P),!.
color(P,black) :- black(P),!.
color(P,unknown).

s(a).
s(b).
r(a).
r(b).
l(c).

p(X,Y):- l(X).
p(X,Y):- r(X),!.
p(X,Y):- m(X).

i(c,e).
i(d,f).
g(f,b).
h(a,c).
h(a,d).
f(X,Y) :- h(X,Z), i(Z,Y).