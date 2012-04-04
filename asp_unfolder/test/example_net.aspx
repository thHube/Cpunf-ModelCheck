                                                                     
                                                                     
                                                                     
                                             
% big bang event (which causes everything)
event(0).

% intial marking
cond(a(0)).
cond(b(0)).
% and corresponding concurrency
conc(a(0), b(0)).


% events t and u, from their causes
event(t(a(H1),b(H2))) :- 
	cond(a(H1)), 
	cond(b(H2)), 
	conc(a(H1),b(H2)).
event(u(a(H1),b(H2))) :- 
	cond(a(H1)), 
	cond(b(H2)), 
	conc(a(H1),b(H2)).
% and the conditions they generate
cond(c(t(S1,S2))) :-
	event(t(S1,S2)), not cutoff(t(S1,S2)).
cond(c(u(S1,S2))) :-
	event(u(S1,S2)), not cutoff(u(S1,S2)).

% event v from its cause
event(v(c(H))) :- 
	cond(c(H)).
% and the conditions it generates
cond(a(v(C))) :- 
	event(v(C)), not cutoff(v(C)).
cond(b(v(C))) :- 
	event(v(C)), not cutoff(v(C)).
% with the corresponding concurrency
conc(a(v(C)), b(v(C))) :- 
	event(v(C)).

% inductive rules for concurrency
conc(a(v(C)), C1) :- 
	conc(C,C1), 
	event(v(C)), 
	C != C1.
conc(b(v(C)), C1) :- 
	conc(C,C1), 
	event(v(C)), 
	C != C1.

% parikh's vector of an history

% height(History, Height)
height(0, 0).

height(t(a(H1),b(H2)),L1+1) :- 
	event(t(a(H1),b(H2))),  
	height(H1,L1), 
	height(H2,L2), 
	L2 < L1.

height(t(a(H1),b(H2)),L2+1) :- 
	event(t(a(H1),b(H2))), 
	height(H1,L1), 
	height(H2,L2),
	L2 >= L1.

height(u(a(H1),b(H2)),L1+1) :- 
	event(u(a(H1),b(H2))),  
	height(H1,L1), 
	height(H2,L2), 
	L2 < L1.

height(u(a(H1),b(H2)),L2+1) :- 
	event(u(a(H1),b(H2))), 
	height(H1,L1), 
	height(H2,L2),
	L2 >= L1.

height(v(c(H)),L+1) :- 
	event(v(c(H))),
	height(H,L).


% height/1: colleziona l'insieme delle altezze
height(L) :- 
	event(H),
	height(H,L).


% or
or(1,1,0).
or(0,0,0).
or(1,0,1).
or(1,1,1).

% parikhLev(parikh vector, storia, altezza)
% vettore di parikh ad una data altezza

parikhLev(p(t(0),u(0),v(0)), H, 0) :- 
	event(H).

parikhLev(p(t(0),u(0),v(0)), H, L) :- 
	event(H), 
	height(H,L1), 
	height(L),
	L1 < L.

% t transition
parikhLev(p(t(I),u(J),v(K)), t(a(H1),b(H2)), L) :- 
  	event(t(a(H1),b(H2))),
  	height(t(a(H1),b(H2)),L),
  	I = 1,
  	J = 0,
  	K = 0.

parikhLev(p(t(I),u(J),v(K)), t(a(H1),b(H2)), L) :- 
	height(L),
  	event(t(a(H1),b(H2))),
  	height(t(a(H1),b(H2)),L1),
  	L < L1,
  	parikhLev(p(t(I1),u(J1),v(K1)), H1, L),
  	parikhLev(p(t(I2),u(J2),v(K2)), H2, L),
  	or(I,I1,I2),
  	or(J,J1,J2),
  	or(K,K1,K2).


% u transition
parikhLev(p(t(I),u(J),v(K)), u(a(H1),b(H2)), L) :- 
  	event(u(a(H1),b(H2))),
  	height(u(a(H1),b(H2)),L),
  	I = 0,
  	J = 1,
  	K = 0.

parikhLev(p(t(I),u(J),v(K)), u(a(H1),b(H2)), L) :- 
	height(L),
  	event(u(a(H1),b(H2))),
  	height(u(a(H1),b(H2)),L1),
  	L < L1,
  	parikhLev(p(t(I1),u(J1),v(K1)), H1, L),
  	parikhLev(p(t(I2),u(J2),v(K2)), H2, L),
  	or(I,I1,I2),
  	or(J,J1,J2),
  	or(K,K1,K2).

% v transition
parikhLev(p(t(I),u(J),v(K)), v(c(H)), L) :- 
  	event(v(c(H))),
  	height(v(c(H)),L),
  	I = 0,
  	J = 0,
  	K = 1.

parikhLev(p(t(I),u(J),v(K)), v(c(H)), L) :- 
	height(L),
  	event(v(c(H))),
  	height(v(c(H)),L1),
  	L < L1,
  	parikhLev(p(t(I1),u(J1),v(K1)), H, L),
  	I = I1,
  	J = J1,
  	K = K1.


% parikh/3 (summing up the different components at various levels)
parikhSum(p(t(0),u(0),v(0)), H, 0) :- 
	event(H).

parikhSum(p(t(I),u(J),v(K)), H, L) :- 
	height(L),
	L>0,
	parikhSum(p(t(I1),u(J1),v(K1)), H, L-1),
	parikhLev(p(t(I2),u(J2),v(K2)), H, L),
	I = I1 + I2,
  	J = J1 + J2,
  	K = K1 + K2.


% parikh/2: the parikh vector

parikh(p(t(I),u(J),v(K)), H) :- 
	event(H),
	height(H,L),
	parikhSum(p(t(I),u(J),v(K)), H, L).


% marking of an history

marking(m(I,J,K), H) :-
 	event(H),
 	parikh(p(t(I1),u(J1),v(K1)), H),
 	I = 1 + K1 - I1 - J1,
 	J = 1 + K1 - I1 - J1,
 	K = I1 + J1 - K1.

% per qualche ragione usando marking(m(a(I), b(J), c(K)), ...) non funziona ...
% marking(m(a(I),b(J),c(K)), H) :-
%  	event(H),
%  	parikh(p(t(I1),u(J1),v(K1)), H),
%  	I = 1 + K1 - I1 - J1,
%  	J = 1 + K1 - I1 - J1,
%  	K = I1 + J1 - K1.


% order on histories (basato su parikh)

cardinality(H, I) :-
	event(H),
	parikh(p(t(I1),u(J1),v(K1)), H),
	I = I1 + K1 + J1.

less(H1, H2) :-
	event(H1), event(H2),
	parikh(p(t(I1),u(J1),v(K1)), H1),
	parikh(p(t(I2),u(J2),v(K2)), H2),
	I1 < I2.

less(H1, H2) :-
	event(H1), event(H2),
	parikh(p(t(I1),u(J1),v(K1)), H1),
	parikh(p(t(I2),u(J2),v(K2)), H2),
	I1 = I2, J1<J2.

less(H1, H2) :-
	event(H1), event(H2),
	parikh(p(t(I1),u(J1),v(K1)), H1),
	parikh(p(t(I2),u(J2),v(K2)), H2),
	I1 = I2, J1=J2, K1 < K2.

% cut-off

cutoff(H) :-
	event(H),
	event(H1),
	marking(M,H),
	marking(M,H1),
	less(H1,H).
	

#hide.

#show height/2.
#show height/1.

% #show parikhLev/3.
% #show parikhSum/3.
% #show parikh/2.

% #show marking/2.

% #show cardinality/2.

#show event/1.
#show cond/1.

%#show less/2.

%#show cutoff/1.

%#show height/2.