% Description of an unfolding of a c-net:
% e(X) -> X is an event
% b(X) -> X is a condition
% t(X,Y) -> Y is the image of event X in the original c-net
% s(X,Y) -> Y is the image of condition X in the original c-net
% pre(X,Y) -> event X consumes condition Y
% read(X,Y) -> event X reads condition Y
% post(X,Y) -> event X produces condition Y
% cutoff(X) -> event X is a cutoff event

% Example 1
%
% e(1..3).
% b(1..4).
% %s(1,1). s(2,2). s(3,3). s(4,4). s(5,5). s(6,6).
% pre(1,1). pre(2,2). pre(3,3). pre(3,4). %pre(4,4)
% post(1,3). post(2,4). %post(3,5). %post(4,6).
% read(1,2). read(2,1).

% Example 2
%
% e(1;2).
% b(1;2).
% cutoff(2).
% pre(1,1).
% pre(2,2).
% post(1,2).

% Example 3
%
% e(1..8).
% b(1..11).
% pre(1,1;2). pre(2,2). pre(3,2).
% post(1,3;4). post(2,5). post(3,6).
% pre(4,3). pre(5,4). pre(6,5).
% post(4,11). post(5,7). post(6,10).
% pre(7,7). pre(8,7).
% post(7,9). post(8,8).
% cutoff(4;6;7).

% Definition of causality relation: X is cause of Y if X produces a
% condition that is consumed by Y
cause(X,Y) :- pre(Y,Z), post(X,Z).

% Definition of asymmetric conflict relation
asymconfl(X,Y) :- read(X,Z), pre(Y,Z).
asymconfl(X,Y) :- pre(X,Z), pre(Y,Z), X != Y, b(Z).
asymconfl(X,Y) :- cause(X,Y).

% The closure is enabled only when all events are being executed
% asymconfl(X,Y) :- asymconfl(X,Z), asymconfl(Z,Y),
% 	exe(X), exe(Z), exe(Y),
% 	Y != Z, X != Z.
% not needed, unless we're using the old new conflict resolutor!

% Marks the execution (or non execution) of events
%exe(X) :- exe(Y) : cause(Y,X),
%	not nexe(X), not cutoff(X), e(X).
%nexe(X) :- not exe(X), not cutoff(X), e(X).
exe(X) :- exe(Y) : cause(Y,X), not cutoff(X), e(X).

% Old conflict resolutor.
%
% This rule doesn't work in the contextual case, since we have to take
% into account that conflict not only does arise when two events have
% a common condition in their preset, but also when the other cases
% described by the asymmetric conflict relation arise.
%
% :- 2{exe(X) : pre(X,Y)}, b(Y).

% NEW conflict resolutor
%
% we ensure that no cycle of asymmetric conflict is in the
% configuration
%
% :- asymconfl(X,X), exe(X).

% ÜBER NEW conflict resolutor: does not need the closure of the
% asymmetric conflict relation!
%
% Works by marking with naac the maximal - by the asymmetric conflict
% relation - events in the configuration and then recursively marking
% all the other events following such relation, unless we reach an
% event which is already marked. In which case we find a cycle of
% asymmetric conflict.
%
% nacc = No Asymmetric Conflict Cycle
naex(X) :- nacc(X).
naex(X) :- nexe(X).
nacc(X) :- exe(X), naex(Y) : asymconfl(Y,X).
% We thus exclude all those configurations where an event is not
% marked with nacc
:- not nacc(X), exe(X).

% This rule creates the marking for the configuration: condition X is
% marked if its preset Y event has been marked as executed and none of
% the events in its postset have been marked.
mark(X) :- exe(Y) : post(Y,X), not exe(Z) : pre(Z,X), b(X).

% DEADLOCK
%
% Essentially the idea here is to remove from the stable models every
% model where there exist two sets of marked conditions Y and Z which
% are respectively the preset and the context of an event X that could
% fire.
%
% This rule has been modified adding the constraint mark(Z) :
% read(Y,Z) which enforces the fact that events can be executed only
% when read conditions are marked.
%
:- mark(Y) : pre(X,Y), mark(Z) : read(X,Z), e(X).

% REACHABILITY
%
% Is the problem of finding whether a marking m is reachable from the
% original marking. We do this by removing all the possible models
% where a place in X is not marked.
%
% Here we define the desired marking m:
%
% m(1;6).
%
% This rule marks the places in the original c-net whose image is
% marked in the unfolding
%
% s_mark(Y) :- mark(X), s(X,Y).
%
% Then we hide all the possible models where m is not marked
%
% :- not s_mark(X), m(X).

#hide.
#show exe/1.
#show mark/1.