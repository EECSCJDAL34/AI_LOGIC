/*******************************************************************************
Chapter 16 - Bratko

Figure 16.4  An interpreter for Bayesian networks.

Belief network is represented by relations:
   parent(ParentNode, Node)
   p(Node, ParentStates, Prob)
     where Prob is conditional probability of Node given
     values of parent variables ParentStates, for example:
     p(alarm, [ burglary, ~ earthquake], 0.99)
   p(Node, Prob)
     probability of node without parents

Query: prob(Event, Condition, Probability).
prob(burgalary, [call], P).
prob(burgalary, [call, lightning], P).
prob(burgalary, [call, ~lightning], P).

NOTE: Need to use mydelete instead of builtin delete.
builtin delete (is deprecated) has arguments in a different order and in the
context of this program returns Item as a list.
    delete(FromList, Item, ResultList)
    mydelete(Item, FromList, ResultList)

********************************************************************************
prob(Event, Condition, P):
    probability of Event, given Cond, is P;
    Event is a variable, its negation,
        or a list of events representing their conjunction
*******************************************************************************/

:-  op(900, fy, ~).             % Pefix operator not

percentProb(Event, Condition, Probability) :-
    prob(Event, Condition, P) , Probability is round(100000*P)/1000.

prob([X | Xs], Cond, P) :-  !,  % Probability of conjunction
    prob(X, Cond, Px),
    prob(Xs, [X | Cond], PRest),
    P is Px * PRest.

prob([], _, 1) :- !.            % Empty conjunction

prob(X, Cond, 1) :-
    member(X, Cond), !.         % Cond implies X

prob(X, Cond, 0) :-             % Cond implies X is false
    member(~ X, Cond), !.

prob(~ X, Cond, P) :- !,        % Probability of negation
    prob(X, Cond, P0),
    P is 1 - P0.

% Use Bayes rule if condition involves a descendant of X

prob(X, Cond0, P) :-
    mydelete(Y, Cond0, Cond),
    predecessor(X, Y), !,       % Y is a descendant of X
    prob(X, Cond, Px),
    prob(Y, [X | Cond], PyGivenX),
    prob(Y, Cond, Py),
    P is Px * PyGivenX / Py.    % Assuming Py > 0

% Cases when condition does not involve a descendant of X.

prob(X, _, P) :-                % X a root cause - its probability given
    p(X, P), !.

% Cases when condition involves parents of X.

prob(X, Cond, P) :-  !,
    findall((CONDi,Pi), p(X,CONDi,Pi), CPlist),
    sum_probs(CPlist, Cond, P).

/*******************************************************************************
sum_probs(CondsProbs, Cond, WeigthedSum)
    CondsProbs is a list of conditions and corresponding probabilities,
    WeightedSum is weighted sum of probabilities of Conds given Cond
*******************************************************************************/

sum_probs([], _, 0).

sum_probs([ (COND1,P1) | CondsProbs], COND, P) :-
    prob(COND1, COND, PC1),
    sum_probs(CondsProbs, COND, PRest),
    P is P1 * PC1 + PRest.

/*************************************/

predecessor(X, ~ Y) :- !,       % Negated variable Y
    predecessor(X, Y).

predecessor(X, Y) :-
    parent(X, Y).

predecessor(X, Z) :-
    parent(X, Y),
    predecessor(Y, Z).

/*************************************/
% Definition of delete as in slides.

mydelete( X, [X | L], L).

mydelete( X, [Y | L], [Y | L2])  :-
    mydelete( X, L, L2).
