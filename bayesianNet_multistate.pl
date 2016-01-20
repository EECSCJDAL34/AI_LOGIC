/*******************************************************************************
Chapter 16

An interpreter for Bayesian Networks where variables can have more than
two states.

Belief network is represented by relations:
   parent(ParentNode, Node)

   p(Node=State, ParentStates, Prob)
     where Prob is conditional probability of Node given
     values of parent variables ParentStates, for example:
     p(node_1=value_5, [parent_node_1=value_1, parent_node_2=value_2], 0)

   p(Node=State, Prob)
     probability of node without parents

Query: prob(Event, Condition, Probability).
       prob(node_1=value_5, [node_3=value_1, node_2=value_2], P).

********************************************************************************
prob(Event, Condition, P):
    probability of Event, given Cond, is P;
    Event is node=state
        or a list of events representing their conjunction
    Condition is a list of events representing their conjunction
*******************************************************************************/

percentProb(Event, Condition, Probability) :-
    prob(Event, Condition, P) , Probability is round(100000*P)/1000.

prob([X | Xs], Cond, P) :-  !,  % Probability of conjunction
    prob(X, Cond, Px),
    prob(Xs, [X | Cond], PRest),
    P is Px * PRest.

prob([], _, 1) :-  !.           % Empty conjunction

prob(X, Cond, 1) :-
    member(X, Cond), !.         % Cond implies X

prob(X, Cond, 0) :-             % Cond implies X is false
    X =.. [_, K, _] ,
    hasNode(K, Cond) , !.

% Use Bayes rule if condition involves a descendant of X

prob(X, Cond0, P) :-
    mydelete(Y, Cond0, Cond),
    predecessor(X, Y), !,       % Y is a descendant of X
    prob(X, Cond, Px),
    prob(Y, [X | Cond], PyGivenX),
    prob(Y, Cond, Py),
    ( Py = 0,  P = 1, !
    ; 
      P is Px * PyGivenX / Py   % Assuming Py > 0
    ).

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
% Have the original predecessor predicate to deal with the node name in the
% parent structure.

predecessor(X, Y) :-
    parent(X, Y).

predecessor(X, Z) :-
    parent(X, Y),
    predecessor(Y, Z).

% Need to add the following definitions to extract the node name from
% the state, variable=value, pair.

predecessor(X, Y) :-
    X =.. [_,Xf,_], Y =.. [_,Yf, _],
    parent(Xf, Yf).

predecessor(X, Z) :-
    X =.. [_,Xf,_], Z =.. [_,Zf, _],
    parent(Xf, Y),
    predecessor(Y, Zf).

/*************************************/
% Definition of delete as in slides.

mydelete( X, [X | L], L).

mydelete( X, [Y | L], [Y | L2])  :-
    mydelete( X, L, L2).

/*************************************/
/* hasNode(Node, EventList)
Return true if the EventList contains the Node. */

hasNode(K, [F | R]) :-
    F =.. [_, CK, _] ,
    ( K = CK , ! ;
      hasNode(K, R)
    ).
