/*******************************************************************************
Chapter 15

Querying a frame pp364..365 Bratko E4.
	A query for a simple value; i.e. not a function call.
    value_simple(Frame, Attribute, Value).
    value_simple(albert, active_at, When).  from bird frames
*******************************************************************************/

% ParentFrame is the next frame up from Frame in the heirarchy.

parent(Frame, ParentFrame) :-
    (Query =.. [Frame, a_kind_of, ParentFrame]
    ;
    Query =.. [Frame, instance_of, ParentFrame]
    ),
    Query.

% This rule succeeds when we reach the frame with the searched for attribute
% is found, in which case return the corresponding value.

value_simple(Frame, Attribute, Value) :-
    Query =.. [Frame, Attribute, Value],
    Query, !.

% This rule takes the search one level in the frame heirarchy.

value_simple(Frame, Attribute, Value) :-
    parent(Frame, ParentFrame),
    value_simple(ParentFrame, Attribute, Value).

/*******************************************************************************

Querying a frame p366 Bratko E4.
  A query that may be a simple value or a function call.
    value(Frame, Attribute, Value).
    value(ross, relative_size, Size).   from bird frames
*******************************************************************************/

% The actual work needs to be done with a 4-tuple, as we need to keep track
% of the Frame from the query, as that frame is the object on which the
% function is called.

value(Frame, Attribute, Value) :- value(Frame, Frame, Attribute, Value).

% This rule succeeds when we find the frame with the searched for attribute,
% in which case return the corresponding value.

value(Frame, SuperFrame, Attribute, Value) :-
    Query =.. [SuperFrame, Attribute, AttributeValue],
    Query,
    process(AttributeValue, Frame, Value), !.

% This rule takes the search one level in the frame heirarchy.

value(Frame, SuperFrame, Attribute, Value) :-
    parent(SuperFrame, ParentSuperFrame),
    value(Frame, ParentSuperFrame, Attribute, Value).

% If the value of an attribute is a compound term with functor execute, then
% this rule excutes the function to determine the value.

process(execute(Goal, Frame, Value), Frame, Value) :- !, Goal.

% This catch all rule is reached if an attribute value is not a function call.

process(Value, _, Value).
