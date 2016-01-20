/**************************************************************************************
EECS 3401 INTRODUCION TO ARTIFICIAL INTELLIGENCE AND LOGIC PROGRAMMING
FALL 2015 - GUNNAR GOTSHALKS 
										
REPORT 1
									
CAMILLO JOHN (CJ) D'ALIMONTE   (212754396 cjdal34)
DINESH KALIA 		       (213273420 dinesh49)

APPENDIX: This report was jointly authored. We both worked on the outline of each 
	  question together as a pair. The programming of the predicates was jointly
	  done. Dinesh wrote the documentation and comments for each predicate while 
	  CJ wrote the test cases for each question.
**************************************************************************************/

/*************************************************************************************
EXERCISE 1

1. An atomic sentence that is entailed by the knowledge base (KB) is "Bay is east of 
   Spadina."

2. "Spadina is right of Bathurst" is not entailed in the KB because neither the set of 
   sentences nor the set of conditionals is written in the form "X is right of Y." The 
   set of atomic sentences in the KB are in relation to X's position left or west of 
   Y's position, thus X is right of Y is not entailed in the KB.
	
3. The 6 backward-chaining steps defined in the Report Specification BACKGROUND will be
   used throughout each sub-question. Step 1 is already completed as a KB has already 
   been defined in the BACKGROUND.

a) 

GOAL 1: "Spadina is west of St. George"
Step 2: Let's define our goal/sentence as Q. 
Step 3: Try to locate Q explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.
	If X is left of Y then X is west of Y. (Let Spadina = X; St. George = Y)
	If Spadina is left of St. George then Spadina is west of St. George.
Step 5: Using backward-chaining, try to establish S1 (Spadina is left of St.George).

GOAL 2: "Spadina is left of St. George"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> SUCCESS
	Return TRUE; Nowhere to backtrack
		
Given the conditional if S1, then Q, we have established that S1 is entailed in the KB
and hence is TRUE. Therefore, we can conclude the statement Q, our original goal is 
also entailed in the KB and hence is also TRUE. 
		
b)

GOAL 1: "Yonge is east of Bay"
Step 2: Let's define our goal/sentence as Q. 
Step 3: Try to locate Q explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.
	If X is west of Y then Y is east of X. (Let Bay = X; Yonge = Y)
	If Bay is west of Yonge then Yonge is east of Bay.
Step 5: Using backward-chaining, try to establish S1 (Bay is west of Yonge).

GOAL 2: "Bay is west of Yonge"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1' then S1 has been found.
	If X is left of Y then X is west of Y. (Let Bay = X; Yonge = Y)
	If Bay is left of Yonge then Bay is west of Yonge.
Step 5: Using backward-chaining, try to establish S1' (Bay is left of Yonge).

GOAL 3: "Bay is left of Yonge"
Step 2: Let's define our goal/sentence as S1'. 
Step 3: Try to locate S1' explicitly in the KB. --> SUCCESS
	Return TRUE; Nowhere to backtrack
		
Given the conditional if S1', then S1, we have established that S1' is entailed in 
the KB and hence is TRUE. Therefore, we can conclude that the statement S1, is also 
entailed in the KB and hence is also TRUE. If both S1' and S1 are TRUE and by the 
conditional, if S1 then Q, we can conclude that Q, our original goal is TRUE and 
hence is entailed in the KB. 

c)

GOAL 1: "Christie is west of Spadina"
Step 2: Let's define our goal/sentence as Q. 
Step 3: Try to locate Q explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.
	If X is left of Y then X is west of Y. (Let Christie = X; Spadina = Y)
	If Christie is left of Spadina then Christie is west of Spadina.
Step 5: Using backward-chaining, try to establish S1 (Christie is left of Spadina).

GOAL 2: "Christie is left of Spadina"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 1!

GOAL 1: "Christie is west of Spadina"
Step 4:	Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 and S2 then Q has been found.	
	If X is left of Y and Y is west of Z then X is west of Z. 
	(Let Christie = X; Bathurst = Y, Spadina = Z)
	If Christie is left of Bathurst and Bathurst is west of Spadina then Christie 
	is west of Spadina.
Step 5: Using backward-chaining, try to establish S1 (Christie is left of Bathurst).

GOAL 3: "Christie is left of Bathurst"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> SUCCESS
	Return TRUE; Backtrack to Goal 1 and try to establish S2 (Bathurst is west 
	of Spadina).
		
GOAL 4: "Bathurst is West of Spadina"
Step 2: Let's define our goal/sentence as S2. 
Step 3: Try to locate S2 explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S2' then S2 has been found.
	If X is left of Y then X is west of Y. (Let Bathurst = X; Spadina = Y)
	If Bathurst is left of Spadina then Bathurst is west of Spadina.
Step 5: Using backward-chaining, try to establish S2' (Bathurst is left of Spadina).

GOAL 5: "Bathurst is left of Spadina"
Step 2: Let's define our goal/sentence as S2'. 
Step 3: Try to locate S2' explicitly in the KB. --> SUCCESS
	Return TRUE; Nowhere to backtrack.

Given the conditional if S2', then S2, we have established that S2' is entailed in the
KB and hence is TRUE. Therefore, we can conclude that the statement S2, is also 
entailed in the KB and hence is also TRUE. S1 was explicitly stated in the KB and hence
is entailed in the KB and TRUE. If both S1 and S2 are TRUE and by the conditional, if 
S1 and S2 then Q, we can conclude that Q, our original goal is TRUE and hence is 
entailed in the KB. 

d) 

GOAL 1: "Yonge is west of Yonge"
Step 2: Let's define our goal/sentence as Q. 
Step 3: Try to locate Q explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.
	If X is left of Y then X is west of Y. (Let Yonge = X; Yonge = Y)
	If Yonge is left of Yonge then Yonge is west of Yonge.
Step 5: Using backward-chaining, try to establish S1 (Yonge is left of Yonge).

GOAL 2: "Yonge is left of Yonge"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 1!
		
GOAL 1: "Yonge is west of Yonge"
Step 4:	Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 and S2 then Q has been found.	
	If X is left of Y and Y is west of Z then X is west of Z. 
	(Let Yonge = X; _ = Y, Yonge = Z)
	If Yonge is left of _ and _ is west of Yonge then Yonge is
	west of Yonge.		
Step 5: Using backward-chaining, try to establish S1 (Yonge is left of _).

GOAL 3: "Yonge is left of _"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 1!

GOAL 1: "Yonge is west of Yonge"
Step 4:	Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.	
	If X is west of Y then Y is east of X. 
	(Let Yonge = X; Yonge = Y)
	If Yonge is west of Yonge, then Yonge is east of Yonge. --> FAIL
	CAN'T BACKTRACK AND THUS CAN'T PROVE YONGE IS EAST OF YONGE AS IT'S
	NOT ENTAILED IN THE KB.
	Nowhere to backtrack; return FALSE
		
Since there is no conditional that proves S1 is entailed in the KB, then we
can conclude that S1 is false. Since S1 is false, we can also conclude that 
Q is false as well and thus, is NOT entailed in the KB.

e)

GOAL 1: "Bay is west of Sherborne"
Step 2: Let's define our goal/sentence as Q. 
Step 3: Try to locate Q explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.
	If X is left of Y then X is west of Y. (Let Bay = X; Sherborne = Y)
	If Bay is left of Sherborne then Bay is west of Sherborne.
Step 5: Using backward-chaining, try to establish S1 (Bay is left of Sherborne).

GOAL 2: "Bay is left of Sherborne"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 1!
		
GOAL 1: "Bay is west of Sherborne"
Step 4:	Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 and S2 then Q has been found.	
	If X is left of Y and Y is west of Z then X is west of Z. 
	(Let Bay = X; Yonge = Y, Sherborne = Z)
	If Bay is left of Yonge and Yonge is west of Sherborne then Bay is
	west of Sherborne.		
Step 5: Using backward-chaining, try to establish S1 (Bay is left of Yonge).

GOAL 2: "Bay is left of Yonge"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> SUCCESS
	Return TRUE; Backtrack and try to establish S2 (Yonge is west of Sherborne).

GOAL 3: "Yonge is west of Sherborne"
Step 2: Let's define our goal/sentence as S2. 
Step 3: Try to locate S2 explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S2 then Q has been found.
	If X is left of Y then X is west of Y. (Let Yonge = X; Sherborne = Y)
	If Yonge is left of Sherborne then Yonge is west of Sherborne.
Step 5: Using backward-chaining, try to establish S2' (Yonge is left of Sherborne).	

GOAL 4: "Yonge is left of Sherborne"
Step 2: Let's define our goal/sentence as S2'. 
Step 3: Try to locate S2' explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 3!

GOAL 3: "Yonge is west of Sherborne"
Step 2: Let's define our goal/sentence as S2. 
Step 3: Try to locate S2 explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S2 and S2'then Q has been found.
	If X is left of Y and Y is west of Z then X is west of Z. 
	(Let Yonge = X; Yonge = _, Sherborne = Z)
	If Yonge is left of _ and _ is west of Sherborne then Yonge is
	west of Sherborne.		
Step 5: Using backward-chaining, try to establish S2' (Yonge is left of _).

GOAL 5: "Yonge is left of _"
Step 2: Let's define our goal/sentence as S2'. 
Step 3: Try to locate S2' explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 3!
		
GOAL 3: "Yonge is west of Sherborne"
Step 2: Let's define our goal/sentence as S2. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 1!	

GOAL 1: "Bay is west of Sherborne"
Step 2: Let's define our goal/sentence as Q. 
Step 3: Try to locate Q explicitly in the KB. --> FAIL
Step 4: Try to locate an untried conditional statement. --> SUCCESS
	CHANGE VARIABLE Y = Yonge to Y = _ where _ \= Yonge
	"If bay is left of _ and _ is west of Sherborne then bay is
	west of Sherborne"
Step 5: Using backward-chaining, try to establish S1 (Bay is left of _).

GOAL 6: "Bay is left of _"
Step 2: Let's define our goal/sentence as S1. 
Step 3: Try to locate S1 explicitly in the KB. --> FAIL	
Step 4: Try to locate an untried conditional statement. --> FAIL
Step 6: Continue iterating through all conditionals in Step 4 --> FAIL
	Since no conditionals can be found, BACKTRACK TO GOAL 1!	
		
GOAL 1: "Bay is west of Sherborne"
Step 4:	Try to locate an untried conditional statement. --> SUCCESS
	A conditional statement of the form if S1 then Q has been found.	
	If X is west of Y then Y is east of X. 
	(Let Bay = X; Sherborne = Y)
	If Bay is west of Sherborne, then Sherborne is east of Bay. --> FAIL
	CAN'T BACKTRACK AND THUS CAN'T PROVE SHERBORNE IS EAST OF BAY AS IT'S
	NOT ENTAILED IN THE KB.
	Nowhere to backtrack; return FALSE
		
Since there is no conditional that proves S1 is entailed in the KB, then we
can conclude that S1 is false. Since S1 is false, we can also conclude that 
Q is false as well and thus, is NOT entailed in the KB.		

4. If we change the second conditional to "If X is west of Y and Y is left of
   Z then X is west of Z," then there will not be a change in what is entailed in the
   KB. The backward-chaining of the query in 3d will enter an infinite loop. It will 
   reach the second conditional and attempt to evaluate "Yonge is west of _" which 
   ultimately leads back to the same conditional where the process is repeated 
   infinitely. 
   
5. If the incorrect atomic sentence "Yonge is left of Bay" were added to the KB, then
   the KB would entail "Spadina is west of X" for the following values of X: 
   - Bay
   - Yonge
   - St. George
   The backward-chaining of the query in 3e will enter an infinite loop if the change
   was made. It will reach the second conditional and attempt to evaluate "Bay is left
   of _". The _ is filled with YONGE. Next, it will attempt to evaluate "Yonge is west
   of Sherborne" which once again leads to the second conditional. It will try to 
   evaluate "Yonge is left of _" with Bay filling the _. This circular pattern
   continues from the beginning and thus, proves that we have entered an infinite loop.

***********************************************************************************/

/***********************************************************************************
EXERCISE 2

The predicate equalPairs(List) asserts that given an arbitrary list with an even number
of components, forall odd-indexed (j), the jth element = jth + 1 element thus forming 
an equal pair. For all even-indexed (j), the jth element \= jth + 1 element thus 
asserting that a pair of identical elements doesn't occur twice in a row.

Require: List is a valid list.
Ensure: forall j : 1.. List-1 | odd(j) ~ List(j) = List(j+1)
	forall j : 1.. List-2 | even(j) ~ List(j) \= List (j+1)

**********************************************************************************/

/***
Base Case: 
A list containing two equivalent elements is a valid equal pair 
***/

equalPairs([X,Y]):- X = Y.

/***
Recursive Case: 
Given that the initial two elements form an equal pair (X=Y), then proceed to check if 
the rest of the list is valid. The remaining elements must not be empty (i.e. [X,Y,[]])
or contain invalid arbitrary elements (i.e. [X,Y, _] with _ consisting of invalid 
ordering). If these cases are satisfied, then run equalPairs on the remaining part of 
the list.
***/

equalPairs([X,Y|Rest]):- Rest \= [Y|_], Rest \= [],equalPairs([X,Y]), equalPairs(Rest).

/***
Test Cases: 
Test the program on both valid and invalid list inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/

:- begin_tests(exercise2tests).
test(no_equal_pair_in_empty_list, [fail]) :- equalPairs([]).
test(single_element_is_not_an_equal_pair, [fail]) :- equalPairs([a]).
test(a_single_pair_with_equal_empty_elements_is_valid, [nondet]) 
:- equalPairs([[], []]).
test(a_single_pair_with_equal_elements_is_valid, [nondet]) 
:- equalPairs([a, a]).
test(two_distinct_elements_is_not_an_equal_pair, [fail]) 
:- equalPairs([a, b]).
test(odd_list_with_irregular_pattern_not_valid, [fail]) 
:- equalPairs([a, b, b]).
test(odd_list_is_not_an_equal_pair, [fail]) :- equalPairs([a, a, b]).
test(multiple_distinct_elements_is_not_an_equal_pair, [fail]) 
:- equalPairs([a, b, c, d, e]).
test(multiple_paired_elements_is_a_valid_equal_pair, [nondet]) 
:- equalPairs([a, a, b, b, c, c, d, d, e, e, f, f]).
test(multiple_same_pairs_in_a_row_is_not_an_equal_pair, [fail]) 
:- equalPairs([a, a, a, a, b, b]).
test(equal_pairs_not_in_a_consecutive_row_is_an_equal_pair, [nondet]) 
:- equalPairs([a, a, b, b, a, a, b, b]).
test(equal_pairs_not_in_a_consecutive_row_is_an_equal_pair_two, [nondet])
:- equalPairs([hello, hello, world, world, hello, hello, world, world]).
test(inner_list_with_same_elements_is_valid, [nondet]) 
:- equalPairs([[a], [a]]).
test(inner_list_with_different_elements_is_not_valid, [fail]) 
:- equalPairs([[a,b], [a,b,c]]).
test(inner_list_with_empty_list_is_valid, [nondet]) 
:- equalPairs([[a,b,[]], [a,b,[]]]).
test(inner_list_with_distinct_and_empty_list_is_not_valid, [fail]) 
:- equalPairs([[a,a, _], [a,b,_]]).
test(inner_list_with__even_number_of_empty_lists_with_odd_elements, [nondet]) 
:- equalPairs( [ [ [], [], [] ], [ [], [], [] ]  ] ).
test(inner_list_with__odd_number_of_empty_lists_is_notvalid, [fail]) 
:- equalPairs( [ [ [], [], [] ], [ [], [], [] ], [ [], [], [] ]  ] ).
test(a_single_pair_with_equal_numbers_is_valid, [nondet]) 
:- equalPairs([34, 34]).
test(two_distinct_numbers_is_not_an_equal_pair, [fail]) 
:- equalPairs([7, 9]).
test(odd_list_with_irregular_pattern_of_numbers_is_not_valid, [fail])
:- equalPairs([56, 12, 12]).
:- end_tests(exercise2tests).

/*******************************************************************************
EXERCISE 3

The predicate middle (List, Mid) asserts that Mid is the middle item of the odd length 
list List. Middle is false if List is of even length. The predicates append (L1, L2, R)
and oneShorter (L1, L2) are used in support of middle(List, Mid).

Require: List is a valid list.
Ensure: Mid is the middle item of an odd length List and that the Mid element is 
	located at List (#List/2+ 1)
		
EXTERNAL SOURCE:  http://stackoverflow.com/questions/10580905/even-sized-list-in-prolog		
********************************************************************************/

/***
Base Case: 
An empty list is shorter than a list with an arbitrary element 
***/

oneShorter([], [_|[]]).

/***
Recursive Case: 
Given that the head of each list is arbitrary, consider the respective tails of each 
list (i.e. TailOne and TailTwo). Run oneShorter(TailOne, TailTwo) to determine which 
list is shorter. 
***/

oneShorter([_|TailOne], [_|TailTwo]) :- oneShorter(TailOne, TailTwo).

/***
Base Case: 
A list consisting of only one element will have a middle equal to that specific element
***/

middle([Mid], Mid).

/***
Recursive Case: 
Given an arbitrary head, consider the tail and the provided Mid. Create a sub-list of 
the tail (ie Sub) and attach an anonymous variable to the end of Sub(i.e. Sub + _ = 
Tail). Since both the head and the tail's end are both anonymous, the size of the 
list has been reduced by two (one on each end). Since the Mid hasn't changed, we know 
that the Mid of the List is still equal to the Mid of the new sub-list (Sub) and thus 
Sub is shorter than Tail. 
***/ 
				
middle([_|Tail], Mid) :- append(Sub,[_],Tail), middle(Sub, Mid), oneShorter(Sub, Tail).

/***
Test Cases: 
Test the program on both valid and invalid list inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/

:- begin_tests(exercise3tests).
test(middle_of_empty_list_is_not_of_odd_length, [fail]) 
:- middle([], []).
test(middle_of_anonymous_is_anonymous, [nondet]) 
:- middle([_], [_]).
test(middle_of_a_single_element_is_that_element, [nondet]) 
:- middle([a], a).
test(middle_of_two_elements_is_not_valid, [fail]) :- middle([a,b], a).
test(middle_of_a_multiple_elements_of_odd_length_test_one, [nondet]) 
:- middle([a,b,c,d,e,f,g], d).
test(middle_of_a_multiple_elements_of_odd_length_test_two, [nondet]) 
:- middle([1,2,56,5675,23,0,45353,67,78], 23).
test(middle_of_a_multiple_elements_of_odd_length_test_three, [nondet]) 
:- middle([a,b,67,34,56,f, 1], 34).
test(middle_of_a_multiple_elements_of_odd_length_test_four, [nondet]) 
:- middle([a,[a,b,c],67,t,[_],56,f, [],u], [_]).
test(middle_of_a_multiple_elements_of_odd_length_test_five, [nondet]) 
:- middle([[a,1,4,[]],b,[z,z,2,z],[c,a],[1]], [z,z,2,z]).
test(middle_of_a_multiple_elements_of_odd_length_test_six, [nondet]) 
:- middle([[a,b,c,d,d,b,b,a]], [a,b,c,d,d,b,b,a]).
test(middle_of_a_multiple_elements_of_even_length_test_one, [fail]) 
:- middle([a,b,c,d,d,b,b,a], [d,d]).
test(middle_of_a_multiple_elements_of_even_length_test_two, [fail]) 
:- middle([a,b,c,d,d,b,b,a], d).
test(middle_of_a_multiple_elements_of_even_length_test_three, [fail]) 
:- middle([[a,b,c,d,d,b,b,a], d,e,f], [a,b,c,d,d,b,b,a]).
test(middle_of_a_multiple_elements_of_even_length_test_four, [fail]) 
:- middle([[a,b,c,45,[], u],[34,a,z,z,[_]]], [[a,b,c,45,[], u],[34,a,z,z,[_]]]).
:- end_tests(exercise3tests).

/*******************************************************************************
EXERCISE 4

The predicate middles(List, Middles) asserts that Middles is the list of middle 
elements, as defined by EXERCISE 3 for each odd-length list at the top level of 
List. List contains any item at the top level. 

Require: List is a valid list.
Ensure: EXERCISE 3's definition of middle is used; The middle element of an 
	element is located at List (#List/2+ 1)
		
********************************************************************************/
/***
Base Case: 
A list consisting of only an empty list will have a base case of an empty list.
***/

middles([],[]).

/***
Recursive Case: 
If the List has more than one sub-list, we check if the first item or head is of
type list. Then, we use recursive check for other sub-lists in our top level list.
***/ 

middles([Head|Rest], Middles) :- 
Head = [_|_], (middle(Head, M), Middles = [M|Mr],
middles(Rest, Mr); middles(Rest, Middles)).

middles([Head|Rest], Middles) :- Head \= [_|_], middles(Rest, Middles).

/***
Test Cases: 
Test the program on both valid and invalid list inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/

:- begin_tests(exercise4tests).
test(middle_of_empty_list_is_empty, [nondet]) :- middles([], []).
test(only_one_element_in_one_list, [fail]) :- middles([2], 2).
test(even_number_of_elements_in_list, [fail]) :- middles([3,6], 3).
test(three_elements_in_one_top_level_list, [fail]) :- middles([1,2,5], 2).
test(same_elements_in_sublists, [nondet]) :- middles([[2,2,2], [9,9,9]], [2,9]).
test(even_length_of_sublists, [fail]) :- middles([[7,8],[45,46]], [8,46]).
test(multiple_odd_length_of_sublists_test_one, [nondet]) 
:- middles([[7,8,c,v,9],[56765, a, b,45,46]], [c,b]).
test(multiple_odd_length_of_sublists_test_two, [nondet]) 
:- middles([[7,[],[_],v,9],[56765, a, [],45,zzzzzz]], [[_],[]]).
test(multiple_odd_length_of_sublists_test_three, [nondet]) 
:- middles([[-1,2,v,67,c,v,9],[56765, a, b,bcc,67,45,46]], [67,bcc]).
test(mix_of_odd_and_even_lengths_test_one, [nondet]) 
:- middles([[-4,4],[56765, rrta, abcc,67,45]], [abcc]).
test(mix_of_odd_and_even_lengths_test_two, [nondet]) 
:- middles([[2,a,-4,4],[56765, [____], [],67,abcded]], [[]]).
test(mix_of_odd_and_even_lengths_test_three, [nondet]) 
:- middles([[a], [a], [a],[-4,4],[56765, rrta, abdc,[],45]], [a,a,a,abdc]).
:- end_tests(exercise4tests).

/***
Why do we get multiple answers?: 
We get multiple, different answers (and thus have to use the ; in the query)
because if there are multiple odd-lengths lists inside the top-level list,
the predicate will only return the middle element of the first odd-length
sub-list it visits. For example, if there are 4 sub-lists inside the input list
and only three of them are of odd-length,then we should expect to see three
distinct middle elements but the query result will only output the first
element until we enter the ; command. This is a direct result of Prolog's
backtracking nature. In order to prevent multiple answers, we use cut (!).
***/

/*******************************************************************************
EXERCISE 5

The predicate arith_prog(TheList) asserts that TheList is an arithmetic progression 
containing integers. An arithmetic progression is a sequence of the form r, r+s,
r+2*s, ....., r+n*s for some integer r and some positive integer s. TheList is not 
an arithmetic progression if its size <= 2. TheList will only contain integers.

Require: TheList is a valid list with a size > 2.
Ensure: TheList is an arithmetic progression/sequence of the form r, r+s, r+2*s, 
	....., r+n*s for some integer r and some positive integer s. 

******************************************************************************/

/***
Base Case: 
Since an arithmetic progression must have a size > 2, the base case consists of a list
of size 3. Let A,B,C denote the respective elements of the list. An arithmetic sequence
has a common difference [x(i+1)-x(i) = d where d is the difference], therefore, we must
take the difference of each pair of elements. If the two sets of differences are 
equivalent, we have a valid arithmetic progression.
***/

arith_prog([A,B,C]) :- Diff1 is B-A, Diff2 is C-B, Diff1 = Diff2. 

/***
Recursive Case: 
Consider the remaining tail of the list (define the head as the first 3 elements).
Use the difference calculations defined in the base case to calculate first two 
differences. Recursively call arith_prog([B,C|Tail]). We pass B,C into the predicate
to establish the first difference of the recursive call (i.e. C-B). We pass in the 
remaining, unevaluated part of the list (i.e. Tail). Eventually, after a series of
calls, Tail will be of size [A,B,C] and thus the recursive case will terminate.
***/

arith_prog([A,B,C | Tail]) :- 
Diff1 is B-A, Diff2 is C-B, Diff1 = Diff2, arith_prog([B,C | Tail]). 

/***
Test Cases: 
Test the program on both valid and invalid list inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/

:- begin_tests(exercise5tests).
test(list_size_smaller_than_two, [fail]) :- arith_prog([1]).
test(list_size_equal_to_two, [fail]) :- arith_prog([1,2]).
test(list_size_equal_to_three_with_valid_elements_test_one, [nondet]) 
:- arith_prog([4,7,10]).
test(list_size_equal_to_three_with_valid_elements_test_two, [nondet]) 
:- arith_prog([89,89,89]).
test(list_size_equal_to_three_with_invalid_elements_test_one, [fail]) 
:- arith_prog([2,7,15]).
test(list_size_equal_to_three_with_invalid_elements_test_two, [fail]) 
:- arith_prog([1,1,2]).
test(larger_list_size_with_valid_elements_test_one, [nondet]) 
:- arith_prog([2,5,8,11]).
test(larger_list_size_with_valid_elements_test_two, [nondet]) 
:- arith_prog([36,37,38,39,40,41,42]).
test(larger_list_size_with_valid_elements_test_three, [nondet]) 
:- arith_prog([567,570,573,576,579,582,585,588]).
test(larger_list_size_with_valid_elements_test_four, [nondet]) 
:- arith_prog([-2,0,2,4,6]).
test(larger_list_size_with_valid_elements_test_five, [nondet]) 
:- arith_prog([-8,-7,-6]).
test(larger_list_size_with_invalid_elements_test_one, [fail]) 
:- arith_prog([2,6,22,24]).
test(larger_list_size_with_invalid_elements_test_two, [fail]) 
:- arith_prog([1,1,1,1,2]).
test(larger_list_size_with_invalid_elements_test_three, [fail]) 
:- arith_prog([8,9,10,9,11]).
test(larger_list_size_with_invalid_elements_test_three, [fail]) 
:- arith_prog([-2,-1,1,2,3]).
:- end_tests(exercise5tests).

/*******************************************************************************
EXERCISE 6

The predicate cousins(Name1, Name2, P, Q) asserts that Name1 and Name2 are P'th cousins
Q'th removed. If the common ancestor is too close or there is no common ancestor then 
return P = Q = -1. The predicates ancestor(Person, Ancestor) and distance(Person,
Ancestor, Distance) are used in support.

Require: Two individual names with a p value >= 1 and a q value >=0
Ensure: Name1 and Name2 are the P'th cousins Q'th removed.

******************************************************************************/
/***
Facts: 
The following facts construct the family tree provided in the Report Specification.
All facts are of the form "X is the parent of Y" (i.e.) Leila is the parent of Min.
***/

parent(leila, min).  % X is the parent of Y.
parent(leila, seema).
parent(min, ali).
parent(min, jesse).
parent(min, john).
parent(seema, zack).
parent(ali, sean).
parent(ali, steven).
parent(jesse, dallas).
parent(jesse, mustafa).
parent(zack, kyle).
parent(zack, nikolay).
parent(zack, wei).
parent(sean, ping).
parent(dallas, farah).
parent(mustafa, ignat).
parent(mustafa, thomas).
parent(kyle, william).
parent(nikolay, saul).

/***
Base Case: 
X is the ancestor of Y if X is the parent of Y.
***/

ancestor(Person, Ancestor) :- parent(Ancestor, Person).

/***
Recursive Case: 
X is the ancestor of Y if X is the parent of Z, where Z is an 
ancestor of Y.
***/

ancestor(Person, Ancestor) :- 
parent(Ancestor, Child), ancestor(Person, Child).

/***
Base Case: 
The path length from X to Y is 1 if Y is the parent of X.
***/

distance(Person, Ancestor, 1) :- parent(Ancestor, Person).

/***
Recursive Case: 
Distance is the path length from X to Y, where Y is an
ancestor of X. The path length from X's parent to Y is 1
less than Distance. NOTE: Using Distance2 is Distance + 1 
was giving us errors* so we decided to swap the order and
thus evaluate Distance is Distance2 + 1.
*ERROR: Variable not instantiated.
***/

distance(Person, Ancestor, Distance) :- 
ancestor(Person, Ancestor), parent(Parent, Person),
distance(Parent, Ancestor, Distance2), Distance is Distance2 + 1. 

/***
Base Case: 
If the query names of the cousins are equal (if any form, empty or otherwise),
then no common ancestor can be found. Set the values of P and Q to -1.
***/
										
cousins(Name1, Name1, P, Q) :- P is -1, Q is -1.

/***
Recursive Case: 
Asserts that Name1 and Name2 are p'th cousins q'th removed.
Name1 and Name2 share the same common ancestor.
We define Dist1, Dist2, P, and Q as the following:
|C-A| >= 2 /\ |C-B| >= 2   (Dist1 & Dist2 respectively)
p = minimum (|C-A|,|C-B|) - 1
q = ||C-A|-|C-B||
***/
	
cousins(Name1, Name2, P, Q) :- 
ancestor(Name1, Ancestor), ancestor(Name2, Ancestor),
distance(Name1, Ancestor, D1), distance(Name2, Ancestor, D2), 
Dist1 is abs(D1), Dist1 >= 2, Dist2 is abs(D2), Dist2 >= 2,
P is (min(D1,D2) - 1), Q is abs(Dist1- Dist2).	
							   
/***
Test Cases: 
Test the program on both valid and invalid query inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/						

:- begin_tests(exercise6tests).
test(no_common_ancestor_or_ancestor_is_too_close, [nondet]) 
:- cousins(jesse, jesse, -1, -1).
test(name_not_defined_in_facts, [nondet]) :- cousins(jimmy, jimmy, -1, -1).
test(name_is_empty_list, [nondet]) :- cousins([],[], -1, -1).
test(name_is_anonymous_variable, [nondet]) :- cousins([_],[_], -1, -1).
test(one_valid_name_and_one_invalid_name, [fail]) :- cousins(dallas, joe, -1, -1).
test(valid_cousin_input_test_one, [nondet]) :- cousins(thomas, farah, 1, 0).
test(valid_cousin_input_test_two, [nondet]) :- cousins(thomas, zack, 1, 2).
test(valid_cousin_input_test_three, [nondet]) :- cousins(thomas, nikolay, 2, 1).
test(valid_cousin_input_test_four, [nondet]) :- cousins(thomas, saul, 3, 0).
test(valid_cousin_input_test_five, [nondet]) :- cousins(ping, dallas, 2, 1).
test(valid_cousin_input_test_six, [nondet]) :- cousins(wei, dallas, 2, 0).
test(valid_cousin_input_test_seven, [nondet]) :- cousins(william, saul, 2, 0).
test(valid_cousin_input_test_eight, [nondet]) :- cousins(farah, john, 1, 2).
:- end_tests(exercise6tests).

/***
Why do we get multiple answers?: 
We get multiple, different answers (and thus have to use the ; in the query)
because there is the possibility of multiple cousins sharing one particular
ancestor. In order to prevent multiple answers, we use cut (!).
***/	   