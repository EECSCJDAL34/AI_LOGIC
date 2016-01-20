/**************************************************************************************
EECS 3401 INTRODUCION TO ARTIFICIAL INTELLIGENCE AND LOGIC PROGRAMMING
FALL 2015 - GUNNAR GOTSHALKS 
										
REPORT 3
									
CAMILLO JOHN (CJ) D'ALIMONTE   (212754396 cjdal34)
DINESH KALIA 		       (213273420 dinesh49)

APPENDIX: This report was jointly authored. We both worked on the outline of each 
	  question together as a pair. The programming of the predicates was jointly
	  done. Dinesh wrote the documentation and comments for each predicate while 
	  CJ wrote the test cases for each question.
**************************************************************************************/

:- style_check(-singleton).   % Silently Compile without Singleton Warnings

/***
EXTERNAL SOURCE: 
http://www.swi-prolog.org/FAQ/SingletonVar.html
***/

/*******************************************************************************
EXERCISE 1

The predicate cousins_acc(Name1, Name2, P, Q) serves as an alternative solution to the
cousins problem defined in EXERCISE 6 of REPORT 1. The predicate asserts that Name1 and
Name2 are P'th cousins Q'th removed. If the common ancestor is too close or there is no
common ancestor then return P = Q = -1. In this specific variation of the problem, we will
find two paths from the root to the names using the predicate pathFromRoot_acc(Name, Path).
This predicate utilizes an accumulator to find such paths. Once two paths are found, we must
remove the common ancestors from the two paths using the removeCommonPrefix(Path1, Path2, 
Unique1, Unique2) predicate. In order to find P and Q, we will use the left over values of
Unique1 and Unique2. 

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

cousins_acc(N1,N2,P,Q) :- 
    ( pathFromRoot_acc(N1,P1),  % get path1 from root using accumulator
	  pathFromRoot_acc(N2,P2),  % get path2 from root using accumulator
	  removeCommonPrefix(P1,P2,A,B),    % removes common ancestor(s)           
      length(A,L1),length(B,L2),    % finds lengths of uniquePath 1 & 2                             
    (                                                          
          ((0 is L1 ; 0 is L2) , Q is -1 , P is Q , !) ;  
		  % if empty then they are not cousins, CUT   
          ((L1 >= L2 , P is L2 , Q is L1 - L2 ,!);   
		  % p = min(L1,L2) , Q = |L1- L2|         
          (L2 >= L1 , P is L1 , Q is L2 - L1,!))
		  % p = min(L1,L2) , Q = |L2- L1|		  
    )) ; Q is -1, P is Q, !. 
		  % if empty then they are not cousins, CUT  
		  
/****************************
pathFromRoot_acc(Name,Path) uses an accumulator to find the path from the 
root of the tree to the names of the descendants.
****************************/
		  
pathFromRoot_acc(Name,Path):- pathFromRoot_acc(Name,Path,[]).  % sets accumulator = []
pathFromRoot_acc(leila,Path,Path).   % leila is the root of the tree                           
pathFromRoot_acc(Name,Path,AccP):-
parent(Parent,Name),        % find the parent of the current name       
pathFromRoot_acc(Parent,Path, [Parent|AccP]),!.  
% append name to accumulator, CUT for unique solution

/****************************
removeCommonPrefix(Path1,Path2,Unique1, Unique2) removes the common ancestor from
both paths in the family tree.
****************************/

removeCommonPrefix(P1,[],P1,[]):-!.  % if one list is empty then return unique solution                                 
removeCommonPrefix([],P2,[],P2):-!.  % if one list is empty then return unique solution                              
removeCommonPrefix([A|P1],[A|P2],U1,U2):-
removeCommonPrefix(P1,P2,U1,U2),!.   
% if the prefix are equal on both, do not set unique paths
removeCommonPrefix([A|P1],[B|P2],[A|P1],[B|P2]):-!.  
% if prefixes are different, set unique paths and CUT

/***
Test Cases: 
Test the program on both valid and invalid query inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/ 

:- begin_tests(exercise1).
test(no_common_ancestor_or_ancestor_is_too_close, [nondet]) 
:- cousins_acc(jesse, jesse, -1, -1).
test(name_not_defined_in_facts, [nondet]) :- cousins_acc(jimmy, jimmy, -1, -1).
test(name_is_empty_list, [nondet]) :- cousins_acc([],[], -1, -1).
test(name_is_anonymous_variable, [nondet]) :- cousins_acc([_],[_], -1, -1).
test(one_valid_name_and_one_invalid_name, [nondet]) :- cousins_acc(dallas, joe, -1, -1).
test(valid_cousin_input_test_one, [nondet]) :- cousins_acc(thomas, farah, 1, 0).
test(valid_cousin_input_test_two, [nondet]) :- cousins_acc(thomas, zack, 1, 2).
test(valid_cousin_input_test_three, [nondet]) :- cousins_acc(thomas, nikolay, 2, 1).
test(valid_cousin_input_test_four, [nondet]) :- cousins_acc(thomas, saul, 3, 0).
test(valid_cousin_input_test_five, [nondet]) :- cousins_acc(wei, dallas, 2, 0).
test(path_accumulator_1) :- pathFromRoot_acc(thomas, P), write(P), nl.
test(path_accumulator_2) :- pathFromRoot_acc(saul, P), write(P), nl.
test(path_accumulator_3) :- pathFromRoot_acc(ignat, P), write(P), nl.
test(path_accumulator_4, [fail]) :- pathFromRoot_acc(dinesh, P), write(P), nl.
test(write_test) :- 
            P1 = [leila,min,jesse,mustafa],
            P2 = [leila,seema,zack,nikolay],
            removeCommonPrefix(P1, P2, U1, U2), 
            write(P1), nl,
            write(P2), nl, 
            write(U1), nl,
            write(U2), nl.
:- end_tests(exercise1).
	
/*******************************************************************************
EXERCISE 2

The predicate cousins_hole(Name1, Name2, P, Q) serves as an alternative solution to the
cousins problem defined in EXERCISE 6 of REPORT 1 and EXERCISE 2 of REPORT 3. The predicate
asserts that Name1 and Name2 are P'th cousins Q'th removed. If the common ancestor is too
close or there is no common ancestor then return P = Q = -1. In this specific variation of
the problem, we will find two paths from the root to the names using a different predicate 
pathFromRoot_hole(Name, Path). This predicate utilizes a hole to find the paths. Once two
paths are found, we must remove the common ancestors from the two paths using the 
removeCommonPrefix(Path1, Path2, Unique1, Unique2) predicate. In order to find P and Q, we
will use the left over values of Unique1 and Unique2. 

Require: Two individual names with a p value >= 1 and a q value >=0
Ensure: Name1 and Name2 are the P'th cousins Q'th removed.

******************************************************************************/		

%% NOTE: SOME PREDICATES DEFINED IN EXERCISE 1 ARE USED IN EXERCISE 2 (ie: removeCommonPrefix)

cousins_hole(N1,N2,P,Q) :- 
    ( pathFromRoot_hole(N1,P1),  % get path1 from root using hole
	  pathFromRoot_hole(N2,P2),  % get path2 from root using hole
	  removeCommonPrefix(P1,P2,A,B),    % removes common ancestor(s)           
      length(A,L1),length(B,L2),    % finds lengths of uniquePath 1 & 2                             
    (                                                          
          ((0 is L1 ; 0 is L2) , Q is -1 , P is Q , !) ;  
		  % if empty then they are not cousins, CUT   
          ((L1 >= L2 , P is L2 , Q is L1 - L2 ,!);   
		  % p = min(L1,L2) , Q = |L1- L2|         
          (L2 >= L1 , P is L1 , Q is L2 - L1,!))
		  % p = min(L1,L2) , Q = |L2- L1|		  
    )) ; Q is -1, P is Q, !. 
		  % if empty then they are not cousins, CUT 
	  
/****************************
pathFromRoot_hole(Name,Path) uses a hole to find the path from the 
root of the tree to the names of the descendants.
****************************/		  
		  
pathFromRoot_hole(Name, Hole) :- pathFromRoot_hole(Name, Hole, []). % sets hole = []
pathFromRoot_hole(leila, Hole, Hole).  % leila is the root of the tree
pathFromRoot_hole(Name, Hole, Acc) :-  
    parent(Parent, Name),        % find the parent of the current name  
    insert(Parent, Acc, NewAcc),
    pathFromRoot_hole(Parent, Hole, NewAcc),!. 
insert(Item, Hole, [Item | Hole]).
% append name to hole, CUT for unique solution

/***
Test Cases: 
Test the program on both valid and invalid query inputs. We focused on testing the base
cases and its respective boundary cases. We also focused on creating an exhaustive
list of test cases on correct and incorrect inputs. The test cases were used throughout
development to determine if any logical errors were present in our program. Every test
case below passes.
***/

:- begin_tests(exercise2).
test(no_common_ancestor_or_ancestor_is_too_close, [nondet]) 
:- cousins_hole(jesse, jesse, -1, -1).
test(name_not_defined_in_facts, [nondet]) :- cousins_hole(jimmy, jimmy, -1, -1).
test(name_is_empty_list, [nondet]) :- cousins_hole([],[], -1, -1).
test(name_is_anonymous_variable, [nondet]) :- cousins_hole([_],[_], -1, -1).
test(one_valid_name_and_one_invalid_name, [nondet]) :- cousins_hole(dallas, joe, -1, -1).
test(valid_cousin_input_test_one, [nondet]) :- cousins_hole(thomas, farah, 1, 0).
test(valid_cousin_input_test_two, [nondet]) :- cousins_hole(thomas, zack, 1, 2).
test(valid_cousin_input_test_three, [nondet]) :- cousins_hole(thomas, nikolay, 2, 1).
test(valid_cousin_input_test_four, [nondet]) :- cousins_hole(thomas, saul, 3, 0).
test(valid_cousin_input_test_five, [nondet]) :- cousins_hole(wei, dallas, 2, 0).
test(path_hole_1) :- pathFromRoot_hole(thomas, P), write(P), nl.
test(path_hole_2) :- pathFromRoot_hole(saul, P), write(P), nl.
test(path_hole_3) :- pathFromRoot_hole(ignat, P), write(P), nl.
test(path_hole_4, [fail]) :- pathFromRoot_hole(dinesh, P), write(P), nl.
test(write_test) :- 
            P1 = [leila,min,jesse,mustafa],
            P2 = [leila,seema,zack,nikolay],
            removeCommonPrefix(P1, P2, U1, U2), 
            write(P1), nl,
            write(P2), nl, 
            write(U1), nl,
            write(U2), nl.
:- end_tests(exercise2).

/*******************************************************************************
EXERCISE 3

For this exercise, we use the predicate CHAT defined in CHATBASIS.pl to define, parse,
and respond to clauses to handle different types of statements described in the
REPORT 3 SPECIFICATIONS. We utilize Prolog's built in DCG translator in order to 
"simulate" a more English like program in Prolog. 
******************************************************************************/	

/*******************************************************************************
          Chat Basis --- a program to make Prolog input more English like.
 
          A project from Clocksin and Mellish, page 244 third edition.

               Original author is Peter Roosen-Runge
               Modified by Gunnar Gotshalks, CJ D'Alimonte, and
			   Dinesh Kalia.

*******************************************************************************
This file has the chat predicate and support predicates that are in common to
most chat programs.  A separate file should be created that contains the parse,
respondTo and splitOff prediates, and any other support predicates needed for
your program.  The other file should consult this one.

*******************************************************************************
The following predicate is the main rule of the program and is simply queried
with  :- chat.
 
The rule repeats itself until the user enters exactly 'Stop.'.
There are two versions.  chat calls parse where the parse rules are written
in DCG.  chat_t calls parse_t where the parse_t rules are the Prolog
translations of the corresponding parse rules.
*******************************************************************************/

chat :- repeat
        , readLine(Sentence)
        , parse(Clause, Sentence, _)
        , respondTo(Clause)
        , Clause = stop , ! .

chat_t :- repeat
        , readLine(Sentence)
        , parse_t(Clause, Sentence, _)
        , respondTo(Clause)
        , Clause = stop , ! .
 
/*******************************************************************************
              Input support predicates

Program input is handled through the readLine predicate and is support
predicates.

readLine(Sentence)
-- asserts that Sentence is a list of atoms entered at the terminal by the
user.  First it reads and stores the input words in the variable Words, as
a list of sublists of characters encoded in ASCII.  Second, it splits off
'.'s, ',', and "'s"s from the end of each word, converts the ASCII encoded
words into atoms.
*******************************************************************************/
 
readLine(Sentence) :- readCharLists(Words) 
                    , morphs(Words,Sentence) , ! .

/*******************************************************************************
                       Input of WORDS from the terminal

readCharLists(Words)
-- asserts that Words is a list of sublists of characters encoded in ASCII.
Each word terminates in either a space character (32), newline (10 -- return)
or Enter (31).  The entire list is terminated by either Enter or Return.

The predicate is similar to Clocksin and Mellish, page 100..103, third edition,
but using a different approach.
*******************************************************************************/

readCharLists([Word|MoreWords]) :- readWord(Word,TC)
                                 , (   ( TC=10        /* Return key, newline */
                                       ; TC=31)       /* Enter key */
                                     , MoreWords=[]
                                   ; readCharLists(MoreWords) ).


/*******
readWord(Word, TC)
-- Word is a list of ASCII codes corresponding to a word read from the terminal.
-- TC is the character code terminating the word.
*******/

readWord(Word, TC) :-  get0(C)         /* Read a character from the terminal */
                    , ( (  C=10        /* Check for end of line. */
                         ; C=32        /* Check for blank. */
                         ; C=31 )      /* Check for enter key */
                      , TC=C , Word=[] /* TC implies empty word */

   /* Have a normal character */

                      ;   readWord(MoreCharacters, TC)
                        , Word=[C|MoreCharacters] ).

/*******************************************************************************
                               MORPHS

Convert list of words (as ASCII encoded lists for each word, for example from
readCharLists, to lists of atoms, applying morphological rules to split off
punctuation and the possessive "'s".
*******************************************************************************/


/******
morphs(WordList, AtomList)
-- Wordlist is list of words where each word is a list of ASCII codes.
-- AtomList is the list of corresponding atoms where word lists have punctuation
such as '.' ',' and 's split off (depending upon the splitOff predicates) as
separate atoms.
******/
 
morphs([],[]).
morphs([Word|MoreWords], Morphs) :- morph(Word, Morph)
                                  , morphs(MoreWords, MoreMorphs)
                                  , append(Morph, MoreMorphs, Morphs).

/******
morph(Word, Atoms)
-- Word is one list of ASCII codes
-- Atoms is the list of either the Word converted to an atom, or, if the Word
can be split, a list of the two component "sub-atoms".
******/

morph([],[]) .
morph(W,L) :- morphrules(W,X) , maplist(name,L,X) .

/******
morphrules(CharList, ComponentLists)
-- CharList is a single list of characters encoded in ASCII.
-- ComponentLists is either CharList or CharList split into two sub-lists
depending upon the predicate SplitOff.
******/

morphrules(CharList, ComponentLists) :- ( append(X,Y,CharList)
                                        , splitOff(Y)
                                        , ComponentLists=[X,Y] )
                                      ; ComponentLists=[CharList] .

/*******************************************************************************
Supporting rules for the parse predicates.
*******************************************************************************/
 
/*******
thing(Name)
-- Asserts that Name is the next word in the input Sentence and Name begins
with a capital letter.

type(T)
-- Asserts that T the next word in the input Sentence and T does not begin
with a capital letter.

captial(Name)
-- Asserts that Name begins with a captial letter.
********/
 
thing(Name) --> [Name] , { capital(Name) } .

type(T) --> [T] , { not(capital(T)) } .

capital(Name) :- name(Name,[F|_]) , F < 96 .


/*******
isVowel(T)
-- Asserts that T the next word in the input Sentence and T begins with 
a vowel
*****/

isVowel(T) --> [T], { vowel(T) } .


/*******
notVowel(T)
-- Asserts that T the next word in the input Sentence and T does not
begin with a vowel 
*****/

notVowel(T) --> [T], { not(vowel(T)) } .


/*******
Define a vowel as the following characters: a,e,i,o,u.
The numerical values (ie: 97, 101, etc.) are the corresponding
ASCII values for each vowel character..

This predicate takes a word from the user's input and determines if
the first character is a vowel. The use of the vowel predicate is used
to determine if one is to use "a" or "an" before the noun. (ie: an owl,
a dog, etc.)
******/

vowel(N) :- name(N, [First | _]),
	    (First is 97;  %a
	    First is 101;  %e
	    First is 105;  %i
	    First is 111;  %o
	    First is 117). %u
		
/***
EXTERNAL SOURCE: 
http://www.theasciicode.com.ar/ascii-printable-characters/
capital-letter-z-uppercase-ascii-code-90.html

***/


/*******
lowercase(U,L)
-- Convert character to lower case by using ASCII values 
*****/

lowercase([], []).  % BASE CASE
lowercase(U, L) :-
name(U,[H|T]), H < 91, H > 64, J is H + 32, name(L,[J|T]).
%Convert upper case character to lower case  by adding 32  to the
%character's ASCII value (only focus on first character - HEAD of list)

/*******
uppercase(L,U)
-- Convert character to upper case by using ASCII values 
*****/

uppercase([], []).   % BASE CASE
uppercase(L, U) :-
name(L,[H|T]), H > 96, H < 122, J is H - 32, name(U,[J|T]).
%Convert lower case character to upper case  by subtracting 32  to the
%character's ASCII value (only focus on first character - HEAD of list)

plural([],[]). % BASE CASE
plural(Word1,Word2):-concat(Word1,'s',Word2).
% if plural, then add an s to the end of Word1


p_to_s(P, S) :- name(P, L1),
					reverse(L1, [First | Rest]),
					reverse(Rest, L2), % reverses rest of list
					name(S, L2). %start with P, end with S

output([]).
output([H|T]) :- write(H),  write(' has pets'), nl, output(T).

headonly([],[]). % BASE CASE
headonly(H,[H|T]). % SEPERATE HEAD FROM TAIL
					
					
/*******
CUSTOM MEMBER PREDICATE
member ( X , [ X | _ ] ).
member ( X , [ _ | RL ] )  :-  member ( X , RL ).

GOT SOME ASSISTANCE ON THIS QUESTION

EXTERNAL SOURCE
http://stackoverflow.com/questions/2984104/prolog-beginner-how-to-make-
unique-values-for-each-variable-in-a-predicate
*****/

memberchanged(N,[N|T]). % BASE CASE
memberchanged(N,[X|T]):-memberchanged(N,T). % Disregard X, N must be in T

make_unique([],[]). % BASE CASE
make_unique([H|T],Y):-memberchanged(H,T),!,make_unique(T,Y).
% If H is member of T, CUT; if not, call on small input (T,Y)
make_unique([H|T],[H|Y]):-make_unique(T,Y).
% Since H is in both lists, recursively call on small input (T,Y)
		
/******************************************************************************/
/******** Apply a predicate to every pair from two lists
Closely related to the "map" functions in Lisp (e.g. mapcar).
maplist(P,L1,L2)  P is the predicate to be applied to pairs from L1 and l2.

maplist is a built-in function in SWI-Prolog.  The following is its definition.
********/

% maplist(_,[],[]).
% maplist(P,[X|L],[Y|M]) :- Q =.. [P,X,Y] , call(Q) , maplist(P,L,M) .

/******************************************************************************/

/******
PROVIDED TO US!
parse(Clause, Sentence, Left-over)
-- Clause is the encoded result of parsing the input sentence.
-- Sentence is a list that matches the prefix of the input.
-- Left_over is what is permitted after the prefix has been parsed.

The first rule, in Prolog, produces the result 'stop' if the input matches 'Stop.'
with nothing left over.  This rule is used to terminate the chat program.
******/

parse(stop, [ 'Stop', '.' ], []) .

/******
The second, third and fourth rules are a clauses in a "Definitive Clause
Grammar" that are translated into standard Prolog.

The second rule matches sentences of the type  Mary is a woman.  The result
Clause is the predicate  T(Name)  formed from the values of T and Name. 
******/
 
parse(Clause) --> thing(Name) , [ is , a ] , type(T) , ['.']
                , { Clause =.. [ T , Name ] , ! } .

/******
The third rule matches sentences of the type  A woman is a person.  The result
Clause is the rule  T2(X) :- T1(X)  formed from the values of T1, T2 and the
variable X.  
******/
 
parse(Clause) --> ['A'] , type(T1) , [ is , a ] , type(T2) , ['.']
                , { Head =.. [T2, X] , Condition =.. [ T1, X ]
                  , Clause = (Head :- Condition) ,! } .

/******
The fourth rule matches sentences of the type  Is Mary a person?  The result
Clause is the structure  ?(T(Name))  formed from the values of T and Name.
******/
 
parse(Clause) --> ['Is'] , thing(Name) , [ a ] , type(T) , ['?']
                , { Goal =.. [T, Name] , Clause = ('?'(Goal)) , ! } .
				
				
				
/******
The fifth rule matches sentences of the type Alice has an owl for a pet.
The rule also checks to see if the next word after "an" begins with a vowel.  
The resulting Clause is the predicate  T2(Name, T) OR Object2(Name,Object1) formed 
from the values of T, T2 and Name. 
******/
 
parse(Clause) --> thing(Name) , [ has , an ] , isVowel(T) , [ for, a] , type(T2), ['.']
                , { Clause =.. [ T2 , Name , T] , ! } .
				
				%IS_VOWEL COMPARED TO NOTVOWEL BELOW!!!!


/******
The sixth rule matches sentences of the type Alice has a cat for a pet. 
The rule also checks to see if the next word after "an" does NOT begin with a vowel.  
The resulting Clause is the predicate  T2(Name, T) OR Object2(Name,Object1) formed 
from the values of T, T2 and Name. 
******/
 
parse(Clause) --> thing(Name) , [ has , a ] , notVowel(T) , [ for, a] , type(T2), ['.']
                , { Clause =.. [ T2 , Name, T] , ! } .



/* How many object(s) does the person have?*/
%PROTOTYPE SENTENCE 2
parse(Clause) --> ['How'], ['many'], type(T1), ['does'], 
thing(Name), ['have'], ['?'],
{ p_to_s(T1, Search), Goal =.. [ Search , Name, Object ],
Clause =('?-'('How many Object does',Goal)), !}.
%P to S depends on if there is more than 1 Object....



/* Who has object?*/
%PROTOTYPE SENTENCE 3
 parse(Clause) --> ['Who'], ['has'], type(T), ['?'],
{ p_to_s(T, Search), Goal =.. [ Search , Who, Object], 
Clause =('?-'('Who has object',Goal)),!}. 				
%P to S depends on whether someone owns more than 1 Object....
				
/******
The last rule, in Prolog, is the catch all rule indicating that none of the
previous parse rules succeeded in matching the input.  The result is 'noparse'
independent of the value of Sentence and Left_over.
******/

parse(noparse, _, _) .
 
/*******************************************************************************
                  SplitOff rules for parsing.
In the example period, '.', comma, ',' and possesive, "'s" are split off the
end of input words.
*******************************************************************************/
	splitOff(ApostropheS) :- name("'s", ApostropheS).
   splitOff(QuestionMark) :- name("?", QuestionMark).
   splitOff(Period) :- name(".", Period).

/*******************************************************************************
Set of rules to notify the user of what the parse rules did.
*******************************************************************************/

/******
If the result of parsing is the structure  ?(Goal) and if the query Goal
succeeds, then write the sentence  Yes A1 is a F.  Otherwise write No to
indicate the Goal failed.
******/

respondTo('?'(Goal)) :- Goal =.. [F,A1] ,
                       ( Goal -> write('Yes, ') , write(A1) , write(' is a '),
                                 write(F), write('.')
                         ; write('No') )
                       , ! , nl , nl.

/******
IF RESULT IS OF THE STRUCTURE ?('How many Object does', Goal) AND IF QUERY SUCCEEDS
THEN WRITE SENTENCE OUTPUTTING NAME HAS SOME NUMBER OF PETS.
ELSE-----FAIL
******/

respondTo('?-'('How many Object does', Goal)) :- 
Goal =.. [T1, Name, Object],  Newgoal =..[findall, Object, Goal, List], 
(Newgoal ->  List \= [], length(List,Num),  write(Name), write(' has '), write(Num), write(' pets.');
write(' has no pets.') ), 
! , nl , nl.  

%findall predicate provided in the report specification!

/******
IF RESULT IS OF THE STRUCTURE ?('Who has object', Goal) AND IF QUERY SUCCEEDS
THEN WRITE SENTENCE OUTPUTTING NAME HAS PETS.
ELSE-----FAIL
******/
respondTo('?-'('Who has object', Goal)) :- 
Goal =.. [T, Who, Object], Newgoal =..[findall, Who, Goal, List],
(Newgoal-> List \= [],  make_unique(List,UniqList), output(UniqList); 
write('No One') ), 
! , nl , nl.
%show all unique solutions (formed through make_unique)

%findall predicate provided in the report specification!
%findall(V,P,L)

/******
PROVIDED TO US!!!!!
If the result of parsing is noparse, then inform the user that the input was
not parsable.
******/
 
respondTo(noparse) :- write('Can''t parse that.' ) , nl , ! .

/******
If the result of parsing is stop, then tell the user the program is terminating.
******/
 
respondTo(stop) :- write('All done.') , nl , ! .

/******
The last rule is the catch all for the example that inserts the clause at the
beginning of the database and informs the user.
******/
 
respondTo(Clause) :- asserta(Clause) , write('Ok') , nl , ! .
					   
/*******************************************************************************
EXERCISE 4

In EXERCISE 3, we had utilized Prolog's built-in DCG translator to parse the input sentences.
In this exercise, we had to replace all of the DCG parse clauses with our own translation of
those clauses in Prolog (ie: we had to remove all --> operators). In order to accomplish this,
we use the predicate chat_t in chatbasis.pl that invokes the parse_t predicate instead of the
parse predicate. The output recorded in this exercise should be the same as EXERCISE 3.
******************************************************************************/		
/**THE CODE CONTAINS COMPILATION ERRORS AND HENCE HAS BEEN COMMENTED OUT! ***/
/* 
/******
parse_t(Clause, Sentence, Left-over)
-- Clause is the encoded result of parsing the input sentence.
-- Sentence is a list that matches the prefix of the input.
-- Left_over is what is permitted after the prefix has been parsed.

The first rule, in Prolog, produces the result 'stop' if the input matches 'Stop.'
with nothing left over.  This rule is used to terminate the chat program.
******/

parse_t(stop, [ 'Stop', '.' ], []) .

/******
The second, third and fourth rules are a clauses in a "Definitive Clause
Grammar" that are translated into standard Prolog.

The second rule matches sentences of the type  Mary is a woman.  The result
Clause is the predicate  T(Name)  formed from the values of T and Name.
******/
 
               

parse_t(Clause, S, Srem) :-     % thing(Name), [is,a], type(T), ['.'].
                thing(Name, S, S0),
                det1(S0, S1),
                type(T, S1, S2),
                det2(S2, Srem).
           
% thing(Name) --> [Name].
thing(Name, S, Srem) :- det3(S, Srem).

% type(T) --> [T].
type(T, S, Srem) :- det4(S, Srem).

det1([is, a] | St], St).
det2(['.'] | St], St).
det3([Name | St], St).
det4([T | St], St).
           

/******
The third rule matches sentences of the type  A woman is a person.  The result
Clause is the rule  T2(X) :- T1(X)  formed from the values of T1, T2 and the
variable X. 
******/
         
 parse_t(Clause, S, Srem) :-     % ['A'] , type(T1) , [ is , a ] , type(T2) , ['.']
                det33(S0, S1),
                type(T1, S1, S2),
                det34(S2, S3),
                type(T2, S3, S4),
                det37(S4, S5).
               
type(T1, S, Srem) :- det35(S, Srem).

% type(T2) --> [T2].
type(T2, S, Srem) :- det36(S, Srem).

det33(['A'] | St], St).
det34([is, a] | St], St).
det35([T1 | St], St).
det36([T2 | St], St).
det37(['.'] | St], St).

/******
The fourth rule matches sentences of the type  Is Mary a person?  The result
Clause is the structure  ?(T(Name))  formed from the values of T and Name.
******/

parse_t(Clause, S, Srem) :-     % Is thing(Name), [a], type(T), ['?'].
                det5(S0, S1),
                thing(Name, S, S1),
                det6(S1, S2),
                type(T, S2, S3),
                det7(S3, Srem).
               
% thing(Name) --> [Name].
thing(Name, S, Srem) :- det8(S, Srem).

% type(T) --> [T].
type(T, S, Srem) :- det9(S, Srem).

det5(['Is'] | St], St).
det6([a] | St], St).
det7(['?'] | St], St).
det8([Name | St], St).
det9([T | St], St).

/******
The fifth rule matches sentences of the type Alice has an owl for a pet.
and also check to make sure that the next word after "an" is a vowel. 
The result Clause is the predicate  T2(Name, T)  or Object2(Name,Object1) formed
from the values of T, T2 and Name.
******/

parse_t(Clause, S, Srem) :-     % thing(Name), [has,an], isVowel(T), [for, a], type(T2), ['.'].
                thing(Name, S, S0),
                det10(S0, S1),
                isVowel(T, S1, S2),
                det11(S2, Srem),
                type(T2, S2, S3),
                det14(S3, Srem).
               
% thing(Name) --> [Name].
thing(Name, S, Srem) :- det12(S, Srem).

% isVowel(T) --> [T].
isVowel(T, S, Srem) :- det13(S, Srem).

% type(T2) --> [T2].
type(T2, S, Srem) :- det15(S, Srem).

det10([has, an] | St], St).
det11([for, a] | St], St).
det12([Name | St], St).
det13([T | St], St).
det14(['.'] | St], St).
det15([T2 | St], St). */