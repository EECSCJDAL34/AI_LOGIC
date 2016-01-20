/*******************************************************************************
          Chat Basis --- a program to make Prolog input more English like.
 
          A project from Clocksin and Mellish, page 244 third edition.

               Original author is Peter Roosen-Runge
               Modified by Gunnar Gotshalks.

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
 

/******************************************************************************/
/******** Apply a predicate to every pair from two lists
Closely related to the "map" functions in Lisp (e.g. mapcar).
maplist(P,L1,L2)  P is the predicate to be applied to pairs from L1 and l2.

maplist is a built-in function in SWI-Prolog.  The following is its definition.
********/

% maplist(_,[],[]).
% maplist(P,[X|L],[Y|M]) :- Q =.. [P,X,Y] , call(Q) , maplist(P,L,M) .
