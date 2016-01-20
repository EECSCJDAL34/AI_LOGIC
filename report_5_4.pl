/**************************************************************************************
EECS 3401 INTRODUCION TO ARTIFICIAL INTELLIGENCE AND LOGIC PROGRAMMING
FALL 2015 - GUNNAR GOTSHALKS 
          
REPORT 5 - EXERCISE 4
         
CAMILLO JOHN (CJ) D'ALIMONTE   (212754396 cjdal34)
DINESH KALIA                   (213273420 dinesh49)
**************************************************************************************/
/***
EXTERNAL SOURCE: 
http://www.swi-prolog.org/FAQ/SingletonVar.html
***/

:- style_check(-singleton).   % Silently Compile without Singleton Warnings

:- [bayesianNet_multistate].

%%%%%%%%SPECIFICATION%%%%%%%%

parent(prizeDoor, doorShown).    % prizeDoor --> doorShown
parent(doorPicked, doorShown).   % doorPicked --> doorShown

%%%%%%PROBABILITIES%%%%%%%%%%%%
% All probabilities were directly stated in the given tables OR
% were deduced given other related statistics

p(prizeDoor=red, 0.333).
p(prizeDoor=blue, 0.333).
p(prizeDoor=green, 0.333).
p(doorPicked=red, 0.333).
p(doorPicked=blue, 0.333).
p(doorPicked=green, 0.333).

p(doorShown=red, [prizeDoor=red, doorPicked=red], 0.0).
p(doorShown=red, [prizeDoor=red, doorPicked=blue],  0.0).
p(doorShown=red, [prizeDoor=red,  doorPicked=green], 0.0).
p(doorShown=blue, [prizeDoor=red, doorPicked=red], 0.5).
p(doorShown=blue, [prizeDoor=red, doorPicked=blue],  0.0).
p(doorShown=blue, [prizeDoor=red,  doorPicked=green], 0.0).
p(doorShown=green, [prizeDoor=red, doorPicked=red], 0.5).
p(doorShown=green, [prizeDoor=red, doorPicked=blue],  1.0).
p(doorShown=green, [prizeDoor=red,  doorPicked=green], 0.0).

p(doorShown=red, [prizeDoor=blue, doorPicked=red], 0.0).
p(doorShown=red, [prizeDoor=blue, doorPicked=blue],  0.5).
p(doorShown=red, [prizeDoor=blue,  doorPicked=green], 1.0).
p(doorShown=blue, [prizeDoor=blue, doorPicked=red], 0.0).
p(doorShown=blue, [prizeDoor=blue, doorPicked=blue],  0.0).
p(doorShown=blue, [prizeDoor=blue,  doorPicked=green], 0.0).
p(doorShown=green, [prizeDoor=blue, doorPicked=red], 1.0).
p(doorShown=green, [prizeDoor=blue, doorPicked=blue],  0.5).
p(doorShown=green, [prizeDoor=blue,  doorPicked=green], 0.0).

p(doorShown=red, [prizeDoor=green, doorPicked=red], 0.0).
p(doorShown=red, [prizeDoor=green, doorPicked=blue],  1.0).
p(doorShown=red, [prizeDoor=green,  doorPicked=green], 0.5).
p(doorShown=blue, [prizeDoor=green, doorPicked=red], 1.0).
p(doorShown=blue, [prizeDoor=green, doorPicked=blue],  0.0).
p(doorShown=blue, [prizeDoor=green,  doorPicked=green], 0.5).
p(doorShown=green, [prizeDoor=green, doorPicked=red], 0.0).
p(doorShown=green, [prizeDoor=green, doorPicked=blue],  0.0).
p(doorShown=green, [prizeDoor=green,  doorPicked=green], 0.0).

%%%RUN OUTPUT OF PROBABILITIES%%%
run(e4) :-
    prob(prizeDoor=green, [ doorShown=blue, doorPicked=red], Prob1), nl,
    write('P( prizeDoor=green | doorShown=blue, doorPicked=red ) = '), write(Prob1).