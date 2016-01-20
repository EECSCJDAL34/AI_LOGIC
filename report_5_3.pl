/**************************************************************************************
EECS 3401 INTRODUCION TO ARTIFICIAL INTELLIGENCE AND LOGIC PROGRAMMING
FALL 2015 - GUNNAR GOTSHALKS 
          
REPORT 5 - EXERCISE 3
         
CAMILLO JOHN (CJ) D'ALIMONTE   (212754396 cjdal34)
DINESH KALIA                   (213273420 dinesh49)
**************************************************************************************/
/***
EXTERNAL SOURCE: 
http://www.swi-prolog.org/FAQ/SingletonVar.html
***/

:- style_check(-singleton).   % Silently Compile without Singleton Warnings

:- ['f16_4_BayesianNet.pl'].

%%%%%%%%SPECIFICATION%%%%%%%%

parent(asia, tuberculosis).    				 % Asia --> tuberculosis
parent(smoker, lungCancer).    				 % smoker --> lungCancer
parent(smoker2, bronchitis).   				 % smoker2 --> bronchitis
parent(tuberculosis, tuberculosisOrCancer).  % tuberculosis --> tuberculosisOrCancer
parent(lungCancer, tuberculosisOrCancer).    % lungCancer --> tuberculosis
parent(tuberculosisOrCancer, positiveXray).  % tuberculosisOrCancer --> positiveXray
parent(tuberculosisOrCancer, dyspnoea).      % tuberculosisOrCancer --> dyspnoea
parent(bronchitis, dyspnoea).		    	 % bronchitis --> dyspnoea

%%%%%%PROBABILITIES%%%%%%%%%%%%
% All probabilities were directly stated in the given tables OR
% were deduced given other related statistics
p(asia, 0.01).
p(smoker, 0.5).
p(smoker2, 0.5).
p(tuberculosis, [asia], 0.05).
p(tuberculosis, [~asia], 0.01).
p(lungCancer, [smoker], 0.1).
p(lungCancer, [~smoker], 0.01).
p(bronchitis, [smoker2], 0.6).
p(bronchitis, [~smoker2], 0.3).
p(tuberculosisOrCancer, [tuberculosis, lungCancer], 1).
p(tuberculosisOrCancer, [tuberculosis, ~lungCancer], 1).
p(tuberculosisOrCancer, [~tuberculosis, lungCancer], 1).
p(tuberculosisOrCancer, [~tuberculosis, ~lungCancer], 0.0).
p(positiveXray, [tuberculosisOrCancer], 0.98).
p(positiveXray, [~tuberculosisOrCancer], 0.05).
p(dyspnoea, [tuberculosisOrCancer, bronchitis], 0.9).
p(dyspnoea, [tuberculosisOrCancer, ~bronchitis], 0.7).
p(dyspnoea, [~tuberculosisOrCancer, bronchitis], 0.8).
p(dyspnoea, [~tuberculosisOrCancer, ~bronchitis], 0.1).

%%%RUN OUTPUT OF PROBABILITIES%%%
run(e3) :-
prob([ dyspnoea], [ ], Probability1),
prob([ smoker], [ tuberculosisOrCancer], Probability2),
write('P( dyspnoea ) = '), write(Probability1), nl,
write('P( smoker | tuberculosis_or_cancer ) = '), write(Probability2).
%Output the probabilities that were asked for in the report specification.