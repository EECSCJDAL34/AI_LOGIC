/**************************************************************************************
EECS 3401 INTRODUCION TO ARTIFICIAL INTELLIGENCE AND LOGIC PROGRAMMING
FALL 2015 - GUNNAR GOTSHALKS 
          
REPORT 5 - EXERCISE 1
         
CAMILLO JOHN (CJ) D'ALIMONTE   (212754396 cjdal34)
DINESH KALIA                   (213273420 dinesh49)
**************************************************************************************/
/***
EXTERNAL SOURCE: 
http://www.swi-prolog.org/FAQ/SingletonVar.html
***/

:- style_check(-singleton).   % Silently Compile without Singleton Warnings

:- [framesQuery].

% The top of the hierarchy is the figure. 

% figure (attribute, value) 
figure(type_of, 
	execute(type_of(Object, Value), Object, Value)).

% Second level of hierarchy 
four_sided_polygon(a_kind_of, figure).

/*
Execute is the functor of a compound term used to 
distinguish a literal slot value and a value that needs
to be computed.
*/

% Attribute: boundary_length
% Execute function of sum_of_side_lengths
four_sided_polygon(boundary_length,
	execute(sum_of_side_lengths(Object, Value), Object, Value)).
% Attribute: area
% Execute function of four_sided_area	
four_sided_polygon(area,
	execute(four_sided_area(Object, Value), Object, Value)).
% 4 sides to a four_sided_polygon	
four_sided_polygon(side_count, 4).

% Add all side lengths using the sum predicate
sum_of_side_lengths(Object, Value) :- 
	value(Object, side_length, Side_length),
	sum(Side_length, Value).

sum([], 0). % Base Case
sum([F | R], Value) :- 
	sum(R, Sum_R), 
	Value is F + Sum_R.	
% Recursive Case
% Sum of 2nd element to Tail + 1st element = TotalSum


% Calculate the area of the different types of
% four sided shapes (rectangle, parallelogram,
% and trapezoid)
four_sided_area(Object, Value) :-
	( % Object is a rectangle or a parallelogram
		parent(Object, P), 
		(P = 'rectangle' ; P = 'parallelogram'),
		value(Object, height, Height),
		value(Object, side_length, [Side_1|_]),
		% Area = L*W
		Value is Height*Side_1
	),!
	;
	( % Object is a trapezoid
		parent(Object, P), 
		P = 'trapezoid',
		value(Object, height, Height),
		value(Object, side_length, [Side_1, _, Side_3|_]),
		% Area = ((A+B)/2)*H
		Value is (Side_3+Side_1)*Height/2
	).

% 2nd level of hierarchy 
% Attribute: boundary_length
% Execute function of side_length_x_side_count
regular_polygon(a_kind_of, figure).
regular_polygon(boundary_length, 
	execute(side_length_x_side_count(Object, Value), Object, Value)).


side_length_x_side_count(Object, Value) :-
	value(Object, side_count, Side_count),
	value(Object, side_length, Side_length),
	Value is Side_length*Side_count.


% 3rd level of hierarchy
% No attributes nor any hardcoded value	
% Descendant of four_sided_polygon
trapezoid(a_kind_of, four_sided_polygon).

% 3rd level of hierarchy
% No attributes nor any hardcoded value	
% Descendant of four_sided_polygon
parallelogram(a_kind_of, four_sided_polygon).

% 4th level of hierarchy
% No attributes nor any hardcoded value	
% Descendant of parallelogram
rectangle(a_kind_of, parallelogram).


% 3rd level of hierarchy
% Descendant of regular_polygon
% Execute function eq_triangle_area for area
% 3 sides to a triangle
eq_triangle(a_kind_of, regular_polygon).
eq_triangle(side_count, 3).
eq_triangle(area, execute(eq_triangle_area(Object, Value), Object, Value)).

% 3rd level of hierarchy
% Descendant of regular_polygon
% Execute function square_area for area
% 4 sides to a square
square(a_kind_of, regular_polygon).
square(side_count, 4).
square(area, execute(square_area(Object, Value), Object, Value)).

% 3rd level of hierarchy
% Descendant of regular_polygon
% Execute function pentagon_area for area
% 5 sides to a pentagon
pentagon(a_kind_of, regular_polygon).
pentagon(side_count, 5).
pentagon(area, execute(pentagon_area(Object, Value), Object, Value)).

% Area = sqrt(3)/4* a^2
eq_triangle_area(Object, Value):-
	value(Object, side_length, Side_length),
	Value is sqrt(3)*Side_length*Side_length / 4.

% Area = l*w	
square_area(Object, Value) :- 
	value(Object, side_length, Side_length),
	Value is Side_length*Side_length.

% Area = 1/4 sqrt(5+(5+2 sqrt()5)*a^2	
pentagon_area(Object, Value) :- 
	value(Object, side_length, Side_length),
	Value is sqrt(5*(5+2*sqrt(5)))*Side_length*Side_length/4.

% Attribute : Side length = 6
eq_triangle_1(instance_of, eq_triangle).
eq_triangle_1(side_length, 6).	

% Attribute : Side length = 4
square_1(instance_of, square).
square_1(side_length, 4).

% Attribute : Side length = 9
pentagon_1(instance_of, pentagon).
pentagon_1(side_length, 9).

% Set lengths of sides and height
% Two pairs of equal sides	
rectangle_1(a_kind_of, rectangle).
rectangle_1(side_length, [8,3,8,3]).
rectangle_1(height, 3).

% Set lengths of sides and height
% Two pairs of parallel sides		
parallelogram_1(instance_of, parallelogram).
parallelogram_1(side_length, [10,5,10,5]).
parallelogram_1(height, 2).

% Set lengths of sides and height
% Sides are parallel
trapezoid_1(instance_of, trapezoid).
trapezoid_1(side_length, [9,4,7,4]).
trapezoid_1(height, 3).

% TEST CASES
:- begin_tests(e1).
test(square) :- value(square_1, area, 16).
test(equilateraltriangle) :- value(eq_triangle_1, area, 15.588457268119896).
test(pentagon) :- value(pentagon_1, area, 139.35866944770632).
test(parallelogram) :- value(parallelogram_1, area, 20).
test(rectangle) :- value(rectangle_1, area, 24).
test(trapezoid) :- value(trapezoid_1, area, 24).
:- end_tests(e1).