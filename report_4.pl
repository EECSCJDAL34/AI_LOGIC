/**************************************************************************************
EECS 3401 INTRODUCION TO ARTIFICIAL INTELLIGENCE AND LOGIC PROGRAMMING
FALL 2015 - GUNNAR GOTSHALKS 
          
REPORT 4
         
CAMILLO JOHN (CJ) D'ALIMONTE   (212754396 cjdal34)
DINESH KALIA                   (213273420 dinesh49)

APPENDIX: This report was jointly authored. We both worked on the outline of each 
          question together as a pair. The programming of the predicates was jointly
          done. Dinesh wrote the documentation and comments for each predicate while 
          CJ wrote the test cases for each question.
**************************************************************************************/

/***
EXTERNAL SOURCE: 
http://www.swi-prolog.org/FAQ/SingletonVar.html
***/

:- [f12_3_Astar].
:- [f13_10_RTA].
:- style_check(-singleton).   % Silently Compile without Singleton Warnings



 % run predicate used to find the path from start node to goal node.
run(Algorithm,H_functor,Start,Goal,Path):-  
	
	% if the Algorithm specified is astar then use a_star routine.
	((Algorithm = 'astar'), a_star(H_functor,Start,Goal,Path)) ; 
	
	% if the Algorithm specified is rta, then use rta routune.
    ((Algorithm = 'rta'), rta_alg(H_functor,Start,Goal,Path)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						DECLARING OBSTICLES							   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the obsticles in the xy plane (or in the robots environment).
% obstcale (MinX/MinY, MaxX/MaxY) where MinX/MinY is the bottom
% left corner of the obsticle and MaxX/MaxY is its top right corner.
obstacle(2/1, 6/2).
obstacle(5/5, 6/8).
obstacle(4/3, 4/5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					DISTANCE FUNCTION IMPLEMENTATION				   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Implementing euclidian Distance function.
euclidian(X,Y,X1,Y1,He) :-
    He is sqrt((X-X1)*(X-X1)+(Y-Y1)*(Y-Y1)).

% Implementing Manhattan Distance function.
manhattan(X,Y,X1,Y1,Hm) :-
    Hm is (abs(X-X1) + abs(Y-Y1)).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								ROUTINES							   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create new functors for Hfunc and Goal. Also run rta routine from f13_10_RTA.pl
% hfunc = H_functor, S = Start, G = Goal, P = Path. 
rta_alg(Hfunc, S, G, P) :-

	% using the built-in predicate retract to remove h_functor(_) from the database.
    (retract(h_functor(_)) , 
    
    % using the built-in predicate to create the h_functor(Hfunc) into beginning of database.
    % this is to make appropriate functor as specified in the run() predicate.
	% Either being h_e or h_m
    asserta(h_functor(Hfunc)) , ! ; asserta(h_functor(Hfunc))) ,

	% using the built-in predicate retract to remove goal(_) from the database.
    (retract(goal(_)) , 
    
    % using the built-in predicate to create the goal(Goal) into beginning of database.
    % this is to make appropriate goal functor as specified in the run() predicate.
    asserta(goal(G)) , ! ; asserta(goal(G))) ,

	% call the rta routine.
    rta(S, P).

% Similarly to rta, create functors for Hfunc and Goal. Also run astar routine from f12_3_Astar.pl
% hfunc = H_functor, S = Start, G = Goal, P = Path. 
a_star(Hfunc, S, G, P) :-

	% using the built-in predicate retract to remove h_functor(_) from the database.
    (retract(h_functor(_)) , 
    
    % using the built-in predicate to create the h_functor(Hfunc) into beginning of database.
    % this is to make appropriate functor as specified in the run() predicate.
	% Either being h_e or h_m
    asserta(h_functor(Hfunc)) , ! ; asserta(h_functor(Hfunc))) ,

	% using the built-in predicate retract to remove goal(_) from the database.
    (retract(goal(_)) , 
    
    % using the built-in predicate to create the goal(Goal) into beginning of database.
    % this is to make appropriate goal functor as specified in the run() predicate.
    asserta(goal(G)) , ! ; asserta(goal(G))) ,

	% call the astar routine.
    astar(S, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						CHOOSING DISTANCES  						   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use manhattan distance between the robot location and goal location 
% from its current xy coordinates. It does not matter what direction the
% robot is facing.
% h_m(State, H)
h_m(X/Y/_, H) :-
   
	% no matter what direction goal is facing.
    goal(GoalX/GoalY/_),
	% calling the manhattan function defined above to calculate distance
	% from start coordinates of X and Y to goal coordinates of Gx and Gy. 
    manhattan(X, Y, GoalX, GoalY, H).
    
% Use euclidean distance between the robot location and goal location 
% from its current xy coordinates. It does not matter what direction the
% robot is facing.
% h_e(State, H)
h_e(X/Y/_, H) :-

	% no matter what direction goal is facing.
    goal(GoalX/GoalY/_),
	% calling the euculidian function defined above to calculate distance
	% from start coordinates of X and Y to goal coordinates of Gx and Gy. 
    euclidian(X, Y, GoalX, GoalY, H).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         	MOVEMENT								   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
							     s
					   /	   |	  |	   \
					 north  south  east  west
*/


% In this predicate, s(State, NewState, Cost), robot makes progress towards 
% the goal state by trying all 4 direction; North, South, East and West.
s(Start_X/Start_Y/Start_Dir, End_X/End_Y/End_Dir, Cost) :-

	% make the goal state.
	goal(GoalX/GoalY/_),
	
	( 
		% attempt to go north from current node.
		s_north(Start_X/Start_Y/Start_Dir, GoalX/GoalY/_, End_X/End_Y/End_Dir, Cost)
		
		% OR attempt to go south from current node.
		; s_south(Start_X/Start_Y/Start_Dir, GoalX/GoalY/_, End_X/End_Y/End_Dir, Cost)
		
		% OR attempt to go east from current node.
		; s_east(Start_X/Start_Y/Start_Dir, GoalX/GoalY/_, End_X/End_Y/End_Dir, Cost)
		
		% OR attempt to go west from current node.
		; s_west(Start_X/Start_Y/Start_Dir, GoalX/GoalY/_, End_X/End_Y/End_Dir, Cost)
		
	).
	
							
							%%%%%%%%%%%%%%
							%    South   % 
							%%%%%%%%%%%%%%
							
% In this predicate, s_south(State, NewState, Cost), you check on the Y-plane
% if there's an obsticle to the south of your current state. If there is one, then
% robot tries to move to one of its corners. If there is no obsticle to south, 
% then robot tries to move to an obsticle to the south. Costs are added as robot moves.
% Assume the robot is dimensionless and is able to move between two obisticles 
% if they are right next to each other.

  
  
s_south(Start_X/Start_Y/Start_Dir, _/GoalY/_, End_X/End_Y/End_Dir, Cost):-

	% make sure your not at the goal and its to the south of your current location.
	Start_Y > GoalY,
	
	% checking for obisticle to the South.
	south_obs(Start_X/Start_Y, Minx/_, Maxx/_, Boolvalue),
	
	
	( 
		% if there is no obsticle to the south, then move to south hoping to be close to an obsticle.
		Boolvalue = false,
		findAllSouth(Start_X/Start_Y,GoalY,MaxY),
		End_Y = MaxY, End_X = Start_X, End_Dir = south,
		(Start_Dir \= north, ! , NewCost = 1 ; NewCost = 0),
		Cost is MaxY - GoalY + NewCost
	;
		% if there is an obsticle to the south, then move to one of the 
		% corner of the obsticle in efforts to pass it.
		Boolvalue = true, End_X = Start_X,
		( End_X = Minx, End_Dir = west, Cost is Start_X - Minx + 1
		; End_X = Maxx, End_Dir = east, Cost is Maxx - Start_X + 1
    )
  ).

		
	
							%%%%%%%%%%%%%%
							%    East    % 
							%%%%%%%%%%%%%%
							
% In this predicate, s_east(State, NewState, Cost), you check on the x - plane
% if there's an obsticle to the east of your current state. If there is one, then
% robot tries to move to one of its corners. If there is no obsticle to east, 
% then robot tries to move to an obsticle to the east. Costs are added as robot moves.
% Assume the robot is dimensionless and is able to move between two obisticles 
% if they are right next to each other.
s_east(Start_X/Start_Y/Start_Dir, GoalX/_/_, End_X/End_Y/End_Dir, Cost) :-

	% make sure your not at the goal and its to the east of your current location.
	GoalX > Start_X,

	% checking for obisticle to the east.
	east_obs(Start_X/Start_Y, _/Miny, _/Maxy, Boolvalue),
	
	( 	
		% if there is no obstacle to the east, then move to east hoping to be close to an obsticle.
		Boolvalue = false,
		findAllEast(Start_X/Start_Y,GoalX,MaxX),
		End_X = MaxX, End_Y = Start_Y, End_Dir = east,
		(Start_Dir \= east , !, NewCost = 1 ; NewCost = 0),
		Cost is MaxX - Start_X + NewCost

	;
		% if there is an obsticle to the east, then move to one of the 
		% corner of the obsticle in efforts to pass it.
		Boolvalue = true, End_X = Start_X,
		( End_Y = Miny, End_Dir = south, Cost is Start_Y - Miny + 1
		; End_Y = Maxy, End_Dir = north, Cost is Maxy - Start_Y + 1
    )
  ).
  
							%%%%%%%%%%%%%%
							%    North   % 
							%%%%%%%%%%%%%%
							
% In this predicate, s_north(State, NewState, Cost), you check on the Y-plane
% if there's an obsticle to the north of your current state. If there is one, then
% robot tries to move to one of its corners. If there is no obsticle to north, 
% then robot tries to move to an obsticle to the north. Costs are added as robot moves.
% Assume the robot is dimensionless and is able to move between two obisticles 
% if they are right next to each other.  
  
s_north(FromX/FromY/FromDirection, _/GoalY/_, ToX/ToY/ToDirection, Cost) :-
	
	% make sure your not at the goal and its to the north of your current location.
	GoalY > FromY,
  	% checking for obisticle to the North.

	north_obs(FromX/FromY, Minx/_, Maxx/_, Boolvalue),
  
	( 
		% if there is no obsticle to the north, 
		% then move to north hoping to be close to an obsticle.
		Boolvalue = false,
		findAllNorth(FromX/FromY,GoalY,MinY),
		ToY = MinY, ToX = FromX, ToDirection = north,
		(FromDirection \= south, ! , NewCost = 1 ; NewCost = 0),
		Cost is MinY - FromY + NewCost
	;
		% if there is an obsticle to the north, then move to one of the 
		% corner of the obsticle in efforts to pass it.
		Boolvalue = true, ToY = FromY,
		( ToX = Minx, ToDirection = west, Cost is FromX - Minx + 1
		; ToX = Maxx, ToDirection = east, Cost is Maxx - FromX + 1
    )
  ).

  
							%%%%%%%%%%%%%%
							%    West    % 
							%%%%%%%%%%%%%%
							
% In this predicate, s_west(State, NewState, Cost), you check on the x - plane
% if there's an obsticle to the east of your current state. If there is one, then
% robot tries to move to one of its corners. If there is no obsticle to east, 
% then robot tries to move to an obsticle to the east. Costs are added as robot moves.
% Assume the robot is dimensionless and is able to move between two obisticles 
% if they are right next to each other.
s_west(Start_X/Start_Y/Start_Dir, GoalX/_/_, End_X/End_Y/End_Dir, Cost):-

	% make sure your not at the goal and its to the west of your current location.
	Start_X > GoalX,
	
	% checking for obisticle to the west.
	west_obs(Start_X/Start_Y, _/Miny, _/Maxy, Boolvalue),
	
	( 
		% if there is no obsticle to the west, then move to west hoping to be close to an obsticle.
		Boolvalue = false,
		findAllWest(Start_X/Start_Y,GoalX,MinX),
		End_X = MinX, End_Y = Start_Y, End_Dir = west,
		(Start_Dir \= west, ! , NewCost = 1 ; NewCost = 0),
		Cost is MinX - GoalX + NewCost
	;
		% if there is an obsticle to the west, then move to one of the 
		% corner of the obsticle in efforts to pass it.
		Boolvalue = true, End_X = Start_X,
		( End_Y = Miny, End_Dir = south, Cost is Start_Y - Miny + 1
		; End_Y = Maxy, End_Dir = north, Cost is Maxy - Start_Y + 1
    )
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%							Finding All Obstacles					   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findAllEast(Start_X/Start_Y, GoalX, Mx) :-
	% find all obstacles in the direction of the robot.
	findall(Mx, (obstacle(Mx/My, _/Max_Y), Start_X < Mx, My < Start_Y, Start_Y < Max_Y), Foundx),
	% use built in predicate min_member() to sum up the list of obstacles in the direction.
	min_member(Mx, [GoalX |Foundx]).    

findAllWest(Start_X/Start_Y, GoalX, Mx) :-
	% find all obstacles in the direction of the robot.
	findall(Max_X, (obstacle(_/My, Max_X/Max_Y), Start_X > Max_X, My < Start_Y, Start_Y < Max_Y), Foundx),
	% use built in predicate min_member() to sum up the list of obstacles in the direction.
	max_member(Mx, [GoalX |Foundx]). 

findAllNorth(Start_X/Start_Y, GoalY, Mx) :-
	% find all obstacles in the direction of the robot.
	findall(My, (obstacle(Mx/My, Max_X/_), Start_Y < My, Mx < Start_X, Start_X < Max_X), Foundy),
	% use built in predicate min_member() to sum up the list of obstacles in the direction.
	min_member(Mx, [GoalY | Foundy]).

findAllSouth(Start_X/Start_Y, GoalY, Mx) :-
	% find all obstacles in the direction of the robot.
	findall(Max_Y, (obstacle(Mx/_, Max_X/Max_Y), Start_Y > Max_Y, Mx < Start_X, Start_X < Max_X), Foundy),
	% use built in predicate min_member() to sum up the list of obstacles in the direction.
	max_member(Mx, [GoalY | Foundy]).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Finding Obstacle next to Robot                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% checking if there is an obstacle to the east of the robot.
% if there is, then set AtObstacale to true else false.
east_obs(Start_X/Start_Y, _/My, Max_X/Max_Y, Adj_Obst) :-
	( obstacle(Start_X/My, Max_X/Max_Y),
    My < Start_Y, Start_Y < Max_Y, !, Adj_Obst = true ; Adj_Obst = false).

% checking if there is an obstacle to the east of the robot.
% if there is, then set AtObstacale to true else false.
west_obs(Start_X/Start_Y, Mx/My, _/Max_Y, Adj_Obst) :-
	( obstacle(Mx/My, Start_X/Max_Y), My < Start_Y, Start_Y < Max_Y, !, Adj_Obst = true
	; Adj_Obst = false ).
    
% checking if there is an obstacle to the east of the robot.
% if there is, then set AtObstacale to true else false.
north_obs(Start_X/Start_Y, Mx/_, Max_X/Max_Y, Adj_Obst) :-
	( obstacle(Mx/Start_Y, Max_X/Max_Y), Mx < Start_X, Start_X < Max_X, !, Adj_Obst = true
	; Adj_Obst = false ).
    
% checking if there is an obstacle to the east of the robot.
% if there is, then set AtObstacale to true else false.
south_obs(Start_X/Start_Y, Mx/My, Max_X/_, Adj_Obst) :-
	( obstacle(Mx/My, Max_X/Start_Y), Mx < Start_X, Start_X < Max_X, !, Adj_Obst = true
	; Adj_Obst = false ).




