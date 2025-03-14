/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */
maximum_printing_depth(100).

:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).

% Define edges
edge(a, b).
edge(a, c).
edge(c, b).
edge(c, a).

% Signature: path(Node1, Node2, Path)/3
% Purpose: Path is a path, denoted by a list of nodes, from Node1 to Node2.
path(N1, N2, [N1,N2]):- edge(N1,N2).
path(N1, N2, [N1|Ps]):- edge(N1,Next), path(Next,N2,Ps).


% Signature: cycle(Node, Cycle)/2
% Purpose: Cycle is a cyclic path, denoted a list of nodes, from Node1 to Node1.

cycle(Start,[Start|Cycle]):- edge(Start,Next),
    path(Next,Start,Cycle).


% Signature: reverse(Graph1,Graph2)/2
% Purpose: The edges in Graph1 are reversed in Graph2


reverse([],[]).
reverse([[N1,N2]|Graph1],[[N2,N1]|Graph2]):- reverse(Graph1,Graph2). 


% Signature: degree(Node, Graph, Degree)/3
% Purpose: Degree is the degree of node Node, denoted by a Church number (as defined in class)


natural_number(zero).
natural_number(s(X)) :- natural_number(X).
degree(_,[],zero).
degree(A,[[A,_]|Graph],s(D)):- degree(A,Graph,D).
degree(A,[[_,_]|Graph],D):- degree(A,Graph,D).









