% # isUnion(Set1,Set2,Union) :-
% #     %% remove fail and add body/other cases for this predicate
% #     fail.

% # isIntersection(Set1,Set2,Intersection) :-
% #     %% remove fail and add body/other cases for this predicate
% #     fail.

% # isEqual(Set1,Set2) :-
% #     %% remove fail and add body/other cases for this predicate
% #     fail.

isUnion([], Set, Set).
isUnion([H|T], Set, Union) :-
    (member(H, Set) -> isUnion(T, Set, Union) ; isUnion(T, Set, UT), Union = [H|UT]).

isIntersection([], _, []).
isIntersection([H|T], Set, Intersection) :-
    (member(H, Set) -> isIntersection(T, Set, IT), Intersection = [H|IT] ; isIntersection(T, Set, Intersection)).

isEqual(Set1, Set2) :-
    subset(Set1, Set2),
    subset(Set2, Set1).

subset([], _).
subset([H|T], Set) :-
    member(H, Set),
    subset(T, Set).
