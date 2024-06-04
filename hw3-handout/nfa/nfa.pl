% reachable(StartState, FinalState, Input) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.

% nfa/nfa.pl
% Complete the definition of the predicates as per the requirements

reachable(State, State, []).
reachable(StartState, FinalState, [H|T]) :-
    transition(StartState, H, NextStates),
    member(NextState, NextStates),
    reachable(NextState, FinalState, T).