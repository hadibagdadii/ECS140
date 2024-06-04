% query/query.pl

% /* All novels published during the year 1953 or 1996*/
% year_1953_1996_novels(Book) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.

% /* List of all novels published during the period 1800 to 1900*/
% period_1800_1900_novels(Book) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.

% /* Characters who are fans of LOTR */
% lotr_fans(Fan) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.

% /* Authors of the novels owned by Chandler */
% author_names(Author) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.

% /* Characters who are fans of Brandon Sanderson's novels */
% fans_names(Fan) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.

% /* Novels common between either of Phoebe, Ross, and Monica */
% mutual_novels(Book) :-
%     %% remove fail and add body/other cases for this predicate
%     fail.


comparisonRule([H|_], L) :- member(H, L).
comparisonRule([_|T], L) :- comparisonRule(T, L).

year_1953_1996_novels(Book) :- 
    novel(Book, Year), (Year = 1953 ; Year = 1996).

period_1800_1900_novels(Book) :-
    novel(Book, Year), Year >= 1800, Year =< 1900.

lotr_fans(Fan) :-
    fan(Fan, Books), member(the_lord_of_the_rings, Books).

author_names(Author) :-
    author(Author, AuthorBooks),
    fan(chandler, ChandlerBooks),
    comparisonRule(AuthorBooks, ChandlerBooks).

fans_names(Fan) :-
    fan(Fan, FanBooks),
    author(brandon_sanderson, SandersonBooks),
    comparisonRule(SandersonBooks, FanBooks).

mutual_novels(Book) :-
    fan(monica, MonicaList),
    fan(ross, RossList),
    fan(phoebe, PhoebeList),
    (
        (member(Book, MonicaList), (member(Book, RossList) ; member(Book, PhoebeList)));
        (member(Book, RossList), member(Book, PhoebeList))
    ).
