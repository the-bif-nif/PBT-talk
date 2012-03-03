%%% How to run
%%% unit tests: listdel:test().
%%% properties: proper:quickcheck(listdel:prop_delete()).

-module(listdel).

-export([delete/2]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(lists,[seq/2]).



%% A lists delete implementation
-spec delete(T, list(T)) -> list(T).
delete(X, L) ->
    delete(X, L, []).

delete(_, [], Acc) ->
    lists:reverse(Acc);
delete(X, [X|Rest], Acc) ->
    lists:reverse(Acc) ++ Rest;
delete(X, [Y|Rest], Acc) ->
    delete(X, Rest, [Y|Acc]).


%% Unit tests
delete_test_() ->
    [?_assertEqual(delete(1,[]), []),
     ?_assertEqual(delete(1,[1,2,3,4]), [2,3,4]),
     ?_assertEqual(delete(5,[1,2,3,4,5]), [1,2,3,4]),
     ?_assertEqual(delete(-3, [-5,-4,-3,-2,-1]), [-5,-4,-2,-1]),
     ?_assertEqual(delete(100, seq(1,100)), seq(1,99))
    ].


%% For any integer x and any list of integers l,
%% if I delete x from l, then l does not contain x
prop_delete() ->
    ?FORALL({X,L},                              % variables 
            {integer(),list(integer())},        % generators
            not lists:member(X, delete(X, L))). % property

%% Let restrict the tests to list with no duplicates
no_duplicates(L) ->
    sets:size(sets:from_list(L)) =:= length(L).

prop_delete2() ->
    ?FORALL({X,L}, {integer(),list(integer())},
            ?IMPLIES(no_duplicates(L),
                     not lists:member(X, delete(X, L)))).

%% You can also use generators, to generate lists with no duplicates
ulist(Elem) -> 
    ?LET(L,list(Elem), 
         lists:usort(L)).

prop_delete3() ->
    ?FORALL({X,L}, {integer(),ulist(integer())},
            not lists:member(X, delete(X, L))).


%% Generate tests that are relevant:
%% test only non-empty lists
%% and only check deletion of actual list elements
prop_delete4() -> 
    ?FORALL(L, list(integer()), 
            ?IMPLIES(L /= [], 
                     ?FORALL(X, elements(L), 
                             not lists:member(X,delete(X,L))))). 


 




