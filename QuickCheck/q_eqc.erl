-module(q_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-record(state,{ptr,size, elements}).


%% Definition of the states
%% Each state is represented by a function listing the transitions from that state
%% together with generators for the calls to make each transition
init_state(_S) ->
    [{created, {call,q,new,[nat()]}}].

created(S) ->
    [{created, {call,q,put,[S#state.ptr, int()]}},
     {created, {call,q,get,[S#state.ptr]}},
     {created, {call,q,size,[S#state.ptr]}}].

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Next state transformation for state data
next_state_data(_From,_To,_S,Ptr,{call,_,new,[Size]}) ->
    #state{ptr=Ptr,size=Size,elements=[]};
next_state_data(_From,_To,S,_V,{call,_,put,[_,X]}) ->
    S#state{elements=S#state.elements++[X]};
next_state_data(_From,_To,S,_V,{call,_,get,_}) ->
    S#state{elements=tl(S#state.elements)};
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.



%% Precondition is checked before command is added to the testcase
precondition(_From,_To,_S,{call,_,new,[Size]}) ->
    Size > 0;
precondition(_From,_To,S,{call,_,get,_}) ->
    length(S#state.elements) > 0;
precondition(_From,_To,S,{call,_,put,_}) -> 
    length(S#state.elements) < S#state.size; % ensure we don't insert in a full queue
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition checked after command has been evaluated
%% S is the state *before* the command is evaluated
postcondition(_From,_To,_S,{call,_,put,[_,V]},Res) ->
    %io:format("put(~w) -> ~w~n",[V,Res]),
    true;
postcondition(_From,_To,S,{call,_,get,_},Res) ->
    %io:format("get() -> ~w~n",[Res]),
    Res == hd(S#state.elements);
postcondition(_From,_To,S,{call,_,size,_},Res) ->
    Res == length(S#state.elements);
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.


%% Testcase 
prop_q() ->
    ?FORALL(Cmds,commands(?MODULE),
            begin
                {History,State,Result} = run_commands(?MODULE,Cmds),
                ?WHENFAIL(
                   io:format("History: ~p\nState: ~p\nResult: ~p\n", [History, State, Result]),
                   aggregate(zip(state_names(History), command_names(Cmds)), Result =:= ok))
            end).

