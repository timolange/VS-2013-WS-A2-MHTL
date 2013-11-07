%%%-------------------------------------------------------------------
%%% @author timey
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Okt 2013 21:18
%%%-------------------------------------------------------------------
-module(node).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2,get_config_value/2,reset_timer/3,
pushSL/2,popSL/1,popfiSL/1,findSL/2,findneSL/2,lengthSL/1,minNrSL/1,maxNrSL/1,emptySL/0,notemptySL/1,delete_last/1,shuffle/1]).
-author("timey").

%% API
-export([start/2]).

%------------Konstanten------------------------------------------------
found()    -> found.
sleeping() -> sleeping.
find()     -> find.

basic()    -> basic.
branch()   -> branch.
rejected() -> rejected().
nil()      -> nil.
%------------Datenstruktur----------------------------------------------
-record(state, {nodeState = sleeping(),
                nodeLevel,
                fragName,
                edgeDict,
                best_Weight,
                best_Edge,
                test_Edge,
                in_Branch,
                find_count}).

-record(edge, {weight,
               state = basic()}).

buildDict(EdgeList) ->
  lists:foldl(
              fun(Edge, Acc) -> {Weight,Nodename} = Edge,
                                dict:store(Nodename, #edge{weight = Weight}, Acc)
              end,
              dict:new(),
              EdgeList
            ).
%------------initialisieren----------------------------------------------
start(NodeName, Nameservice)->

  %beim globalen namensservice registrieren
  global:whereis_name(Nameservice),
  %Nodename global verfuegbar machen
  global:register_name(NodeName, self()),

  {ok, EdgeList} = file:consult("node.cfg"),
  State = #state{edgeDict = buildDict(EdgeList)},
  loop(State).
%------------Loop--------------------------------------------------------
loop(State)->
  receive
    {initiate,Level,FragName,NodeState,Edge} ->
         NewState = response_initiate(State,Level,FragName,NodeState,Edge),
         loop(NewState);
    {test,Level,FragName,Edge} ->
         NewState = response_test(State,Level,FragName,Edge),
         loop(NewState);
    {accept,Edge} ->
         NewState = response_accept(State,Edge),
         loop(NewState);
    {reject,Edge} ->
         NewState = response_reject(State,Edge),
         loop(NewState);
    {report,Weight,Edge} ->
         NewState = response_report(State,Weight,Edge),
         loop(NewState);
    {changeroot,Edge} ->
         NewState = response_changeroot(State,Edge),
         loop(NewState);
    {connect,Level,Edge} ->
         NewState = response_connect(State,Level,Edge),
         loop(NewState)
  end.
%------------Algorithmus-Funktionen----------------------------------------------
response_connect(State,Level,Edge) ->State.
response_initiate(State,Level,FragName,NodeState,Edge) ->   State.
response_test(State,Level,FragName,Edge) ->       State.
response_accept(State,Edge) ->     State.
response_reject(State,Edge) ->     State.
response_report(State,Weight,Edge) ->     State.
response_changeroot(State,Edge) -> State.

wakeup(BasicEdgeList, BranchEdgeList) ->
  NodeState= found(),
  NodeLevel= 0,
  Find_count = 0,
  AKMG = findSL(BasicEdgeList, minNrSL(BasicEdgeList)),
  NewBasicEdgeList =  popSL(BasicEdgeList),
  NewBranchEdgeList = pushSL(BranchEdgeList, AKMG),
  {NodeState, NodeLevel, NewBasicEdgeList, NewBranchEdgeList, Find_count}.


test(NodeLevel,FragName, BasicEdgeList) ->
  ListNotEmpty = notemptySL(BasicEdgeList),
  if ListNotEmpty
    -> Test_Edge = findSL(BasicEdgeList, minNrSL(BasicEdgeList)),
       Test_Edge ! {test,NodeLevel,FragName,self()};
    true ->  Test_Edge = nil(),
             report(Test_Edge)
  end.


report(Find_count, Test_Edge, Best_Weight, In_Branch, NodeState) ->
  if Find_count == 0 and Test_Edge == nil()
    -> NewNodeState = found(),
       In_Branch ! {report,Best_Weight,self()};
    true -> NewNodeState = NodeState
  end,
  NewNodeState.

change_root(BranchEdgeList, Best_Edge_Nr, NodeLevel) ->
  BestEdge = findSL(BranchEdgeList, Best_Edge_Nr),
  if BestEdge /= {-1,nok}
     -> BestEdge ! {changeroot,self()};
    true -> BestEdge ! {connect,NodeLevel,self()},
            pushSL(BranchEdgeList, {Best_Edge_Nr,BestEdge})
  end,
  BranchEdgeList.
%------------Hilfs-Funktionen----------------------------------------------
getMinWeightEdge(EdgeDict) ->
  [FirstKey | _ ] = dict:fetch_keys(EdgeDict),
  FirstEdge = dict:fetch(FirstKey, EdgeDict),
  dict:fold(
            fun(Edge, MinWeightEdge) -> case Edge#edge.weight < MinWeightEdge#edge.weight of
                                          true -> Edge;
                                          false -> MinWeightEdge
                                        end
            end,
            FirstEdge,
            EdgeDict
            ).