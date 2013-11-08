%%%-------------------------------------------------------------------
%%% @author timey
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Okt 2013 21:18
%%%-------------------------------------------------------------------
-module(node).
-import(werkzeug, [to_String/1, timeMilliSecond/0, logging/2, get_config_value/2, reset_timer/3,
pushSL/2, popSL/1, popfiSL/1, findSL/2, findneSL/2, lengthSL/1, minNrSL/1, maxNrSL/1, emptySL/0, notemptySL/1, delete_last/1, shuffle/1]).
-author("timey", "Michael").

%% API
-export([start/2]).

%------------Konstanten------------------------------------------------
found() -> found.
sleeping() -> sleeping.
find() -> find.

basic() -> basic.
branch() -> branch.
rejected() -> rejected().
nil() -> nil.
%------------Datenstruktur----------------------------------------------
-record(state, {nodeState = sleeping(),
  nodeLevel,
  fragName,
  edgeDict,
  best_Weight,
  best_Edge,
  test_Edge,
  in_Branch,
  find_count,
  nodeName}).

-record(edge, {weight,
               state = basic()}).

buildDict(EdgeList) ->
  lists:foldl(
    fun(Edge, Acc) -> {Weight, Nodename} = Edge,
      dict:store(Nodename, #edge{weight = Weight}, Acc)
    end,
    dict:new(),
    EdgeList
  ).
%------------initialisieren----------------------------------------------
start(NodeName, Nameservice) ->

  %beim globalen namensservice registrieren
  global:whereis_name(Nameservice),
  %Nodename global verfuegbar machen
  global:register_name(NodeName, self()),

  {ok, EdgeList} = file:consult("node.cfg"),
  State = #state{edgeDict = buildDict(EdgeList),
                 nodeName = NodeName
                },
  loop(State).
%------------Loop--------------------------------------------------------
loop(State) ->
  receive
    {initiate, Level, FragName, NodeState, Edge} ->
      NewState = response_initiate(State, Level, FragName, NodeState, Edge),
      loop(NewState);
    {test, Level, FragName, Edge} ->
      NewState = response_test(State, Level, FragName, Edge),
      loop(NewState);
    {accept, Edge} ->
      NewState = response_accept(State, Edge),
      loop(NewState);
    {reject, Edge} ->
      NewState = response_reject(State, Edge),
      loop(NewState);
    {report, Weight, Edge} ->
      NewState = response_report(State, Weight, Edge),
      loop(NewState);
    {changeroot, Edge} ->
      NewState = response_changeroot(State, Edge),
      loop(NewState);
    {connect, Level, Edge} ->
      NewState = response_connect(State, Level, Edge),
      loop(NewState)
  end.
%------------Algorithmus-Funktionen----------------------------------------------
response_connect(State, Level, Edge) ->
  if State#state.nodeState == sleeping()
    -> State#state{nodeState = found()};
  true -> false
  end,

   if Level < State#state.nodeLevel
     -> getEdge(State,Edge)#edge{state = branch()},
          Edge ! {initiate,Level,State#state.fragName,State#state.nodeState,Edge},
          if State#state.nodeState == find()
            ->State#state{find_count = (State#state.find_count +1)};
            true-> false
          end;
      true -> if getEdge(State,Edge)#edge.state == basic()
                -> self() !  {connect, Level, Edge};
                true -> Edge ! {initiate, (State#state.nodeLevel + 1), getEdge(State,Edge)#edge.weight, find(), Edge}
              end
   end.


response_initiate(State, Level, FragName, NodeState, Edge) -> State.
response_test(State, Level, FragName, Edge) -> State.
response_accept(State, Edge) -> State.
response_reject(State, Edge) -> State.
response_report(State, Weight, Edge) -> State.
response_changeroot(State, Edge) -> State.

wakeup(State) ->
  {EdgeKey, EdgeVal} = getMinWeightEdgeKey(State#state.edgeDict),

  NewDict = dict:update(EdgeKey,
                          fun(Edge) -> Edge#edge{state = branch()} end,
                          State#state.edgeDict
                        ),
  NewState =  State#state{
              edgeDict = NewDict,
              nodeLevel = 0,
              nodeState = found(),
              find_count = 0
              },
  EdgeKey ! {connect, NewState#state.nodeLevel, getTupelFromEdgeKey(NewState,EdgeKey)},
  NewState
  .


test(State) ->
  AnyBasicEdge = anyEdgeInState(State, basic()),
  if AnyBasicEdge
    -> Test_Edge = findSL(BasicEdgeList, minNrSL(BasicEdgeList)),
    Test_Edge ! {test, NodeLevel, FragName, self()};
    true -> report(State#state{test_Edge = nil()})
  end.


report(Find_count, Test_Edge, Best_Weight, In_Branch, NodeState) ->
  if Find_count == 0 and Test_Edge == nil()
    -> NewNodeState = found(),
    In_Branch ! {report, Best_Weight, self()};
    true -> NewNodeState = NodeState
  end,
  NewNodeState.

change_root(BranchEdgeList, Best_Edge_Nr, NodeLevel) ->
  BestEdge = findSL(BranchEdgeList, Best_Edge_Nr),
  if BestEdge /= {-1, nok}
    -> BestEdge ! {changeroot, self()};
    true -> BestEdge ! {connect, NodeLevel, self()},
      pushSL(BranchEdgeList, {Best_Edge_Nr, BestEdge})
  end,
  BranchEdgeList.
%------------Hilfs-Funktionen----------------------------------------------
getMinWeightEdgeKey(EdgeDict) ->
  [FirstKey | _] = dict:fetch_keys(EdgeDict),
  FirstEdge = dict:fetch(FirstKey, EdgeDict),
  {MinKey, MinVal} = dict:fold(
                                fun(EdgeKey, EdgeVal, MinWeightEdge) -> {MinKey, MinVal} = MinWeightEdge,
                                                                        case EdgeVal#edge.weight < MinVal#edge.weight of
                                                                          true -> {EdgeKey, EdgeVal};
                                                                          false -> MinWeightEdge
                                                                        end
                                end,
                                FirstEdge,
                                EdgeDict
                              ).

getEdge(State,EdgeKey)->
  dict:fetch(EdgeKey,State#state.edgeDict).

getAdjacentNodeFromTupel(State, Tupel) -> {Weight, NodeX, NodeY} = Tupel,
                          case NodeX == State#state.nodeName of
                            true -> NodeY;
                            false -> NodeX
                          end.
getTupelFromEdgeKey(State,EdgeKey) ->NodeX = State#state.nodeName,
                                  NodeY = EdgeKey,
                                  Weight = fetch(EdgeKey, State#state.edgeDict)#edge.weight,
                                  {Weight, NodeX, NodeY}.

anyEdgeInState(State, Edgestate) ->
  dict:fold(
            fun(EdgeKey,EdgeVal,Acc) -> case EdgeVal#edge.state == Edgestate of
                                          true -> true;
                                          false -> Acc
                                        end
            end,
            false,
            State#state.edgeDict
          ).
