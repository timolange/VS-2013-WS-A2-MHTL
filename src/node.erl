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
-author("Timo, Michael").

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
                nodeName,
                infinity_weight}).

-record(edge, {weight,
               state = basic()}).
% der globale nodeName ist gleichzeitig der Key jeder Edge, nachrichten werden also an den key gesendet
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
  nodeName = NodeName},
  loop(State).
%------------Loop--------------------------------------------------------
loop(State) ->
  receive
    {initiate, Level, FragName, NodeState, Edge} ->
      NewState = response_initiate(State, Level, FragName, NodeState, getEdgeKeyFromTupel(State, Edge)),
      loop(NewState);
    {test, Level, FragName, Edge} ->
      NewState = response_test(State, Level, FragName, getEdgeKeyFromTupel(State, Edge)),
      loop(NewState);
    {accept, Edge} ->
      NewState = response_accept(State, getEdgeKeyFromTupel(State, Edge)),
      loop(NewState);
    {reject, Edge} ->
      NewState = response_reject(State, getEdgeKeyFromTupel(State, Edge)),
      loop(NewState);
    {report, Weight, Edge} ->
      NewState = response_report(State, Weight, getEdgeKeyFromTupel(State, Edge)),
      loop(NewState);
    {changeroot, _Edge} ->
      NewState = response_changeroot(State),
      loop(NewState);
    {connect, Level, Edge} ->
      NewState = response_connect(State, Level, getEdgeKeyFromTupel(State, Edge)),
      loop(NewState)
  end.
%------------Algorithmus-Funktionen----------------------------------------------
response_connect(State, Level, Edge) ->
  Sleeping = sleeping(),
  Find = find(),
  Basic = basic(),
  NewState = if State#state.nodeState == Sleeping
    -> wakeup(State);
    true -> State
  end,
  EdgeVal = getEdge(NewState, Edge),
  if Level < NewState#state.nodeLevel
      -> SecondNewState = NewState#state{edgeDict = updateEdgeState(NewState, Edge, branch())},
         Edge ! {initiate, SecondNewState#state.nodeLevel, SecondNewState#state.fragName, SecondNewState#state.nodeState, getTupelFromEdgeKey(SecondNewState, Edge)},
         if SecondNewState#state.nodeState == Find
           -> SecondNewState#state{find_count = SecondNewState#state.find_count + 1};
           true -> SecondNewState
         end;
     EdgeVal#edge.state == Basic
       -> self() ! {connect, Level, getTupelFromEdgeKey(NewState, Edge)};
     true -> Edge ! {initiate, (NewState#state.nodeLevel + 1), EdgeVal#edge.weight, find(), getTupelFromEdgeKey(NewState, Edge)},
             NewState

  end.


response_initiate(State, Level, FragName, NodeState, Edge) ->
  NewFindCount = dict:fold(
    fun(EdgeKey, EdgeVal, Fcount) -> case EdgeVal#edge.state == branch() and not(EdgeKey == Edge) of
                                       true -> EdgeKey ! {initiate,Level,FragName,NodeState,getTupelFromEdgeKey(State, EdgeKey)},
                                               Fcount + 1;
                                       false -> Fcount
                                     end
    end,
    State#state.find_count,
    State#state.edgeDict
  ),
  NewState = State#state{nodeLevel = Level,
                         fragName = FragName,
                         nodeState = NodeState,
                         in_Branch = Edge,
                         best_Edge = nil(),
                         best_Weight = State#state.infinity_weight,
                         find_count = case NodeState == find() of
                                        true -> NewFindCount;
                                        false -> State#state.find_count
                                      end},
  Find = find(),
  if NodeState == Find
   -> test(NewState);
   true -> NewState
end.


response_test(State, Level, FragName, Edge) ->
  Sleeping = sleeping(),
  NewState = if State#state.nodeState == Sleeping
                ->wakeup(State);
                true -> State
             end,
  EdgeVal = getEdge(NewState, Edge),
  case Level > NewState#state.nodeLevel of
    true -> self() ! {test, Level, FragName, getTupelFromEdgeKey(NewState, Edge)},
            NewState;
    false -> case FragName /= NewState#state.fragName of
               true -> Edge ! {accept,getTupelFromEdgeKey(NewState, Edge)},
                       NewState;
               false -> SecondNewState = case EdgeVal#edge.state == basic() of
                                           true -> State#state{edgeDict = updateEdgeState(NewState, Edge, rejected())};
                                           false -> NewState
                                         end,
                        case SecondNewState#state.test_Edge /= Edge of
                          true -> Edge ! {reject,getTupelFromEdgeKey(State, Edge)},
                                  SecondNewState;
                          false -> test(SecondNewState)
                        end
             end

  end.


response_accept(State, Edge) ->
  EdgeVal = getEdge(State, Edge),
  {NewBestEdge, NewBestWeight} = case EdgeVal#edge.weight < State#state.best_Weight of
                                   true -> {Edge, EdgeVal#edge.weight};
                                   false -> {State#state.best_Edge, State#state.best_Weight}
                                 end,
  NewState = State#state{test_Edge = nil(),
                         best_Edge = NewBestEdge,
                         best_Weight = NewBestWeight},
  report(NewState).


response_reject(State, Edge) ->
  Basic = basic(),
  EdgeVal = getEdge(State, Edge),
  NewState = if EdgeVal#edge.state == Basic
               -> State#state{edgeDict = updateEdgeState(State, Edge, rejected())};
               true -> State
             end,
  test(NewState).

response_report(State, Weight, Edge) ->
  Find = find(),
  EdgeVal = getEdge(State, Edge),
  if Edge /= State#state.in_Branch
      -> {NewBestEdge, NewBestWeight} = case EdgeVal#edge.weight < State#state.best_Weight of
                                          true -> {Edge, EdgeVal#edge.weight};
                                          false -> {State#state.best_Edge, State#state.best_Weight}
                                        end,
         NewState = State#state{find_count = State#state.find_count - 1,
                                best_Edge = NewBestEdge,
                                best_Weight = NewBestWeight},
         report(NewState);
    State#state.nodeState == Find
      -> self() ! {report, Weight, getTupelFromEdgeKey(State, Edge)};
    Weight > State#state.best_Weight
      -> change_root(State);
    (Weight == State#state.best_Weight) and (Weight == State#state.infinity_weight) ->
      halt()
  end.


response_changeroot(State) ->
  change_root(State).

wakeup(State) ->
  MinWeightEdgeKey = getMinWeightEdgeKey(State#state.edgeDict),
  NewState = State#state{
    edgeDict = updateEdgeState(State, MinWeightEdgeKey, branch()),
    nodeLevel = 0,
    nodeState = found(),
    find_count = 0
  },
  MinWeightEdgeKey ! {connect, NewState#state.nodeLevel, getTupelFromEdgeKey(NewState, MinWeightEdgeKey)},
  NewState.

test(State) ->
  AnyBasicEdge = anyEdgeInState(State, basic()),

  if AnyBasicEdge
    -> BasicEdges = dict:filter(fun(_Key,Val) -> Val#edge.state == basic() end,State#state.edgeDict),
       Test_Edge = getMinWeightEdgeKey(BasicEdges),
       Test_Edge ! {test, State#state.nodeLevel, State#state.fragName, getTupelFromEdgeKey(State, Test_Edge)},
       NewState = State#state{test_Edge = Test_Edge};
    true ->NewState = report(State#state{test_Edge = nil()})
  end,
  NewState.


report(State) ->
  Nil = nil(),
  if (State#state.find_count == 0) and (State#state.test_Edge == Nil)
    -> NewState = State#state{nodeState = found()},
       State#state.in_Branch ! {report, NewState#state.best_Weight, getTupelFromEdgeKey(State, State#state.in_Branch)};
    true -> NewState = State
  end,
  NewState.

change_root(State) ->
  Branch = branch(),
  BestEdgeKey = State#state.best_Edge,
  BestEdgeVal = getEdge(State, BestEdgeKey),
  BestEdgeState = BestEdgeVal#edge.state,
  Tupel = getTupelFromEdgeKey(State, BestEdgeKey),
  if BestEdgeState == Branch
    -> BestEdgeKey ! {changeroot, Tupel},
       NewState = State;
    true -> BestEdgeKey ! {connect, State#state.nodeLevel, Tupel},
            NewState = State#state{edgeDict = updateEdgeState(State, BestEdgeKey, branch())}
  end,
  NewState.
%------------Hilfs-Funktionen----------------------------------------------
getMinWeightEdgeKey(EdgeDict) ->
  [FirstKey | _] = dict:fetch_keys(EdgeDict),
  FirstEdge = dict:fetch(FirstKey, EdgeDict),
  {MinKey, _MinVal} = dict:fold(
    fun(EdgeKey, EdgeVal, MinWeightEdge) -> {_MinEdgeKey, MinEdgeVal} = MinWeightEdge,
                                            case EdgeVal#edge.weight < MinEdgeVal#edge.weight of
                                              true -> {EdgeKey, EdgeVal};
                                              false -> MinWeightEdge
                                            end
    end,
    FirstEdge,
    EdgeDict
  ),
  MinKey.

getEdge(State, EdgeKey) ->
  dict:fetch(EdgeKey, State#state.edgeDict).

updateEdgeState(State, EdgeKey, NewEdgeState) ->
  NewDict = dict:update(EdgeKey,
    fun(Edge) -> Edge#edge{state = NewEdgeState} end,
    State#state.edgeDict
  ),
  NewDict.

%gibt den key der Edge zurueck,
%jedes tupel muss den globalen namen des sendenden knoten enthalten und des empfangenen(unser nodename), da alle knoten nur ihre nachbarn kennen
getEdgeKeyFromTupel(State, Tupel) ->
  {_Weight, NodeX, NodeY} = Tupel,
  case NodeX == State#state.nodeName of
    true -> NodeY;
    false -> NodeX
  end.

getTupelFromEdgeKey(State, EdgeKey) ->
  NodeX = State#state.nodeName,
  NodeY = EdgeKey,
  EdgeVal = getEdge(State, EdgeKey),
  Weight = EdgeVal#edge.weight,
  {Weight, NodeX, NodeY}.

anyEdgeInState(State, Edgestate) ->
  dict:fold(
    fun(_EdgeKey, EdgeVal, Acc) -> case EdgeVal#edge.state == Edgestate of
                                     true -> true;
                                     false -> Acc
                                   end
    end,
    false,
    State#state.edgeDict
  ).