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
confPath() -> "node_config/".

found() -> found.
sleeping() -> sleeping.
find() -> find.

basic() -> basic.
branch() -> branch.
rejected() -> rejected().
nil() -> nil.
infinity_weight() -> 9999999.
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
                infinity_weight = infinity_weight()}).

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
%der nodeName muss einer node.cfg ohne dateiendung im dir /node_config/ config entsprechen
%% bspw.: Nodename="node0", datei="/node_config/node0.cfg"
start(NodeName, Nameservice) ->
  File = confPath()++NodeName++".cfg",
  {ok, EdgeList} = file:consult(File),
  State = #state{edgeDict = buildDict(EdgeList),
                 nodeName = list_to_atom(NodeName)},

  NodePID = spawn(fun() -> loop(State) end),
  %verbindung mit globalen namensservice herstellen
  net_adm:ping(Nameservice),
  %Nodename global verfuegbar machen
  global:register_name(list_to_atom(NodeName), NodePID),
  logging("Node.log", io_lib:format(NodeName++" Startzeit: ~s mit PID ~s~n", [timeMilliSecond(),to_String(NodePID)])).
%------------Loop--------------------------------------------------------
loop(State) ->
  receive
    {wakeup} ->
      NewState = wakeup(State),
      logState(NewState, "wakeup"),
      loop(NewState);
    {initiate, Level, FragName, NodeState, Edge} ->
      NewState = response_initiate(State, Level, FragName, NodeState, getEdgeKeyFromTupel(State, Edge)),
      logState(NewState, "initiate"),
      loop(NewState);
    {test, Level, FragName, Edge} ->
      NewState = response_test(State, Level, FragName, getEdgeKeyFromTupel(State, Edge)),
      logState(NewState, "test"),
      loop(NewState);
    {accept, Edge} ->
      NewState = response_accept(State, getEdgeKeyFromTupel(State, Edge)),
      logState(NewState, "accept"),
      loop(NewState);
    {reject, Edge} ->
      NewState = response_reject(State, getEdgeKeyFromTupel(State, Edge)),
      logState(NewState, "reject"),
      loop(NewState);
    {report, Weight, Edge} ->
      NewState = response_report(State, Weight, getEdgeKeyFromTupel(State, Edge)),
      logState(NewState, "report"),
      loop(NewState);
    {changeroot, _Edge} ->
      NewState = response_changeroot(State),
      logState(NewState, "changeroot"),
      loop(NewState);
    {connect, Level, Edge} ->
      NewState = response_connect(State, Level, getEdgeKeyFromTupel(State, Edge)),
      logState(NewState, "initiate"),
      loop(NewState);
    {exit, Msg} ->
      logState(State, Msg)
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
         global:whereis_name(Edge) ! {initiate, SecondNewState#state.nodeLevel, SecondNewState#state.fragName, SecondNewState#state.nodeState, getTupelFromEdgeKey(SecondNewState, Edge)},
         if SecondNewState#state.nodeState == Find
           -> SecondNewState#state{find_count = SecondNewState#state.find_count + 1};
           true -> SecondNewState
         end;
     EdgeVal#edge.state == Basic
       -> self() ! {connect, Level, getTupelFromEdgeKey(NewState, Edge)},
          NewState;
     true -> global:whereis_name(Edge) ! {initiate, (NewState#state.nodeLevel + 1), EdgeVal#edge.weight, find(), getTupelFromEdgeKey(NewState, Edge)},
             NewState

  end.


response_initiate(State, Level, FragName, NodeState, Edge) ->
  NewFindCount = dict:fold(
    fun(EdgeKey, EdgeVal, Fcount) -> case (EdgeVal#edge.state == branch()) and (not(EdgeKey == Edge)) of
                                       true -> global:whereis_name(EdgeKey) ! {initiate,Level,FragName,NodeState,getTupelFromEdgeKey(State, EdgeKey)},
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
               true -> global:whereis_name(Edge) ! {accept,getTupelFromEdgeKey(NewState, Edge)},
                       NewState;
               false -> SecondNewState = case EdgeVal#edge.state == basic() of
                                           true -> State#state{edgeDict = updateEdgeState(NewState, Edge, rejected())};
                                           false -> NewState
                                         end,
                        case SecondNewState#state.test_Edge /= Edge of
                          true -> global:whereis_name(Edge) ! {reject,getTupelFromEdgeKey(State, Edge)},
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
      -> self() ! {report, Weight, getTupelFromEdgeKey(State, Edge)},
         State;
    Weight > State#state.best_Weight
      -> change_root(State);
    (Weight == State#state.best_Weight) and (Weight == State#state.infinity_weight)
      -> self() ! {exit, halt},
         State
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
  global:whereis_name(MinWeightEdgeKey) ! {connect, NewState#state.nodeLevel, getTupelFromEdgeKey(NewState, MinWeightEdgeKey)},
  NewState.

test(State) ->
  AnyBasicEdge = anyEdgeInState(State, basic()),

  if AnyBasicEdge
    -> BasicEdges = dict:filter(fun(_Key,Val) -> Val#edge.state == basic() end,State#state.edgeDict),
       Test_Edge = getMinWeightEdgeKey(BasicEdges),
       global:whereis_name(Test_Edge) ! {test, State#state.nodeLevel, State#state.fragName, getTupelFromEdgeKey(State, Test_Edge)},
       NewState = State#state{test_Edge = Test_Edge};
    true ->NewState = report(State#state{test_Edge = nil()})
  end,
  NewState.


report(State) ->
  Nil = nil(),
  if (State#state.find_count == 0) and (State#state.test_Edge == Nil)
    -> NewState = State#state{nodeState = found()},
       global:whereis_name(State#state.in_Branch) ! {report, NewState#state.best_Weight, getTupelFromEdgeKey(State, State#state.in_Branch)};
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
    -> global:whereis_name(BestEdgeKey) ! {changeroot, Tupel},
       NewState = State;
    true -> global:whereis_name(BestEdgeKey) ! {connect, State#state.nodeLevel, Tupel},
            NewState = State#state{edgeDict = updateEdgeState(State, BestEdgeKey, branch())}
  end,
  NewState.
%------------Hilfs-Funktionen----------------------------------------------
getMinWeightEdgeKey(EdgeDict) ->
  [FirstKey | _] = dict:fetch_keys(EdgeDict),
  FirstEdge = dict:fetch(FirstKey, EdgeDict),
  {MinKey, _MinVal} = dict:fold(
    fun(EdgeKey, EdgeVal, MinWeightEdge_KeyValPair) -> {_MinEdgeKey, MinEdgeVal} = MinWeightEdge_KeyValPair,
                                            case EdgeVal#edge.weight < MinEdgeVal#edge.weight of
                                              true -> {EdgeKey, EdgeVal};
                                              false -> MinWeightEdge_KeyValPair
                                            end
    end,
    {FirstKey, FirstEdge},
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

logState(State, Msg) ->
  logging("Node.log", io_lib:format("~p erhalten, um ~s , neuer Status:
                                     nodeState ~p,
                                     nodeLevel ~p,
                                     fragName ~p,
                                     edgeDict ~p,
                                     best_Weight ~p,
                                     best_Edge ~p,
                                     test_Edge ~p,
                                     in_Branch ~p,
                                     find_count ~p,
                                     nodeName ~p,
                                     infinity_weight ~p~n",
                                    [Msg,
                                     timeMilliSecond(),
                                     State#state.nodeState,
                                     State#state.nodeLevel,
                                     State#state.fragName,
                                     State#state.edgeDict,
                                     State#state.best_Weight,
                                     State#state.best_Edge,
                                     State#state.test_Edge,
                                     State#state.in_Branch,
                                     State#state.find_count,
                                     State#state.nodeName,
                                     State#state.infinity_weight
                                    ])).