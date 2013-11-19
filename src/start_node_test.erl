%%%-------------------------------------------------------------------
%%% @author timey
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2013 17:06
%%%-------------------------------------------------------------------
-module(start_node_test).
-author("timey").

%% API
-export([start/1]).

%Nameservice muss ein atom sein und in '' uebergeben werden, bsp.:
start(Nameservice) ->
  NodeList = ["node0","node1","node2","node3","node4","node5","node6"],
  Startnode = fun(NodeName) ->
                %erlang:display(NodeName),
                node:start(NodeName, Nameservice)
              end,
  lists:foreach(Startnode, NodeList),
  %wakeup an den ersten knoten senden und damit die suche nach dem Minimum-Weight-Spanning-Tree starten
  [FirstNode | _ ] = NodeList,
  FirstNode_GlobalName = list_to_atom(FirstNode),
  global:whereis_name(FirstNode_GlobalName) ! {wakeup}.
