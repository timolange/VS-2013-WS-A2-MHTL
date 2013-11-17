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

start(Node) ->
  NodeList = ["node0","node1","node2","node3","node4","node5","node6"],
  Startnode = fun(NodeName) ->
                erlang:display(NodeName),
                node:start(NodeName, Node)
              end,
  lists:foreach(Startnode, NodeList),
  [FirstNode | _ ] = NodeList,
  FirstNode ! {wakeup}.
