%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a very simple implementation of map-reduce, in both
%% sequential and parallel versions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(map_reduce).
-compile(export_all).

%% We begin with a simple sequential implementation, just to define
%% the semantics of map-reduce.  The input is a collection of
%% key-value pairs. The map function maps each key value pair to a
%% list of key-value pairs. The reduce function is then applied to
%% each key and list of corresponding values, and generates in turn a
%% list of key-value pairs. These are the result.

map_reduce_seq(Map,Reduce,Input) ->
    Mapped = [{K2,V2}
              || {K,V} <- Input,
                 {K2,V2} <- Map(K,V)],
    reduce_seq(Reduce,Mapped).

reduce_seq(Reduce,KVs) ->
    [KV || {K,Vs} <- group(lists:sort(KVs)),
           KV <- Reduce(K,Vs)].

group([]) ->
    [];
group([{K,V}|Rest]) ->
    group(K,[V],Rest).
group(K,Vs,[{K,V}|Rest]) ->
    group(K,[V|Vs],Rest);
group(K,Vs,Rest) ->
    [{K,lists:reverse(Vs)}|group(Rest)].

map_reduce_par(Map,M,Reduce,R,Input) ->
    Parent = self(),
    Splits = split_into(M,Input),
    Mappers =
        [spawn_mapper(Parent,Map,R,Split)
         || Split <- Splits],
    Mappeds =
        [receive {Pid,L} -> L end || Pid <- Mappers],
    Reducers = [spawn_reducer(Parent,Reduce,I,Mappeds)
                || I <- lists:seq(0,R-1)],
    Reduceds =
        [receive {Pid,L} -> L end || Pid <- Reducers],
    lists:sort(lists:flatten(Reduceds)).

spawn_mapper(Parent,Map,R,Split) ->
    spawn_link(fun() ->
                       Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                                 || {K,V} <- Split,
                                    {K2,V2} <- Map(K,V)],
                       Parent !
                           {self(),group(lists:sort(Mapped))}
               end).

split_into(N,L) ->
    split_into(N,L,length(L)).

split_into(1,L,_) ->
    [L];
split_into(N,L,Len) ->
    {Pre,Suf} = lists:split(Len div N,L),
    [Pre|split_into(N-1,Suf,Len-(Len div N))].

spawn_reducer(Parent,Reduce,I,Mappeds) ->
    Inputs = [KV|| Mapped <- Mappeds,
                   {J,KVs} <- Mapped,
                   I==J,
                   KV <- KVs],
    spawn_link(fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)}
               end).

map_reduce_dist(Map,M,Reduce,R,Input,Nodes) ->
    Parent = self(),
    Splits = lists:zip(lists:seq(0, M-1), split_into(M, Input)),
    NoNodes = length(Nodes),
    Mappers =
        [spawn_mapper_dist(Parent, Map, R, Split,
                           lists:nth(erlang:phash2(SplitNo, NoNodes) + 1, Nodes))
         || {SplitNo, Split} <- Splits],
    Mappeds =
        [receive {Pid,L} -> L end || Pid <- Mappers],
    Reducers = [spawn_reducer_dist(Parent, Reduce, I, Mappeds,
                                   lists:nth(erlang:phash2(I, NoNodes) + 1, Nodes))
                || I <- lists:seq(0,R-1)],
    Reduceds =
        [receive {Pid,L} -> L end || Pid <- Reducers],
    lists:sort(lists:flatten(Reduceds)).

spawn_mapper_dist(Parent,Map,R,Split,Node) ->
    spawn_link(Node, fun() ->
                             Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                                       || {K,V} <- Split,
                                          {K2,V2} <- Map(K,V)],
                             Parent !
                                 {self(),group(lists:sort(Mapped))}
                     end).

spawn_reducer_dist(Parent,Reduce,I,Mappeds,Node) ->
    Inputs = [KV || Mapped <- Mappeds,
                    {J,KVs} <- Mapped,
                    I==J,
                    KV <- KVs],
    spawn_link(Node, fun() -> Parent ! {self(),reduce_seq(Reduce,Inputs)}
                     end).

map_reduce_bal(Map, M, Reduce, R, Input, Nodes, WorkersPerNode) ->
    Me = self(),
    Splits = split_into(M, Input),
    Mappers =
        [mapper_bal(Map, R, Split)
         || Split <- Splits],
    Ref = make_ref(),
    spawn_link(fun () -> Me ! {Ref, worker_pool(Mappers, Nodes, WorkersPerNode)} end),
    receive {Ref, Mappeds} ->
            Reducers = [reducer_bal(Reduce, I, Mappeds)
                        || I <- lists:seq(0,R-1)],
            RefReducerWorker = make_ref(),
            spawn_link(fun () -> Me ! {RefReducerWorker, worker_pool(Reducers, Nodes, WorkersPerNode)} end),
            receive {RefReducerWorker, Reduceds} ->
                    lists:sort(lists:flatten(Reduceds))
            end
    end.

mapper_bal(Map,R,Split) ->
    fun() ->
            Mapped = [{erlang:phash2(K2,R),{K2,V2}}
                      || {K,V} <- Split,
                         {K2,V2} <- Map(K,V)],
            group(lists:sort(Mapped))
    end.

reducer_bal(Reduce,I,Mappeds) ->
    fun() ->
            Inputs = [KV || Mapped <- Mappeds,
                            {J,KVs} <- Mapped,
                            I==J,
                            KV <- KVs],
            reduce_seq(Reduce,Inputs)
    end.

worker(Master) ->
    Me = self(),
    receive {job, Job} ->
            Master ! {done, Me, Job()},
            worker(Master)
    end.

worker_pool(Funs, Nodes, WorkersPerNode) ->
    Me = self(),
    Workers = lists:flatten([ [ spawn_link(Node, fun () -> worker(Me) end)
                                || _ <- lists:seq(1, WorkersPerNode) ]
                              || Node <- Nodes ]),
    pool(Workers, Funs, []).


% stores working workers in the process dictionary
pool([Worker|Workers], [Fun|Funs], Acc) ->
    put(Worker, 1),
    Worker ! {job, Fun},
    pool(Workers, Funs, Acc);
pool(Workers, Funs, Acc) ->
    case {get(), Funs} of
        {[], []} -> Acc;
        _ -> receive {done, Worker, Result} ->
                     erase(Worker),
                     pool([Worker|Workers], Funs, [Result|Acc])
             end
    end.


map_reduce_fault(Map, M, Reduce, R, Input, Nodes, WorkersPerNode) ->
    Me = self(),
    Splits = split_into(M, Input),
    Mappers =
        [mapper_bal(Map, R, Split)
         || Split <- Splits],
    Ref = make_ref(),
    spawn_link(fun () -> Me ! {Ref, worker_pool_fault(Mappers, Nodes, WorkersPerNode)} end),
    receive {Ref, Mappeds} ->
            Reducers = [reducer_bal(Reduce, I, Mappeds)
                        || I <- lists:seq(0,R-1)],
            RefReducerWorker = make_ref(),
            spawn_link(fun () -> Me ! {RefReducerWorker, worker_pool_fault(Reducers, Nodes, WorkersPerNode)} end),
            receive {RefReducerWorker, Reduceds} ->
                    lists:sort(lists:flatten(Reduceds))
            end
    end.

worker_pool_fault(Funs, Nodes, WorkersPerNode) ->
    Me = self(),
    Workers = lists:flatten([ [ spawn_link(Node, fun () -> worker(Me) end)
                                || _ <- lists:seq(1, WorkersPerNode) ]
                              || Node <- Nodes ]),
    process_flag(trap_exit, true), % trap 'EXIT' messages
    pool_fault(Workers, Funs, []).


pool_fault([Worker|Workers], [Fun|Funs], Acc) ->
    put(Worker, Fun),
    Worker ! {job, Fun},
    pool_fault(Workers, Funs, Acc);

pool_fault(Workers, Funs, Acc) ->
    case {get(), Funs} of
        {[], []} -> Acc;
        _ -> receive {done, Worker, Result} ->
                     erase(Worker),
                     pool_fault([Worker|Workers], Funs, [Result|Acc]);

                     {'EXIT', Worker, noconnection} ->
                     Fun = erase(Worker),
                     Node = node(Worker),
                     Alive = lists:filter(fun (Pid) -> node(Pid) /= Node end, Workers),
                     pool_fault(Alive, [Fun|Funs], Acc)
             end
    end.
