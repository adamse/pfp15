-module(page_rank).
-compile(export_all).

-define(DATABASE, "web.dat").

map(Url, ok) ->
    [{Url, Body}] = dets:lookup(web, Url),
    Urls = crawl:find_urls(Url, Body),
    [{U,1} || U <- Urls].

reduce(Url, Ns) ->
    [{Url, lists:sum(Ns)}].

% 362.713768
page_rank() ->
    dets:open_file(web, [{file, ?DATABASE}]),
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    Res = map_reduce:map_reduce_seq(fun map/2, fun reduce/2,
                              [{Url,ok} || Url <- Urls]),
    dets:close(web),
    Res.

% 195.89102
page_rank_par() ->
    dets:open_file(web, [{file, ?DATABASE}]),
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    Res = map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32,
                              [{Url,ok} || Url <- Urls]),
    dets:close(web),
    Res.

% 197,133911
% three nodes on same computer
page_rank_dist() ->
    Nodes = [node() | nodes()],
%    dets:open_file(web, [{file, ?DATABASE}]),
    % open the database on all nodes
%    [ rpc:call(Node, dets, open_file, [web, [{file, ?DATABASE}]]) || Node <- Nodes ],
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    Res = map_reduce:map_reduce_dist(fun map/2, 32, fun reduce/2, 32,
                              [{Url,ok} || Url <- Urls], Nodes),
    % close database on all nodes
%    [ rpc:call(Node, dets, close, [web]) || Node <- Nodes ],
    Res.

page_rank_dist2() ->
    Nodes = [node() | nodes()],
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    Res = map_reduce:map_reduce_bal(fun map/2, 32, fun reduce/2, 32,
                              [{Url,ok} || Url <- Urls], Nodes, 5),
    Res.



bm() ->
    Funs = [page_rank, page_rank_par],
    [ case timer:tc(?MODULE, Fun, []) of {T,_} -> T/1000000 end || Fun <- Funs ].
