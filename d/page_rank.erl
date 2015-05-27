-module(page_rank).
-compile(export_all).

map(Url, ok) ->
    [{Url, Body}] = dets:lookup(web, Url),
    Urls = crawl:find_urls(Url, Body),
    [{U,1} || U <- Urls].

reduce(Url, Ns) ->
    [{Url, lists:sum(Ns)}].

% 9,663,770
page_rank() ->
    dets:open_file(web, [{file, "web_horv.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    map_reduce:map_reduce_seq(fun map/2, fun reduce/2,
                              [{Url,ok} || Url <- Urls]).

% 4,531,846
page_rank_par() ->
    dets:open_file(web, [{file, "web_horv.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32,
                              [{Url,ok} || Url <- Urls]).

page_rank_dist() ->
    dets:open_file(web, [{file, "web_horv.dat"}]),
    Urls = dets:foldl(fun({K,_},Keys) ->
                              [K|Keys] end,[],web),
    map_reduce:map_reduce_dist(fun map/2, 32, fun reduce/2, 32,
                              [{Url,ok} || Url <- Urls], nodes()).
