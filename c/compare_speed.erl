-module(compare_speed).

-compile(export_all).

% A is baseline, B what we're benchmarking
compare({Atotal, Atimes}, {Btotal, Btimes}) ->
    io:format("overall speedup: ~.2f~n", [Atotal / Btotal]),
    Times = compare_outs(Atimes, Btimes),
    [io:format("~w: ~.2f~n", [Name, Speedup])
     || {Name, Speedup} <- Times
    ],
    Prod = lists:foldr(
             fun ({_, Speedup}, Acc) ->
                     Acc * Speedup end,
             1, Times),
    Geom = math:pow(Prod, 1 / length(Times)),
    io:format("geometric mean: ~.2f~n", [Geom])
    .

compare_outs(A, B) ->
    [{Name, Atime / Btime}
     || {{Name, Atime}, {Name, Btime}} <- lists:zip(lists:sort(A), lists:sort(B))
    ].
