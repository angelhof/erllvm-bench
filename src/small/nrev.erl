% file: "nrev.erl"

-module(nrev).
-export([main/1,compile/1]).

-spec nrev([integer()]) -> [integer()].
nrev([H|T]) -> app(nrev(T),[H]);
nrev([])    -> [].

-spec app([integer()], [integer()]) -> [integer()].
app([H|T],L) -> [H|app(T,L)];
app([],L)    -> L.

-spec iota(integer()) -> list(integer()).
iota(N) -> iota(N,[]).
-spec iota(integer(), list(integer())) -> list(integer()).
iota(0,L) -> L;
iota(N,L) -> iota(N-1,[N|L]).

-spec loop(integer(), [integer()], integer()) -> [integer()].
loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,nrev(L)).

-spec main([]) -> [integer()].  
main([]) ->
    L = iota(1000),
    loop(1500,L,0).


compile(Flags) ->
    hipe:c(?MODULE,Flags).
