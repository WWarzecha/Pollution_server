%%%-------------------------------------------------------------------
%%% @author HP 8-Gen
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. kwi 2024 23:39
%%%-------------------------------------------------------------------
-module(pow).
-behaviour(gen_server).
%% API
-export([start_link/0, step/0, read/0, close/0, crash/0, set/1]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2, handle_info/2]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,2,[]).
init(N)        -> {ok,N}.

%% INTERFEJS KLIENT -> SERWER %%
set(N)       -> gen_server:call(?MODULE, {N, set}).
step()      -> gen_server:cast(?MODULE,step).
read()      -> gen_server:call(?MODULE,read).
close()     -> gen_server:call(?MODULE,terminate).
crash()     -> gen_server:cast(?MODULE,crash).

%% OBSŁUGA WIADOMOŚCI %%
handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N}.

handle_call({X, set}, _From, _N) -> {reply, ok, X};
handle_call(read,_From, N)      -> {reply, N, N};
handle_call(terminate,_From,N) -> {stop, normal, ok, N}.


handle_info(Message, N) -> io:format("The unexpected message is: ~p.~n",[Message]), {noreply, N}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.