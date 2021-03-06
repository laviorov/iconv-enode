-module(iconv_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, 
     {{one_for_all, 10, 1},
      [
       {iconv, {iconv, start_link, []}, permanent, 1000, worker, [iconv]}
      ]}}.
