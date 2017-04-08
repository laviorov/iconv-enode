-module(iconv).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-export([start_link/0, conv/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%-spec convert(binary(), binary(), binary()) -> {ok, binary()} | {error, Reason}.
conv(From, To, Text) when is_binary(From), is_binary(To), is_binary(Text) ->
	gen_server:call(?MODULE, {conv, From, To, Text}).

%-spec stop() -> ok.
stop() ->
	gen_server:cast(?MODULE, stop).

%------------------------------------------------------------
-record(state, {
	id,
	port,
	mbox, % The Iconv Node gets messages sent to this Mbox.
	retmap = #{}, % The client where results need to be sent back to.
	infotext = [], % Stores up any info text coming from the Iconv Node.
	infoline = [] % Builds up complete lines of info text.
}).
-define(MAX_INFOTEXT_LINES, 1000).

init([]) ->
    Id = "iconv_cnode",
    Tracelevel = 1,
    process_flag(trap_exit, true),
    {Clean_Id, Host, Iconv_Node_Name} = mk_node_name(Id),
    Path = case code:priv_dir(iconv) of
               {error, bad_name} -> os:getenv("PATH");
               Folder -> Folder
           end,
    case os:find_executable("iconv_enode", Path) of
        false -> 
            {stop, executable_not_found};
        Elf -> 
            Cmd = mk_cmdline(Elf, Id, Host, Tracelevel),
            lager:debug("starting iconv:~p", [[{iconv_node, Clean_Id}, {start, Cmd}]]),
            Port = open_port({spawn, Cmd}, [stream, {line, 100}, stderr_to_stdout, exit_status]),
            wait_for_startup(#state{id=Id, port=Port, mbox={iconv, Iconv_Node_Name}})
    end.

mk_cmdline(Elf, Id, Host, Tracelevel) ->
	lists:flatten([
		Elf,
		quote(Id),
		quote(Host),
		quote(atom_to_list(node())),
		quote(atom_to_list(erlang:get_cookie())),
		quote(integer_to_list(Tracelevel))
	]).

wait_for_startup(#state{port=Port} = State) ->
	receive
		{Port, {exit_status, N}} ->
			lager:error("iconv startup failure, exit_status: ~p", [N]),
			{stop, {exit_status, N}};
		{Port, {data, {eol, "READY"}}} ->
			lager:debug("iconv cnode ready"),
			{ok, State};
		{Port, {data, {eol, "."}}} ->
			wait_for_startup(State);
		{Port, {data, {eol, S}}} ->
			lager:debug("iconv startup reply~p", [S]),
			wait_for_startup(State)
	end.


handle_call({conv, FromEnc, To, Text}, {From, Ref}, #state{mbox=Mbox, retmap=M} = State) ->
	Mbox ! {conv, self(), Ref, FromEnc, To, Text},
	{noreply, State#state{retmap = maps:put(Ref, {From, systime()}, M)}}.

handle_cast(stop, State) ->
	{stop, normal, State}.


handle_info({Port, {exit_status, 0}}, #state{port=Port} = State) ->
	{stop, normal, State};
handle_info({Port, {exit_status, N}}, #state{port=Port} = State) ->
	{stop, {port_status, N}, State};
handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
	{stop, {port_exit, Reason}, State};

% Unfinished output lines are tagged with noeol.
handle_info({Port, {data, {noeol, S}}}, #state{port=Port} = State) ->
	{noreply, noeol_port_data(S, State)};
% Finished lines are tagged with eol.
% The convention in the cnode is to send a solitary "." line to signal
% that this particular bit of output is complete; we flush in this case.
handle_info({Port, {data, {eol, "."}}}, #state{port=Port, infoline = []} = State) ->
	{noreply, flush_port_data(State)};
% Otherwise, we handle the complete line.
handle_info({Port, {data, {eol, S}}}, #state{port=Port} = State) ->
	{noreply, eol_port_data(S, State)};

% Finally, we can get proper returns coming from the cnode:
handle_info({error, Ref, Reason}, #state{retmap = M} = State) ->
    {From, _T} = maps:get(Ref, M),
    gen_server:reply({From, Ref}, {error, Reason}),
    {noreply, State#state{retmap = maps:remove(Ref, M)}};
handle_info({ok, Ref, Result}, #state{retmap = M} = State) ->
    case maps:get(Ref, M, undefined) of
        undefined -> 
            {noreply, State};
        {From, _T} ->  
            gen_server:reply({From, Ref}, {ok, Result}),
            M1 = maps:remove(Ref, M),
            Size = maps:size(M1),
            M2 = if Size < 10 -> M1;
                    true -> gcmap(M1)
                 end,
            {noreply, State#state{retmap = M2}}
    end;
handle_info(Info, State) ->
	lager:info("unknowk info ~p", [Info]),
	{noreply, State}.

% remove stuff older than 10 secs
gcmap(M)->
    Now = systime(), 
    maps:fold(fun(Ref, {From, T}, Acc) ->
                      if Now - T > 10*1000000000 -> Acc;
                         true -> maps:put(Ref, {From, T}, Acc)
                      end
              end, #{}, M).

terminate(Reason, #state{mbox=Mbox} = State) ->
	lager:info("iconv terminating for reason ~p", [Reason]),
	Mbox ! stop,
	wait_for_exit(State).

wait_for_exit(#state{port=Port} = State) ->
	receive
		{Port, {exit_status, 0}} ->
			lager:info("iconv cnode exit status ~p", [0]),
			ok;
		{Port, {exit_status, N}} ->
			lager:error("iconv cnode exit status ~p", [N]),
			ok;
		{'EXIT', Port, Reason} ->
			lager:error("iconv cnode exit reason ~p", [Reason]),
			ok;
		{Port, {data, {eol, "."}}} ->
			wait_for_exit(flush_port_data(State));
		{Port, {data, {noeol, S}}} ->
			wait_for_exit(noeol_port_data(S, State));
		{Port, {data, {eol, S}}} ->
			wait_for_exit(eol_port_data(S, State));
		Other ->
			lager:error("iconv cnode exit unknown message ~p", [Other]),
			wait_for_exit(State)
	end.

code_change(_Old, State, _Extra) ->
	{ok, State}.


% We accumulate the std output of cnode line by line; potentially having to assemble
% each line from pieces. Everything is accumulated through list cons'ing.
% Thus results have to be reversed before use.
% We don't accumulate forever, flushing regularly.

% We accumulate the line pieces.
noeol_port_data(S, #state{infotext = Text, infoline = []} = State)
		when length(Text) >= ?MAX_INFOTEXT_LINES ->
	noeol_port_data(S, flush_port_data(State));
noeol_port_data(S, #state{infoline = Line} = State) ->
	State#state{infoline = [S | Line]}.

% We accumulate the completed line into the text.
eol_port_data(S, #state{infotext = Text, infoline = []} = State)
		when length(Text) >= ?MAX_INFOTEXT_LINES ->
	eol_port_data(S, flush_port_data(State));
eol_port_data(S, #state{infotext = Text, infoline = Line} = State) ->
	Full_Line = lists:flatten(lists:reverse([S | Line])),
	State#state{infotext = [Full_Line | Text], infoline = []}.

% We write any info report of the completed text.
% If there's any half accumulated line, then process that first.
flush_port_data(#state{infotext = [], infoline = []} = State) ->
	State;
flush_port_data(#state{infoline = [_ | _]} = State) ->
	flush_port_data(eol_port_data("", State));
flush_port_data(#state{infotext = Text} = State) ->
	case lists:reverse(Text) of
		["FATAL: " ++ S | Rest] -> lager:critical ([S | Rest]);
		["ERROR: " ++ S | Rest] -> lager:error    ([S | Rest]);
		["WARN: " ++ S | Rest] ->  lager:warning  ([S | Rest]);
		["INFO: " ++ S | Rest] ->  lager:info     ([S | Rest]);
		["DEBUG: " ++ S | Rest] -> lager:debug    ([S | Rest]);
		Other ->                   lager:info     (Other, State)
	end,
	State#state{infotext = [], infoline = []}.


mk_node_name(Id) ->
	This_Id = re:replace(Id, "[^_0-9a-zA-Z]+", "_", [global, {return, list}]),
	This_Host = string:sub_word(atom_to_list(node()), 2, $@),
	{This_Id, This_Host, list_to_atom(lists:flatten([This_Id, "@", This_Host]))}.

quote(S) -> [" '", S, "'"].

systime() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).
