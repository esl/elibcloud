%%%=============================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd
%%% @author Csaba Hoch <csaba.hoch@erlang-solutions.com>
%%% @doc Interface module for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud).
-copyright("2014, Erlang Solutions Ltd.").

-export([create_instance/8]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type json_term() :: list({binary(), json_term()})
                   | list(json_term())
                   | true
                   | false
                   | null
                   | integer()
                   | float()
                   | binary().

%%%=============================================================================
%%% External functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates a virtual machine instance in the cloud.
%%
%% <ul>
%% <li>`<<"id">> :: binary()' - The node ID.</li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec create_instance(Provider  :: string() | binary(),
                      UserName  :: string() | binary(),
                      Password  :: string() | binary(),
                      NodeName  :: string() | binary(),
                      SizeId    :: string() | binary(),
                      ImageId   :: string() | binary(),
                      PubKeyId  :: string() | binary(),
                      Firewalls :: [string() | binary()]) ->
          {'ok', json_term()} |
          {'error', string()}.
create_instance(Provider, UserName, Password, NodeName, SizeId, ImageId,
                PubKeyId, Firewalls) ->

    JsonInput = [{<<"provider">>,  bin(Provider)},
                 {<<"userName">>,  bin(UserName)},
                 {<<"password">>,  bin(Password)},
                 {<<"nodeName">>,  bin(NodeName)},
                 {<<"sizeId">>,    bin(SizeId)},
                 {<<"imageId">>,   bin(ImageId)},
                 {<<"pubKeyId">>,  bin(PubKeyId)},
                 {<<"firewalls">>, bin_list(Firewalls)}],

    case run_script("create_instance", JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, _String} = Error ->
            Error
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% TODO We should not send all input parameter as an argument, because they
%% contain sensitive data (like the password)
-spec run_script(Script :: string(),
                 JsonInput :: json_term()) -> {ok, json_term()} |
                                              {error, string()}.
run_script(Script, JsonInput) ->
    JsonTextInput = jsx:encode(JsonInput),
    {Ret, Output} = command(get_script(Script), [JsonTextInput]),
    case Ret of
        0 ->
            {ok, jsx:decode(list_to_binary(Output))};
        _ ->
            {error, Output}
    end.

%%------------------------------------------------------------------------------
%% @doc Execute the given OS command.
%%
%% The command's output is printed, and its exit code is returned.
%%
%% Original code from
%% http://erlang.org/pipermail/erlang-questions/2007-February/025210.html
%% @end
%%------------------------------------------------------------------------------
-spec command(Cmd :: string(), Args :: [string() | binary()]) ->
          {ExitCode :: integer(),
           Output :: iodata()}.
command(Cmd, Args) ->
     Opts = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof,
             {args, Args}],
     Port = open_port({spawn_executable, Cmd}, Opts),
     command_loop(Port, []).

command_loop(Port, Acc) ->
     receive
         {Port, {data, Data}} ->
             command_loop(Port, [Data|Acc]);
         {Port, eof} ->
             port_close(Port),
             receive
                 {Port, {exit_status, Ret}} ->
                     {Ret, lists:reverse(Acc)}
             end
     end.

-spec get_script(string()) -> FullPath :: string().
get_script(Script) ->
    filename:join([code:priv_dir(elibcloud),
                   "libcloud_wrappers",
                   Script ++ ".py"]).

bin_list(Strings) ->
    [bin(String) || String <- Strings].

bin(Bin) when is_binary(Bin) ->
    Bin;
bin(List) when is_list(List) ->
    list_to_binary(List).

%% str(Bin) ->
%%     binary_to_list(Bin).
%%
%% find(Key, List) ->
%%     case lists:keyfind(Key, 1, List) of
%%         {Key, Value} ->
%%             Value;
%%         false ->
%%             throw({error, {key_not_found, Key, List}})
%%     end.
