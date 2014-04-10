%%%=============================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd
%%% @author Csaba Hoch <csaba.hoch@erlang-solutions.com>
%%% @doc Interface module for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud).
-copyright("2014, Erlang Solutions Ltd.").

-export([create_instance/9,
         create_security_group/5,
         delete_security_group_by_name/4]).

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
%% @doc Create a virtual machine instance in the cloud.
%%
%% Contents of the result in case of success:
%%
%% <ul>
%% <li>`<<"id">> :: binary()' - The node ID.</li>
%% <li>`<<"publicIps">> :: [binary()]' - The public IP addresses of the node.
%% </li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec create_instance(Provider   :: string() | binary(),
                      UserName   :: string() | binary(),
                      Password   :: string() | binary(),
                      NodeName   :: string() | binary(),
                      SizeId     :: string() | binary(),
                      ImageId    :: string() | binary(),
                      PubKeyName :: string() | binary(),
                      PubKeyData :: string() | binary(),
                      Firewalls  :: [string() | binary()]) ->
          {'ok', json_term()} |
          {'error', string()}.
create_instance(Provider, UserName, Password, NodeName, SizeId, ImageId,
                PubKeyName, PubKeyData, Firewalls) ->

    lager:debug("Create instance (NodeName=~p)", [NodeName]),
    JsonInput = [{<<"provider">>,   bin(Provider)},
                 {<<"userName">>,   bin(UserName)},
                 {<<"password">>,   bin(Password)},
                 {<<"nodeName">>,   bin(NodeName)},
                 {<<"sizeId">>,     bin(SizeId)},
                 {<<"imageId">>,    bin(ImageId)},
                 {<<"pubKeyName">>, bin(PubKeyName)},
                 {<<"pubKeyData">>, bin(PubKeyData)},
                 {<<"firewalls">>,  bin_list(Firewalls)}],

    case run_script("create_instance", JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Instance created successfully (NodeName=~p)",
                        [NodeName]),
            {ok, JsonRes};
        {error, String} = Error ->
            lager:debug("Instance creation error (NodeName=~p): ~p",
                        [NodeName, String]),
            Error
    end.
%%------------------------------------------------------------------------------
%% @doc Create a security group.
%%
%% Contents of the result in case of success:
%%
%% <ul>
%% <li>`<<"group_id">> :: binary()' - The ID of the security group.</li>
%% </li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec create_security_group(Provider          :: string() | binary(),
                            UserName          :: string() | binary(),
                            Password          :: string() | binary(),
                            SecurityGroupName :: string() | binary(),
                            Description       :: string() | binary()) ->
          {'ok', json_term()} |
          {error, {ErrorAtom :: group_already_exists,
                   Details :: json_term()}} |
          {'error', string()}.
create_security_group(Provider, UserName, Password, SecurityGroupName,
                      Description) ->

    lager:debug("Create security group (Name=~p)", [SecurityGroupName]),
    JsonInput = [{<<"action">>,            <<"create">>},
                 {<<"provider">>,          bin(Provider)},
                 {<<"userName">>,          bin(UserName)},
                 {<<"password">>,          bin(Password)},
                 {<<"securityGroupName">>, bin(SecurityGroupName)},
                 {<<"description">>,       bin(Description)}],

    case run_script("manage_security_group", JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security group created successfully (Name=~p)",
                        [SecurityGroupName]),
            {ok, JsonRes};
        {error, String} = Error ->
            lager:debug("Security group creation error (Name=~p): ~p",
                        [SecurityGroupName, String]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a security group.
%%
%% Contents of the result in case of success:
%%
%% <ul>
%% <li>`<<"success">> :: bool()'</li>
%% </li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec delete_security_group_by_name(Provider          :: string() | binary(),
                                    UserName          :: string() | binary(),
                                    Password          :: string() | binary(),
                                    SecurityGroupName :: string() | binary()) ->
          {ok, json_term()} |
          {error, {ErrorAtom :: no_such_group,
                   Details :: json_term()}} |
          {error, string()}.
delete_security_group_by_name(Provider, UserName, Password,
                              SecurityGroupName) ->

    lager:debug("Delete security group (Name=~p)", [SecurityGroupName]),
    JsonInput = [{<<"action">>,            <<"delete_by_name">>},
                 {<<"provider">>,          bin(Provider)},
                 {<<"userName">>,          bin(UserName)},
                 {<<"password">>,          bin(Password)},
                 {<<"securityGroupName">>, bin(SecurityGroupName)}],

    case run_script("manage_security_group", JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security group created successfully (Name=~p)",
                        [SecurityGroupName]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security group creation error (Name=~p): ~p",
                        [SecurityGroupName, Error]),
            {error, Error}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% TODO We should not send all input parameter as an argument, because they
%% contain sensitive data (like the password)
-spec run_script(Script :: string(),
                 JsonInput :: json_term()) -> {ok, json_term()} |
                                              {error, {atom(), json_term()}} |
                                              {error, string()}.
run_script(Script, JsonInput) ->
    JsonTextInput = jsx:encode(JsonInput),
    {Ret, Output} = command(get_script(Script), [JsonTextInput]),
    case Ret of
        0 ->
            % JSON printed at the Python side
            {ok, jsx:decode(list_to_binary(Output))};
        1 ->
            % Python exception not caught -> string output
            {error, Output};
        2 ->
            % Error string printed at the Python side
            {error, Output};
        3 ->
            % Error JSON printed at the Python side
            case jsx:decode(list_to_binary(Output)) of
                {incomplete, _} ->
                    {error, "Incomplete JSON: " ++ Output};
                JsonError ->
                    ErrorAtom =
                        case lists:keyfind(<<"error">>, 1, JsonError) of
                            {_, ErrorBin} ->
                                list_to_atom(binary_to_list(ErrorBin));
                            false ->
                                error_field_missing
                        end,
                    {error, {ErrorAtom, JsonError}}
            end
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
    lager:debug("Run command (Cmd=~p, Args=~p)", [Cmd, Args]),
    Opts = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof,
            {args, Args}],
    Port = open_port({spawn_executable, Cmd}, Opts),
    {Ret, Output} = command_loop(Port, []),
    lager:debug("Command finished (Ret=~p, Output=~p)", [Ret, Output]),
    {Ret, Output}.

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
