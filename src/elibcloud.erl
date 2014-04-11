%%%=============================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd
%%% @author Csaba Hoch <csaba.hoch@erlang-solutions.com>
%%% @doc Interface module for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud).
-copyright("2014, Erlang Solutions Ltd.").

-export([list_instances/3,
         create_instance/8,
         destroy_instance/4,
         get_key_pair/4,
         import_key_pair_from_string/5,
         delete_key_pair/4,
         create_security_group/5,
         delete_security_group_by_name/4,
         create_security_rules/5]).

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

-type elibcloud_func_result(ErrorAtoms) ::
          {'ok', json_term()} |
          {error, {ErrorAtom :: ErrorAtoms,
                   Details :: json_term()}} |
          {'error', string()}.

-type security_rule() :: {FromPort :: integer(),
                          ToPort :: integer(),
                          Port :: tcp | udp | icmp}.

%%%=============================================================================
%%% External functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc List virtual machine instances.
%%
%% In case of success, the result is a JSON list containins JSON objects (once
%% JSON object = one instance). The JSON objects contain the following fields:
%%
%% <ul>
%% <li>`<<"id">>': Node ID.</li>
%% <li>`<<"name">>': Node Name.</li>
%% <li>`<<"state">>' (`"RUNNING" | "REBOOTING" | "TERMINATED" | "PENDING" |
%% "UNKNOWN" | "STOPPED"'): Node State</li>
%% <li>`<<"public_ips">>': Public IP addresses associated with this node.</li>
%% <li>`<<"private_ips">>': Private IP addresses associated with this node.</li>
%% <li>`<<"image">>': Size of this node. (optional)</li>
%% <li>`<<"size">>': Image of this node. (optional)</li>
%% <li>`<<"extra">>': Optional provider specific attributes associated with this
%% node.</li>
%% </ul>
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_instances(Provider   :: string() | binary(),
                     UserName   :: string() | binary(),
                     Password   :: string() | binary()) ->
          elibcloud_func_result(no_predefined_error).
list_instances(Provider, UserName, Password) ->

    JsonInput = [{<<"action">>,     <<"list_instances">>},
                 {<<"provider">>,   bin(Provider)},
                 {<<"userName">>,   bin(UserName)},
                 {<<"password">>,   bin(Password)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, _} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Create a virtual machine instance.
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
                      KeyName    :: string() | binary(),
                      SecurityGroupNames :: [string() | binary()]) ->
          elibcloud_func_result(no_such_size |
                                no_such_image |
                                no_such_key |
                                no_such_group).
create_instance(Provider, UserName, Password, NodeName, SizeId, ImageId,
                KeyName, SecurityGroupNames) ->

    lager:debug("Create instance (NodeName=~p)", [NodeName]),
    JsonInput = [{<<"action">>,             <<"create_instance">>},
                 {<<"provider">>,           bin(Provider)},
                 {<<"userName">>,           bin(UserName)},
                 {<<"password">>,           bin(Password)},
                 {<<"nodeName">>,           bin(NodeName)},
                 {<<"sizeId">>,             bin(SizeId)},
                 {<<"imageId">>,            bin(ImageId)},
                 {<<"keyName">>,            bin(KeyName)},
                 {<<"securityGroupNames">>, bin_list(SecurityGroupNames)}],

    case libcloud_wrapper(JsonInput) of
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
%% @doc Destroy a virtual machine instance.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec destroy_instance(Provider   :: string() | binary(),
                       UserName   :: string() | binary(),
                       Password   :: string() | binary(),
                       NodeId     :: string() | binary()) ->
          elibcloud_func_result(no_such_instance).
destroy_instance(Provider, UserName, Password, NodeId) ->

    lager:debug("Destroy instance (NodeId=~p)", [NodeId]),
    JsonInput = [{<<"action">>,     <<"destroy_instance">>},
                 {<<"provider">>,   bin(Provider)},
                 {<<"userName">>,   bin(UserName)},
                 {<<"password">>,   bin(Password)},
                 {<<"nodeId">>,     bin(NodeId)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Instance destroyed successfully (NodeId=~p)",
                        [NodeId]),
            {ok, JsonRes};
        {error, String} = Error ->
            lager:debug("Instance destruction error (NodeId=~p): ~p",
                        [NodeId, String]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Get a key pair.
%%
%% Contents of the result in case of success:
%%
%% <ul>
%% <li>`<<"name">> :: binary()' - Key name.</li>
%% <li>`<<"fingerprint">> :: binary()'</li>
%% <li>`<<"public_key">> :: binary()'</li>
%% <li>`<<"private_key">> :: binary()'</li>
%% <li>`<<"extra">> :: term()'</li>
%% </li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec get_key_pair(Provider    :: string() | binary(),
                   UserName    :: string() | binary(),
                   Password    :: string() | binary(),
                   KeyName     :: string() | binary()) ->
          elibcloud_func_result(no_such_key).
get_key_pair(Provider, UserName, Password, KeyName) ->
    JsonInput = [{<<"action">>,      <<"get_key_pair">>},
                 {<<"provider">>,    bin(Provider)},
                 {<<"userName">>,    bin(UserName)},
                 {<<"password">>,    bin(Password)},
                 {<<"keyName">>,     bin(KeyName)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, Error} ->
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Import a key pair from a string.
%%
%% Contents of the result in case of success:
%%
%% <ul>
%% <li>`<<"group_id">> :: binary()' - The ID of the security group.</li>
%% </li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec import_key_pair_from_string(Provider    :: string() | binary(),
                                  UserName    :: string() | binary(),
                                  Password    :: string() | binary(),
                                  KeyName     :: string() | binary(),
                                  KeyMaterial :: string() | binary()) ->
          elibcloud_func_result(key_already_exists).
import_key_pair_from_string(Provider, UserName, Password, KeyName,
                            KeyMaterial) ->

    lager:debug("Import key pair from string (KeyName=~p)", [KeyName]),
    JsonInput = [{<<"action">>,      <<"import_key_pair_from_string">>},
                 {<<"provider">>,    bin(Provider)},
                 {<<"userName">>,    bin(UserName)},
                 {<<"password">>,    bin(Password)},
                 {<<"keyName">>,     bin(KeyName)},
                 {<<"keyMaterial">>, bin(KeyMaterial)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Key pair imported successfully (KeyName=~p)",
                        [KeyName]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Key pair import error (KeyName=~p): ~p",
                        [KeyName, Error]),
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a key pair.
%%
%% In case of success, the result is an empty JSON object.
%%
%% <ul>
%% @end
%%------------------------------------------------------------------------------
-spec delete_key_pair(Provider    :: string() | binary(),
                      UserName    :: string() | binary(),
                      Password    :: string() | binary(),
                      KeyName     :: string() | binary()) ->
          elibcloud_func_result(no_such_key).
delete_key_pair(Provider, UserName, Password, KeyName) ->
    JsonInput = [{<<"action">>,      <<"delete_key_pair">>},
                 {<<"provider">>,    bin(Provider)},
                 {<<"userName">>,    bin(UserName)},
                 {<<"password">>,    bin(Password)},
                 {<<"keyName">>,     bin(KeyName)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, Error} ->
            {error, Error}
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
          elibcloud_func_result(group_already_exists).
create_security_group(Provider, UserName, Password, SecurityGroupName,
                      Description) ->

    lager:debug("Create security group (Name=~p)", [SecurityGroupName]),
    JsonInput = [{<<"action">>,            <<"create_security_group">>},
                 {<<"provider">>,          bin(Provider)},
                 {<<"userName">>,          bin(UserName)},
                 {<<"password">>,          bin(Password)},
                 {<<"securityGroupName">>, bin(SecurityGroupName)},
                 {<<"description">>,       bin(Description)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security group created successfully (Name=~p)",
                        [SecurityGroupName]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security group creation error (Name=~p): ~p",
                        [SecurityGroupName, Error]),
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a security group.
%%
%% In case of success, the result is an empty JSON object.
%% @end
%%------------------------------------------------------------------------------
-spec delete_security_group_by_name(Provider          :: string() | binary(),
                                    UserName          :: string() | binary(),
                                    Password          :: string() | binary(),
                                    SecurityGroupName :: string() | binary()) ->
          elibcloud_func_result(no_such_group |
                                group_in_use).
delete_security_group_by_name(Provider, UserName, Password,
                              SecurityGroupName) ->

    lager:debug("Delete security group (Name=~p)", [SecurityGroupName]),
    JsonInput = [{<<"action">>,            <<"delete_security_group_by_name">>},
                 {<<"provider">>,          bin(Provider)},
                 {<<"userName">>,          bin(UserName)},
                 {<<"password">>,          bin(Password)},
                 {<<"securityGroupName">>, bin(SecurityGroupName)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security group created successfully (Name=~p)",
                        [SecurityGroupName]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security group creation error (Name=~p): ~p",
                        [SecurityGroupName, Error]),
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Add a rule to a security group.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_security_rules(Provider          :: string() | binary(),
                            UserName          :: string() | binary(),
                            Password          :: string() | binary(),
                            SecurityGroupName :: string() | binary(),
                            Rules             :: [security_rule()]) ->
          elibcloud_func_result(no_such_group).
create_security_rules(Provider, UserName, Password, SecurityGroupName,
                      Rules) ->

    lager:debug("Create security rule (SecurityGroupName=~p)",
                [SecurityGroupName]),
    JsonInput = [{<<"action">>,            <<"create_security_rules">>},
                 {<<"provider">>,          bin(Provider)},
                 {<<"userName">>,          bin(UserName)},
                 {<<"password">>,          bin(Password)},
                 {<<"securityGroupName">>, bin(SecurityGroupName)},
                 {<<"rules">>,             security_rules_to_json(Rules)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security rule created successfully "
                        "(SecurityGroupName=~p)", [SecurityGroupName]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security rule creation error "
                        "(SecurityGroupName=~p): ~p",
                        [SecurityGroupName, Error]),
            {error, Error}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% TODO We should not send all input parameter as an argument, because they
%% contain sensitive data (like the password)
-spec libcloud_wrapper(JsonInput :: json_term()) ->
          {ok, json_term()} |
          {error, {atom(), json_term()}} |
          {error, string()}.
libcloud_wrapper(JsonInput) ->
    JsonTextInput = jsx:encode(JsonInput),
    Script = filename:join([code:priv_dir(elibcloud),
                            "libcloud_wrapper.py"]),
    {Ret, Output} = command(Script, [JsonTextInput]),
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

-spec security_rules_to_json([security_rule()]) -> json_term().
security_rules_to_json([]) ->
    [];
security_rules_to_json([Rule|Rules]) ->
    [security_rule_to_json(Rule)|security_rules_to_json(Rules)].

-spec security_rule_to_json(security_rule()) -> json_term().
security_rule_to_json({FromPort, ToPort, Protocol}) ->
    [{<<"from_port">>, FromPort},
     {<<"to_port">>, ToPort},
     {<<"protocol">>, protocol_to_bin(Protocol)}].

-spec protocol_to_bin(atom()) -> binary().
protocol_to_bin(Protocol) when Protocol =:= tcp;
                               Protocol =:= udp;
                               Protocol =:= icmp ->
    list_to_binary(atom_to_list(Protocol)).

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
