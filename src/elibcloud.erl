%%%=============================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd
%%% @author Csaba Hoch <csaba.hoch@erlang-solutions.com>
%%% @doc Interface module for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud).
-copyright("2014, Erlang Solutions Ltd.").

-export([get_instance/2,
         list_instances/1,
         create_instance/6,
         destroy_instance/2,
         get_key_pair/2,
         import_key_pair_from_string/3,
         import_key_pair_from_file/3,
         delete_key_pair/2,
         list_security_groups/1,
         create_security_group/3,
         delete_security_group_by_name/2,
         create_security_rules/3]).

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

%% Supported provider: "EC2".
-type credentials() :: {Provider   :: string() | binary(),
                        UserName   :: string() | binary(),
                        Password   :: string() | binary()}.

-export_type([credentials/0]).

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
-spec list_instances(Credentials :: credentials()) ->
          elibcloud_func_result(no_predefined_error).
list_instances({Provider, UserName, Password}) ->

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
%% @doc Get a virtual machine instance.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_instance(Credentials :: credentials(),
                   NodeId      :: string() | binary()) ->
          elibcloud_func_result(no_such_instance).
get_instance(Credentials, NodeId) ->

    BinNodeId = bin(NodeId),
    case list_instances(Credentials) of
        {ok, Instances} ->
            MatchingInstances =
                [Instance
                 || Instance <- Instances,
                    lists:keyfind(<<"id">>, 1, Instance) =:=
                        {<<"id">>, BinNodeId}],

            case MatchingInstances of
                [] ->
                    {error, {no_such_instance, [{<<"id">>, BinNodeId}]}};
                [Instance] ->
                    Instance;
                _ when is_list(MatchingInstances) ->
                    % This can happen only if the cloud provider is buggy,
                    % that's why it is not reported in the type spec.
                    {error, {multiple_instances, [{<<"id">>, BinNodeId}]}}
            end;
        {error, Error} ->
            {error, Error}
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
-spec create_instance(Credentials        :: credentials(),
                      NodeName           :: string() | binary(),
                      SizeId             :: string() | binary(),
                      ImageId            :: string() | binary(),
                      KeyName            :: string() | binary(),
                      SecurityGroupNames :: [string() | binary()]) ->
          elibcloud_func_result(no_such_size |
                                no_such_image |
                                no_such_key |
                                no_such_group).
create_instance({Provider, UserName, Password}, NodeName, SizeId, ImageId,
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
-spec destroy_instance(Credentials :: credentials(),
                       NodeId      :: string() | binary()) ->
          elibcloud_func_result(no_such_instance).
destroy_instance({Provider, UserName, Password}, NodeId) ->

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
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec get_key_pair(Credentials :: credentials(),
                   KeyName     :: string() | binary()) ->
          elibcloud_func_result(no_such_key).
get_key_pair({Provider, UserName, Password}, KeyName) ->
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
%% In case of success, the result is an empty JSON object.
%% @end
%%------------------------------------------------------------------------------
-spec import_key_pair_from_string(Credentials :: credentials(),
                                  KeyName     :: string() | binary(),
                                  KeyMaterial :: string() | binary()) ->
          elibcloud_func_result(key_already_exists).
import_key_pair_from_string({Provider, UserName, Password}, KeyName,
                            KeyMaterial) ->

    lager:debug("Import key pair (KeyName=~p)", [KeyName]),
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
%% @doc Import a key pair from a file.
%%
%% In case of success, the result is an empty JSON object.
%% @end
%%------------------------------------------------------------------------------
-spec import_key_pair_from_file(Credentials :: credentials(),
                                KeyName     :: string() | binary(),
                                FileName    :: string() | binary()) ->
          elibcloud_func_result(key_already_exists |
                                file_read_error).
import_key_pair_from_file(Credentials, KeyName, FileName) ->

    case file:read_file(FileName) of
        {ok, KeyMaterial} ->
            import_key_pair_from_string(Credentials, KeyName, KeyMaterial);
        {error, Reason} ->
            lager:debug("Key pair import error (KeyName=~p, FileName=~p): ~p",
                        [KeyName, FileName, Reason]),
            Details = [{<<"reason">>, Reason},
                       {<<"keyName">>, KeyName},
                       {<<"fileName">>, FileName}],
            {error, {file_read_error, Details}}
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a key pair.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec delete_key_pair(Credentials :: credentials(),
                      KeyName     :: string() | binary()) ->
          elibcloud_func_result(no_such_key).
delete_key_pair({Provider, UserName, Password}, KeyName) ->
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
%% @doc List security group names.
%%
%% In case of success, the result contains a list containing the names of the
%% security groups as binaries.
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_security_groups(Credentials :: credentials()) ->
          {ok, [SecGroupName :: binary()]} |
          elibcloud_func_result(no_predefined_error).
list_security_groups({Provider, UserName, Password}) ->

    JsonInput = [{<<"action">>,     <<"list_security_groups">>},
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
%% @doc Create a security group.
%%
%% Contents of the result in case of success:
%%
%% <ul>
%% <li>`<<"group_id">> :: binary()' - The ID of the security group.</li>
%% </ul>
%% @end
%%------------------------------------------------------------------------------
-spec create_security_group(Credentials       :: credentials(),
                            SecurityGroupName :: string() | binary(),
                            Description       :: string() | binary()) ->
          elibcloud_func_result(group_already_exists).
create_security_group({Provider, UserName, Password}, SecurityGroupName,
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
-spec delete_security_group_by_name(Credentials       :: credentials(),
                                    SecurityGroupName :: string() | binary()) ->
          elibcloud_func_result(no_such_group |
                                group_in_use).
delete_security_group_by_name({Provider, UserName, Password},
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
-spec create_security_rules(Credentials       :: credentials(),
                            SecurityGroupName :: string() | binary(),
                            Rules             :: [security_rule()]) ->
          elibcloud_func_result(no_such_group).
create_security_rules({Provider, UserName, Password}, SecurityGroupName,
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
