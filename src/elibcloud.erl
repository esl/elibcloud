%%%=============================================================================
%%% Copyright (C) 2014, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc Interface module for elibcloud.
%%% @end
%%%=============================================================================
-module(elibcloud).

-export([create_credentials/3,
         create_credentials/4,
         get_node/2,
         list_nodes/1,
         create_node/6,
         destroy_node/2,
         get_key_pair/2,
         import_key_pair_from_string/3,
         import_key_pair_from_file/3,
         delete_key_pair/2,
         list_security_groups/1,
         create_security_group/3,
         delete_security_group/2,
         create_security_rules/3]).

-export_type([credentials/0]).

-define(SUPPORTED_PROVIDERS, [<<"EC2">>,
                              <<"OPENSTACK_HP">>,
                              <<"OPENSTACK_RACKSPACE">>,
                              <<"RACKSPACE">>,
                              <<"DUMMY">>]).

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
          {ok, json_term()} |
          {error, {ErrorAtom :: invalid_creds_error |
                                socket_error |
                                unsupported_provider |
                                action_not_supported_on_provider |
                                ErrorAtoms,
                   Details   :: json_term()}} |
          {error, string()}.

-type security_rule() :: {FromPort :: integer(),
                          ToPort   :: integer(),
                          Port     :: tcp | udp | icmp}.

-opaque credentials() :: json_term().

%%%=============================================================================
%%% External functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Return a `credentials()' term which can be passed to other elibcloud
%% functions to set up the connection with the provider.
%%
%% The supported providers are the following: EC2, OPENSTACK_HP,
%% OPENSTACK_RACKSPACE, DUMMY.
%%
%% The OPENSTACK_RACKSPACE provider does not support security groups (on
%% Rackspace instances, all ports are open by default).
%% @end
%%------------------------------------------------------------------------------
-spec create_credentials(Provider :: string() | binary(),
                         UserName :: string() | binary(),
                         Password :: string() | binary()) ->
          {ok, credentials()} |
          {error, {not_supported_provider, binary()}}.
create_credentials(Provider, UserName, Password) ->
    create_credentials(Provider, UserName, Password, []).

-spec create_credentials(Provider :: string() | binary(),
                         UserName :: string() | binary(),
                         Password :: string() | binary(),
                         Extra :: []) ->
          {ok, credentials()} |
          {error, {not_supported_provider, binary()}}.
create_credentials(Provider, UserName, Password, Extra) ->
    BinProv = bin(Provider),
    ExtraObj =
        case Extra of
            [] ->
                [{}]; % Empty JSON object in JSX
            _ ->
                Extra
        end,
    case lists:member(BinProv, ?SUPPORTED_PROVIDERS) of
        true ->
            {ok, [{<<"provider">>, BinProv},
                  {<<"userName">>, bin(UserName)},
                  {<<"password">>, bin(Password)},
                  {<<"extra">>,    ExtraObj}]};
        false ->
            {error, {not_supported_provider, BinProv}}
    end.

%%------------------------------------------------------------------------------
%% @doc List virtual machine nodes.
%%
%% In case of success, the result is a JSON list containins JSON objects (once
%% JSON object = one node). The JSON objects contain the following fields:
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
-spec list_nodes(Credentials :: credentials()) ->
          elibcloud_func_result(no_predefined_error).
list_nodes(Credentials) ->
    JsonInput = Credentials ++
                [{<<"action">>, <<"list_nodes">>}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Node listing error: ~p", [Error]),
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Get a virtual machine node.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_node(Credentials :: credentials(),
               NodeId      :: string() | binary()) ->
          elibcloud_func_result(no_such_node).
get_node(Credentials, NodeId) ->

    BinNodeId = bin(NodeId),
    case list_nodes(Credentials) of
        {ok, Nodes} ->
            MatchingNodes =
                [Node || Node <- Nodes,
                         lists:keyfind(<<"id">>, 1, Node) =:=
                             {<<"id">>, BinNodeId}],

            case MatchingNodes of
                [] ->
                    lager:debug("Node getting error (NodeId=~p): no such node"),
                    {error, {no_such_node, [{<<"id">>, BinNodeId}]}};
                [Node] ->
                    Node;
                _ when is_list(MatchingNodes) ->
                    % This can happen only if the cloud provider is buggy,
                    % that's why it is not reported in the type spec.
                    lager:debug("Node getting error (NodeId=~p): multiple "
                                "nodes"),
                    {error, {multiple_nodes, [{<<"id">>, BinNodeId}]}}
            end;
        {error, Error} ->
            % debug log printed by list_nodes call
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Create a virtual machine node.
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
-spec create_node(Credentials        :: credentials(),
                  NodeName           :: string() | binary(),
                  SizeId             :: string() | binary(),
                  ImageId            :: string() | binary(),
                  KeyName            :: string() | binary(),
                  SecurityGroupNames :: [string() | binary()]) ->
          elibcloud_func_result(no_such_size |
                                no_such_image |
                                no_such_key |
                                no_such_group).
create_node(Credentials, NodeName, SizeId, ImageId, KeyName,
            SecurityGroupNames) ->

    lager:debug("Create node (NodeName=~p)", [NodeName]),
    JsonInput = Credentials ++
                [{<<"action">>,             <<"create_node">>},
                 {<<"nodeName">>,           bin(NodeName)},
                 {<<"sizeId">>,             bin(SizeId)},
                 {<<"imageId">>,            bin(ImageId)},
                 {<<"keyName">>,            bin(KeyName)},
                 {<<"securityGroupNames">>, bin_list(SecurityGroupNames)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Node created successfully (NodeName=~p)", [NodeName]),
            {ok, JsonRes};
        {error, String} = Error ->
            lager:debug("Node creation error (NodeName=~p): ~p",
                        [NodeName, String]),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc Destroy a virtual machine node.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec destroy_node(Credentials :: credentials(),
                   Node        :: {id, string() | binary()} |
                                  {name, string() | binary()}) ->
          elibcloud_func_result(no_such_node).
destroy_node(Credentials, Node) ->

    lager:debug("Destroy node (Node=~p)", [Node]),
    NodeIdOrName =
        case Node of
            {id, Id} ->
                [{<<"nodeId">>, bin(Id)}];
            {name, Name} ->
                [{<<"nodeName">>, bin(Name)}]
        end,
    JsonInput = Credentials ++ NodeIdOrName ++
                [{<<"action">>, <<"destroy_node">>}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Node destroyed successfully (Node=~p)", [Node]),
            {ok, JsonRes};
        {error, String} = Error ->
            lager:debug("Node destruction error (Node=~p): ~p", [Node, String]),
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
get_key_pair(Credentials, KeyName) ->
    JsonInput = Credentials ++
                [{<<"action">>,   <<"get_key_pair">>},
                 {<<"keyName">>,  bin(KeyName)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Get key pair error: ~p", [Error]),
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
import_key_pair_from_string(Credentials, KeyName, KeyMaterial) ->

    lager:debug("Import key pair (KeyName=~p)", [KeyName]),
    JsonInput = Credentials ++
                [{<<"action">>,      <<"import_key_pair_from_string">>},
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
delete_key_pair(Credentials, KeyName) ->
    JsonInput = Credentials ++
                [{<<"action">>,   <<"delete_key_pair">>},
                 {<<"keyName">>,  bin(KeyName)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Delete key pair error: ~p", [Error]),
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
list_security_groups(Credentials) ->

    JsonInput = Credentials ++
                [{<<"action">>, <<"list_security_groups">>}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security group listing error: ~p", [Error]),
            {error, Error}
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
create_security_group(Credentials, SecurityGroupName, Description) ->

    lager:debug("Create security group (Name=~p)", [SecurityGroupName]),
    JsonInput = Credentials ++
                [{<<"action">>,            <<"create_security_group">>},
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
-spec delete_security_group(Credentials   :: credentials(),
                            SecurityGroup :: {id,   string() | binary()} |
                                             {name, string() | binary()}) ->
          elibcloud_func_result(no_such_group |
                                group_in_use).
delete_security_group(Credentials, SecurityGroup) ->

    lager:debug("Delete security group (Group=~p)", [SecurityGroup]),
    GroupIdOrName =
        case SecurityGroup of
            {id, Id} ->
                [{<<"securityGroupId">>, bin(Id)}];
            {name, Name} ->
                [{<<"securityGroupName">>, bin(Name)}]
        end,

    JsonInput = Credentials ++ GroupIdOrName ++
                [{<<"action">>, <<"delete_security_group_by_name">>}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security group deleted successfully (Group=~p)",
                        [SecurityGroup]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security group deletion error (Group=~p): ~p",
                        [SecurityGroup, Error]),
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Add a rule to a security group.
%%
%% In case of success, the result is an empty JSON object.
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_security_rules(Credentials   :: credentials(),
                            SecurityGroup :: {id,   string() | binary()} |
                                             {name, string() | binary()},
                            Rules         :: [security_rule()]) ->
          elibcloud_func_result(no_such_group).
create_security_rules(Credentials, SecurityGroup, Rules) ->

    lager:debug("Create security rule (SecurityGroup=~p)",
                [SecurityGroup]),
    GroupIdOrName =
        case SecurityGroup of
            {id, Id} ->
                [{<<"securityGroupId">>, bin(Id)}];
            {name, Name} ->
                [{<<"securityGroupName">>, bin(Name)}]
        end,

    JsonInput = Credentials ++ GroupIdOrName ++
                [{<<"action">>, <<"create_security_rules">>},
                 {<<"rules">>,  security_rules_to_json(Rules)}],

    case libcloud_wrapper(JsonInput) of
        {ok, JsonRes} ->
            lager:debug("Security rule created successfully "
                        "(SecurityGroup=~p)", [SecurityGroup]),
            {ok, JsonRes};
        {error, Error} ->
            lager:debug("Security rule creation error "
                        "(SecurityGroup=~p): ~p",
                        [SecurityGroup, Error]),
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
%% The command's output is and its exit code are returned.
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

%% Convert a term into a binary, which will be used as a JSON string.
bin(Bin) when is_binary(Bin) ->
    Bin;
bin(List) when is_list(List) ->
    list_to_binary(List);
bin(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

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
