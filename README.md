elibcloud
=========

Erlang wrapper around [Libcloud][libcloud].

It currently works with the "elibcloud" branch of the [hcs42/libcloud GitHub
repository][hcs42/libcloud].

### Installation

Install Python 3. It is recommended to use virtualenv.

Install Libcloud from [hcs42/libcloud GitHub repository][hcs42/libcloud]:

    $ git clone git@github.com:hcs42/libcloud.git
    $ git checkout elibcloud
    $ cd libcloud
    $ pip install -e .

When starting your Erlang node, make sure that the first `python3` executable in
the `PATH` is the same as the one that has Libcloud installed.

Clone elibcloud and start it from the shell:

    $ git clone git@github.com:esl/libcloud.git
    $ cd elarm
    $ make
    $ erl -pa ../elibcloud/ebin -pa deps/goldrush/ebin -pa deps/jsx/ebin -pa deps/lager/ebin
    > application:start(syntax_tools).
    > application:start(goldrush).
    > application:start(lager).

Try elibcloud with a DUMMY provider, which will print the details of a dummy
node:

    1> {ok, Cred} = elibcloud:create_credentials(
                          _Provider = "DUMMY",
                          _UserName = "something",
                          _Password = "something").
    {ok,[{<<"provider">>,<<"DUMMY">>},
         {<<"userName">>,<<"something">>},
         {<<"password">>,<<"something">>},
         {<<"extra">>,[{}]}]}

    2> elibcloud:list_nodes(Cred).
    {ok,[[{<<"state">>,<<"RUNNING">>},
          {<<"image">>,null},
          {<<"public_ips">>,[<<"127.0.0.1">>]},
          {<<"size">>,null},
          {<<"id">>,<<"1">>},
          {<<"private_ips">>,[]},
          {<<"extra">>,[{<<"foo">>,<<"bar">>}]},
          {<<"name">>,<<"dummy-1">>}],
         [{<<"state">>,<<"RUNNING">>},
          {<<"image">>,null},
          {<<"public_ips">>,[<<"127.0.0.1">>]},
          {<<"size">>,null},
          {<<"id">>,<<"2">>},
          {<<"private_ips">>,[]},
          {<<"extra">>,[{<<"foo">>,<<"bar">>}]},
          {<<"name">>,<<"dummy-2">>}]]}

For using elibcloud in a project, you can add it as a rebar dependency.

### Usage

To use elibcloud, first you need to create a "credentials" term which contains
the details of your provider and account.

When you call a function that performs that operation, you then always pass the
credentials term (e.g. by invoking `list_nodes(Cred)`).

### Creating credentials

**Creating Amazon EC2 credentials:**

    {ok, Cred} = elibcloud:create_credentials(
                   _Provider = "EC2",
                   _UserName = "My_Access_Key_ID",
                   _Password = "My_Secret_Access_Key")

- Replace `My_Access_Key_ID` and `My_Secret_Access_Key` with the API key and API
  code generated on the [Amazon EC2 web dashboard][amazon-ec2]: click on your
  user name, select Security Credentials, open Access Keys and use the keys
  there (or generate new ones).

**Creating HP Cloud credentials:**

    {ok, Cred} = elibcloud:create_credentials(
                   _Provider = "OPENSTACK_HP",
                   _UserName = "My_username",
                   _Password = "My_password",
                   _Extra = [{<<"ex_force_auth_url">>, <<"https://region-a.geo-1.identity.hpcloudsvc.com:35357/v2.0/tokens">>},
                             {<<"ex_force_auth_version">>, <<"2.0_password">>},
                             {<<"ex_tenant_name">>, <<"My_project_name">>},
                             {<<"ex_force_service_name">>, <<"Compute">>},
                             {<<"ex_force_service_region">>, <<"region-b.geo-1">>}]);

- Replace `My_project_name` with the number in the URL after you log in to the
  [HP Cloud dashboard][hp-cloud].

**Creating Rackspace credentials using the general `OPENSTACK` provider of Libcloud:**

    {ok, Cred} = elibcloud:create_credentials(
                   _Provider = "OPENSTACK_RACKSPACE",
                   _UserName = "My_username",
                   _Password = "My_password",
                   _Extra = [{<<"ex_force_auth_url">>, <<"https://lon.identity.api.rackspacecloud.com/v2.0/tokens">>},
                             {<<"ex_force_auth_version">>, <<"2.0_password">>},
                             {<<"ex_tenant_name">>, <<"My_tenant_name">>},
                             {<<"ex_force_service_name">>, <<"cloudServersOpenStack">>},
                             {<<"ex_force_service_region">>, <<"LON">>}]);

- Replace `My_tenant_name` with the number in the URL after you log in to the
  [Rackspace dashboard][rackspace].

**Creating Rackspace credentials using the `RACKSPACE` provider of Libcloud:**

    {ok, Cred} = elibcloud:create_credentials(
                   _Provider = "RACKSPACE",
                   _UserName = "My_username",
                   _Password = "My_password",
                   _Extra = [{<<"region">>, <<"lon">>}]).

### Features

**Supported providers:**

- Amazon EC2
- HP Cloud
- Rackspace

**Nodes:**

- List nodes
- Get node
- Create node (i.e. provision a virtual machine instance)
- Destroy node

**Key pairs:**

- Get key pair
- Import key pair from string/file
- Delete key pair

**Security groups:**

- List security groups
- Create a security group
- Delete a security group
- Create security rules

[libcloud]: https://libcloud.readthedocs.org/
[hcs42/libcloud]: https://github.com/hcs42/libcloud/tree/elibcloud
[amazon-ec2]: https://console.aws.amazon.com/ec2/
[hp-cloud]: https://horizon.hpcloud.com/
[rackspace]: https://mycloud.rackspace.com/
