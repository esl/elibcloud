elibcloud tutorial
==================

Part I: Installing elibcloud and its dependencies
-------------------------------------------------

#### 1. Virtual Python environment

Create a virtual Python environment. `virtualenv` for Python is like `kerl` for
Erlang or `rbenv` and `rvm` for Ruby.

On Ubuntu, the followings dependencies are needed:

    $ sudo apt-get install python3-dev python3-pip python-virtualenv

Create a virtual Python environment and install `pycrypto`:

    $ virtualenv -p python3 python-libcloud
    $ . python-libcloud/bin/activate  # Mind the dot in the beginning!
    $ pip install pycrypto

Ensure that `CA_CERTS_PATH` works for you by installing the necessary package,
which is different for different operating systems. See a list [here]. For OS X,
you can use brew:

    $ brew install curl-ca-bundle

For Ubuntu, the necessary package is probably already installed.

[here]: http://libcloud.readthedocs.org/en/latest/other/ssl-certificate-validation.html

**Note:** If you open a new shell, you need to run the `activate` script again
to use Libcloud/elibcloud. To avoid that, you can add that line to your
`.bashrc` (or the equivalent file).

#### 2. Install Libcloud

I have submitted a few fixes recently to Libcloud that are needed by elibcloud,
so let's use my branch:

    $ git clone https://github.com/hcs42/libcloud
    $ cd libcloud
    $ pip install -e .

#### 3. Generate RSA keys

    $ ssh-keygen
    <enter>
    <enter>
    <enter>

#### 4. Verify Libcloud installation

    $ python3

    >>> from libcloud.compute.types import Provider
    >>> from libcloud.compute.providers import get_driver
    >>> driver = get_driver(Provider.OPENSTACK)

    # Use the username, password, tenant name as provided
    >>> username = ....
    >>> password = ....
    >>> tenant_name = ....
    >>> conn = driver(username,
                      password,
                      ex_force_auth_url='https://region-a.geo-1.identity.hpcloudsvc.com:35357/v2.0/tokens',
                      ex_force_auth_version='2.0_password',
                      ex_tenant_name=tenant_name,
                      ex_force_service_name='Compute',
                      ex_force_service_region='region-a.geo-1')

    # The following should return a long list of nodes:
    >>> conn.list_sizes()

    # Assign your name to my_key, like 'csaba_hoch_key'
    >>> my_key = ....
    >>> key_pair = conn.import_key_pair_from_file(
                       name=my_key,
                       key_file_path='/home/lubuntu/.ssh/id_rsa.pub')

Log in to https://horizon.hpcloud.com/. Check that the key appeared on the
web interface: Project → Compute → Access & Security → Key Pairs. Delete it.

You can quit from the Python shell with CTRL-D.

#### 5. Install and start elibcloud:

    $ git clone https://github.com/esl/elibcloud
    $ cd elibcloud
    $ make

Part II: Using elibcloud
------------------------

Have a look at the [elibcloud.erl][elibcloud.erl] module. In this section, we
will call the functions exported by this module:

[elibcloud.erl]: https://github.com/esl/elibcloud/blob/master/src/elibcloud.erl

1. First we import an SSH key into the HP Cloud.
2. Next we create a security group and add a security rule to define ports to
   open.
3. We deploy a virtual machine instance which uses the given SSH key and
   security group – i.e. we can SSH into the machine using the SSH key we
   imported, and the machine will have those ports open that we defined in the
   previous step.
4. We terminate the instance, delete the security group and delete the SSH key.

#### 0. Registering an HP Cloud Account

To follow this part of the tutorial, you need to have an HP Cloud account.

Good to know:

- After registering, you get a $100 credit each months for 3 months.

You need:

- A credit card number.
- Your phone: after creating an account, you will be called in 30 minutes by
  someone from HP Cloud for checking the accuracy of the registration details
  and what would you like to use the HP Cloud for.

Steps:

- Go to https://horizon.hpcloud.com/ and click on "Sign up" and follow the
  steps.
- Wait for the call.
- Log in at https://horizon.hpcloud.com/
- Activate the "US West/Compute" service by clicking the "Activate" button.

Log in to https://horizon.hpcloud.com/ and observe there what's happening as a
result of our actions.

#### 1. Starting an Erlang shell with elibcloud

Usually elibcloud is used as part of a release, but it can also be **started
manually**. In that case, we need to specify the paths to the Erlang
modules. If we want to see the lager logs, we need to start a few applications.
Let's also set the log level to `debug` to see what's going on behind the curtain.

    $ erl -pa ../elibcloud/ebin -pa deps/goldrush/ebin -pa deps/jsx/ebin -pa deps/lager/ebin
    > application:start(syntax_tools).
    > application:start(compiler).
    > application:start(goldrush).
    > application:start(lager).
    > lager:set_loglevel(lager_console_backend, debug).

#### 2. Dummy provider

To use the functions that communicate with the cloud, we first need to create a
"credentials" term that will contain the cloud provider's name, our user name
and password. The optional fourth argument may contain extra information needed
for the authentication.

Libcloud and elibcloud have a "DUMMY" provider which is useful for unit test.
Start elibcloud and use the "DUMMY" provider to list the running nodes:

    > {ok, Cred} = elibcloud:create_credentials(
                       _Provider = "DUMMY",
                       _UserName = "something",
                       _Password = "something").
    > elibcloud:list_nodes(Cred).

#### 3. Connecting to a real cloud

Connecting to the HP Cloud requires a bit more work, because we need to provide
some **extra information** for the authentication in the fourth argument of
`create_credentials`. (Elibcloud will pass the contents of this argument to
Libcloud as parameters of the `libcloud.compute.providers.get_driver` function.
The argument needs to be provided in `json_term()` format as defined by the
[JSX][jsx] library, because this way elibcloud can easily and automatically be
pass it to Libcloud, without having to know what is inside.)

[jsx]: https://github.com/talentdeficit/jsx

    % Use the values provided
    > Username = ....
    > Password = ....
    > TenantNameBinary = ....
    > f(Cred).
    > {ok, Cred} = elibcloud:create_credentials(
                     "OPENSTACK_HP", Username, Password,
                     _Extra = [{<<"ex_force_auth_url">>, <<"https://region-a.geo-1.identity.hpcloudsvc.com:35357/v2.0/tokens">>},
                               {<<"ex_force_auth_version">>, <<"2.0_password">>},
                               {<<"ex_tenant_name">>, TenantNameBinary},
                               {<<"ex_force_service_name">>, <<"Compute">>},
                               {<<"ex_force_service_region">>, <<"region-a.geo-1">>}]).

Check the list of nodes:

    > elibcloud:list_nodes(Cred).
    {ok,[]}

#### 4. Importing SSH keys

When creating the virtual machine instance, we want to specify an SSH key, which
will be automatically placed in the `authorized_keys` files of the instance, so
we will be able to log in to the instance without a password.

To do that, we first need to add an SSH key to the cloud provider:

    % Bind MyKeyName to your name, like "csaba_hoch_key"
    > MyKeyName = ....
    > KeyFile = "/home/lubuntu/.ssh/id_rsa.pub".
    > elibcloud:import_key_pair_from_file(Cred, MyKeyName, KeyFile).
    {ok,[{}]}

Check the key in the HP Cloud Console: Project → Cloud → Access & Security → Key
Pairs.

Let's play a bit with the SSH keys, and examine elibcloud's **return values**
and **error handling** in the process.

All exported elibcloud functions that interact with the cloud provider (that is
all of them except `create_credentials/3,4`) return `{ok, json_term()}` in case
of success. That's why we see `{ok,[{}]}` above: the operation was successful,
and there is nothing to return. (`[{}]` is the JSX notation for an empty
object.)

Try to import the SSH key again; we should get a `key_already_exists` error:

    > elibcloud:import_key_pair_from_file(Cred, MyKeyName, KeyFile).
    {error,{key_already_exists,[{<<"message">>,
                                 <<"409 Conflict Key pair 'mykey' already exists.">>},
                                {<<"key_name">>,<<"mykey">>},
                                {<<"error">>,<<"key_already_exists">>}]}}

When an error occurs, and elibcloud recognizes the error, it will return an
`{error, {ErrorAtom, Details}}` tuple like above. The
[elibcloud.erl][elibcloud.erl] module documents the exact error atoms that might
be returned by different functions. Finally, elibcloud function can return
`{error, String}` if something went wrong but it doesn't know what.

Let's delete the SSH key:

    > elibcloud:delete_key_pair(Cred, MyKeyName).
    {ok,[{}]}

Try to delete it again; you should get a `no_such_key` error.

Create the key again, because we will need it later.

#### 5. Creating security groups

Security groups are entities in the cloud that define **a set of ports** to be
open. We can connect security groups with virtual machine instances. Such a
connection will mean that the ports defined by the security group will be open
on the instance.

Let's create a security group:

    % As usual, bind MyGroupName to your name, like "csaba_hoch_group"
    > elibcloud:create_security_group(Cred, MyGroupName, "My group").
    {ok,[{<<"group_id">>,
          <<"31743d2a-9f1d-4ec1-96e4-13c0cf2f23b1">>}]}

Specify that we want the incoming TCP port 22 to be open:

    > Rules = [{22, 22, tcp}].
    > elibcloud:create_security_rules(Cred, {name, MyGroupName}, Rules).

Check the key in the HP Cloud Console: Project → Cloud → Access & Security →
Security Groups → Manage rules (next to your group).

If you don't need the security group and more, it can be deleted with the
following call (if you invoke it now, please recreate the group as we will need
it in the next step):

    > elibcloud:delete_security_group(Cred, {name, MyGroupName}).
    {ok,[{}]}

#### 6. Creating virtual machine instances

Now we have everything ready for an instance: an SSH key and a security group.

Let's create a new instance using these. In Libcloud's terminology, instances
are called nodes, so elibcloud calls them nodes too. You can use "Project →
Compute → Images" to get the id of the virtual machine image to be used.

    > MyNodeName = .... % Your name, e.g. "csaba_hoch_node"

    % A virtual hardware configuration we will use
    > SizeId = "100".

    % The id of the virtual machine image we will use
    > ImageId = "2925ec9a-3c29-4960-acd8-ffe33007ad62".

    % The following takes a few minutes
    > elibcloud:create_node(Cred, MyNodeName, SizeId, ImageId, MyKeyName,
                            [MyGroupName]).
    {ok,[{<<"publicIps">>,[<<"15.125.113.68">>]},
         {<<"id">>,<<"febec630-b0f4-48aa-a510-0e137157331a">>}]}

Check the instance in the HP Cloud Console: Project → Compute → Instances.

SSH into our new instance to verify that it is running, its port 22 is open, and
that it accepts our SSH key (replace the IP address with the one you got from
`create_node`):

    $ ssh ubuntu@15.125.113.68

#### 7. Cleaning up

Let's clean up:

    > elibcloud:destroy_node(Cred, {name, MyNodeName}).
    {ok,[{}]}
    > elibcloud:delete_security_group(Cred, {name, MyGroupName}).
    {ok,[{}]}
    > elibcloud:delete_key_pair(Cred, MyKeyName).
    {ok,[{}]}
