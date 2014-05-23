elibcloud
=========

Erlang wrapper around [Libcloud][libcloud].

It currently work with the "elibcloud" branch of the [hcs42/libcloud GitHub
repository][hcs42/libcloud].

### Creating credentials

Creating Amazon EC2 credentials:

    {ok, Cred} = elibcloud:create_credentials(
                   _Provider = "EC2",
                   _UserName = "My_Access_Key_ID",
                   _Password = "My_Secret_Access_Key")

- Replace `My_Access_Key_ID` and `My_Secret_Access_Key` with the API key and API
  code generated on the [Amazon EC2 web dashboard][amazon-ec2]: click on your
  user name, select Security Credentials, open Access Keys and use the keys
  there (or generate new ones).

Creating HP Cloud credentials:

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

Creating Rackspace credentials using the general `OPENSTACK` provider of Libcloud:

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

Creating Rackspace credentials using the `RACKSPACE` provider of Libcloud:

    {ok, Cred} = elibcloud:create_credentials(
                   _Provider = "RACKSPACE",
                   _UserName = "My_username",
                   _Password = "My_password",
                   _Extra = [{<<"region">>, <<"lon">>}]).

[libcloud]: https://libcloud.readthedocs.org/
[hcs42/libcloud]: https://github.com/hcs42/libcloud/tree/elibcloud
[amazon-ec2]: https://console.aws.amazon.com/ec2/
[hp-cloud]: https://horizon.hpcloud.com/
[rackspace]: https://mycloud.rackspace.com/
