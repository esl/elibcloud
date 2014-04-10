#!/usr/bin/env python3

# Copyright (C) 2014, Erlang Solutions Ltd

import sys
import json

from libcloud.compute.types import Provider
from libcloud.compute.providers import get_driver

def normal_exit(result):
    print(json.dumps(result))
    sys.exit(0)

def text_exit(format, data):
    print(format % data)
    sys.exit(2)

def json_exit(result):
    print(json.dumps(result))
    sys.exit(3)

params = json.loads(sys.argv[1])
action = params['action']
provider = params['provider']
user_name = params['userName']
password = params['password']

if action == 'create':
    sec_group_descr = params['description']
    sec_group_name = params['securityGroupName']
elif action == 'delete_by_name':
    sec_group_name = params['securityGroupName']
else:
    text_exit("No such action: %s", action)

cls = get_driver(getattr(Provider, provider))

if provider == 'DUMMY':
    conn = cls(0)
else:
    conn = cls(user_name, password)

if action == 'create':
    try:
        group = conn.ex_create_security_group(name=sec_group_name,
                                              description=sec_group_descr)
    except Exception as e:
        if 'InvalidGroup.Duplicate' in e.args[0]:
            json_exit({'error': 'group_already_exists',
                       'group_name': sec_group_name})
        else:
            raise
    normal_exit({'group_id': group['group_id']})

elif action == 'delete_by_name':

    try:
        success = conn.ex_delete_security_group_by_name(group_name=sec_group_name)
    except Exception as e:
        if 'InvalidGroup.NotFound' in e.args[0]:
            json_exit({'error': 'no_such_group',
                       'group_name': sec_group_name})
        else:
            raise
    normal_exit({'success': success})
