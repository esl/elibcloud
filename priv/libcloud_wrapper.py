#!/usr/bin/env python3

# Copyright (C) 2014, Erlang Solutions Ltd

import sys
import json
import time

from libcloud.compute.types import Provider
from libcloud.compute.providers import get_driver


##### Exit functions #####


def exit_success(result):
    print(json.dumps(result))
    sys.exit(0)


def exit_str_err(format, data):
    print(format % data)
    sys.exit(2)


def exit_json_err(result):
    print(json.dumps(result))
    sys.exit(3)


##### Business logic functions #####


def create_instance(conn, params):
    """Create a virtual machine instance in the cloud."""

    node_name = params['nodeName']
    size_id = params['sizeId']
    image_id = params['imageId']
    pub_key_name = params['pubKeyName']
    pub_key_data = params['pubKeyData']

    # Get size
    sizes = conn.list_sizes()
    try:
        size = next(x for x in sizes if x.id == size_id)
    except StopIteration:
        exit_str_err("No such size: %s", size_id)

    # Get image
    images = conn.list_images()
    try:
        image = next(x for x in images if x.id == image_id)
    except StopIteration:
        exit_str_err("No such image: %s", image_id)

    # Upload keypair
    try:
        conn.import_key_pair_from_string(name=pub_key_name,
                                         key_material=pub_key_data)
    except Exception as e:
        # If the key already exists, that is fine
        if 'InvalidKeyPair.Duplicate' not in e.args[0]:
            raise

    # Create node
    node = conn.create_node(name=node_name, size=size, image=image,
                            ex_keyname=pub_key_name)

    if params.get('waitUntilRunning'):
        booting_time = params.get('bootingTime', 0)
        wait_period = params.get('waitPeriod', 3)
        timeout = params.get('timeout', 600)
        time.sleep(booting_time)
        conn.wait_until_running([node], wait_period, timeout)

    # Wait until the public IP of the node appears
    while True:
        node = next(x for x in conn.list_nodes() if x.id == node.id)
        if node.public_ips == []:
            time.sleep(60)
        else:
            break

    exit_success({'id': node.id,
                 'publicIps': node.public_ips})


def create_security_group(conn, params):
    """Create a security group."""

    sec_group_descr = params['description']
    sec_group_name = params['securityGroupName']

    try:
        group = conn.ex_create_security_group(name=sec_group_name,
                                              description=sec_group_descr)
    except Exception as e:
        if 'InvalidGroup.Duplicate' in e.args[0]:
            exit_json_err({'error': 'group_already_exists',
                       'group_name': sec_group_name})
        else:
            raise
    exit_success({'group_id': group['group_id']})


def delete_security_group_by_name(conn, params):
    """Delete a security group."""

    sec_group_name = params['securityGroupName']

    try:
        success = conn.ex_delete_security_group_by_name(group_name=sec_group_name)
    except Exception as e:
        if 'InvalidGroup.NotFound' in e.args[0]:
            exit_json_err({'error': 'no_such_group',
                       'group_name': sec_group_name})
        else:
            raise
    exit_success({'success': success})


def connect(params):
    """Connect to the cloud provider."""

    provider = params['provider']
    user_name = params['userName']
    password = params['password']

    # Create connection

    cls = get_driver(getattr(Provider, provider))

    if provider == 'DUMMY':
        conn = cls(0)
    else:
        conn = cls(user_name, password)
    return conn


def main():
    params = json.loads(sys.argv[1])
    conn = connect(params)

    action = params['action']
    if action == 'create_instance':
        create_instance(conn, params)
    elif action == 'create_security_group':
        create_security_group(conn, params)
    elif action == 'delete_security_group_by_name':
        delete_security_group_by_name(conn, params)
    else:
        exit_str_err("No such action: %s", action)


if __name__ == '__main__':
    main()
