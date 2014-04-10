#!/usr/bin/env python3

# Copyright (C) 2014, Erlang Solutions Ltd

import sys
import json
import time

from libcloud.compute.types import Provider, KeyPairDoesNotExistError
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
    key_name = params['keyName']

    # Get size
    sizes = conn.list_sizes()
    try:
        size = next(x for x in sizes if x.id == size_id)
    except StopIteration:
        exit_json_err({'error': 'no_such_size',
                       'size_id': size_id})

    # Get image
    images = conn.list_images()
    try:
        image = next(x for x in images if x.id == image_id)
    except StopIteration:
        exit_json_err({'error': 'no_such_image',
                       'image_id': image_id})

    # Create node
    try:
        node = conn.create_node(name=node_name, size=size, image=image,
                                ex_keyname=key_name)
    except KeyPairDoesNotExistError:
        exit_json_err({'error': 'no_such_key',
                       'key_name': key_name})

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


def destroy_instance(conn, params):
    """Destroy a virtual machine instance in the cloud."""

    node_id = params['nodeId']

    # Get node
    try:
        node = next(x for x in conn.list_nodes() if x.id == node_id)
    except StopIteration:
        exit_json_err({'error': 'no_such_instance',
                       'node_id': node_id})

    if node.destroy():
        exit_success({})
    else:
        exit_json_err({'error': 'false_returned'})


def get_key_pair(conn, params):
    """Get a key pair."""

    key_name = params['keyName']

    # Upload keypair
    try:
        key_pair = conn.get_key_pair(name=key_name)
    except KeyPairDoesNotExistError:
        exit_json_err({'error': 'no_such_key',
                       'key_name': key_name})

    exit_success({'name': key_pair.name,
                  'fingerprint': key_pair.fingerprint,
                  'public_key': key_pair.public_key,
                  'private_key': key_pair.private_key,
                  'extra': key_pair.extra})


def import_key_pair_from_string(conn, params):
    """Import a key pair from a string."""

    key_name = params['keyName']
    key_material = params['keyMaterial']

    # Upload keypair
    try:
        key_pair = conn.import_key_pair_from_string(name=key_name,
                                                    key_material=key_material)
    except Exception as e:
        if 'InvalidKeyPair.Duplicate' in e.args[0]:
            exit_json_err({'error': 'key_already_exists',
                           'key_name': key_name})
        else:
            raise

    exit_success({})


def delete_key_pair(conn, params):
    """Delete a key pair."""

    key_name = params['keyName']

    # Upload keypair
    try:
        key_pair = conn.get_key_pair(name=key_name)
        conn.delete_key_pair(key_pair)
    except KeyPairDoesNotExistError:
        exit_json_err({'error': 'no_such_key',
                       'key_name': key_name})

    exit_success({})


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

    if success:
        exit_success({})
    else:
        exit_json_err({'error': 'false_returned'})


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
    globs = globals()
    if action in globs:
        globs[action](conn, params)
    else:
        exit_str_err("No such action: %s", action)


if __name__ == '__main__':
    main()
