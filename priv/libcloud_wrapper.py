#!/usr/bin/env python3

# Copyright (C) 2014, Erlang Solutions Ltd

import sys
import json
import time

from libcloud.compute.types import Provider
from libcloud.compute.types import KeyPairDoesNotExistError
from libcloud.compute.types import NodeState
from libcloud.compute.base import Node, NodeSize, NodeImage
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


##### Converter functions #####


def jf(obj):
    """Return the JSON-friendly (hence "jf") version of the object."""
    if obj is None:
        return None
    elif isinstance(obj, str):
        return obj
    elif isinstance(obj, bytes):
        return obj.decode('utf-8')
    elif isinstance(obj, int):
        return obj
    elif isinstance(obj, float):
        return obj
    elif isinstance(obj, list):
        return [jf(item) for item in obj]
    elif isinstance(obj, dict):
        return {jf(key): jf(value)
                for key, value in obj.items()}
    elif isinstance(obj, NodeSize):
        return size_to_jf(obj)
    elif isinstance(obj, NodeImage):
        return image_to_jf(obj)
    elif isinstance(obj, Node):
        return node_to_jf(obj)
    else:
        return str(obj)


def size_to_jf(size):
    return {'id':        jf(size.id),
            'name':      jf(size.name),
            'ram':       jf(size.ram),
            'disk':      jf(size.disk),
            'bandwidth': jf(size.bandwidth),
            'price':     jf(size.price),
            'extra':     jf(size.extra)}


def image_to_jf(image):
    return {'id':    jf(image.id),
            'name':  jf(image.name),
            'extra': jf(image.extra)}


def state_to_jf(state):
    for state_name in ('RUNNING',
                       'REBOOTING',
                       'TERMINATED',
                       'PENDING',
                       'UNKNOWN',
                       'STOPPED'):
        if state == getattr(NodeState, state_name):
            return state_name
    return 'UNKNOWN'


def node_to_jf(node):
    d = {'id':          jf(node.id),
         'name':        jf(node.name),
         'state':       state_to_jf(node.state),
         'public_ips':  jf(node.public_ips),
         'private_ips': jf(node.private_ips),
         'extra':       jf(node.extra)}
    if hasattr(node, 'size'):
        d['size'] = jf(node.size)
    if hasattr(node, 'image'):
        d['image'] = jf(node.image)
    return d


##### Business logic functions #####


def list_instances(conn, params):
    """List all instances."""

    nodes = conn.list_nodes()
    exit_success(jf(nodes))


def create_instance(conn, params):
    """Create a virtual machine instance."""

    node_name = params['nodeName']
    size_id = params['sizeId']
    image_id = params['imageId']
    key_name = params['keyName']
    sec_group_names = params['securityGroupNames']

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
                                ex_keyname=key_name,
                                ex_security_groups=sec_group_names)
    except KeyPairDoesNotExistError:
        exit_json_err({'error': 'no_such_key',
                       'key_name': key_name})
    except Exception as e:
        if 'InvalidGroup.NotFound' in e.args[0]:
            exit_json_err({'error': 'no_such_group',
                           'group_names': sec_group_names})
        else:
            raise

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
    """Destroy a virtual machine instance."""

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


def list_security_groups(conn, params):
    """List all security groups."""

    sec_groups = conn.ex_list_security_groups()
    exit_success(jf(sec_groups))


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
        elif 'InvalidGroup.InUse' in e.args[0]:
            exit_json_err({'error': 'group_in_use',
                           'group_name': sec_group_name})
        else:
            raise

    if success:
        exit_success({})
    else:
        exit_json_err({'error': 'false_returned'})


def create_security_rules(conn, params):
    """Add a rules a security group."""

    sec_group_name = params['securityGroupName']

    # Rule example: {'protocol': 'tcp', 'from_port': 22, 'to_port': 22}
    sec_rules = params['rules']

    try:
        f = conn.ex_authorize_security_group_ingress
        for rule in sec_rules:
            success = f(sec_group_name,
                        from_port=rule['from_port'],
                        to_port=rule['to_port'],
                        protocol=rule['protocol'])
            if not success:
                exit_json_err({'error': 'false_returned'})
    except Exception as e:
        if 'InvalidGroup.NotFound' in e.args[0]:
            exit_json_err({'error': 'no_such_group',
                           'group_name': sec_group_name})
        else:
            raise
    exit_success({})


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
