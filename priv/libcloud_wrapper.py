#!/usr/bin/env python3

#=============================================================================
# Copyright (C) 2014, Erlang Solutions Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#=============================================================================

import datetime
import json
import socket
import sys
import time

from libcloud.compute.types import Provider
from libcloud.compute.types import KeyPairDoesNotExistError, InvalidCredsError
from libcloud.compute.types import NodeState
from libcloud.compute.base import Node, NodeSize, NodeImage
from libcloud.compute.providers import get_driver


##### Helper functions #####


def is_exception(e, string):
    """Check if the given exception is a known Libcloud exception with the
    given content."""

    try:
        if string in e.args[0]:
            return True
    except Exception:
        pass
    return False


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


##### Business logic helper functions #####


def assert_provider(params, supported_providers):
    if params['provider'] not in supported_providers:
        exit_json_err({'error': 'action_not_supported_on_provider',
                       'action': params['action'],
                       'provider': params['provider']})


def find_security_group_by_name_ec2(conn, sec_group_name):
    sec_groups = conn.ex_get_security_groups(group_names=[sec_group_name])
    if len(sec_groups) == 0:
        exit_json_err({'error': 'no_such_group'},
                      {'group_name': sec_group_name})
    else:
        return sec_groups[0]

def find_security_group_openstack(conn, f, error_dict):
    sec_groups = conn.ex_list_security_groups()
    try:
        return next(x for x in sec_groups if f(x))
    except StopIteration:
        error_dict.update({'error': 'no_such_group'})
        exit_json_err(error_dict)


def find_security_group_by_name_openstack(conn, sec_group_name):
    return find_security_group_openstack(
               conn,
               lambda group: group.name == sec_group_name,
               {'group_name': sec_group_name})


def find_security_group_by_id_openstack(conn, sec_group_id):
    return find_security_group_openstack(
               conn,
               lambda group: group.id == sec_group_id,
               {'group_id': sec_group_id})


def sec_group_names_to_objects_openstack(conn, sec_group_names):
    if len(sec_group_names) == 0:
        return []
    sec_groups = conn.ex_list_security_groups()
    d = {group.name: group for group in sec_groups}
    return [d[name] for name in sec_group_names]


##### Business logic functions #####


def list_sizes(conn, params):
    """List all available sizes."""

    sizes = conn.list_sizes()
    exit_success(jf(sizes))


def list_images(conn, params):
    """List all available images."""

    images = conn.list_images()
    exit_success(jf(images))


def list_nodes(conn, params):
    """List all nodes."""

    nodes = conn.list_nodes()
    exit_success(jf(nodes))


def create_node(conn, params):
    """Create a virtual machine node."""

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
    if params['provider'] == 'EC2':
        images = conn.list_images(ex_image_ids=[image_id])
    else:
        images = conn.list_images()

    try:
        image = next(x for x in images if x.id == image_id)
    except StopIteration:
        exit_json_err({'error': 'no_such_image',
                       'image_id': image_id})

    # Create node
    try:
        if params['provider'] == 'EC2':
            ex_security_groups = sec_group_names
        elif params['provider'] == 'OPENSTACK_HP':
            ex_security_groups = sec_group_names_to_objects_openstack(
                                     conn, sec_group_names)
        else:
            assert sec_group_names == []
            ex_security_groups = []
        node = conn.create_node(name=node_name, size=size, image=image,
                                ex_keyname=key_name,
                                ex_security_groups=ex_security_groups)
    except KeyPairDoesNotExistError:
        exit_json_err({'error': 'no_such_key',
                       'key_name': key_name})
    except Exception as e:
        if is_exception(e, 'InvalidGroup.NotFound'):
            exit_json_err({'error': 'no_such_group',
                           'group_names': sec_group_names})
        else:
            raise

    booting_time = params.get('bootingTime', 0)
    wait_period = params.get('waitPeriod', 3)
    timeout = params.get('timeout', 600)
    time.sleep(booting_time)
    conn.wait_until_running([node], wait_period, timeout)

    if params['provider'] == 'OPENSTACK_HP':
        node = next(x for x in conn.list_nodes() if x.id == node.id)
        ip = conn.ex_create_floating_ip()
        conn.ex_attach_floating_ip_to_node(node, ip)

    # Wait until the public IP of the node appears
    ip_wait_timeout = params.get('ipWaitTimeout', 10 * 60) # 10 minutes
    timeout_dt = (datetime.datetime.now() +
                  datetime.timedelta(minutes=ip_wait_timeout))
    while datetime.datetime.now() < timeout_dt:
        node = next(x for x in conn.list_nodes() if x.id == node.id)
        if node.public_ips == []:
            time.sleep(60)
        else:
            break

    exit_success({'id': node.id,
                  'publicIps': node.public_ips})


def destroy_node(conn, params):
    """Destroy a virtual machine node."""

    # Get the node
    if 'nodeId' in params:
        node_id = params['nodeId']
        try:
            node = next(x for x in conn.list_nodes() if x.id == node_id)
        except StopIteration:
            exit_json_err({'error': 'no_such_node',
                           'node_id': node_id})
    elif 'nodeName' in params:
        node_name = params['nodeName']
        try:
            node = next(x for x in conn.list_nodes() if x.name == node_name)
        except StopIteration:
            exit_json_err({'error': 'no_such_node',
                           'node_name': node_name})

    # If we are using the HP cloud, we also want to delete the floating IPs
    # associated with the instance.
    if params['provider'] == 'OPENSTACK_HP':
        ip_str = node.public_ips[0]
        ip = conn.ex_get_floating_ip(ip_str)
        ip.delete()

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
        if (is_exception(e, 'InvalidKeyPair.Duplicate') or
            is_exception(e, '409 Conflict Key pair')):
            exit_json_err({'error': 'key_already_exists',
                           'key_name': key_name,
                           'message': e.args[0]})
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

    assert_provider(params, ('EC2', 'OPENSTACK_HP'))
    sec_groups = conn.ex_list_security_groups()
    if params['provider'] == 'OPENSTACK_HP':
        sec_groups = [s.name for s in sec_groups]

    exit_success(jf(sec_groups))


def create_security_group(conn, params):
    """Create a security group."""

    assert_provider(params, ('EC2', 'OPENSTACK_HP'))
    sec_group_descr = params['description']
    sec_group_name = params['securityGroupName']

    try:
        group = conn.ex_create_security_group(name=sec_group_name,
                                              description=sec_group_descr)
    except Exception as e:
        if is_exception(e, 'InvalidGroup.Duplicate'):
            exit_json_err({'error': 'group_already_exists',
                           'group_name': sec_group_name})
        else:
            raise

    if params['provider'] == 'EC2':
        group_id = group['group_id']
    elif params['provider'] == 'OPENSTACK_HP':
        group_id = group.id

    exit_success({'group_id': group_id})


def delete_security_group_by_name(conn, params):
    """Delete a security group."""

    assert_provider(params, ('EC2', 'OPENSTACK_HP'))

    try:
        use_name = False
        if 'securityGroupId' in params:
            sec_group_id = params['securityGroupId']
            if params['provider'] == 'EC2':
                success = conn.ex_delete_security_group_by_id(
                              group_id=sec_group_id)
            elif params['provider'] == 'OPENSTACK_HP':
                sec_group = find_security_group_by_id_openstack(conn,
                                sec_group_id)
                success = conn.ex_delete_security_group(sec_group)
        elif 'securityGroupName' in params:
            use_name = True
            sec_group_name = params['securityGroupName']
            if params['provider'] == 'EC2':
                success = conn.ex_delete_security_group_by_name(
                              group_name=sec_group_name)
            elif params['provider'] == 'OPENSTACK_HP':
                sec_group = find_security_group_by_name_openstack(conn,
                                sec_group_name)
                success = conn.ex_delete_security_group(sec_group)

    except Exception as e:
        if use_name:
            error_dict = {'group_name': sec_group_name}
        else:
            error_dict = {'group_id': sec_group_id}

        if is_exception(e, 'InvalidGroup.NotFound'):
            error_dict.update({'error': 'no_such_group'})
            exit_json_err(error_dict)
        elif is_exception(e, 'InvalidGroup.InUse'):
            error_dict.update({'error': 'group_in_use'})
            exit_json_err(error_dict)
        else:
            raise

    if success:
        exit_success({})
    else:
        exit_json_err({'error': 'false_returned'})


def create_security_rules(conn, params):
    """Add a rules a security group."""

    assert_provider(params, ('EC2', 'OPENSTACK_HP'))
    if params['provider'] == 'EC2':
        return create_security_rules_ec2(conn, params)
    elif params['provider'] == 'OPENSTACK_HP':
        return create_security_rules_openstack(conn, params)


def create_security_rules_ec2(conn, params):

    # Rule example: {'protocol': 'tcp', 'from_port': 22, 'to_port': 22}
    sec_rules = params['rules']

    try:
        use_name = False
        if 'securityGroupId' in params:
            sec_group_id = params['securityGroupId']
        elif 'securityGroupName' in params:
            use_name = True
            sec_group_name = params['securityGroupName']
            sec_group_id = find_security_group_by_name_ec2(conn, sec_group_name).id

        f = conn.ex_authorize_security_group_ingress
        for rule in sec_rules:
            try:
                success = f(sec_group_id,
                            from_port=rule['from_port'],
                            to_port=rule['to_port'],
                            protocol=rule['protocol'],
                            cidr_ips=['0.0.0.0/0'])
                if not success:
                    exit_json_err({'error': 'false_returned'})
            except Exception as e:
                if is_exception(e, 'InvalidPermission.Duplicate'):
                    # The rules might contain duplicates, but that is OK.
                    pass
                else:
                    raise
    except Exception as e:
        if (is_exception(e, 'InvalidGroup.NotFound') or
            is_exception(e, 'InvalidGroupId.Malformed')):
            if use_name:
                exit_json_err({'error': 'no_such_group',
                               'group_name': sec_group_name})
            else:
                exit_json_err({'error': 'no_such_group',
                               'group_id': sec_group_id})
        else:
            raise
    exit_success({})


def create_security_rules_openstack(conn, params):

    # Rule example: {'protocol': 'tcp', 'from_port': 22, 'to_port': 22}
    sec_rules = params['rules']

    if 'securityGroupId' in params:
        sec_group_id = params['securityGroupId']
        sec_group = find_security_group_by_id_openstack(conn, sec_group_id)
    elif 'securityGroupName' in params:
        sec_group_name = params['securityGroupName']
        sec_group = find_security_group_by_name_openstack(conn, sec_group_name)

    try:

        f = conn.ex_create_security_group_rule
        for rule in sec_rules:
            try:
                conn.ex_create_security_group_rule(
                    security_group=sec_group,
                    from_port=rule['from_port'],
                    to_port=rule['to_port'],
                    ip_protocol=rule['protocol'],
                    cidr='0.0.0.0/0')
            except Exception as e:
                if is_exception(e, '400 Bad Request This rule already exists'):
                    # The rules might contain duplicates, but that is OK.
                    pass
                else:
                    raise
    except Exception as e:
        raise
    exit_success({})


def fetch(d, key):
    """Get a key from a dictionary. Exit if the key is not found."""
    try:
        return d[key]
    except KeyError:
        exit_json_err({'error': 'mandatory_field_not_specified',
                       'key': 'extra/' + key,
                       'dict': d})


def connect(params):
    """Connect to the cloud provider."""

    provider = params['provider']
    user_name = params['userName']
    password = params['password']
    extra = params.get('extra', {})

    # Create connection

    if provider == 'DUMMY':
        conn = get_driver(Provider.DUMMY)(0)

    elif provider == 'EC2':
        conn = get_driver(Provider.EC2)(user_name, password, **extra)
    elif provider in ('OPENSTACK_HP', 'OPENSTACK_RACKSPACE'):
        conn = get_driver(Provider.OPENSTACK)(user_name, password, **extra)
    elif provider == 'RACKSPACE':
        conn = get_driver(Provider.RACKSPACE)(user_name, password, **extra)
    else:
        exit_json_err({'error': 'unsupported_request',
                       'provider': params['provider']})

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
    try:
        main()
    except InvalidCredsError:
        exit_json_err({'error': 'invalid_creds_error'})
    except socket.gaierror as e:
        exit_json_err({'error': 'socket_error',
                       'details': str(e)})
