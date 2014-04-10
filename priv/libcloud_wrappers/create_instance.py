#!/usr/bin/env python3

# Copyright (C) 2014, Erlang Solutions Ltd

import sys
import json
import time
from libcloud.compute.types import Provider
from libcloud.compute.providers import get_driver

def error(format, data):
    print(format % data)
    sys.exit(2)

params = json.loads(sys.argv[1])
provider = params['provider']
user_name = params['userName']
password = params['password']
node_name = params['nodeName']
size_id = params['sizeId']
image_id = params['imageId']
pub_key_name = params['pubKeyName']
pub_key_data = params['pubKeyData']
firewalls = params['firewalls']

cls = get_driver(getattr(Provider, provider))

if provider == 'DUMMY':
    driver = cls(0)
else:
    driver = cls(user_name, password)

sizes = driver.list_sizes()
try:
    size = next(x for x in sizes if x.id == size_id)
except StopIteration:
    error("No such size: %s", size_id)

images = driver.list_images()
try:
    image = next(x for x in images if x.id == image_id)
except StopIteration:
    error("No such image: %s", image_id)

# Upload keypair
try:
    driver.import_key_pair_from_string(name=pub_key_name,
                                       key_material=pub_key_data)
except Exception as e:
    # If the key already exists, that is fine
    if 'InvalidKeyPair.Duplicate' in e.args[0]:
        raise

node = driver.create_node(name=node_name, size=size, image=image,
                          ex_keyname=pub_key_name)

if params.get('waitUntilRunning'):
    booting_time = params.get('bootingTime', 0)
    wait_period = params.get('waitPeriod', 3)
    timeout = params.get('timeout', 600)
    time.sleep(booting_time)
    driver.wait_until_running([node], wait_period, timeout)

debug = []
while True:
    node = next(x for x in driver.list_nodes() if x.id == node.id)
    if node.public_ips == []:
        debug.append("wait")
        time.sleep(60)
    else:
        break

result = {'id': node.id,
          'publicIps': node.public_ips,
          'debug': debug}
print(json.dumps(result))
