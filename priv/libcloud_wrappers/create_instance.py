#!/usr/bin/env python3

# Copyright (C) 2014, Erlang Solutions Ltd

import sys
import json
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
pub_key_id = params['pubKeyId']
firewalls = params['firewalls']

cls = get_driver(getattr(Provider, provider))

if provider == 'DUMMY':
    driver = cls(0)
else:
    driver = cls(user_name, api_key)

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

node = driver.create_node(name='libcloud', size=size, image=image)

result = {'id': node.id}
print(json.dumps(result))
