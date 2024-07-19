#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import os
import codecs
import getopt
import re
import fnmatch

def usage():
    print('''usage: python proto.py''')

server_node_r = re.compile(r'//server_node=(.+)')
service_name_r = re.compile(r'//service_name=(.+)')
r1 = re.compile(r'message(.+){')
r2 = re.compile(r'enum(.+){')
r3 = re.compile(r'\[\s*default')

def natural_sort(l):
	convert = lambda text: int(text) if text.isdigit() else text.lower()
	alphanum_key = lambda key: [convert(c) for c in re.split('([0-9]+)', key)]
	return sorted(l, key=alphanum_key)

def gen_proto():
    file_path = os.path.dirname(os.path.abspath("__file__"))
    fn_list = []
    for fn in os.listdir(file_path):
        if os.path.isfile(fn):
            for v in ['.proto']:
                if fnmatch.fnmatch(fn, '*' + v):
                    if fn != 'all.proto':
                        fn_list.append(fn)
    cache = []
    fn_list = natural_sort(fn_list)
    with codecs.open('all.proto', 'w', 'utf8') as f2:
        # header
        f2.write('syntax = "proto3";\n')
        f2.write('package google.protobuf;\n\n')
        for fn in fn_list:
            lst = ['\n']
            in_branch = False
            left_bracket_num = 0
            inner_ids = [0, 0, 0, 0, 0]
            src = os.path.join(file_path, fn)
            server_node = None
            service_name = None

            j = -1
            for i in range(len(fn)):
                if fn[i] >= '0' and fn[i] <= '9':
                    j = i
                else:
                    break
            if j == -1:
                print('file need start with number!')
                exit()
            start_id = int(fn[:j + 1]) * 100
            with codecs.open(src, 'r', 'utf8') as f:
                for line in f.readlines():
                    ret = server_node_r.match(line.strip())
                    if ret is not None:
                        server_node = ret[1].strip()
                        lst.append(line)
                        continue
                    ret = service_name_r.match(line.strip())
                    if ret is not None:
                        service_name = ret[1].strip()
                        lst.append(line)
                        continue
                    if line.strip() == '':
                        lst.append(line)
                        continue
                    ss = line.find('{')
                    if ss != -1:
                        left_bracket_num += 1
                    lstr = line.lstrip()
                    s1 = r1.match(lstr)
                    s2 = r2.match(lstr)
                    e = line.find('}')
                    new = line.rstrip()
                    if s1 is not None:
                        name = s1[1].strip()
                        if left_bracket_num == 1:
                            cache.append([start_id, name, server_node, service_name])
                            start_id += 1
                        in_branch = True
                        inner_ids[left_bracket_num] = 1
                    elif s2 is not None:
                        name = s2[1].strip()
                        in_branch = True
                        inner_ids[left_bracket_num] = 0
                    elif e != -1:
                        inner_ids[left_bracket_num] = 0
                        left_bracket_num -= 1
                        in_branch = False
                    else:
                        if in_branch or left_bracket_num == 1 :
                            start = None
                            for m in r3.finditer(line):
                                start = m.start()
                            if start is not None:
                                new = new[:start] + ' = ' + str(inner_ids[left_bracket_num]) + new[start:] + ';'
                            else:
                                new = new + ' = ' + str(inner_ids[left_bracket_num]) + ';'
                            inner_ids[left_bracket_num] += 1
                    lst.append(new + '\n')
                f2.writelines(lst)

    with codecs.open('route.erl', 'w', 'utf8') as f2:
        # header
        f2.write('-module(route).\n')
        f2.write('-compile(export_all).\n\n')
        f2.write(f'%% get_name begin---------\n')
        for v in cache:
            # erlang route generate
            start_id, name, server_node, service_name = v[0], v[1], v[2], v[3]
            f2.write(f'get_name({start_id}) ->\n\t{name};\n')
        f2.write(f'get_name(_) ->\n\tundefined.\n\n\n')

        f2.write(f'%% get_id begin---------\n')
        for v in cache:
            # erlang route generate
            start_id, name, server_node, service_name = v[0], v[1], v[2], v[3]
            f2.write(f'get_id({name}) ->\n\t{start_id};\n')
        f2.write(f'get_id(_) ->\n\tundefined.\n\n\n')

        f2.write(f'%% route begin---------\n')
        for v in cache:
            # erlang route generate
            start_id, name, server_node, service_name = v[0], v[1], v[2], v[3]
            f2.write(f'route({name}) ->\n\t{{{server_node},{service_name}}};\n')
        f2.write(f'route(_) ->\n\tundefined.\n')

if __name__ == '__main__':
    gen_proto()
