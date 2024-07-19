#!/usr/bin/python
# -*- coding: utf-8 -*-

import socket
import threading
import time
import struct
import all_pb2

'''
Client
1. create socket
2. connect
3. recv & send
'''

id_name_map = {
    100:'cs_login',
    101:'sc_login',
    102:'cs_heart',
    103:'sc_heart',
    200:'cs_base_info',
    201:'sc_base_info'
}

name_id_map = {
    'cs_login':100,
    'sc_login':101,
    'cs_heart':102,
    'sc_heart':103,
    'cs_base_info':200,
    'sc_base_info':201
}

def startClient(remoteServerAddr, port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    tuple = (remoteServerAddr, port)
    s.connect(tuple)
    #print(s.recv(1024))

    sendThread = threading.Thread(target = sendThreadFunc, args = (s,))
    recvThread = threading.Thread(target = recvThreadFunc, args = (s,remoteServerAddr,port))
    sendThread.start()
    recvThread.start()

def sendThreadFunc(s):
    while True:
        sendStr = input("input:")
        if sendStr != "":
            try:
                login = all_pb2.cs_login()
                login.username = "1234"
                login.password = "1244"
                login.uuid = 999
                msg = login.SerializeToString()

                # erlang {packet, 4}时 数据总字节数=id的4字节+msg的字节长度（不包含4字节头）
                id = name_id_map['cs_login']
                msg_len = len(msg)
                data_length = msg_len + 4#struct.calcsize('I')
                pack = struct.pack(">II{0}s".format(msg_len), data_length, id, msg)
                s.send(pack)

                print("--------send pack detail--------")
                print("header[4] %s" % struct.pack(">I", data_length))
                print("id[4] %s" % struct.pack(">I", id))
                print("msg[%d] %s" % (msg_len, struct.pack(">{0}s".format(msg_len), msg)))
                print("send [{0}] ----> {1} ".format(len(pack), pack))

#                 sendStr = sendStr.encode()
#                 data_length = len(sendStr)
#                 a=s.send(struct.pack(">I{0}s".format(len(sendStr)), data_length, sendStr))
#                 print("send length ", a)
                time.sleep(2)
                base = all_pb2.cs_base_info()
                msg = base.SerializeToString()
                id = name_id_map['cs_base_info']
                msg_len = len(msg)
                data_length = msg_len + 4#struct.calcsize('I')
                pack = struct.pack(">II{0}s".format(msg_len), data_length, id, msg)
                s.send(pack)
            except Exception as err:
                print(err)
            if sendStr == "exit":
                s.close()

def recvThreadFunc(s, remoteServerAddr, port):
    # ret = struct.unpack(">4x{0}s".format(len(data) - 4), data)
    # ret2 = struct.unpack_from(">{0}s".format(len(data) - 4), data, 4)
    close = False
    while True:
        if close:
            break
        header = s.recv(4)
        if not header:
            print("maybe close")
            break
        data_length = struct.unpack('>I', header)[0]
        print("\n\n--------recv pack detail--------")
        print("recv header[4] <---- {0}".format(header))
        recv_size = 0
        data = b''
        while recv_size < data_length:
            r = s.recv(data_length)
            if not r:
                print("maybe close")
                close = True
                break
            data += r
            recv_size += len(r)
        print("recv data[{0}] <---- {1}".format(data_length, data))
        (id, msg) = struct.unpack('>I{0}s'.format(data_length - 4), data)
        print("--------recv unpack--------")
        name = id_name_map[id]
        # d = all_pb2[name]()
        print("id:{0}, msg:{1} name:{2}".format(id, msg, name))
        fun = getattr(all_pb2, name)
        d = fun()
        d.ParseFromString(msg)
        print(d)

if __name__=="__main__":
   startClient("10.10.10.50", 55555)
# protoc --python_out=./ all.proto
# pip install google
# conda install protobuf

#     login = all_pb2.cs_login()
#     login.username = "123"
#     login.password = "123"
#     ret = login.SerializeToString()
#     print(ret)
#
#     login = all_pb2.cs_login()
#     login.ParseFromString(ret)
#     print(login.username, login.password)

   # login = all_pb2.sc_login()
   # login.errcode = 2
   # ret = login.SerializeToString()
   # print(ret)
   # login = all_pb2.sc_login()
   # login.ParseFromString(ret)
   # print(login.errcode)
