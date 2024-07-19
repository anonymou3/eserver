#!/bin/bash

shopt -s  expand_aliases
source ~/.bashrc

SH_PATH=`dirname $(readlink -f $0)`

source ${SH_PATH}/base.sh
result=$(ps -fe|grep ${SERVER_NODE} |grep -v grep)
if [ "$result" != "" ]
then
        echo -e  ${SERVER_NODE}
        echo "process is running"
else
#https://www.erlang.org/doc/man/erl.html
#+K true    OTP < 21 https://www.erlang.org/blog/io-polling/
#kernel-space poll
#
#-detached
#Starts the Erlang runtime system detached from the system console. Useful for running daemons and backgrounds processes. Implies -noinput.
#
#+P Number
#Sets the maximum number of simultaneously existing processes for this system if a Number is passed as value. The default value is 262144. The actual maximum chosen may be much larger than the actual Number passed.
#
#+Q Number
#Sets the maximum number of simultaneously existing ports for this system if a Number is passed as value. The default value used is normally 65536, On Windows the default value is set to 8196. The actual maximum chosen may be much larger than the actual Number passed.
#
#+t size
#Sets the maximum number of atoms the virtual machine can handle. Defaults to 1,048,576.
        erl  -detached \
         -name ${SERVER_NODE} \
         +t 100000000 \
         +Q 1048576 +P 1048576 \
         -setcookie ${SERVER_COOKIE} \
         -pa app 3rd/ebin ebin setting -config app/app \
         -mnesia dir mnesia_data \
         -s main start
fi