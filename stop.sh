#!/bin/bash --login
shopt -s  expand_aliases
source ~/.bashrc

SH_PATH=`dirname $(readlink -f $0)`
source ${SH_PATH}/base.sh

#https://www.erlang.org/doc/man/erl_call.html#
#erl_call makes it possible to start and/or communicate with a distributed Erlang node.
erl_call -c ${SERVER_COOKIE} -n ${SERVER_NODE} -a "user_default stop []"

#load or reload module    erl -> help().
#erl_call -c ${SERVER_COOKIE} -n ${SERVER_NODE} -a "shell_default l [file]"

#config reload
#erl_call -c ${SERVER_COOKIE} -n ${SERVER_NODE} -a "user_default lc [file]"
