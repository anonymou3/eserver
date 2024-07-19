#!/bin/bash --login
shopt -s  expand_aliases
source ~/.bashrc

SH_PATH=`dirname $(readlink -f $0)`
source ${SH_PATH}/base.sh

erl -name rem_${SERVER_NODE} -hidden -setcookie ${SERVER_COOKIE} -pa app 3rd/ebin ebin setting -config app/app -remsh ${SERVER_NODE}