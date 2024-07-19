#!/bin/bash --login 
shopt -s  expand_aliases 
source ~/.bashrc

SH_PATH=`dirname $(readlink -f $0)`


SERVER_HOST=`cat  $SH_PATH/./setting/setting.config  | grep server_host | cut -d ',' -f2 | cut -d '"' -f2 | sed s/[[:space:]]//g`
SERVER_ID=`cat  $SH_PATH/./setting/setting.config  | grep -m 1 server_id | cut -d ',' -f2 | cut -d '}' -f1 | sed s/[[:space:]]//g`
NODE_TYPE=`cat  $SH_PATH/./setting/setting.config  | grep server_type | cut -d ',' -f2 | cut -d '}' -f1 | sed s/[[:space:]]//g`
NODE_COOKIE=test

SERVER_NODE=${NODE_COOKIE}_game_${NODE_TYPE}_${SERVER_ID}@${SERVER_HOST}
SERVER_COOKIE=${NODE_COOKIE}_game