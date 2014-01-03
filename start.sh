#!/bin/bash
rm  -f ebin/*
erl +S 2:2 -sname head-ND -setcookie kobokosrv -pa /usr/local/lib/yaws/ebin/ -s make all load -s nd start
#until erl -sname head-OP -setcookie kobokosrv -pa /usr/local/lib/yaws/ebin/ -s make all load -s op start; do
#	echo "optimise.me crashed with exit code $?.  Respawning... " >&2
#	rm  -f ebin/*
#	sleep 1
#done
