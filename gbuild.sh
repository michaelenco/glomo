#!/bin/bash
FileNamePart=$1
FileName=$1
if [ -n "$FileName" ]
then
	erlc -I /lib/ejabberd/include -I ./ejabberd_src/deps -o ./ejabberd_src/ebin -v src/$FileName.erl
	/usr/bin/install -c -m 644 ./ejabberd_src/ebin/$FileName.beam //lib/ejabberd-16.01.46/ebin/$FileName.beam
fi