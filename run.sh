#!/bin/sh
# Inference engine for object recognition KBS
# 3D model *.obj file should be provided as argument

base=`pwd`
PL=swipl

$base/obj2prolog.py $base/$1 > $base/database.pl

exec $PL -nodebug -f none -g "load_files(['$base/load'],[silent(true)])" \
	 -t "object(chair,G,P),writeln(G),writeln(P)" -- $*
