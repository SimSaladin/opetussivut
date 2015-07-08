#!/bin/bash

#-------------------------------------------------------------------
# Make the installation files used to run the code on the server.
#
# This script copies the neceserry files to the webapp directory,
# then it's just to copy the content to the server folder.
#-------------------------------------------------------------------

if [ -d ./dist ]; then
	mkdir -p -v webapp
	cp -v ./dist/build/opetussivut/opetussivut ./webapp/update
	cp -v ./config.yaml ./webapp/
	cp -v ./wrapper.sh ./webapp/
	
	# Temporary code generating the extra folders
	mkdir -p webapp/sivut
	mkdir -p webapp/test/opetus/kurssit
	mkdir -p webapp/test/svenska/opetus/kurssit
	mkdir -p webapp/test/english/studying/courses
else
	echo "You need to build the project before using this script"
fi

