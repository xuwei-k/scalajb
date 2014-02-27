#!/bin/sh
SDK=appengine-java-sdk-1.8.9
SDK_ZIP=$SDK.zip
wget http://googleappengine.googlecode.com/files/$SDK_ZIP &&
unzip $SDK_ZIP &&
unset APPENGINE_SDK_HOME &&
export APPENGINE_SDK_HOME=./$SDK &&
sbt test
