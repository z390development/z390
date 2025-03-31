#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

if [ "$1" == "clean" ];         # if "clean" parm passed
then
    git clean -dfX              # delete all git ignored files
fi

# build the jar and required libs
bash/bldjar $1
bash/bldzstrmac
bash/bldlib
bash/bldcbllib

# run the tests
z390test/gradlew -p z390test test

# build the package
bash/blddist
