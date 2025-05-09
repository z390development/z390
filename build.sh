#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

if [ "$1" == "clean" ];         # if "clean" parm passed
then
    git clean -dfX              # delete all git ignored files
fi

#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

if [ "$1" == "clean" ];         # if "clean" parm passed
then
    git clean -dfX              # delete all git ignored files
fi

# build the package
bash/blddist $1

# run the tests
z390test/gradlew -p z390test cleanTest
z390test/gradlew -p z390test test
