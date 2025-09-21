#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

# build the package
bash/blddist "$@"

# delete results output file
rm -f ./z390test/build/z390test-output.txt
# run the tests
z390test/gradlew -p z390test cleanTest
z390test/gradlew -p z390test test
