#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

# build the package
bash/blddist "$@"

# run the tests
z390test/gradlew -p z390test cleanTest
z390test/gradlew -p z390test test
