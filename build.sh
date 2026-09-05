#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

# build the package
bash/blddist "$@"
bash/ivp

# delete results output file
rm -f ./z390test/build/z390test-output.txt
# run the tests
if [ "$(echo "$1" | tr '[:upper:]' '[:lower:]')" = "*all" ]; then
  z390test/gradlew -p z390test cleanTest test cleanOptionalTest optionalTest
else
  z390test/gradlew -p z390test cleanTest test
fi