#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

# build the package
bash/blddist "$@"
bash/ivp

# delete results output file
rm -f ./z390test/build/z390test-output.txt
# run the tests
test_mode=standard
for arg in "$@"; do
  if [ "$(printf '%s' "$arg" | tr '[:upper:]' '[:lower:]')" = "*all" ]; then
    test_mode=full
  fi
done
z390test/gradlew -p z390test cleanTest
z390test/gradlew -p z390test test -PtestMode="$test_mode"
