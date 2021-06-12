#!/bin/sh -l
set -e
echo "::set-output name=javaversion::$(java -version)"

bash/bldjar
bash/bldcbllib
bash/zopcheck
bash/runasmdemos
bash/runcbldemos
bash/runasmtests
bash/runcbltests
