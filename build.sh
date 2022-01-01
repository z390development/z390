#!/bin/bash
set -e
echo "::set-output name=javaversion::$(java -version)"

if [ "$1" == "clean" ];         # if "clean" parm passed
then
    git clean -dfX              # delete all git ignored files
fi

bash/bldjar
bash/bldlib
bash/bldcbllib
bash/zopcheck
bash/runasmdemos
bash/runcbldemos
bash/runasmtests
bash/runcbltests
bash/runrtqsam "TIME(30)"
bash/runsort
bash/runzpar
bash/runassist
bash/runcmdproc
bash/runvsedemos

bash/blddist
