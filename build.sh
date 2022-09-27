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
rt/bash/zopcheck
bash/runasmdemos
bash/runcbldemos
rt/bash/runasmtests
rt/bash/runcbltests
rt/bash/runrtqsam "TIME(30)"
rt/bash/runrtbsam
rt/bash/runsort
rt/bash/runzpar
bash/runassist
bash/runcmdproc
bash/runvsedemos
bash/runzstrmactest
rt/bash/runtbrctx
rt/bash/runtestopt
rt/bash/runbr14owe

bash/blddist
