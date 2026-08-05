The Regression Test (RT) directory was added to z390_v1707.
A new bash subdirectory has been added with a readme.txt plus test commands moved
from the primary bash directory for common system commands and demos.
The current regression test commands are

errora2btests    - verify A2B  error tests
errorisbintests  - verify ISBIN error tests
errorisdectests  - verify ISDEC error tests
errorishextests  - verify ISHEX error tests
errorissymtests  - verify ISSYM error tests
errorslatests    - verify SLA error tests
runasmtests      - verifies assembler instruction execution
runbr14owe       - verifies test IEFBR14 asm[l[g]] with Okay, Warning, Error variants
runcbltests      - verifies ZCOBOL instructions
runcmdproc       - verifies internal command processor (shell calls)
runcodepagetests - verifies usage of CODEPAGE issue #451
runhlasmbiftests - run HLASM built-in function tests issue 509
runmfacc         - run and verify all MFACC solution programs
runmvstests      - run and verify all mvs demo/test programs
runoptable       - run and verify a single optable/machine option
runoptables      - run and verify all optable/machiine options
runrtbsam        - verifies BSAM support for READ/WRITE with/without large blocks over 32K
runrtqsam        - verifies QSAM support for large blocks over 32K
runrttest        - run and verify testcases from rt/test folder
runscripts       - run scripted validations
runsort          - verifies sort merge
runtbrctx        - verify closed issue #238
runtestopt       - verify indirection usage in options files
runvsam1         - run zVSAM V1 tests
runvsam2         - run zVSAM V2 tests
runzpar          - verify ZCOBOL Program Analysis Report
testcmd1         - for use by runcmdproc, testcase for testcmd1
x00c2bdxtests    - verify C2B, C2D, C2X tests that have X'00' in argument
zopcheck         - verify all z390 assembler instructions generate correct code

End regression test commands
