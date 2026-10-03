The Regression Test (RT) directory has been added to z390_v1707.
A new bat subdirectory has been added with a readme.txt plus test commands moved
from the primary bat directory for common system commands and demos.
The current regression test commands are:
RUNASMTESTS - verifies assembler instruction execution
RUNBR14OWE  - verifies test IEFBR14 asm[l[g]] with Okay, Warning, Error variants
RUNCBLTESTS - verifies zcobol instructions
RUNCMDPROC  - verifies internal command processor (shell calls)
RUNCODEPAGETESTS - verifies usage of CODEPAGE issue #451
RUNMFACC    - verify MainFrame Assembler Coding Contest solutions
RUNMVSTEST  - verify MVS 3.8 maclib usage
RUNOPTABLE  - verify a single optable or machine option
RUNOPTABLES - verify all optable and machine options
RUNRTBSAM   - verifies BSAM support for READ/WRITE with/without large blocks over 32K
RUNRTQSAM   - verifies QSAM support for large blocks over 32k
RUNRTTEST   - verify various test cases in rt/test subfolder
RUNSCRIPTS  - Run scripted validation tests
RUNSORT     - verifies sort merge
RUNTBRCTX   - verify issue #238 which has been closed.
RUNTESTOPT  - verify indirection usage in options files
RUNVSAM1    - verify zVSAM V1 testcases
RUNVSAM2    - verify zVSAM V2 macro code
RUNZPAR     - verify Zcobol Program Analysis Report
TESTCMD1    - for use by RUNCMDPROC, testcase for TESTCMD1
TESTCMDA    - for use by RUNCMDPROC, testcase for TESTCMD2
TESTCMDB    - for use by RUNCMDPROC, testcase for TESTCMD2
ZOPCHECK    - verify all z390 assembler instructions generate correct code

End regression test commands
