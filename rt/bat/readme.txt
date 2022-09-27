The Regression Test (RT) directory has been added to z390_v1707.
A new bat subdirectory has been added with a readme.txt plus test commands moved
from the primary bat directory for common system commands and demos.
The current regression test commands are:
RUNASMTESTS - verifies assembler instruction execution
RUNCBLTESTS - verifies zcobol instructions
RUNRTBSAM   - verifies BSAM support for READ/WRITE with/without large blocks over 32K
RUNRTQSAM   - verifies QSAM support for large blocks over 32k
RUNSORT     - verifies sort merge
RUNZPAR     - verify Zcobol Program Analysis Report
ZOPCHECK    - verify all z390 assembler instructions generate correct code
RUNTBRCTX   - verify issue #238 which has been closed.
RUNTESTOPT  - verify indirection usage in options files
RUNBR14OWE  - verifies test IEFBR14 asm[l[g]] with Okay, Warning, Error variants

Double click on above commands to run them.

don@higgins.net 2022-09-27
