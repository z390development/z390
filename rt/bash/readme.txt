The Regression Test (RT) directory was added to z390_v1707.
A new bash subdirectory has been added with a readme.txt plus test commands moved
from the primary bash directory for common system commands and demos.
The current regression test commands are

runasmtests - verifies assembler instruction execution
runcbltests - verifies ZCOBOL instructions
runrtbsam   - verifies BSAM support for READ/WRITE with/without large blocks over 32K
runrtqsam   - verifies QSAM support for large blocks over 32K
runsort     - verifies sort merge
runzpar     - verify ZCOBOL Program Analysis Report
zopcheck    - verify all z390 assembler instructions generate correct code
runtbrctx   - verify closed issue #238
runtestopt  - verify indirection usage in options files
runbr14owe  - verifies test IEFBR14 asm[l[g]] with Okay, Warning, Error variants

End regression test commands
