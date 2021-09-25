This qsam directory contains regression tests for qsam get and put
sequential files with and without LBI (large block interface over 32k).
Each record is snapped for visual verification and debugging.
For variable records LLLL is either LL00 without LBI or LLLL with LBI
and for LBI the LLLL high bit is on.

BLDF   - put blocks of 10000 without LBI
BLDFL  - put blocks of 100000 with LBI
BLDFT  - put blocks of 10000 without LBI
BLDFTL - put blocks of 100000 with LBI
BLDV   - put blocks of 10000 without LBI
BLDVL  - put blocks of 100000 with LBI
BLDVT  - put blocks of 10000 without LBI
BLDVTL - put blocks of 100000 with LBI
BLDVB  - put blocks of 10000 without LBI
BLDVBL - put blocks of 100000 with LBI
CHKF   - get blocks of 10000 without LBI
CHKFL  - get blocks of 100000 with LBI
CHKFT  - get blocks of 10000 without LBI
CHKFTL - get blocks of 100000 with LBI
CHKV   - get blocks of 10000 without LBI
CHKVL  - get blocks of 100000 with LBI
CHKVT  - get blocks of 10000 without LBI
CHKVTL - get blocks of 100000 with LBI
CHKVB  - get blocks of 10000 without LBI
CHKVBL - get blocks of 100000 with LBI
CPYVT1 - copy all blocks of 10000 to new file using get/put without LBI
CPYVT1L- copy all blocks of 100000 to new file using get/put with LBI