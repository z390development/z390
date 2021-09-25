This bsam directory contains regression tests for bsam read, write, check
sequential files with and without LBI (large block interface over 32k)
each record is snaped for visual verification and debugging

BLDW   - write 4 blocks of 10000 with characters 1-4
CHKW   - read all blocks and verify 1-4 content
BLDWL  - write 4 blocks of 100000 with characters 1-4 with LBI
CHKWL  - read all blocks and verify 1-4 content with LBI
CPYRW  - copy all blocks of 10000 to new file using read and write
CPYRWL - copy all blocks of 100000 to new file using read and write with LBI