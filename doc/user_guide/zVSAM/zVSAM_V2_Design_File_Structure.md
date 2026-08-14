### Block Header Structure







*Example 4:* This example shows an index of only one block, holding two record pointers.

![Diagram showing a single Index Block](img/zVSAM_V2_Drawing_Chain_Index_Blocks_1.jpg)

*Example 5:* This example shows the index after adding three more record pointers,
causing the only index block to overflow and split. Now there are two leaf blocks, still on the LVL0 chain,
and a new root block has been created on the LVL1 chain.

![Diagram showing two Chained Index Blocks](img/zVSAM_V2_Drawing_Chain_Index_Blocks_2.jpg)

### Prefix Area Structure

The prefix area occurs only on the prefix block. It is located directly after the block header on the prefix block.
No record pointer list lies in between.

The prefix area contains basic structural information about the file. It has the following format:

| Label    | Offset | Field type | Function                                                      |
|----------|--------|------------|---------------------------------------------------------------|
| ZVSAMPFX |        | DSECT      | Prefix area                                                   |
| PFXEYE   | X'000' | CL4        | Eye catcher                                                   |
| PFXZPFX  |        | =C'zPFX'   |                                                               |
| PFXRCLEN | X'004' | XL4        | record length, max length if variable                         |
| PFXKYLEN | X'008' | XL4        | key length                                                    |
| PFXKYOFF | X'00C' | XL4        | key offset, excluding SDW/RDW                                 |
| PFXDVOL@ | X'010' | XL3        | offset to the data component's volume label                   |
| PFXDNAM@ | X'013' | XL3        | offset to the data component's filename                       |
| PFXDPAT@ | X'016' | XL3        | offset to the data component's pathname                       |
| PFXXVOL@ | X'019' | XL3        | offset to the index component's volume label                  |
| PFXXNAM@ | X'01C' | XL3        | offset to the index component's filename                      |
| PFXXPAT@ | X'01F' | XL3        | offset to the index component's pathname                      |
| PFXIXLVL | X'022' | XL1        | nr of index levels                                            |
| PFXALTHR | X'023' | XL1        | allocation redrive threshold                                  |
| PFXBLKSZ | X'024' | XL4        | blocksize used for this file (except prefix block)            |
| PFXHXLRA | X'028' | XL8        | XLRA of highest allocated block                               |
| PFXBMAP  | X'030' | XL8        | XLRA of first spacemap block                                  |
| PFXEMAP  | X'038' | XL8        | XLRA of last spacemap block                                   |
| PFXMAPNW | X'040' | XL8        | XLRA of spacemap block last used for allocation               |
| PFXBDATA | X'048' | XL8        | XLRA of first data block                                      |
| PFXEDATA | X'050' | XL8        | XLRA of last data block                                       |
| PFXBSEGM | X'058' | XL8        | XLRA of first segment block                                   |
| PFXESEGM | X'060' | XL8        | XLRA of last segment block                                    |
| PFXROOT  | X'068' | XL8        | XLRA of root index block                                      |
| PFXBLVL0 | X'070' | XL8        | XLRA of Header Block index level 0                            |
| PFXELVL0 | X'078' | XL8        | XLRA of End Block index level 0                               |
| PFXBLVL1 | X'080' | XL8        | XLRA of Header Block index level 1                            |
| PFXELVL1 | X'088' | XL8        | XLRA of End Block index level 1                               |
| PFXBLVL2 | X'090' | XL8        | XLRA of Header Block index level 2                            |
| PFXELVL2 | X'098' | XL8        | XLRA of End Block index level 2                               |
| PFXBLVL3 | X'0A0' | XL8        | XLRA of Header Block index level 3                            |
| PFXELVL3 | X'0A8' | XL8        | XLRA of End Block index level 3                               |
| PFXBLVL4 | X'0B0' | XL8        | XLRA of Header Block index level 4                            |
| PFXELVL4 | X'0B8' | XL8        | XLRA of End Block index level 4                               |
| PFXBLVL5 | X'0C0' | XL8        | XLRA of Header Block index level 5                            |
| PFXELVL5 | X'0C8' | XL8        | XLRA of End Block index level 5                               |
| PFXBLVL6 | X'0D0' | XL8        | XLRA of Header Block index level 6                            |
| PFXELVL6 | X'0D8' | XL8        | XLRA of End Block index level 6                               |
| PFXBLVL7 | X'0E0' | XL8        | XLRA of Header Block index level 7                            |
| PFXELVL7 | X'0E8' | XL8        | XLRA of End Block index level 7                               |
| PFXBLVL8 | X'0F0' | XL8        | XLRA of Header Block index level 8                            |
| PFXELVL8 | X'0F8' | XL8        | XLRA of End Block index level 8                               |
| PFXBLVL9 | X'100' | XL8        | XLRA of Header Block index level 9                            |
| PFXELVL9 | X'108' | XL8        | XLRA of End Block index level 9                               |
| PFXBLVLA | X'110' | XL8        | XLRA of Header Block index level 10                           |
| PFXELVLA | X'118' | XL8        | XLRA of End Block index level 10                              |
| PFXBLVLB | X'120' | XL8        | XLRA of Header Block index level 11                           |
| PFXELVLB | X'128' | XL8        | XLRA of End Block index level 11                              |
| PFXBLVLC | X'130' | XL8        | XLRA of Header Block index level 412                          |
| PFXELVLC | X'138' | XL8        | XLRA of End Block index level 12                              |
| PFXBLVLD | X'140' | XL8        | XLRA of Header Block index level 13                           |
| PFXELVLD | X'148' | XL8        | XLRA of End Block index level 13                              |
| PFXBLVLE | X'150' | XL8        | XLRA of Header Block index level 14                           |
| PFXELVLE | X'158' | XL8        | XLRA of End Block index level 14                              |
| PFXBLVLF | X'160' | XL8        | XLRA of Header Block index level 15                           |
| PFXELVLF | X'168' | XL8        | XLRA of End Block index level 15                              |
| PFXMAPOF | X'170' | XL3        | offset within spacemap block to last used byte for allocation |
| PFXFRSPC | X'173' | XL1        | initial freespace % within block                              |
| PFXFRBLK | X'174' | XL2        | initial freespace blocks                                      |
| PFXFRINT | X'176' | XL2        | initial freespace interval between free blocks                |
| PFXFFLGS | X'178' | XL1        | file flags                                                    |
| PFX_ESDS |        | =X'80'     | ESDS                                                          |
| PFX_KSDS |        | =X'40'     | KSDS                                                          |
| PFX_RRDS |        | =X'20'     | RRDS                                                          |
| PFX_LDS  |        | =X'10'     | LDS                                                           |
| PFX_AIX  |        | =X'08'     | AIX                                                           |
| PFX_INDX |        | =X'01'     | index component                                               |
| PFXRFLGS | X'179' | XL1        | record flags                                                  |
| PFX_RFIX |        | =X'80'     | 1=fixed, 0=variable                                           |
| PFX_RSPN |        | =X'40'     | 1=spanned, 0=non-spanned                                      |
| PFX_KUNQ |        | =X'20'     | 1=AIX unique, 0=AIX non-unique                                |
| PFX_AIXT |        | =X'10'     | 1=AIX on KSDS, 0=AIX on ESDS                                  |
|          | X'17A' | XL6        | reserved                                                      |
| PFXDTSKC | X'180' | XL8        | STCK of data component creation                               |
| PFXIXSKC | X'188' | XL8        | STCK of index component creation                              |
| PFXDTSKU | X'190' | XL8        | STCK of last update to data component                         |
| PFXIXSKU | X'198' | XL8        | STCK of last update to index component                        |
| PFXMAPDT | X'1A0' | XL8        | STCK of last allocation action                                |
| PFXCTRS@ | X'1A8' | XL3        | pointer to counters area                                      |
|          | X'1AB' | XL5        | reserved                                                      |

There are 7 pointer fields in the prefix area. These point to fields allocated elsewhere in the prefix block.
Their exact addresses on the prefix block may vary.

The `PFXDVOL@`, `PFXDPAT@`, `PFXDNAM@` pointers and the `PFXXVOL@`, `PFXXPAT@`, `PFXXNAM@` all point to a halfword-prefixed string.
The `PFXCTRS@` pointer addresses a separate area that holds various counters.
This area is expected to move into the catalog dataset in a future release.

### Counters Area Structure

The counters area occurs only on the prefix block. Its location can be found by following the prefix area's `PFXCTRS@` field.

The counters area has the following format:

| Label      | Offset | Field type | Function                                  |
|------------|--------|------------|-------------------------------------------|
| ZVSAMCTR   |        | DSECT      | Counters area                             |
| CTREYE     | X'000' | CL4        | Eyecatcher                                |
| CTRZCTR    |        | ='zCTR'    |                                           |
| CTRAVGRL   | X'004' | XL4        | average record length                     |
| CTRAVSPAC  | X'008' | XL8        | available space                           |
| CTRHALCRBA | X'010' | XL8        | high-allocated RBA                        |
| CTRENDRBA  | X'018' | XL8        | high water mark for the component         |
| CTRNCIS    | X'020' | XL8        | nr of block-split operations              |
| CTRNDELR   | X'028' | XL8        | nr of delete operations                   |
| CTRNEXCP   | X'030' | XL8        | nr of I/O operations                      |
| CTRNEXT    | X'038' | XL8        | nr of physical files allocated (always 1) |
| CTRNINSR   | X'040' | XL8        | nr of insert operations                   |
| CTRNLOGR   | X'048' | XL8        | nr of records in this component           |
| CTRNRETR   | X'050' | XL8        | nr of retrieval operations                |
| CTRNNUIW   | X'058' | XL8        | nr of zVSAM writes                        |
| CTRNUPDR   | X'060' | XL8        | nr of updates                             |
| CTRSDTA    | X'068' | XL8        | uncompressed data size                    |
| CTRSTMST   | X'070' | XL8        | system timestamp of last close operation  |
| CTRNUIW    | X'078' | XL8        | nr of user writes                         |
| CTRLOKEY@  | X'080' | XL3        | pointer to lowest valid key value         |
|            | X'083' | XL5        | Reserved                                  |

The values in the counters area are maintained to support SHOWCB ACB and/or TESTCB ACB requests.
They are mapped as follows:

| Label      | Keyword  | SHOWCB | TESTCB |
|------------|----------|--------|--------|
| CTRAVSPAC  | AVSPAC   | ACB    | ACB    |
| CTRHALCRBA | HALCRBA  | ACB    | n.a.   |
| CTRENDRBA  | ENDRBA   | ACB    | ACB    |
| CTRNCIS    | NCIS     | ACB    | ACB    |
| CTRNDELR   | NDELR    | ACB    | ACB    |
| CTRNEXCP   | NEXCP    | ACB    | ACB    |
| CTRNEXT    | NEXT     | ACB    | ACB    |
| CTRNINSR   | NINSR    | ACB    | ACB    |
| CTRNLOGR   | NLOGR    | ACB    | ACB    |
| CTRNRETR   | NRETR    | ACB    | ACB    |
| CTRNNUIW   | NUIW     | ACB    | n.a.   |
| CTRNUPDR   | NUPDR    | ACB    | ACB    |
| CTRSDTA    | SDTASIZE | ACB    | n.a.   |
| CTRSTMST   | STMST    | ACB    | ACB    |
| CTRNUIW    | UIW      | ACB    | n.a.   |
| CTRLOKEY@  | LOKEY    | ACB    | n.a.   |

### Spacemap Area Structure

The spacemap area fills every spacemap block. It consumes the entire area between the block header and the block footer.
No free space is left.

The spacemap area is formatted as follows:

| Label    | Offset | Field type | Function                                            |
|----------|--------|------------|-----------------------------------------------------|
| ZVSAMMAP |        | DSECT      | Spacemap area                                       |
| MAPXLRA  | X'000' | XL8        | XLRA of first block addressed by this spacemap area |
| MAPBITS  | X'008' | 0B         | Bitmap indicating availability                      |
 
The `MAPBITS` label addresses an array of bytes, each of which addresses 4 blocks of the file,
the status of each block being represented by two bits. Each byte relates to 4 blocks in direct succession to one another,
the bytes in the array mapping to successive sequences of 4 blocks.

The bits in the `MAPBITS` array are encoded as follows:

| Value | Meaning                                                                                                                                                                                                                                                                     |
|-------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| B'00' | block is not allocated. I.e. the block resides on no chain. The block's `BHDRNEXT`/`BHDRPREV` fields are meaningless.                                                                                                                                                       |
| B'01' | block is allocated but may have insufficient free space. I.e. last allocation attempt failed, but a smaller record might fit. Or last allocation succeeded but left fewer than `CTRAVGRL` bytes of free space. Not used for blocks holding a segment of a segmented record. |
| B'10' | block is allocated and eligible for record allocation. i.e. last allocation succeeded and left enough free space for a record of average size. (`CTRAVGRL`) Not used for blocks holding a segment of a segmented record.                                                    |
| B'11' | Nothing can be allocated to this block. i.e. last allocation attempt failed, block holds a segment of a segmented record, block is an ELIX block, or block is a spacemap block.                                                                                             |

### Fixed Record Structure

### Variable Record Structure

### Fixed-Segmented Record Structure

### Variable-Segmented Record Structure
