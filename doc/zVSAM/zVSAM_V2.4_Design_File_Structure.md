### Prefix Block chain summary

The following table summarizes the way that blocks in the file are chained from the prefix block.
The prefix block doesn't reside on any chain.

| Block Type                | Beginning of chain | End of chain |
|---------------------------|--------------------|--------------|
| Prefix                    | foxes              | foxes        |
| Spacemap                  | `PFXBMAP`          | `PFXEMAP`    |
| Data (in use and free)    | `PFXBDATA`         | `PFXEDATA`   |
| Data (non-first segments) | `PFXBSEGM`         | `PFXESEGM`   |
| Index (in use and free)   | `PFXBLVLn`         | `PFXELVLn`   |

### Counters Area and its maintenance

All fields are 8 bytes except `CTRAVGRL` which is 4 bytes.

| Counter    | Data/Index | Initialized by zREPRO                                     | Maintenance                                                                           |
|------------|------------|-----------------------------------------------------------|---------------------------------------------------------------------------------------|
| CTRAVGRL   | Both       | Yes. For fixed, =`PFXRECLN` even if the dataset is empty. | For variable files only:                                                              |
|            |            | For variable, calculated or zero if the dataset is empty. | At CLOSE, calculate `CTRTOTRL`/`CTRNLOGR`                                             |
| CTRAVSPAC  | Both       | Yes.                                                      | For every block update use the old and new `BHDRFREE` to increase/decrease this value |
| CTRHALCRBA | Both       | Yes.                                                      | Updated when blocks are added to the end of the dataset component                     |
|            |            |                                                           | or when the existing `HALCRBA` block has all records deleted. It's the                |
|            |            |                                                           | block RBA+1 (XLRA+256) of the last data or level 0 index block containing records.    |
| CTRHLRBA   | Index only | Yes.                                                      | Block RBA of `PFXROOT`. Update if it changes                                          |
| CTRENDRBA  | Both       | Yes.                                                      | Updated when blocks are added to the end of the dataset component.                    |
|            |            |                                                           | It's the block RBA+1 (XLRA+256) of the last data or level 0 index block               |
| CTRNBFRFND | Both       | No                                                        | +1 for each LSR buffer read                                                           |
| CTRNBUFNO  | Both       | No                                                        | +1 for each buffer allocated                                                          |
| CTRBUFUSE  | Both       | No                                                        | +1 for each buffer used                                                               |
| CTRBUFRDS  | Both       | No                                                        | +1 for each buffer read                                                               |
| CTRNCIS    | Both       | No                                                        | +1 for each block split                                                               |
| CTRNDELR   | Both       | No                                                        | KSDS or RRDS: +1 for each record delete                                               |
| CTRNEXCP   | Both       | No                                                        | +1 for each physical block read/write                                                 |
| CTRNEXT    | Both       | Yes                                                       | Always 1, not maintained                                                              |
| CTRNINSR   | Both       | No                                                        | +1 for each record added. For RRDS, any empty slots added to the end are not counted  |
| CTRNLOGR   | Both       | Yes                                                       | +1 for each record added; -1 for each record deleted.                                 |
|            |            |                                                           | For RRDS, any empty slots added to the end are not counted                            |
|            |            |                                                           | For Index, all records in all levels are counted                                      |
| CTRNRETR   | Both       | No                                                        | +1 for each record read                                                               |
| CTRNNUIW   | Both       | No                                                        | +1 for each maintenance write for block splits, chain repair, segment, spacemap       |
|            |            |                                                           | and ELIX block management                                                             |
| CTRNUPDR   | Both       | No                                                        | +1 for each record update                                                             |
| CTRSDTASZ  | Both       | Yes                                                       | +block size for each block added                                                      |
| CTRSTMST   | Both       | Yes                                                       | Write STCK value at CLOSE                                                             |
| CTRSTRMAX  | Both       | No                                                        | +1 for each string created                                                            |
| CTRNUIW    | Both       | No                                                        | +1 for each user-requested block write                                                |
| CTRTOTRL   | Data only  | Yes                                                       | Maintained for variable files only:                                                   |
|            |            |                                                           | +record size for each record added; -record size for each record deleted              |
|            |            |                                                           | SPX is not included; RLF is included; Adjusted for change to variable length          |
|            |            |                                                           | For RRDS, empty slots are not included                                                |
| CTRLOKEY   | Data only  | Yes                                                       | KSDS only. Update when a lower key is added or this key is deleted                    |

## AIX Blocks

### AIX Block Structure (Unique)

![Diagram showing layout of a chain of AIX Data Blocks](zVSAM_V2.4_Drawing_Chain_AIX_Blocks.jpg)

### AIX Block (Unique)

![Diagram showing layout of an AIX Data Block](zVSAM_V2.4_Drawing_Block_Type_AIX.jpg)

AIX unique records have the following format:

| AIX on ... | Record Format                   |
|------------|---------------------------------|
| KSDS       | AIX key followed by primary key |
| ESDS       | AIX key followed by XRBA(8)     |

### AIX Block Structure (Non-unique)

![Diagram showing layout of a Chain of Segmented AIX Data Blocks](zVSAM_V2.4_Drawing_Chain_Segmented_AIX_Blocks.jpg)

### AIX Block (Non-unique and not segmented)

![Diagram showing layout of an unsegmented AIX Data Block](zVSAM_V2.4_Drawing_Block_Type_AIX_Unseg.jpg)

AIX non-unique non-segmented records have the following format:

| AIX on ... | Record Format                                             |
|------------|-----------------------------------------------------------|
| KSDS       | AIX key, an element count n(4) followed by n primary keys |
| ESDS       | AIX key, an element count n(4) followed by n XRBAs(n\*8)  |

### AIX Block (Non-unique and segmented)

![Diagram showing layout of a Segmented AIX Data Block](zVSAM_V2.4_Drawing_Block_Type_AIX_Seg.jpg)

Each segment contains a whole number of elements.

AIX non-unique segmented records have the following formats:

| AIX on ... | Record Format of FIRST segment                                                        |
|------------|---------------------------------------------------------------------------------------|
| KSDS       | SPX, AIX key, an element count(4) which is the total no. of elements in all segments. |
|            | The actual number of primary keys in this segment can be calculated from `SPXSEGLN`   |
| ESDS       | SPX, AIX key, an element count(4) which is the total no. of elements in all segments. |
|            | The actual number of XLRAs in this segment can be calculated from `SPXSEGLN`          |

| AIX on ... | Record Format of MIDDLE or LAST segments                                              |
|------------|---------------------------------------------------------------------------------------|
| KSDS       | SPX and a number of primary keys.                                                     |
|            | The actual number of primary keys in this segment can be calculated from `SPXSEGLN`   |
| ESDS       | SPX and a number of XLRAs.                                                            |
|            | The actual number of XLRAs in this segment can be calculated from `SPXSEGLN`          |

### ELIX Block

A single ELIX block is created for each non-unique AIX record that is segmented.
It has the same blocksize as a Data record.

zVSAM lifts the current IBM restriction of 32K elements in a non-unique AIX record, because of this there
may be many segments to read to find an element to delete or an insertion point for a new record.

The ELIX Block provides an extra index on the segments and contains the highest element in each segment.
As there is currently only one ELIX Block per AIX key this places a limit on the number of elements.

When a non-unique AIX is built zREPRO will issue a message on the log like this:
`zREPRO AIX MAX ELEMENT LIMIT 87654`
If the number of elements is too low then rebuild the AIX with a larger blocksize.

IBM does not maintain elements in any particular order but for the ELIX structure to work zVSAM will
maintain elements in sequence.

![Diagram showing layout of an ELIX Block](zVSAM_V2.4_Drawing_Block_Type_ELIX.jpg)

The ELIX record has the following format:

| AIX on ... | Record Format                                                             |
|------------|---------------------------------------------------------------------------|
| KSDS       | Highest Primary key followed by the XLRA of the segment (always record 1) |
| ESDS       | Highest XRBA followed by the XLRA of the segment (always record 1)        |

### Index Blocks

Each record has an RPTR block, they are created after the Block Header.
In addition to the offset, the RPTR contains flags to identify the type and status of each record.
`RPTR_END` marks the end of record pointers in this block.

The records are placed in reverse order in the block to consolidate free space at the centre.

For Level 0 each record is the key (KSDS), XRBA (ESDS) or RRN (RRDS) and is followed by an XLRA.
The XLRA is a record pointer to the Data block.

For other levels, each record pointer is the highest key, XRBA or RRN followed by an XLRA.
The XLRA is a block pointer to the previous level.

As each index record is a fixed size it is recommended to specify `INDEXADJUST=YES` to avoid unusable
free space

### Index Block Structure: Single level

This example shows an index of only one block, holding two record pointers

![Diagram showing layout of a Chain of 1 Index Blocks](zVSAM_V2_Drawing_Chain_Index_Blocks_1.jpg)

### Index Block Structure: Two Levels

This example shows the index after adding three more record pointers, causing the only index block to overflow and
split. Now there are two leaf blocks, still on the LVL0 chain, and a new root block has been created on the
LVL1 chain

![Diagram showing layout of a Chain of 2 Index Blocks](zVSAM_V2_Drawing_Chain_Index_Blocks_2.jpg)

### Index Block Level 0

![Diagram showing layout of a Leaf Index Block](zVSAM_V2.4_Drawing_Block_Type_Index_Leaf.jpg)

### Index Block other levels

![Diagram showing layout of a Non-Leaf Index Block](zVSAM_V2.4_Drawing_Block_Type_Index_NLeaf.jpg)

It is possible to reserve an amount of freespace at load time which also applies if a block is split.
It is specified in the catalog as `INDEXFREESPACE=nn`, where nn is a percentage of the available space.
Only a fixed non-spanned KSDS can specify free space.

For all types of fixed non-spanned datasets, the available space may not be a multiple of the index record size
resulting in unusable space. To correct this use `INDEXADJUST=YES` which will calculate an optimal
blocksize less than the specified one.

## Structure and Functions by dataset type

### KSDS Fixed non-Spanned

F-type records are conceptually stored one after another, filling the block until no space is left.
When the remaining free space is insufficient to accommodate another record, that free space remains
unusable. Unusable space can be eliminated by building the dataset with `DATAADJUST=YES`.
Blocks can be allocated with free space for adds (`DATAFREESPACE=nn%`), when the block is full the
block will be split and any new block will have nn% free space.

Format:

![Diagram showing layout of a KSDS Block with Fixed records](zVSAM_V2.4_Drawing_Block_Type_KSDS_F.jpg)

| Function  | Notes                                              |
|-----------|----------------------------------------------------|
| Add       | Yes                                                |
| Update    | Yes, the primary key must not be changed           |
| Delete    | Yes                                                |
| Length    | change n/a                                         |
| Access by | Primary key or AIX key. (X)RBA not yet implemented |

### KSDS Fixed Spanned

FS-type records are conceptually stored one after another, using a block for each segment and starting each
record on a new block. Record size is expected to exceed block size, so the record is split into segments, the
first segment is created to fill an entire block, and the rest of the record goes into one or more secondary
segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow)

zVSAM extension: The primary key and any AIX keys need not be in the first segment.

Below we show an example where each record requires three segments:

![Diagram showing layout of a KSDS Block with Fixed Spanned records](zVSAM_V2.4_Drawing_Block_Type_KSDS_FS.jpg)

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes                                                |
| Update        | Yes, the primary key must not be changed           |
| Delete        | Yes                                                |
| Length change | n/a                                                |
| Access by:    | Primary key or AIX key. (X)RBA not yet implemented |

### KSDS Variable non-Spanned

V-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey).

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

Below we show an example showing how various numbers of records might fit into the blocks:

![Diagram showing layout of a KSDS Block with Variable records](zVSAM_V2.4_Drawing_Block_Type_KSDS_V.jpg)

| Function      | Notes                                                                             |
|---------------|-----------------------------------------------------------------------------------|
| Add           | Yes                                                                               |
| Update        | Yes, the primary key must not be changed                                          |
| Delete        | Yes                                                                               |
| Length change | Yes. When a record is shortened it must not affect the primary key or any AIX key |
| Access by:    | Primary key or AIX key. (X)RBA not yet implemented                                |

### KSDS Variable Spanned

VS-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey).
When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

Only if the record size exceeds the usable block size is the record is split into segments and each segment is
prefixed with a Segment Prefix. The first segment is created to fill an entire block, and the rest of the record
goes into one or more secondary segments which are stored on the next blocks.
Each segment is preceded by a Segment Prefix (SPX, marked in yellow).

zVSAM extension: The primary key and any AIX keys need not be in the first segment.

Below we show an example showing how various numbers of records might fit into the blocks of the file,
or how a single record might occupy multiple blocks of the file.

![Diagram showing layout of a KSDS Block with Spanned Variable records](zVSAM_V2.4_Drawing_Block_Type_KSDS_VS.jpg)

| Function      | Notes                                                                             |
| Add           | Yes                                                                               |
| Update        | Yes, the primary key must not be changed                                          |
| Delete        | Yes                                                                               |
| Length change | Yes. When a record is shortened it must not affect the primary key or any AIX key |
| Access by:    | Primary key or AIX key. (X)RBA not yet implemented                                |

### ESDS Fixed non-Spanned

F-type records are conceptually stored one after another, filling the block until no space is left.
When the remaining free space is insufficient to accommodate another record, that free space remains
unusable. Unusable space can be eliminated by building the dataset with `DATAADJUST=YES`.

Format:

![Diagram showing layout of an ESDS Block with Fixed records](zVSAM_V2.4_Drawing_Block_Type_KSDS_F.jpg)

**Note**: The format of an ESDS block with Fixed records is identical to that for a KSDS.
The only difference being that free-space (`DATAFREESPACE=nn%`) does not apply to ESDS datasets.

| Function      | Notes                                   |
|---------------|-----------------------------------------|
| Add           | Yes, but only to the end of the dataset |
| Update        | Yes                                     |
| Delete        | No                                      |
| Length change | n/a                                     |
Access by:      | (X)RBA or AIX key                       |

### ESDS Fixed Spanned

FS-type records are conceptually stored one after another, using a block for each segment and starting each
record on a new block. Record size is expected to exceed block size, so the record is split into segments, the
first segment is created to fill an entire block, and the rest of the record goes into one or more secondary
segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow)

zVSAM extension: Any AIX keys need not be in the first segment.

Below we show an example where each record requires three segments:

![Diagram showing layout of an ESDS Block with Fixed Spanned records](zVSAM_V2.4_Drawing_Block_Type_KSDS_FS.jpg)

**Note**: The format of an ESDS block with Fixed Spanned records is identical to that for a KSDS.

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes, but only to the end of the dataset            |
| Update        | Yes                                                |
| Delete        | No                                                 |
| Length change | n/a                                                |
| Access by:    | (X)RBA or AIX key                                  |

### ESDS Variable non-Spanned

V-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey).

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

This dataset type is a zVSAM extension.

Below we show an example showing how various numbers of records might fit into the blocks

![Diagram showing layout of an ESDS Block with Variable records](zVSAM_V2.4_Drawing_Block_Type_KSDS_V.jpg)

**Note**: The format of an ESDS block with Variable records is identical to that for a KSDS.

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes, but only to the end of the dataset            |
| Update        | Yes                                                |
| Delete        | No                                                 |
| Length change | No                                                 |
| Access by:    | (X)RBA or AIX key                                  |

### ESDS Variable Spanned

VS-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey).
When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

Only if the record size exceeds the usable block size is the record is split into segments and each segment is
prefixed with a Segment Prefix. The first segment is created to fill an entire block, and the rest of the record
goes into one or more secondary segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow).

This dataset type is a zVSAM extension.

zVSAM extension: Any AIX keys need not be in the first segment.

Below we show an example showing how various numbers of records might fit into the blocks of the file,
or how a single record might occupy multiple blocks of the file

![Diagram showing layout of an ESDS Block with Spanned Variable records](zVSAM_V2.4_Drawing_Block_Type_KSDS_VS.jpg)

**Note**: The format of an ESDS block with Variable Spanned records is identical to that for a KSDS.

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes, but only to the end of the dataset            |
| Update        | Yes                                                |
| Delete        | No                                                 |
| Length change | No                                                 |
| Access by:    | (X)RBA or AIX key                                  |

### RRDS Fixed non-Spanned

F-type records are conceptually stored one after another, filling the block until no space is left.
When the remaining free space is insufficient to accommodate another record, that free space remains
unusable. Unusable space can be eliminated by building the dataset with `DATAADJUST=YES`.

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots are initially binary zeros with `RPTR_MTY` set.

Below we show an example where 8 record slots fit into a block:

![Diagram showing layout of an RRDS Block with Fixed records](zVSAM_V2.4_Drawing_Block_Type_RRDS_F.jpg)

| Function      | Notes                                          |
|---------------|------------------------------------------------|
| Add           | Yes, but only to the end of the dataset        |
| Update        | Yes                                            |
| Delete        | Yes, slots may not be deleted. RPTR_MTY is set |
| Length change | n/a                                            |
| Access by:    | RRN                                            |

### RRDS Fixed Spanned

FS-type records are conceptually stored one after another, using a block for each segment and starting each
record on a new block. Record size is expected to exceed block size, so the record is split into segments, the
first segment is created to fill an entire block, and the rest of the record goes into one or more secondary
segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow).

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots are initially binary zeros with `RPTR_MTY` set.

This dataset type is a zVSAM extension

Below we show an example where each record requires three segments:

![Diagram showing layout of an RRDS Block with Fixed Spanned records](zVSAM_V2.4_Drawing_Block_Type_RRDS_FS.jpg)

| Function      | Notes                                          |
|---------------|------------------------------------------------|
| Add           | Yes, but only to the end of the dataset        |
| Update        | Yes                                            |
| Delete        | Yes, slots may not be deleted. RPTR_MTY is set |
| Length change | n/a                                            |
| Access by:    | RRN                                            |

### RRDS Variable non-Spanned

V-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF).

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots consist of a dummy RLF containing `X'00000004'` with `RPTR_MTY` set, these are shown
in green in the diagram. Non-empty slots have a grey RLF.

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

Below we show an example showing how various numbers of records might fit into the blocks

![Diagram showing layout of an RRDS Block with Variable records](zVSAM_V2.4_Drawing_Block_Type_RRDS_V.jpg)

| Function      | Notes                                                            |
|---------------|------------------------------------------------------------------|
| Add           | Yes, but only to the end of the dataset                          |
| Update        | Yes                                                              |
| Delete        | Yes, slots may not be deleted. `RPTR_MTY is set` instead.        |
|               | The record is replaced by a dummy RLF and the space is reclaimed |
| Length change | Yes                                                              |
| Access by:    | RRN                                                              |

### RRDS Variable Spanned

VS-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF).

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots consist of a dummy RLF containing `X'00000004'` with `RPTR_MTY` set, these are shown
in green in the diagram. Non-empty slots have a grey RLF.

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

When a record length exceeds the available space in a block the record is split into segments, the first
segment is created to fill an entire block, and the rest of the record goes into one or more secondary segments
which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow).

This dataset type is a zVSAM extension.

Below we show an example showing how various numbers of records might fit into the blocks

![Diagram showing layout of an RRDS Block with Variable Spanned records](zVSAM_V2.4_Drawing_Block_Type_RRDS_VS.jpg)

| Function      | Notes                                                            |
|---------------|------------------------------------------------------------------|
| Add           | Yes, but only to the end of the dataset                          |
| Update        | Yes                                                              |
| Delete        | Yes, slots may not be deleted. RPTR_MTY is set                   |
|               | The record is replaced by a dummy RLF and the space is reclaimed |
|               | For segmented records, the freed blocks are marked as available  |
| Length change | Yes                                                              |
| Access by:    | RRN                                                              |

