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
