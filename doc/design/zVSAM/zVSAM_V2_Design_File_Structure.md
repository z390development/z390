# zVSAM V2 - Physical structure of the files

This document describes the file structure for implementing zVSAM V2 data sets.

## Basic Concepts

### Files, Blocks, Records

The logical unit of access or storage is the record. Yet the unit for any given I/O operation is the block.
Block sizes may vary from 512 bytes to 16MB. Each block holds up to 255 records. For any given cluster
component, choosing an appropriate block size is important. Block size can greatly affect not only
performance, but also both internal and external storage consumption.

A cluster consists of one or more files that belong together and should be managed together. Whether you
take a backup, perform a restore, or perform other administrative tasks, the files that make up a cluster should
be managed alike. When creating a backup copy of a cluster or restoring a cluster, make sure no other
processes try to access the data at the same time.

zVSAM implements a number of checks and balances to prevent inadvertent access to data that may have
been compromised. Names and locations of files are managed. Tampering with files or file attributes may
render the cluster unusable.

As a result, it is not possible to rename a zVSAM cluster or file. Unload and reload your cluster in order to
move the data or to assign a different name to cluster or file.

Just like files in a cluster belong together and should be managed together, clusters in a sphere are logically
connected and should be managed together. Again, failing to manage the files in a correct and
comprehensive manner may render your data inaccessible.

### Cluster types and Cluster Components

Each cluster consists of a data component and an index component as follows:

| Cluster type | Index content                |
|--------------|------------------------------|
| ESDS         | Index on XLRSN               |
| KSDS         | Index on key value           |
| RRDS         | Index on RRN                 |
| LDS          | No index                     |
| AIX          | Index on alternate key value |

> [!NOTE]
> Melvyn dropped LDS support. I don't know why. We should re-evaluate.

### Record Formats

In zVSAM we support the following record formats:

| Format | Properties                                                                                      |
|--------|-------------------------------------------------------------------------------------------------|
| F      | Fixed. All records have the same length. Records never span a Block boundary.                   |
| FS     | Fixed Spanned. All records have the same length. Records are expected to span a Block boundary. |
| V      | Variable. Records have varying lengths. Records never span a Block boundary.                    |
| VS     | Variable Spanned. Records have varying lengths. Records may or may not span a Block boundary.   |

For ESDS, KSDS, and RRDS all record types are supported.
For AIX only F and VS record formats are supported: F for unique, and VS for non-unique indexes.

Supported Record Formats per Cluster Type:

| Cluster Type     | F   | FS  | V   | VS  |
|------------------|-----|-----|-----|-----|
| ESDS             | Y   | Y   | Y\* | Y\* |
| KSDS             | Y   | Y   | Y   | Y   |
| RRDS             | Y   | Y\* | Y   | Y\* |
| LDS              | N   | N   | N   | N   |
| AIX - unique     | Y   | N   | N   | N   |
| AIX - non-unique | N   | N   | N   | Y   |

\* zVSAM extension

For a unique AIX each record holds an alternate key value plus the primary key (KSDS) or XLRA (ESDS)
of the associated record in the cluster's data component. This fixed configuration dictates a record type of F.

For a non-unique AIX each record holds an alternate key value and as many primary keys (KSDS) or
XLRAs (ESDS) of associated records in the cluster's data component as there are records holding that
specific alternate key value. The table of primary keys may vary in length from 1 to very large numbers. No
block size is guaranteed to be large enough to hold the largest possible index record, therefore a record type
of VS is mandated. When a non-unique index record needs to be split into segments, no primary key value or
XLRA is ever split; i.e. only an exact number of these reside within a single segment of the record.

Supported Index-types per Cluster Type

| Cluster Type | Primary - Unique | AIX - unique | AIX - Non-unique |
|--------------|------------------|--------------|------------------|
| ESDS         | Y\*              | Y            | Y                |
| KSDS         | Y                | Y            | Y                |
| RRDS         | Y\*              | N            | N                |
| LDS          | N                | N            | N                |

\* zVSAM extension

### Concept of Fixed-length records stored in blocks

Disregarding block structure elements, F-type records are conceptually stored one after another,
filling the block until no space is left. When  remaining free space is insufficient to accommodate another record,
that free space remains unallocated (marked in blue). The actual implementation is quite different,
but we'll leave those details alone for the moment.

![Diagram showing Blocked records of type F](img/zVSAM_V2_Drawing_Record_Type_F.jpg)

This holds for all cluster types, except LDS. In an LDS there is no block structure.
Each block and each record holds 4096 bytes of data.

Below we show an example of records in a LDS:

![Diagram showing records in an LDS](img/zVSAM_V2_Drawing_Record_Type_LDS.jpg)

### Concept of Fixed-length Segmented records stored in blocks

Disregarding block structure elements, FS-type records are conceptually stored one after another, using a block for each segment
and starting each record on a new block. Record size is expected to exceed block size, so the record is split into segments,
the first segment is created to fill an entire block, and the rest of the record goes into a second segment, which is stored on the next block.
Each segment is preceded by a Segment Prefix (SPX, marked in yellow). Depending on record size and usable block size,
more than two segments may be needed to store the record. The actual implementation is quite different, but we'll leave those details alone for the moment.

Below we show an example where each record requires three blocks and is therefore split into three segments:

![Diagram showing Blocked records of type FS](img/zVSAM_V2_Drawing_Record_Type_FS.jpg)

### Concept of Variable-length records stored in blocks

Disregarding block structure elements, V-type records are conceptually stored one after another, filling the block until no space is left.
When  remaining free space is insufficient to accommodate another record, that free space remains unallocated (marked in blue).
Every record is preceded by a Record Length Field (RLF, marked in grey). The actual implementation is quite different,
but we'll leave those details alone for the moment.

Below we show an example showing how various numbers of records might fit into the blocks of the file:

![Diagram showing Blocked records of type V](img/zVSAM_V2_Drawing_Record_Type_V.jpg)

### Concept of Variable-length Segmented records stored in blocks

Disregarding block structure elements, VS-type records are conceptually stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey). When  remaining free space is insufficient to accommodate a complete record,
the record is placed on the next block. Only if the record size exceeds usable block size, the record is split into segments
and each segment is prefixed with a Segment Prefix. The first segment is created to fill a block and the rest of the record goes into a second segment,
which is stored on the next block. Each segment is preceded by a Segment Prefix (SPX, marked in yellow).
Please note that the RLF occurs only once in each record, whereas each record segment has its own SPX.

Depending on record size and usable block size, more than two segments may be needed to store a record.
The actual implementation is quite different, but we'll leave those details alone for the moment.

Below we show an example showing how various numbers of records might fit into the blocks of
the file, or how a single record might occupy multiple blocks of the file:

![Diagram showing Blocked records of type VS](img/zVSAM_V2_Drawing_Record_Type_VS.jpg)

## File Structure

### Physical files

All zVSAM data is stored in physical files, as defined to the operating system.
Each component consists of one file. This file is formatted as a zVSAM file, the structure of which is
explained in the next set of chapters.

> {!NOTE]
> the hosting operating system may impose a limit on physical file size and not every host OS
> supports a physical file spanning a volume boundary of the storage device(s). Therefore, to support clusters
> that exceed the maximum size of a single physical file, in the future we may need to support clusters that
> consist of multiple files.

### Structure of physical files

Every zVSAM file has a block size. The block being the basic unit of I/O.
The first block of every file is the prefix block, which is always 4096 bytes in size.
The prefix block holds information about the cluster, its data, and its structure.

Data in the prefix block are not accessible to user programs.
However, selected fields in the prefix block can be queried using a SHOWCB ACB= request.

All other blocks in the file have a user-defined blocksize. That is, the user defines the blocksize:
`DATABLOCKSIZE=` for blocks in a data component and `INDEXBLOCKSIZE=` for blocks in an index component.
All blocks in the file are created with that size, except the prefix block which is always 4096 bytes,
irrespective of the size of the other blocks in the file.
The file is assumed to logically begin with the first block after the prefix block.

There are 6 types of blocks that may occur in zVSAM files:

1. Prefix block – one for each file, being the first 4096 bytes of every file
2. Spacemap block – used to manage free space in the file
3. Data block – used to hold user data, or AIX data records (in an AIX only)
4. Index block – used to hold index information
5. ELIX block – used to index segmented (read: large) non-unique AIX records
6. Raw block – used to hold a block's worth of LDS data

Every block, except a raw block, has an internal structure consisting of a block header,
a list of record pointers (data and index blocks only), a block body and a block footer.
The block header and footer have a fixed structure. The list of record pointers, if present,
has a variable length. The block body contains record data and/or free space.

Raw blocks have no internal structure, as far as zVSAM is concerned.
Any and all internal structure(s) in an LDS are to be maintained by the application program.
Each of the 6 block types is explained in more detail below.

> [!NOTE]
> Melvyn removed all mention of LDS support. We should reconsider.
> Melvyn addede support for ELIX blocks. We should reconsider that, too.

### Components of a Block

With the exception of Raw Blocks, all blocks have an internal structure that comprises:
- a Block header
- a Block Footer
- a Record Pointer List

All Blocks (except Prefix Block and Raw Blocks) are chained into a chain which is anchored in the Prefix Block.
The type of Block determines on which chain it resides:
- Spacemap Chain
- Data Chain (data blocks, with the exception of non-first segment blocks)
- Segment chain (for non-first segment blocks exclusively)
- Index chains (one for each index level)

Every Block (except Raw Blocks) has a header holding information to implement the applicable chain.
Every Block (except Raw Blocks) also has a footer, which mainly serves to guard integrity of the data stored on the Data Block.

#### Block Header Structure

Every non-Raw Block has a block header (`ZVSAMHDR`).
All block headers have the same structure.

`BHDRSEQ#` is incremented by one every time the block is written out to the file.
The footer area contains a comparable field: `BFTRSEQ#`. Together they guard against incomplete writes.

`BHDRXLVL` indicates the index level. Zero is the leaf level. Index blocks are chained by level. That is, for
every index level in use there is a pair of pointers in the prefix block (`PFXBLVLn`/`PFXELVLn`) that starts and ends
the chain for that level.

`BHDRSELF` contains the block's own XLRA. This helps to guard against misdirected reads and/or writes.

`BHDRNEXT`/`BHDRPREV` point to the next and previous block on the chain. Which chain this is, depends
on the `BHDRFLAG` setting, and, if this is an index block, by the `BHDRXLVL` value.
For the prefix block, these two fields are set to foxes.

Segmented records are a special case. Segments of a segmented record never share their block with other
data. The block holding the first segment is part of the data chain. A block holding a non-first segment is part
of the segment chain. A block that holds a record's first segment has an SPX pointing to the block holding
the next segment. Subsequent segments are retrieved by following the SPXs to the last segment of the record

The Segment chain starting at `PFXBSEGM` and ending at `PFXESEGM` has no role in processing a spanned
dataset but just provides an extra integrity check.

#### Block Footer Structure

Every block has a block footer `zVSAMFTR`). All block footers have the same structure.

`BFTRSEQ#` is incremented by one every time the block is written out to the file.
The header area contains a comparable field: `BHDRSEQ#`. Together they guard against incomplete writes.

#### Record List Structure (RPTR)

Every block that contains data records contains a record list (`ZVSAMRPT`). Records are accessible only
through their Record pointer or RPTR. Every entry in the list corresponds with a single record on the block. The last
byte of the record's XLRA is the index into the Record List. Index value of X'00' is reserved for block pointers;
values X'01' through X'FF' inclusive are usable as RPTR index values. The difference of 1 always needs to
be taken into account when indexing the RPTR list.

The RPTR list always follows the block header directly.
The number of entries on the RPTR list varies with the number of records stored on the block (`BHDR#REC`)
and is terminated with an entry of foxes to mark the end of the list.

When `RPTR_END` is set, `RPTRREC@` is set to foxes.
`RPTR_ACT` and `RPTR_MTY` are mutually exclusive. Either one must be set, otherwise the RPTR list is
compromised and data access will fail. `RPTR_MTY` indicates an empty RRDS slot.

#### Segment Prefix (SPX)

All segments begin with a segment prefix (`ZVSAMSEG`).
The first segment is on the Data chain and subsequent segments are retrieved via `SPXBNEXT`.
The flag `SPXSEGCC` indicates the first, middle or last segments.

### Prefix Block

The prefix block (`ZVSAMPFX`) consists of the first 4096 bytes of every physical file.
It contains meta-data defining the file and its attributes. It also contains various counters.

The prefix block consists of a block header immediately followed by the prefix area.
The prefix block also contains other data fields, these are addressed from the prefix area.
The prefix block ends with a block footer. A record pointer list is not present on the prefix block.

There are various pointer fields in the prefix area. These point to fields allocated elsewhere in the prefix block.
Their exact addresses on the prefix block may vary:
- `PFXDPAT@`, `PFXDNAM@`, `PFXXPAT@`, `PFXXNAM@` all point to a halfword-prefixed string.
- `PFXDVOL@` and `PFXXVOL@` contain foxes (future option)

The `PFXCTRS@` pointer addresses a separate area that holds various counters.

The Counters area (`ZVSAMCTR`) directly follows the Prefix area on the Prefix Block, it is doubleword aligned.This area is expected to move into the catalog dataset in a future release.
The overall structure of the prefix block would look something like this (areas not to scale):

![Diagram showing layout of a Prefix Block](img/zVSAM_V2_Drawing_Block_Type_Prefix.jpg)

### Spacemap Blocks

Spacemap blocks (`ZVSAMMAP`) are used to manage available free space in a component.
Each spacemap block has a size that matches the blocksize of all other blocks
(except possibly the prefix block) in the component.

A component will hold as many spacemap blocks as needed to map all of its allocated blocks,
including all spacemap blocks but excluding the prefix block. Whenever a single spacemap block is not enough,
the spacemap blocks are chained together by means of the `BHDRNEXT`/`BHDRPREV` pointers in the block header area.
The spacemap chain starts/ends from the prefix block, fields `PFXBMAP`/`PFXEMAP`.

When a single spacemap block suffices, `PFXBMAP` and `PFXEMAP` will both point to that block.

Each spacemap block consists of a block header immediately followed by the spacemap area, which in turn
is followed directly by the block footer. No free space exists on a spacemap block.
Thus, the last spacemap block may map blocks that do not exist in the dataset.
The bit settings for blocks beyond the `PFXHXLRA` should all be zero to indicate an unallocated block.
zVSAM is aware that any block beyond `PFXHXLRA` needs to be created and initialized before it can be allocated.

Conceptually, the overall structure of a spacemap block would look something like this (areas not to scale):

![Diagram showing layout of a Spacemap Block](img/zVSAM_V2_Drawing_Block_Type_Spacemap.jpg)

### Data Blocks

Each record has an Record Pointer List (RPTR block). The RPTR immediately follows the Block Header.
In addition to the offset, the RPTR contains flags to identify the type and status of each record.
`RPTR_END` marks the end of records in this block.

The records are allocated from the other end of the block (preceding the Footer area) to consolidate free space at the centre.

![Diagram showing layout of a Data Block](zVSAM_V2_Drawing_Block_Type_Data.jpg)

It is possible to reserve an amount of freespace at load time which also applies if a block is split.
It is specified in the catalog as `DATAFREESPACE=nn`, where nn is a percentage of the available space.
Only a fixed non-spanned KSDS can specify free space.

For all types of fixed non-spanned datasets, the available space may not be a multiple of the data record size
resulting in unusable space. To correct this use `DATAADJUST=YES` which will calculate an optimal
blocksize less than the specified one.

How the Data Blocks are laid out in the file depends on whether the cluster is defined with Spanned records,
or with unspanned records.

#### Data Block Structure (SPANNED=NO)

Assume we have a cluster with three data blocks holding records. The blocks are on the data chain as
outlined in the picture below. Please note that all depicted pointers are block pointers. Each thus originates with the
indicated field, and ends at the block it points to. The location where the arrows attach has no meaning since
it's a block pointer.

![Diagram showing layout of a Data Block Chain](zVSAM_V2_Drawing_Chain_Data_Blocks.jpg)

#### Data Block Structure (SPANNED=YES)

Now suppose we have a cluster with three data blocks, the first block holding two unsegmented records, the
second block holding the first segment of a record consisting of three segments and the third block holding
the first segment of a record consisting of two segments.

In the picture we show the data chain as a solid line (as in the picture above), we show the segment chain as a
dotted line, and we show the SPX s as a fat line.

The picture shows the prefix area's pointer to start/end block of both the data chain and the segment chain
It also shows the first and second block pointer on each chain pointing to one another. Same thing for the second and
third block pointer on each chain.

![Diagram showing layout of a Segmented Data Block Chain](zVSAM_V2_Drawing_Chain_Segmented_Data_Blocks.jpg)

All depicted pointers are block pointers.
Each originates with the indicated field, and ends at the block it points to.
The location where the arrows attach has no meaning since it's a block pointer.


















### Index Blocks

### Raw Blocks

