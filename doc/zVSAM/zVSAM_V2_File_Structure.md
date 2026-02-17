# zVSAM V2 - Physical structure of the files

This document describes the file structure for implementing zVSAM V2 data sets.
It contains the following major chapters:

1. [Basic Concepts](#Basic-Concepts)
2. [File Structure](#File-Structure)
**!! !!**

## Basic Concepts

### Files, Blocks, Records

The logical unit of access or storage is the record. Yet the unit for any given I/O operation is the block.
Block sizes may vary from 512 bytes to 16MB. Each block holds up to 255 records. For any given cluster
component, choosing an appropriate block size is important. Block size can greatly affect not only
performance, but also both internal and external storage consumption.

A cluster consists of one or more files that belong together and should be managed together. Whether you
take a backup, perform a restore, or perform other administrative tasks, the files that make up a cluster should
be managed alike. When creating a backup copy of a cluster or restoring a cluster, make sure no other
processes try to access the data at the same time

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
| ESDS         | Index on XRBA                |
| KSDS         | Index on key value           |
| RRDS         | Index on RRN                 |
| AIX          | Index on alternate key value |
| LDS          | Not supported                |

### Record Formats

In zVSAM we support the following record formats:

| Format | Properties                                                                                    |
|--------|-----------------------------------------------------------------------------------------------|
| F      | Fixed. All records have the same length Records never span a Block boundary.                  |
| FS     | Fixed Spanned. All records have the same length. Records expected to span a Block boundary.   |
| V      | Variable. Records have varying lengths. Records never span a Block boundary.                  |
| VS     | Variable Spanned. Records have varying lengths. Records may or may not span a Block boundary. |

For ESDS, KSDS, and RRDS all record types are supported.
For AIX only F and VS record formats are supported: F for unique, and VS for non-unique indexes.

Supported Record Formats

| Cluster Type     | F   | FS  | V   | VS  |
|------------------|-----|-----|-----|-----|
| ESDS             | Y   | Y   | Y\* | Y\* |
| KSDS             | Y   | Y   | Y   | Y   |
| RRDS             | Y   | Y\* | Y   | Y\* |
| AIX - unique     | Y   | N   | N   | N   |
| AIX - non-unique | N   | N   | N   | Y   |

** \* denotes a zVSAM extension **

For a unique AIX each record holds an alternate key value plus the primary key (KSDS) or XLRA (ESDS)
of the associated record in the cluster's data component. This fixed configuration dictates a record type of F.

For a non-unique AIX each record holds an alternate key value and as many primary keys (KSDS) or
XLRAs (ESDS) of associated records in the cluster's data component as there are records holding that
specific alternate key value. The table of primary keys may vary in length from 1 to very large numbers. No
block size is guaranteed to be large enough to hold the largest possible index record, therefore a record type
of VS is mandated. When a non-unique index record needs to be split into segments, no primary key value or
XLRA is ever split; i.e. only an exact number of these reside within a single segment of the record

Supported Index-types

| Cluster Type | Primary - Unique | AIX - unique | AIX - Non-unique |
|--------------|------------------|--------------|------------------|
| ESDS         | Y\*              | Y            | Y                |
| KSDS         | Y                | Y            | Y                |
| RRDS         | Y\*              | N            | N                |

** \* denotes a zVSAM extension **

## File Structure

All zVSAM data is stored in physical files, as defined to the operating system.
Each component consists of one file. This file is formatted as a zVSAM file, the structure of which is
explained in the next set of chapters.

**Please note:** the hosting operating system may impose a limit on physical file size and not every host OS
supports a physical file spanning a volume boundary of the storage device(s). Therefore, to support clusters
that exceed the maximum size of a single physical file, in the future we may need to support clusters that
consist of multiple files

### Structure of physical files

Every zVSAM file has a block size. The block being the basic unit of I/O. The first block of every file is the
prefix block, which is always 4096 bytes in size. The prefix block holds information about the cluster, its
data, and its structure.

Data in the prefix block are not accessible to user programs. However, selected fields in the prefix block can
be queried using a SHOWCB ACB= request.

All Data and Index blocks in the file have a user-defined blocksize (`DATABLOCKSIZE=` and
`INDEXBLOCKSIZE=`). The file is assumed to logically begin with the first block after the prefix block.

There are 5 types of blocks that may occur in zVSAM files:

1) Prefix block – one for each file, being the first 4096 bytes of every file
2) Spacemap block – used to manage free space in the file
3) Data block – used to hold user data, or AIX data records (in an AIX only)
4) Index block – used to hold index information
5) ELIX block – used to index segmented (read: large) non-unique AIX records

Every block has an internal structure consisting of a block header, a list of records, a block body and a block
footer. The block header and footer have a fixed structure. The list of records has a variable length. The
block body contains record data and/or free space.

Each of the 5 block types is explained in more detail below.

### Block Header Structure

Every block has a block header (`ZVSAMHDR`). All block headers have the same structure.

`BHDRSEQ#` is incremented by one every time the block is written out to the file.
The footer area contains a comparable field: `BFTRSEQ#`. Together they guard against incomplete writes.

`BHDRXLVL` indicates the index level. Zero is the leaf level. Index blocks are chained by level. That is, for
every index level in use there is a pair of s in the prefix block (`PFXBLVLn`/`PFXELVLn`) that starts and ends
the chain for that level.

`BHDRSELF` contains the block's own XLRA. This helps to guard against misdirected reads and/or writes.

`BHDRNEXT`/`BHDRPREV` point to the next and previous block on the chain. Which chain this is, depends
on the `BHDRFLAG` setting, and, if this is an index block, by the `BHDRXLVL` value.
For the prefix block, these two fields are set to foxes.

Segmented records are a special case. Segments of a segmented record never share their block with other
data. The block holding the first segment is part of the data chain. A block holding a non-first segment is part
of the segment chain. A block that holds a record's first segment has an SPX pointing to the block holding
the next segment. Subsequent segments are retrieved by following the SPXs to the last segment of the record.
The Segment chain starting at `PFXBSEGM` and ending at `PFXESEGM` has no role in processing a spanned
dataset but just provides an extra integrity check.

### Block Footer Structure

Every block has a block footer `ZVSAMFTR`). All block footers have the same structure.

`BFTRSEQ#` is incremented by one every time the block is written out to the file.
The header area contains a comparable field: `BHDRSEQ#`. Together they guard against incomplete writes.

### Prefix Block

The prefix block (`ZVSAMPFX`) consists of the first 4096 bytes of every physical file. It contains meta-data
defining the file and its attributes. It also contains various counters.

The prefix block consists of a block header immediately followed by the prefix area.
The prefix block also contains other data fields, these are addressed from the prefix area.
The prefix block ends with a block footer. A record list is not present on the prefix block.

There are various fields in the prefix area. These point to fields allocated elsewhere in the prefix block.
Their exact addresses on the prefix block may vary.
The `PFXDPAT@`, `PFXDNAM@`, `PFXXPAT@`, `PFXXNAM@` all point to a halfword-prefixed string.
`PFXDVOL@` and `PFXXVOL@` contain foxes (this may change in the future).

The Counters area (`ZVSAMCTR`) directly follows the Prefix area; it is doubleword aligned.
This area is expected to move into the catalog dataset in a future release

The overall structure of the prefix block would look something like this (areas not to scale):
![Graphical overview of Prefix Block](zVSAM_V2_Block_Type_Prefix.jpg)

#### Prefix Block chain summary

The following table summarizes the way that blocks in the file are chained from the prefix block.
The prefix block doesn't reside on any chain.

| Block Type                | Beginning of chain | End of chain |
|---------------------------|--------------------|--------------|
| Prefix                    | foxes              | foxes        |
| Spacemap                  | `PFXBMAP`          | `PFXEMAP`    |
| Data (in use and free)    | `PFXBDATA`         | `PFXEDATA`   |
| Data (non-first segments) | `PFXBSEGM`         | `PFXESEGM`   |
| Index (in use and free)   | `PFXBLVLn`         | `PFXELVLn`   |

#### Counters Area and its maintenance

All fields are 8 bytes except `CTRAVGRL` which is 4 bytes.






