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

