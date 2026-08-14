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
| ESDS         | Index on XLRSN ??            |
| KSDS         | Index on key value           |
| RRDS         | Index on RRN                 |
| LDS          | No index                     |
| AIX          | Index on alternate key value |

> [!NOTE]
> Melvyn dropped LDS support. I don't know why. We should re-evaluate.
> And why do we need an index on an ESDS? That seems rather superfluous.
> I think Melvyn added an index in XRBA. We should re-evaluate that too.

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

zVSAM clusters consist of one or two components.

ESDS, RRDS, and LDS clusters consist of a data component only.
KSDS and AIX clusters have a data component and an additional index component.

The index component holds the primary keys in a tree-like structure. Every primary key
is associated with the XLRSN of the data record that it represents.

E.g. if a KSDS holds information on cars, the license plate ID could be the primary key.
The index component then would hold the license plate IDs, each with the XLRSN of the complete record
in the data component.

An alternate index cluster - an AIX for short - is used to create an access path to a base cluster,
using other information than the primarty key to retrieve the data record.

E.g. if a KSDS holds information on cars, an alternate key might be defined on the owner's last name.

An AIX does not contain XLRSNs to the base cluster. Instead, every AIX data record contains a single
alternate key value, followed by the primary keys of all data records that have the alternate key value.

E.g. The entry for "Smith" followed by a long list of cars owned by a person named "Smith".

Every AIX is a KSDS in its own right. The data records that associate each alternate key with their respective
primary keys are stored in the data component. The index component then holds an entry for each data record's
primary key, associated with the XLRSN for the AIX data record in the AIX's data component.

Note: the alternate key defined on the base cluster thus becomes the primary key on the AIX cluster.

The rest of this document explains:
1. how clusters are built from files
2. how files are built from Blocks
3. how Blocks are constructed from structure elements

### Physical files

All zVSAM data is stored in physical files, as defined to the operating system.
Each component consists of one file. This file is formatted as a zVSAM file, the structure of which is
explained in the next set of chapters.

> [!NOTE]
> the hosting operating system may impose a limit on physical file size and not every host OS
> supports a physical file spanning a volume boundary of the storage device(s). Therefore, to support clusters
> that exceed the maximum size of a single physical file, in the future we may need to support clusters that
> consist of multiple files.

### Structure of physical files

Every zVSAM file has a block size. The block being the basic unit of I/O.
The first block of every file is the prefix block, which is always 4096 bytes in size.
The prefix block holds information about the cluster, its data, and its structure.

Data in the prefix block are not accessible to user programs.
However, selected fields in the prefix block can be queried using a `SHOWCB ACB=` request.

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
> Melvyn added support for ELIX blocks. We should reconsider that, too.

Not all block types occur in all file types. The relation is as follows:

| File Type  | Prefix | Spacemap | Data | Index | Raw |
|------------|--------|----------|------|-------|-----|
| ESDS       | Y      | Y        | Y    | N     | N   |
| KSDS-data  | Y      | Y        | Y    | N     | N   |
| KSDS-index | Y      | Y        | N    | Y     | N   |
| RRDS       | Y      | Y        | Y    | N     | N   |
| AIX-data   | Y      | Y        | Y    | N     | N   |
| AIX-index  | Y      | Y        | N    | Y     | N   |
| LDS        | Y      | N        | Y    | N     | Y   |

> [!NOTE]
> KSDS and AIX clusters consist of a data component and an index component.
> ESDS, RRDS, and LDS clusters consist of a data component only.

### Components of a Block

With the exception of Raw Blocks, all blocks have internal structure elements, such as:
- a Block Header
- a Block Footer
- a Record Pointer List
- data records

All Blocks (except Prefix Block and Raw Blocks) are chained into a chain which is anchored in the Prefix Block.
The type of Block determines on which chain it resides:
- Spacemap Chain
- Data Chain (data blocks, with the exception of non-first segment blocks)
- Segment chain (for non-first segment blocks exclusively)
- Index chains (one for each index level)

Every Block (except Raw Blocks) has a header holding information to implement the applicable chain.
Every Block (except Raw Blocks) also has a footer, which mainly serves to guard integrity of the data stored on the Data Block.

Not all structure elements occur in all Block types. The relation is as follows:

| Structure Element   | Prefix | Spacemap | Data | Index | Raw |
|---------------------|--------|----------|------|-------|-----|
| Block Header        | Y      | Y        | Y    | Y     | N   |
| Block Footer        | Y      | Y        | Y    | Y     | N   |
| Record Pointer List | N      | N        | Y    | Y     | N   |
| Record data         | N      | N        | Y    | Y     | Y   |
| Free Space          | Y      | N        | Y    | Y     | N   |
| Prefix Area         | Y      | N        | N    | N     | N   |
| Counters Area       | Y      | N        | N    | N     | N   |
| Spacemap            | N      | Y        | N    | N     | N   |

### ESDS Data Organization

> [!NOTE]
> this paragrpah still needs to be created, including a drawing.

### KSDS Data Organization

> [!NOTE]
> this paragrpah still needs to be created, including a drawing.

### KSDS Index Organization

> [!NOTE]
> this paragrpah still needs to be created, including a drawing.

### RRDS Data Organization

> [!NOTE]
> this paragrpah still needs to be created, including a drawing.

### AIX Data Organization

AIX data records for a Unique Alternate Index have fixed-length records.
The AIX cluster is mostly treated as a KSDS with a record type of F.
Only when the AIX is opened as a path, will zVSAM use the AIX data to retrieve
records from the underlying base cluster.

AIX data records for a Non-Unique Alternate Index have variable-length segmented records.
The AIX cluster is mostly treated as a KSDS with a record type of VS.
Only when the AIX is opened as a path, will zVSAM use the AIX data to retrieve
records from the underlying base cluster.

AIX unique records have the following format:

| AIX on ... | Record Content                  |
|------------|---------------------------------|
| ESDS       | AIX key followed by XRBA(8)     |
| KSDS       | AIX key followed by primary key |
| RRDS       | AIX key followed by RRN         |

AIX non-unique records have the following format:

| AIX on ... | Record Content                               |
|------------|----------------------------------------------|
| ESDS       | AIX key followed by 1 or more XRBA(8) values |
| KSDS       | AIX key followed by 1 or more primary keys   |
| RRDS       | AIX key followed by 1 or more RRN values     |

The diagram below shows how AIX data blocks are chained in a Unique AIX's data component.

![Diagram showing layout of a chain of AIX Data Blocks](img/zVSAM_V2_Drawing_Chain_AIX_Unique.jpg)

The diagram below shows how AIX data blocks are chained in a Non-Unique AIX's data component.

![Diagram showing layout of a Chain of Segmented AIX Data Blocks](img/zVSAM_V2_Drawing_Chain_AIX_NonUnique.jpg)

### AIX Data Organization

> [!NOTE]
> this paragrpah still needs to be created, including a drawing.

### LDS Data Organization

> [!NOTE]
> this paragrpah still needs to be created, including a drawing.

### ESDS Fixed non-Spanned

The records are stored one after another, filling the block until no space is left.
When the remaining free space is insufficient to accommodate another record, that free space remains
unusable. Unusable space can be eliminated by building the dataset with `DATAADJUST=YES`.

Format:

![Diagram showing layout of an ESDS Block with Fixed records](img/zVSAM_V2_Drawing_Block_Type_ESDS_F.jpg)

**Note**: The format of an ESDS block with Fixed records is identical to that for a KSDS.
The only difference being that free-space (`DATAFREESPACE=nn%`) does not apply to ESDS datasets.

| Function      | Notes                                   |
|---------------|-----------------------------------------|
| Add           | Yes, but only to the end of the dataset |
| Update        | Yes                                     |
| Delete        | No                                      |
| Length change | n/a                                     |
Access by:      | (X)RBA or AIX key                       |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

### ESDS Fixed Spanned

The records are stored one after another, using a block for each segment and starting each
record on a new block. Record size is expected to exceed block size, so the record is split into segments, the
first segment is created to fill an entire block, and the rest of the record goes into one or more secondary
segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow)

zVSAM extension: Any AIX keys need not be in the first segment.

Below we show an example where each record requires three segments:

![Diagram showing layout of an ESDS Block with Fixed Spanned records](img/zVSAM_V2_Drawing_Block_Type_ESDS_FS.jpg)

**Note**: The format of an ESDS block with Fixed Spanned records is identical to that for a KSDS.

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes, but only to the end of the dataset            |
| Update        | Yes                                                |
| Delete        | No                                                 |
| Length change | n/a                                                |
| Access by:    | (X)RBA or AIX key                                  |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

### ESDS Variable non-Spanned

The records are stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey).

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

This dataset type is a zVSAM extension.

Below we show an example showing how various numbers of records might fit into the blocks

![Diagram showing layout of an ESDS Block with Variable records](img/zVSAM_V2_Drawing_Block_Type_ESDS_V.jpg)

**Note**: The format of an ESDS block with Variable records is identical to that for a KSDS.

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes, but only to the end of the dataset            |
| Update        | Yes                                                |
| Delete        | No                                                 |
| Length change | No                                                 |
| Access by:    | (X)RBA or AIX key                                  |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

> [!NOTE]
> A rewrite that lengthens a record may require more room that is available on the block.
> In this case the RPTR is marked as a displaced record, and the record is physically stored
> on a nearby Block that has enough free space to accommodate the lengthened record.

### ESDS Variable Spanned

The records are stored one after another, filling the block until no space is left.
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

![Diagram showing layout of an ESDS Block with Spanned Variable records](img/zVSAM_V2_Drawing_Block_Type_ESDS_VS.jpg)

**Note**: The format of an ESDS block with Variable Spanned records is identical to that for a KSDS.

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes, but only to the end of the dataset            |
| Update        | Yes                                                |
| Delete        | No                                                 |
| Length change | No                                                 |
| Access by:    | (X)RBA or AIX key                                  |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

> [!NOTE]
> A rewrite that lengthens a record may require more room that is available on the block.
> In this case the RPTR is marked as a displaced record, and the record is physically stored
> on a nearby Block that has enough free space to accommodate the lengthened record.

> [!NOTE]
> When a record is lengthened, it may be necessary to convert it from a normal varaible-length
> record to a segmented one. When a record is shortened - at least in theory - a segmented record
> might qualify to be converted to a normal variable-length record. Whether we implement this latter
> conversion remains to be seen. There is no hard reason to object against having shortened VS record
> consisting of a single segment that occupies less than a single Block.

### KSDS Fixed non-Spanned

The records are stored one after another, filling the block until no space is left.
When the remaining free space is insufficient to accommodate another record, that free space remains
unusable. Unusable space can be eliminated by building the dataset with `DATAADJUST=YES`.

Blocks can be allocated with free space for add operations (`DATAFREESPACE=nn%`).
During add operations available free space gets allocated to the records being added.
When the block is full the block will be split and any new block will have at least nn% free space. 

Format:

![Diagram showing layout of a KSDS Block with Fixed records](img/zVSAM_V2_Drawing_Block_Type_KSDS_F.jpg)

| Function  | Notes                                              |
|-----------|----------------------------------------------------|
| Add       | Yes                                                |
| Update    | Yes, the primary key must not be changed           |
| Delete    | Yes                                                |
| Length    | change n/a                                         |
| Access by | Primary key or AIX key. (X)RBA not yet implemented |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

### KSDS Fixed Spanned

The records are stored one after another, using a block for each segment and starting each
record on a new block. Record size is expected to exceed block size, so the record is split into segments, the
first segment is created to fill an entire block, and the rest of the record goes into one or more secondary
segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow)

zVSAM extension: The primary key and any AIX keys need not be in the first segment.

Below we show an example where each record requires three segments:

![Diagram showing layout of a KSDS Block with Fixed Spanned records](img/zVSAM_V2_Drawing_Block_Type_KSDS_FS.jpg)

| Function      | Notes                                              |
|---------------|----------------------------------------------------|
| Add           | Yes                                                |
| Update        | Yes, the primary key must not be changed           |
| Delete        | Yes                                                |
| Length change | n/a                                                |
| Access by:    | Primary key or AIX key. (X)RBA not yet implemented |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

### KSDS Variable non-Spanned

The records are stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF, marked in grey).

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

Below we show an example showing how various numbers of records might fit into the blocks:

![Diagram showing layout of a KSDS Block with Variable records](img/zVSAM_V2_Drawing_Block_Type_KSDS_V.jpg)

| Function      | Notes                                                                             |
|---------------|-----------------------------------------------------------------------------------|
| Add           | Yes                                                                               |
| Update        | Yes, the primary key must not be changed                                          |
| Delete        | Yes                                                                               |
| Length change | Yes. When a record is shortened it must not affect the primary key or any AIX key |
| Access by:    | Primary key or AIX key. (X)RBA not yet implemented                                |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

> [!NOTE]
> A rewrite that lengthens a record may require more room that is available on the block.
> In this case the RPTR is marked as a displaced record, and the record is physically stored
> on a nearby Block that has enough free space to accommodate the lengthened record.

### KSDS Variable Spanned

The records are stored one after another, filling the block until no space is left.
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

![Diagram showing layout of a KSDS Block with Spanned Variable records](img/zVSAM_V2_Drawing_Block_Type_KSDS_VS.jpg)

| Function      | Notes                                                                             |
| Add           | Yes                                                                               |
| Update        | Yes, the primary key must not be changed                                          |
| Delete        | Yes                                                                               |
| Length change | Yes. When a record is shortened it must not affect the primary key or any AIX key |
| Access by:    | Primary key or AIX key. (X)RBA not yet implemented                                |

> [!NOTE]
> RBA/XRBA not supported by zVSAM. We'll use XLRSN instead.
> Need to investigate how much RBA was implemented by Melvyn.

> [!NOTE]
> A rewrite that lengthens a record may require more room that is available on the block.
> In this case the RPTR is marked as a displaced record, and the record is physically stored
> on a nearby Block that has enough free space to accommodate the lengthened record.

> [!NOTE]
> When a record is lengthened, it may be necessary to convert it from a normal varaible-length
> record to a segmented one. When a record is shortened - at least in theory - a segmented record
> might qualify to be converted to a normal variable-length record. Whether we implement this latter
> conversion remains to be seen. There is no hard reason to object against having shortened VS record
> consisting of a single segment that occupies less than a single Block.

### RRDS Fixed non-Spanned

The records are stored one after another, filling the block until no space is left.
When the remaining free space is insufficient to accommodate another record, that free space remains
unusable. Unusable space can be eliminated by building the dataset with `DATAADJUST=YES`.

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots are initially binary zeros with `RPTR_MTY` set.

Below we show an example where 8 record slots fit into a block:

![Diagram showing layout of an RRDS Block with Fixed records](img/zVSAM_V2_Drawing_Block_Type_RRDS_F.jpg)

| Function      | Notes                                          |
|---------------|------------------------------------------------|
| Add           | Yes, but only to the end of the dataset        |
| Update        | Yes                                            |
| Delete        | Yes, slots may not be deleted. RPTR_MTY is set |
| Length change | n/a                                            |
| Access by:    | RRN                                            |

### RRDS Fixed Spanned

The records are stored one after another, using a block for each segment and starting each
record on a new block. Record size is expected to exceed block size, so the record is split into segments, the
first segment is created to fill an entire block, and the rest of the record goes into one or more secondary
segments which are stored on the next blocks.

Each segment is preceded by a Segment Prefix (SPX, marked in yellow).

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots are initially binary zeros with `RPTR_MTY` set.

This dataset type is a zVSAM extension

Below we show an example where each record requires three segments:

![Diagram showing layout of an RRDS Block with Fixed Spanned records](img/zVSAM_V2_Drawing_Block_Type_RRDS_FS.jpg)

| Function      | Notes                                          |
|---------------|------------------------------------------------|
| Add           | Yes, but only to the end of the dataset        |
| Update        | Yes                                            |
| Delete        | Yes, slots may not be deleted. RPTR_MTY is set |
| Length change | n/a                                            |
| Access by:    | RRN                                            |

### RRDS Variable non-Spanned

The records are stored one after another, filling the block until no space is left.
Every record is preceded by a Record Length Field (RLF).

An RRDS consists of slots (RRNs) which may or may not contain a record.
Empty slots consist of a dummy RLF containing `X'00000004'` with `RPTR_MTY` set, these are shown
in green in the diagram. Non-empty slots have a grey RLF.

When remaining free space is insufficient to accommodate another record, that free space remains
unallocated (marked in blue) and the record is placed on the next block.

> [!NOTE]
> This will not work as it tends to push a storage shortage on one block out to the next.
> Which may have a cascading effect, affecting many blocks in a row.
> Instead we should mark the RPTR entry as a displaced record, and store the record physically
> after the last allocated record slot. Or use a nearby free page, if sufficient free pages
> were allocated when the cluster was initially loaded.

> [!NOTE]
> A rewrite that lengthens a record may require more room that is available on the block.
> In this case the RPTR is marked as a displaced record, and the record is physically stored
> on a nearby Block that has enough free space to accommodate the lengthened record.

Below we show an example showing how various numbers of records might fit into the blocks

![Diagram showing layout of an RRDS Block with Variable records](img/zVSAM_V2_Drawing_Block_Type_RRDS_V.jpg)

| Function      | Notes                                                            |
|---------------|------------------------------------------------------------------|
| Add           | Yes, but only to the end of the dataset                          |
| Update        | Yes                                                              |
| Delete        | Yes, slots may not be deleted. `RPTR_MTY is set` instead.        |
|               | The record is replaced by a dummy RLF and the space is reclaimed |
| Length change | Yes                                                              |
| Access by:    | RRN                                                              |

### RRDS Variable Spanned

The records are stored one after another, filling the block until no space is left.
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

![Diagram showing layout of an RRDS Block with Variable Spanned records](img/zVSAM_V2_Drawing_Block_Type_RRDS_VS.jpg)

| Function      | Notes                                                            |
|---------------|------------------------------------------------------------------|
| Add           | Yes, but only to the end of the dataset                          |
| Update        | Yes                                                              |
| Delete        | Yes, slots may not be deleted. RPTR_MTY is set                   |
|               | The record is replaced by a dummy RLF and the space is reclaimed |
|               | For segmented records, the freed blocks are marked as available  |
| Length change | Yes                                                              |
| Access by:    | RRN                                                              |

> [!NOTE]
> A rewrite that lengthens a record may require more room that is available on the block.
> In this case the RPTR is marked as a displaced record, and the record is physically stored
> on a nearby Block that has enough free space to accommodate the lengthened record.

> [!NOTE]
> When a record is lengthened, it may be necessary to convert it from a normal varaible-length
> record to a segmented one. When a record is shortened - at least in theory - a segmented record
> might qualify to be converted to a normal variable-length record. Whether we implement this latter
> conversion remains to be seen. There is no hard reason to object against having shortened VS record
> consisting of a single segment that occupies less than a single Block.

### AIX Unique

AIX data records for a Unique Alternate Index have fixed-length records.
The AIX cluster is mostly treated as a KSDS with a record type of F.
Only when the AIX is opened as a path, will zVSAM use the AIX data to retrieve
records from the underlying base cluster.

Below we show an example showing how various numbers of records might fit into a unique AIX's data block

![Diagram showing layout of an AIX Data Block](img/zVSAM_V2_Drawing_Block_Type_AIX_Unique.jpg)

> [!NOTE]
> The drawing shows V-type records. Must be fixed.

### AIX Non-unique not segmented

AIX non-unique non-segmented records have the following format:

| AIX on ... | Record Format                                             |
|------------|-----------------------------------------------------------|
| ESDS       | AIX key, an element count n(4) followed by n XRBAs(n\*8)  |
| KSDS       | AIX key, an element count n(4) followed by n primary keys |
| RRDS       | AIX key, an element count n(4) followed by n RRNs         |

Below we show an example showing how various numbers of records might fit into a non-unique AIX's data block

![Diagram showing layout of an unsegmented AIX Data Block](img/zVSAM_V2_Drawing_Block_Type_AIX_Unseg.jpg)

> [!NOTE]
> This paragraph should probably be deleted. For a Non-Unique key you never knwo
> what the maximum number of synonyms in the base cluster will be. An AIX data record
> with a large number of synonyms may exceed Block capacity, requiring the AIX data
> record to be split into segments. An unsegmented non-unique AIX does not seem to make sense.

### AIX Non-unique segmented

AIX non-unique segmented records have the following formats:

| AIX on ... | Record Format of FIRST segment                                                        |
|------------|---------------------------------------------------------------------------------------|
| ESDS       | SPX, AIX key, an element count(4) which is the total no. of elements in all segments. |
|            | The actual number of XLRAs in this segment can be calculated from `SPXSEGLN`          |
| KSDS       | SPX, AIX key, an element count(4) which is the total no. of elements in all segments. |
|            | The actual number of primary keys in this segment can be calculated from `SPXSEGLN`   |
| RRDS       | SPX, AIX key, an element count(4) which is the total no. of elements in all segments. |
|            | The actual number of RRNs in this segment can be calculated from `SPXSEGLN`           |

| AIX on ... | Record Format of MIDDLE or LAST segments                                              |
|------------|---------------------------------------------------------------------------------------|
| ESDS       | SPX and a number of XLRAs.                                                            |
|            | The actual number of XLRAs in this segment can be calculated from `SPXSEGLN`          |
| KSDS       | SPX and a number of primary keys.                                                     |
|            | The actual number of primary keys in this segment can be calculated from `SPXSEGLN`   |
| RRDS       | SPX and a number of RRNs.                                                             |
|            | The actual number of RRNs in this segment can be calculated from `SPXSEGLN`           |

Below we show an example showing how various numbers of records might fit into a non-unique AIX's data block

![Diagram showing layout of a Segmented AIX Data Block](img/zVSAM_V2_Drawing_Block_Type_AIX_Seg.jpg)

> [!NOTE]
> Each segment contains a whole number of elements.

### ELIX Block

> [!NOTE]
> The ELIX was introduced for managing AIX data records with an extremely large number of synonyms.
> Although it is an elegant solution, we should seriously consisder NOT to implement the ELIX.
> In the first place, AIX design should not allow for very large numbers of synonyms. Extend your AIX key if you can.
> In the second place, creating ELIX support is a considerable effort spent on solving a niche problem
> that should not occur in the first place. Maybe, if you do create an AIX with extreme numbers of
> synonyms, bad performance is simply part of the price for having a badly designed index structure.

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

![Diagram showing layout of an ELIX Block](img/zVSAM_V2_Drawing_Block_Type_ELIX.jpg)

The ELIX record has the following format:

| AIX on ... | Record Format                                                             |
|------------|---------------------------------------------------------------------------|
| ESDS       | Highest XRBA followed by the XLRA of the segment (always record 1)        |
| KSDS       | Highest Primary key followed by the XLRA of the segment (always record 1) |
| RRDS       | Highest RRN followed by the XLRA of the segment (always record 1)         |

### LDS Blocks

LDS Blocks have no structure imposed by zVSAM. The entire data block is treated as user data.
There is no Block Header, Block Footer, and no RPTR list. Just user data; as many bytes of
user data as the block size indicates. LDS blocks can be addressed only by their XLRA Block pointer.
















#### Block Header Structure

Every non-Raw Block has a block header (`ZVSAMHDR`).
All block headers have the same structure.

Block Headers are formatted as follows:

| Label    | Offset | Field type | Function                              |
|----------|--------|------------|---------------------------------------|
| ZVSAMHDR |        | DSECT      | Block header area                     |
| BHDREYE  | X'000' | CL3        | =C'HDR' – eyecatcher to mark the area |
| BHDRSEQ# | X'003' | XL1        | Write control value                   |
| BHDRVER  | X'004' | XL1        | Design sequence number                |
| BHDR_V2  |        | =X'02'     | Current design version number         |
| BHDRFLG1 | X'005' | XL1        | Flags                                 |
| BHDR_PFX |        | =X'80'     | Prefix block                          |
| BHDR_MAP |        | =X'40'     | Spacemap block                        |
| BHDR_DTA |        | =X'20'     | Data block                            |
| BHDR_IDX |        | =X'10'     | Index block                           |
| BHDR_SEG |        | =X'08'     | Segment block                         |
| BHDR_LEF |        | =X'04'     | Index leaf Block                      |
| BHDR_INT |        | =X'02'     | Index intermediate block              |
| BHDR_ROT |        | =X'01'     | Index root block                      |
| BHDR#REC | X'006' | XL1        | Nr of records on this block           |
| BHDRXLVL | X'007' | XL1        | Index level                           |
| BHDRSELF | X'008' | XL8        | XLRA of this block                    |
| BHDRNEXT | X'010' | XL8        | XLRA of next block on chain           |
| BHDRPREV | X'018' | XL8        | XLRA of previous block on chain       |
| BHDRFRE@ | X'020' | XL3        | Offset of free area on this block     |
| BHDRFLG2 | X'023' | XL1        | Flags                                 |
| BHDR_ELX |        | =X'80'     | ELIX block                            |
| BHDRFREE | X'024' | XL3        | Length of free area on this block     |
|          |        | XL2        | Reserved                              |

`BHDRSEQ#` is incremented by one every time the block is written out to the file.
The footer area contains a comparable field: `BFTRSEQ#`. Together they guard against incomplete writes.

`BHDRXLVL` indicates the index level. Zero is the leaf level. Index blocks are chained by level.
That is, for every index level in use there is a pair of pointers in the prefix block (`PFXBLVLn`/`PFXELVLn`)
that starts and ends the chain for that level.

`BHDRSELF` contains the block's own XLRA. This helps to guard against misdirected reads and/or writes.

`BHDRNEXT`/`BHDRPREV` point to the next and previous block on the chain. Which chain this is, depends
on the `BHDRFLAG` setting, and, if this is an index block, by the `BHDRXLVL` value.
For the prefix block, these two fields are set to foxes.

`BHDRNEXT`/`BHDRPREV` point to the next and previous block on the chain. Which chain this is, depends
on the `BHDRFLAG` setting, and, if this is an index block, by the `BHDRXLVL` value.
For the prefix block, these two fields are set to foxes.

> [!NOTE]
> Melvyn added that:
> Free blocks are not on any chain, for these blocks the `BHDRPREV`/`BHDRNEXT` pointers can have any value.
> - we need to re-evaluate whether or not the performance gain is worth the imbalance in design and the increased vulnerability.

Segmented records are a special case. Segments of a segmented record never share their block with other data.
The block holding the first segment is part of the data chain. A block holding a non-first segment is part
of the segment chain. A block that holds a record's first segment has an SPX pointing to the block holding
the second segment. Subsequent segments are retrieved by following the SPXs to the last segment of the record

> [!NOTE]
> Melvyn suggested:
> From the second segment the `BHDRPREV`/`BHDRNEXT` chain can be used to sequentially read subsequent segments up to the last segment of the record.
> - I think we need to re-evaluate whether to follow this scheme, or to ues the SPX chain consistently.
> - If we follow Melvyn's line of reasoning, then the following paragraph should be removed.

The Segment chain starting at `PFXBSEGM` and ending at `PFXESEGM` has no role in processing a spanned
dataset but just provides an extra integrity check.

The following table summarizes the way that blocks in the file are chained from the prefix block.
Please note that free data blocks do not reside on any chain. Nor does the prefix block.

> [!NOTE]
> originally, it was planned that free blocks reside on a free chain.
> the table below does not include the free chain implementation:

| Block Type               | Begin of chain | End of chain |
|--------------------------|----------------|--------------|
| Prefix                   | foxes          | foxes        |
| Spacemap                 | PFXBMAP        | PFXEMAP      |
| Data (in use)            | PFXBDATA       | PFXEDATA     |
| Data (non-first segment) | PFXBSEGM       | PFXESEGM     |
| Data (free)              | foxes          | foxes        |
| Index                    | PFXBLVLn       | PFXELVLn     |
| Free                     | n.a.           | n.a.         |

### Block Footer Structure

Every block (except raw blocks) has a block footer. All block footers have the same structure.
It is formatted as follows:

| Label    | Offset | Field type | Function                              |
|----------|--------|------------|---------------------------------------|
| ZVSAMFTR |        | DSECT      | Block header area                     |
| BFTREYE  | X'000' | CL3        | =C'FTR' – eyecatcher to mark the area |
| BFTRSEQ# | X'003' | XL1        | Write control value                   |

`BFTRSEQ#` is incremented by one every time the block is written out to the file.
The header area contains a comparable field: `BHDRSEQ#`. Together they guard against incomplete writes.

### Record Pointer List Structure

Every block that contains data records contains a record list (`ZVSAMRPT`).
Records are accessible only through their Record Pointer or RPTR.
Every entry in the list corresponds with a single record on the block.
The last byte of a record's XLRA is the index into the Record Pointer List.
Index value of X'00' is reserved for block pointers; values X'01' through X'FF' inclusive are usable as RPTR index values.
The difference of 1 always needs to be taken into account when indexing the RPTR list.

The RPTR list always follows the block header directly.

The number of entries on the RPTR list varies with the number of records stored on the block (`BHDR#REC`)
and is terminated by `RPTR_END` to mark the end of the list.

Record Pointer List entries are formatted as follows:

| Label    | Offset | Field type | Function                                                |
|----------|--------|------------|---------------------------------------------------------|
| ZVSAMRPT |        | DSECT      | Record Pointer                                          |
| RPTRFLGS | X'000' | XL1        | Flag byte                                               |
| RPTR_ACT |        | =X'80'     | Active record                                           |
| RPTR_MTY |        | =X'40'     | Empty record slot                                       |
| RPTR_DIS |        | =X'20'     | Record has been displaced to another block              |
| RPTR_MOV |        | =X'10'     | New location of a moved record                          |
| RPTR_SEG |        | =X'08'     | Record segment                                          |
| RPTR_END |        | =X'01'     | Terminating entry                                       |
| RPTRREC@ | X'001' | AL3        | Record offset within block - foxes when RPTR_END is set |

`RPTR_ACT` and `RPTR_MTY` are mutually exclusive. Either one must be set, otherwise the RPTR list is compromised and data access will fail.

When `RPTR_END` is set, `RPTRREC@` is set to foxes. `RPTR_MTY` indicates an empty RRDS slot or a logically deleted record in an ESDS, KSDS, or AIX.

When `RPTR_DIS` is set, the RPTR addresses a Displaced Record Pointer, rather than the actual data.
The format of a Displaced Record Pointer is as follows:

| Label    | Offset | Field type | Function                                       |
|----------|--------|------------|------------------------------------------------|
| ZVSAMDRP |        | DSECT      | Displaced Record Pointer                       |
| DRPIXLRA | X'000' | XL8        | Indirect XLRA = location of actual record data |

#### Segment Prefix

All segments begin with a segment prefix or SPX (`ZVSAMSEG`).
The first segment is on the Data chain and subsequent segments are retrieved via `SPXBNEXT`.
The flag `SPXSEGCC` indicates the first, middle or last segments.

Alternative designs handle organize the segments and their pointers differently.

> [!NOTE]
> A design choice is yet to be made. Whichever variant we choose, we'll
> ahve to thoroughly check the design documents and the code Melvyn has already written.

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

The Counters area (`ZVSAMCTR`) directly follows the Prefix area on the Prefix Block, it is doubleword aligned.
This area is expected to move into the catalog dataset in a future release.
The overall structure of the prefix block would look something like this (areas not to scale):

![Diagram showing layout of a Prefix Block](img/zVSAM_V2_Drawing_Block_Type_Prefix.jpg)

The addenda part of this document contains more details on the [counters area](zVSAM_V2_Design_Addenda.md.md#counters-area) and its maintenance.

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

![Diagram showing layout of a Data Block](img/zVSAM_V2_Drawing_Block_Type_Data.jpg)

It is possible to reserve an amount of freespace at load time which also applies if a block is split.
It is specified in the catalog as `DATAFREESPACE=nn`, where nn is a percentage of the available space.
Only a fixed non-spanned KSDS can specify free space.

For all types of fixed non-spanned datasets, the available space may not be a multiple of the data record size
resulting in unusable space. To correct this use `DATAADJUST=YES` which will calculate an optimal
blocksize less than the specified one.

How the Data Blocks are laid out in the file depends on whether the cluster is defined with Spanned records,
or with unspanned records.

### Free Space

Free space on any block is maintained in a single extent, usually but not necessarily following the RPTR list and preceding the stored record data.

Additionally, there may be empty records on the block. These are marked with the `RPTR_MTY` bit in their RPTR list entry.
These empty record slots are available for reuse and may (if needed) be merged with each other and with the available
free space on the block to create a larger area of free space to satisfy an allocation request.

#### Data Block Structure (SPANNED=NO)

Assume we have a cluster with three data blocks holding unsegmented records.
The blocks are on the data chain as outlined in the picture below. Please note that all depicted pointers are block pointers.
Each pointer thus originates with the indicated field, and ends at the block it points to.
The location where the arrows attach has no meaning since it's a block pointer.

![Diagram showing layout of a Data Block Chain](img/zVSAM_V2_Drawing_Chain_Data_Blocks.jpg)

#### Data Block Structure (SPANNED=YES)

Now suppose we have a cluster with three data blocks, the first block holding two unsegmented records, the
second block holding the first segment of a record consisting of three segments and the third block holding
the first segment of a record consisting of two segments.

In the picture we show the data chain as a solid line (as in the picture above), we show the segment chain as a
dotted line, and we show the SPX s as a fat line.

The picture shows the prefix area's pointer to start/end block of both the data chain and the segment chain.
It also shows the first and second block on each chain pointing to one another.
Same thing for the second and third block on each chain.

The picture also shows that the SPX only occurs on the first segment of each segmented record.

All depicted pointers are block pointers. Each pointer originates with the indicated field,
and ends at the block it points to. The location where the arrows attach has no meaning since it's a block pointer.

![Diagram showing layout of a Segmented Data Block Chain](img/zVSAM_V2_Drawing_Chain_Segmented_Data_Blocks.jpg)

There are two design alternative to the above.

The difference is that in this variant all segments go onto the segment chain.
The SPX resides by itself on the data block and just points to the first segment on the segment chain.

##### Variant 1

![Diagram showing layout of a Segmented Data Block Chain - alternative design](img/zVSAM_V2_Drawing_Chain_Data_Blocks_alt.jpg)

##### Variant 2

![Diagram showing layout of a Segmented Data Block Chain - alternative design](img/zVSAM_V2_Drawing_Chain_Segmented_Data_Blocks_alt.jpg)

### Index Blocks








### Raw Blocks

