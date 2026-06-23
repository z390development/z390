# zVSAM v1 Catalog User Guide

## Catalog and zVSAM Structure

The catalog is currently implemented as one or more assembled modules.
A catalog dataset is on our wish-list.

The `DEFINE` macro described below differs from the mainframe
version, not only because of PC/Mainframe file definitions, but
because we have constructed the VSAM datasets differently.

Despite these differences we have strived to maintain all forms of
application access and update to all types of VSAM datasets in
conformity with IBM's specifications.

The concept of Control Interval has been replaced by Data Blocks
which have similarities.
There is no exact equivalent of a Control Area, but the parameters
`DATA` and `INDEXFREEBLOCKS` allow interspersed empty blocks.

The Catalog structure is as follows:
- DEFINE CATALOG,NAME=,VERSION= name the catalog and version Id
- DEFINE CLUSTER, AIX, PATH define the components
- DEFINE END end the definition

The syntax for `DEFINE` supports the following:
- CATALOG
- CLUSTER
- ALTERNATEINDEX or AIX
- PATH
- END

### DEFINE CATALOG

Must be the first entry. Supports the following parameters:

NAME=
- is mandatory

VERSION=
- is optional and should be allowed to default.
- Default VERSION is 2

### DEFINE CLUSTER

Supports the following parameters:

NAME=
- is mandatory

DATABLOCKSIZE=
- The initial data blocksize setting, this may be modified by other parameters.
- The parameter may be specified in bytes or as nnnnK.
- Minimum 512 bytes, maximum 4096K.
- Default=4K.

INDEXBLOCKSIZE=
- The initial index blocksize setting, this may be modified by other parameters.
- The parameter may be specified in bytes or as nnnnK.
- Minimum 512 bytes, maximum 4096K. Default=4K.

DATAADJUST=YES/NO
- Subject to rules outlined in the zVSAM Technical Guide,
  the DATABLOCKSIZE may be reduced to optimise it.
- Default=NO.

INDEXADJUST=YES/NO
- Subject to rules outlined in the zVSAM Technical Guide,
  the INDEXBLOCKSIZE may be reduced to optimise it.
- Default=NO.

DATAFREESPACE=nn
- Subject to rules outlined in the zVSAM Technical Guide, a
  percentage of the total free space within a data block is
  reserved for expansion.
- Permitted values are 0-99.
- Default=0.

INDEXFREESPACE=nn
- Subject to rules outlined in the zVSAM Technical Guide, a
  percentage of the totalfree space within an index block is
  reserved for expansion.
- Permitted values are 0-99.
- Default=0.

DATAFREEBLOCKS=(x,y)
- Subject to rules outlined in the zVSAM Technical Guide,
  after every y data blocks are written, x empty blocks are
  written to facilitate expansion.\
- Maximum values for x and y are 65535.
- Default=(0,0).
- Note: The values (x,0) are permitted, this causes x empty
  blocks to be written before the first one.

INDEXFREEBLOCKS=(x,y)
- Subject to rules outlined in the zVSAM Technical Guide,
  after every y index blocks are written, x empty blocks are
  written to facilitate expansion.
- Maximum values for x and y are 65535.
- Default=(0,0).
- Note: The values (x,0) are permitted, this causes x empty
  blocks to be written before the first one.

### DEFINE INDEX

Supports the following options:

- INDEX=INDEXED Create a KSDS
- INDEX=NONINDEXED Create an ESDS
- INDEX=NUMBERED Create an RRDS
- INDEX=LINEAR Create an LDS (not likely to be supported)

Supports the following parameters:

RECORDSIZE
- RECORDSIZE=n Fixed length.
- RECORDSIZE=(avg,max) Variable length
- Specifies the record length(s) in bytes.
- If two parameters are present the dataset is assumed to be variable.
- Maximum value is 4GB.

SPANNED=YES/NO
- Subject to rules outlined in the zVSAM Technical Guide,
  records may be split between data blocks and segment
  blocks.
- Default=NO.

KEYS=(length,offset)
- For KSDS only.
- The length and offset of the key within the record.

REUSE=YES/NO
- The cluster is considered empty when opened.
- Default=NO.

DTADSN=
- Override for the data file, must have the full drive:\path\file.sfx
- Must not be the same as IDXDSN=
- This override will be used to open the data file and be used in the Prefix Block.

IDXDSN=
- Override for the index file, must have the full drive:\path\file.sfx
- Must not be the same as DTADSN=
- This override will be used to open the index file and be used in the Prefix Block.

### DEFINE AIX or ALTERNATEINDEX

AIX and ALTERNATEINDEX are synonyms.

Supports the following parameters:

NAME=
- is mandatory

DATABLOCKSIZE=
- The initial AIX data blocksize setting, this may be modified by other parameters.
- The parameter may be specified in bytes or as nnnnK.
- Minimum 512 bytes, maximum 4096K.
- Default=4K.

INDEXBLOCKSIZE=
- The initial AIX index blocksize setting, this may be modified by other parameters.
- The parameter may be specified in bytes or as nnnnK.
- Minimum 512 bytes, maximum 4096K.
- Default=4K.

DATAADJUST=YES/NO
- Subject to rules outlined in the zVSAM Technical Guide, 
  the DATABLOCKSIZE may be reduced to optimise it.
- Default=NO.

INDEXADJUST=YES/NO
- Subject to rules outlined in the zVSAM Technical Guide,
  the INDEXBLOCKSIZE may be reduced to optimise it.
- Default=NO.

DATAFREESPACE=nn
- Subject to rules outlined in the zVSAM Technical Guide, a
  percentage of the total free space within a data block is
  reserved for expansion.
- Permitted values are 0-99.
- Default=0.

INDEXFREESPACE=nn
- Subject to rules outlined in the zVSAM Technical Guide, a
  percentage of the total free space within an index block
  is reserved for expansion.
- Permitted values are 0-99.
- Default=0.

DATAFREEBLOCKS=(x,y)
- Subject to rules outlined in the zVSAM Technical Guide,
  after every y data blocks are written, x empty blocks are
  written to facilitate expansion.
- Maximum values for x and y are 65535.
- Default=(0,0).
- Note: The values (x,0) are permitted, this causes x empty
  blocks to be written before the first one.

INDEXFREEBLOCKS=(x,y)
- Subject to rules outlined in the zVSAM Technical Guide,
  after every y index blocks are written, x empty blocks are
  written to facilitate expansion.
- Maximum values for x and y are 65535.
- Default=(0,0).
- Note: The values (x,0) are permitted, this causes x empty
  blocks to be written before the first one.
  
KEYS=(length,offset)
- The length and offset of the alternate key within the base record.

REUSE=YES/NO
- The AIX is considered empty when opened.
- Default=NO.

DTADSN=
- Override for the AIX data file, must have the full drive:\path\file.sfx
- Must not be the same as IDXDSN=
- This override will be used to open the data file and be used in the Prefix Block.

IDXDSN=
- Override for the AIX index file, must have the full drive:\path\file.sfx
- Must not be the same as DTADSN=
- This override will be used to open the index file and be used in the Prefix Block.

RELATE=clustername
- Mandatory and must have a valid cluster name within the same catalog.

UNIQUEKEY=YES/NO
- The alternate keys are/are not unique.
- Default=YES.

UPGRADE=YES/NO
- The AIX has/has not the potential to be updated in line
  with updates to the base cluster.
- Default=YES

### DEFINE PATH

Supports the following parameters:

NAME=
- is mandatory.

ENTRY=name
- Mandatory and must have the name of an AIX within the same catalog.

UPDATE=YES/NO
- Allows the associated AIX to be updated in line with
  updates to the base cluster.
- Default=YES.

### DEFINE END

Defines the end of definitions for this catalog.

**Note:** this document refers to the "zVSAM Technical Guide"
but that document is not currently available on any archive.