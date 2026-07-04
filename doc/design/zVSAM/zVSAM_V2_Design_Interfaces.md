# zVSAM V2 - API for Assembler and zCobol programs

This document describes the macro interfaces for working with zVSAM V2 data sets.

There is much more syntax checking in zVSAM V2 than in IBM macros and this may result in unexpected
MNOTEs. All macros will continue processing after an MNOTE except for the most serious.
This may result in assembler errors. Once the reason for the MNOTE has been resolved the assembler errors
will be eliminated.

## ACB-based interfaces

The ACB is the primary interface for operations at the cluster level.
Each cluster is represented by an ACB.

The ACB interface consists of an ACB control block, possibly an Exit list Control Block,
and a set of macros to manage and manipulate the ACB and EXLST control blocks.
These macros can be used in your assembler programs. For zCobol and/or other higher-level languages,
these macros will be generated from specifications for the files as appropriate in the host language's syntax.

The following macros for assembler programs implement functions to manage ACBs:

| Macro         | Function                                             |
|---------------|------------------------------------------------------|
| ACB           | Create/instantiate an ACB during assembly            |
| ACBD          | Describe ACB subfields                               |
| CBMR          | Create/instatiate Control Block Modification Request |
| GENCB BLK=ACB | Dynamically create/instantiate ACB(s)                |
| MODCB ACB=    | Dynamically modify an ACB                            |
| SHOWCB ACB=   | Extract ACB subfield(s) (generic getter method)      |
| TESTCB ACB=   | Test ACB subfield(s) (generic tester method)         |

**Note:** The ACB macro defines a statically allocated ACB.
This macro is primarily intended for use in non-reentrant programs.
GENCB BLK=ACB should be used to create an ACB in dynamically acquired storage,
or in private static storage. MODCB ACB= can be used to modify an existing ACB,
whereas SHOWCB ACB= can be used to query specific fields of an ACB
and TESTCB ACB= can be used to validate specific fields of an ACB.

The following macros for assembler programs implement functions to manage EXLSTs:

| Macro           | Function                                             |
|-----------------|------------------------------------------------------|
| EXLST           | Create/instantiate an EXLST during assembly          |
| EXLSTD          | Describe EXLST subfields                             |
| GENCB BLK=EXLST | Dynamically create/instantiate EXLST(s)              |
| MODCB EXLST=    | Dynamically modify an EXLST                          |
| SHOWCB EXLST=   | Extract EXLST subfield(s) (generic getter method)    |
| TESTCB EXLST=   | Test EXLST subfield(s) (generic tester method)       |

**Note:** The EXLST macro defines a statically allocated EXLST.
This macro is primarily intended for use in non-reentrant programs.
GENCB BLK=EXLST should be used to create an EXLST in dynamically acquired storage,
or in private static storage. MODCB EXLST= can be used to modify an existing EXLST,
whereas SHOWCB EXLST= can be used to query specific fields of an EXLST
and TESTCB EXLST= can be used to validate specific fields of an EXLST.

The following macros for assembler programs implement data manipulation functions for ACB-defined clusters:

| Macro           | Function                                             |
|-----------------|------------------------------------------------------|
| OPEN            | Open a cluster for processing                        |
| CLOSE           | Close a cluster to terminate processing              |

**Note:** OPEN and CLOSE macros can be used to open and close either sequential files represented by a DCB
and/or zVSAM files represented by an ACB.

A description of these interfaces as implemented for z390 and zVSAM is detailed in the next chapters.

### ACB macro

The ACB macro will generate an ACB and initialize it according to the parameters
specified on the macro invocation.

The ACB macro's function depends on the ZVSAM option in effect:

| Option   | Effect                  |
|----------|-------------------------|
| ZVSAM(0) | Error: zVSAM disabled   |
| ZVSAM(1) | ACB1 macro is expanded  |
| ZVSAM(2) | ACB2 macro is expanded  |

The structure and layout of the generated ACB are not part of the interface definition
and are therefore not shown in this chapter. For details please see the ACB, ACB1 and ACB2 macros
in the mac folder.

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
MODCB ACB= to inspect, test, and/or modify the ACB's content.

All keywords on the ACB macro are optional. Before the cluster is opened,
all ACB values can be modified using MODCB ACB=, or by changing the ACB directly.
The latter is not recommended, as it is not guaranteed to be portable or compatible
with future versions of zVSAM.

The table below shows how the ACB macro can be coded:

| Opcode      | Operand                | Remarks                                                                                                 |
|-------------|------------------------|---------------------------------------------------------------------------------------------------------|
| [label] ACB | [AM=VSAM]              | Designates this ACB as a zVSAM ACB; this is the default                                                 |
|             | [DDNAME=ddname]        | DDNAME: name of an environment variable in the host OS holding the name of the cluster to be processed  |
|             | [PASSWD=address]       | Address of password for the cluster.                                                                    |
|             | [EXLST=address]        | Address of an exit list.                                                                                |
|             | [MACRF=(keyword list)] | List of keywords for processing options.                                                                |
|             | [BUFSP=value]          | Max amount of storage (in bytes) to use for buffers                                                     |
|             | [BUFND=value]          | Number of data buffers to allocate for this ACB.                                                        |
|             | [BUFNI=value]          | Number of index buffers to allocate for this ACB.                                                       |
|             | [RMODE31=keyword]      | Indicates whether buffers and/or control blocks can be allocated above the line                         |
|             | [STRNO=value]          | Number of concurrent requests allowable for this ACB.                                                   |
|             | [BSTRNO=value]         | Beginning number of concurrent requests allocated to this ACB when a path is opened.                    |
|             | [MAREA=address]        | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|             | [MLEN=value]           | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|             | [RLSREAD=keyword]      | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|             | [SHRPOOL=value]        | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |

With the exception of the DDNAME= parameter explained below, all supported parameters are implemented
compatibly with IBM's VSAM implementation. For details, please refer to the relevant IBM manual.

> [!NOTE]
> There is no MF= parameter defined for the ACB macro.
> Use GENCB to generate ACBs in dynamically acquired storage.

> [!NOTE]
> The parameter DSNAME= which is supported for zVSAM V1 will not be supported for zVSAM V2.
> zVSAM V2 will require a DDNAME pointing to a host environment variable holding the file specification
> for a catalog load module, a period, and the catalog entry name.

#### AM=

Optional parameter. `AM=VSAM` is the default. No other values are supported.

#### DDNAME=

DDNAME is required before open is executed. If DDNAME is not supplied on the ACB macro, the label
used on the ACB macro is used as DDNAME. If neither is specified, a proper value must be supplied by
using MODCB ACB=.

In zVSAM V1 and V2 the DDNAME refers to the name of an environment variable in the host OS. This variable in turn
should contain the path and qualified filename of the catalog load module that defines the cluster to be opened.
The qualifier must specify the catalog entry's name instead of the catalog's required `.390` extension.

For more information on zVSAM catalogs, please refer to the
[zVSAM Catalog User Guide](../../user_guide/zVSAM/zVSAM_V1_Catalog_User_Guide.md).

> [!NOTE]
> We are planning to replace the static catalog load modules with a dynamic catalog
> after zVSAM V2 KSDS support covers all required functionality to implement such a dynamic catalog.
> A host environment then may also specify the cluster's base file name.
> The SYSCAT host environment can then be used to point to the catalog to be used.

#### PASSWD=

Supply the address of the password, consisting of a single byte with the password's length (1-8 characters) followed by the password value.

#### EXLST=

Connects this ACB to an EXLST, if any. Please see the [EXLST macro description](#exlst-macro) for details.

#### MACRF=

List of keywords specifying how the cluster will be processed after open.

Defined options for the MACRF parameter are listed below:

| Keyword subset    | Keyword | Remarks                                                                                                                               |
|-------------------|---------|---------------------------------------------------------------------------------------------------------------------------------------|
| [ADR/KEY/CNV]     |         | Non-exclusive keywords indicating whether the cluster may be accessed by address or by key; ADR is the default                        |
|                   | ADR     | Addressed access to ESDS by (X)RBA. Using (X)RBA to access a KSDS is not supported                                                    |
|                   | KEY     | Keyed access to a KSDS or RRDS                                                                                                        |
|                   | CNV     | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |
| [DFR/NDF]         |         | Mutually exclusive keywords indicating whether buffer changes need to be written out to the file immediately                          |
|                   | DFR     | Allows zVSAM to defer writing and keep changes in the buffer                                                                          |
|                   |         | When multiple changes are combined, fewer I/Os are needed which should improve program performance                                    |
|                   | NDF     | Disallows zVSAM to defer writing, forcing a buffer write for every single change to the buffer                                        |
| [DIR]/[SEQ]/[SKP] |         | Can be coded in any combination. If none of the three is specified `SEQ` is used as a default                                         |
|                   | DIR     | Cluster will be processed directly. `DIR` can be used with ESDS, KSDS, or RRDS to access data randomly                                |
|                   | SEQ     | Cluster will be processed sequentially. `SEQ` can be used with ESDS, KSDS, or RRDS to access data sequentially                        |
|                   | SKP     | Allow skip-sequential access. Enables usage of the POINT macro to position the file to a specific position to access data randomly    |
|                   |         | SKP can be used with KSDS or RRDS to randomly position the file to a specific key or RRN prior to sequential access                   |
| [IN]/[OUT]        |         | Non-exclusive keywords indicating whether the cluster will be processed for input only or for both input and output                   |
|                   | IN      | Read-only access for ESDS, KSDS or RRDS                                                                                               |
|                   | OUT     | Both read and write/delete access for ESDS, KSDS or RRDS                                                                              |
| [NIS/SIS]         |         | Mutually exclusive keywords indicating how zVSAM inserts new records into the cluster                                                 |
|                   |         | Relevant only for KSDS clusters. NIS is the default                                                                                   |
|                   | NIS     | Normal insert strategy: zVSAM will insert records optimizing for inserts that are dispersed randomly across the data set              |
|                   | SIS     | Sequential insert stragegy: zVSAM will insert records optimizing for inserts that are (mostly) packed together in a sequential manner |
| [NRM/AIX]         |         | Mutually exclusive keywords indicating how zVSAM is to process accesses to an AIX                                                     |
|                   |         | Relevant only when the DDname specifies a path. NRM is the default                                                                    |
|                   | NRM     | Normal mode: zVSAM will use the AIX to access records in the underlying base cluster                                                  |
|                   | AIX     | AIX mode: zVSAM treats the AIX data as a normal KSDS. This allows direct access to the AIX's data records                             |
| [NRS/RST]         |         | Mutually exclusive keywords to control dataset reset processing; NRS is the default                                                   |
|                   | NRS     | No-ReSet: after OPEN the data in the dataset are available                                                                            |
|                   | RST     | ReSeT: During OPEN the high water mark is reset effectively deleting all the data in the dataset                                      |
| [NSR/LSR/GSR/RLS] |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |
| [NUB/UBF]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |
| [CFX/NFX]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |
| [DDN/DSN]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |
| [ICI/NCI]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |
| [LEW/NLW]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                                   |

**Note 1:** Option `DFR`: 

**Note 2:** Option RRN for access to RRDS was defined by Melvyn, but is not documented for IBM VSAM. Instead we use option KEY to indicate access by RRN to a RRDS.

**Note 3:** Options LSR and GSR were defined by Melvyn, but will not be implemented on z390.

#### BUFSP=

Maximum buffer space in virtual storage for this cluster.

This is the combined size in bytes of all buffers allocated for this cluster. If `(BUFND + BUFNI) * Block_size`
exceeds the value specified for `BUFSP`, then `BUFND` and `BUFNI` will be reduced proportionally to keep the
total allocation below the limit specified in the `BUFSP` parameter.

#### BUFND=

Number of data buffers to allocate for this ACB. Specify a number between 1 and 65535.
When over-allocating (see `BUFSP` parameter above) fewer data buffers will be allocated than requested.

#### BUFNI=

Number of index buffers to allocate for this ACB. Specify a number between 1 and 65535.
When over-allocating (see `BUFSP` parameter above) fewer index buffers will be allocated than requested.
#### RMODE31=

Specifies whether buffers and/or control blocks should be allocated below the 16M line, or may be allocated above the 16M line.
The default is `NONE`.

The following keywords are supported:
- `NONE` Control Blocks and buffers below 16M
- `CB` Control Blocks above or below 16M, buffers below 16M
- `BUFF` Control Blocks below 16M, buffers above or below 16M
- `ALL` Control Blocks and buffers above 16M or below 16M

#### STRNO=

Number of concurrent requests allowable for this ACB. Specify a number between 1 and 255. The default is 1.

#### BSTRNO=

Beginning number of concurrent requests allocated to this ACB when a path is opened. Specify a number between 1 and 255. The default is 1.

### ACBD macro

The ACBD macro maps the ACB. Its behaviour depends on the ZVSAM option in effect:

| Option   | Effect                  |
|----------|-------------------------|
| ZVSAM(0) | Error: zVSAM disabled   |
| ZVSAM(1) | ACBD1 macro is expanded |
| ZVSAM(2) | ACBD2 macro is expanded |

The mappings defined in the ACBD1 and ACBD2 macros are very different.

For mapping details, please see the [zACB layout](zVSAM_V2_Design_Addenda.md#zacb-description)
or the `ACBD`, `ACBD1` and `ACBD2` macros in the mac folder.

> [!NOTE]
> The ACBD macro generates no executable code.

> [!NOTE]
> The ACBD macro can be invoked multiple times, but will generate the DSECT mapping
> only on its first invocation.

### CBMR macro

A CBMR is generated for all forms of the '`GENCB`, `MODCB`, `SHOWCB`, and `TESTCB` macros.
The CBMR is then used to direct the Control Block Management Program to carry out the
request(s) encoded on the macro invocation.

The CBMR macro maps the Control Block Management Request.
The CBMR encodes a GENCB, MODCB, SHOWCB or TESTCB request
and can be used with `BLK=ACB` to indicata an ACB-related Request,
with `BLK=EXLST` to indicate an EXLST-related request, or with
`BLK=RPL` to indicate an RPL-related request.

The behaviour of the CBMR macro depends on the ZVSAM option in effect:

| Option   | Effect                   |
|----------|--------------------------|
| ZVSAM(0) | Error: requires zVSAM(2) |
| ZVSAM(1) | Error: requires zVSAM(2) |
| ZVSAM(2) | CBMR macro is expanded   |

The CBMR is not available with zVSAM V1; it is implemented for zVSAM V2 only.

For mapping details, please see the [CBMR layout](zVSAM_V2_Design_Addenda.md#cbmr-description)
or the `CBMR` macro in the mac folder.



