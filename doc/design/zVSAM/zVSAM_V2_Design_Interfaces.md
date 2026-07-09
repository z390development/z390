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
| GENCB BLK=ACB | Dynamically create/instantiate ACB(s)                |
| MODCB ACB=    | Dynamically modify an ACB                            |
| SHOWCB ACB=   | Extract ACB subfield(s) (generic getter method)      |
| TESTCB ACB=   | Test ACB subfield(s) (generic tester method)         |
| CBMR          | Create/instatiate Control Block Modification Request |

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

======================

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

> [!NOTE]
> Direct access to subfields in the ACB is strongly discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
> MODCB ACB= to inspect, test, and/or modify the ACB's content.

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

======================

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

======================

### GENCB ACB macro

The GENCB macro with BLK=ACB will generate or manipulate ACBs and initialize or change them
according to the parameters specified on the macro invocation. It is for this reason that
all supported parameters and keywords of the ACB macro (as described above) are supported
on the GENCB macro when BLK=ACB is specified.

The GENCB macro's function depends on the ZVSAM option in effect:

| Option   | Effect                   |
|----------|--------------------------|
| ZVSAM(0) | Error: zVSAM disabled    |
| ZVSAM(1) | GENCB1 macro is expanded |
| ZVSAM(2) | GENCB2 macro is expanded |

The structure and layout of the ACB are not part of the interface definition
and are therefore not shown in this chapter. For details please see the
[zACB description](zVSAM_V2_Design_Addenda.md#zacb-description) or the ACB2 macro in the mac folder.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the GENCB request to the CBMR handler
are not part of the interface and are therefore not shown in this chapter. For details please see the
[CBMR description](zVSAM_V2_Design_Addenda.md#cbmr-description) or the CBMR macro in the mac folder.

> [!NOTE]
> Direct access to subfields in the ACB or CBMR is strongly discouraged. Use GENCB BLK=ACB, SHOWCB ACB=,
> TESTCB ACB= and/or MODCB ACB= to generate, inspect, test, and/or modify the ACB's content.

All keywords on the GENCB ACB macro are optional. Except BLK= which is required.

The GENCB ACB macro can be coded as follows:

| Opcode        | Operand                   | Remarks                                                                                                 |
|---------------|---------------------------|---------------------------------------------------------------------------------------------------------|
| [label] GENCB | BLK=ACB                   | Instructs GENCB to generate 1 or more ACBs                                                              |
|               | [AM=VSAM]                 | Optional, no other values allowed; VSAM is the default                                                  |
|               | [COPIES=nr]               | The number of identical ACBs to generate                                                                |
|               | [WAREA=addr]              | The work area where the ACBs are to be constructed                                                      |
|               | [LENGTH=nr]               | Length of the work area in bytes                                                                        |
|               | [LOC=keyword]             | Where GENCB is to allocate dynamically acquired storage - if needed                                     |
|               | **[other]**               | **Any parameter supported on the ACB macro**                                                            |
|               | [MF=]                     | Use standard form of GENCB ACB; this is the default                                                     |
|               | [MF=L/MF=(L,addr,[label]] | Use list form of GENCB ACB                                                                              |
|               | [MF=(E,addr)]             | Use execute form of GENCB ACB                                                                           |
|               | [MF=(G,addr,[label])]     | Use generate form of GENCB ACB                                                                          |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

#### BLK=

Required parameter; specify ACB to generate 1 or more ACBs

#### AM=

VSAM is the default and the only supported value.

#### COPIES=

Number of identical ACBs to generate ranging from 1 to 65535. Defaults to 1.

#### WAREA=

The work area where the ACBs are to be constructed.

- When WAREA is specified, LENGTH must be specified too.
- When WAREA is not specified, the CBMR handler allocates an area of storage.
- The address of this area whether via GETMAIN or WAREA is returned in R1.
- The length of the generated ACB(s) is returned in R0.

#### LENGTH=

- If WAREA= is specified, this paramter is required and specifies the length of the area.
- If WAREA= is not specified, this parameter is ignored. zVSAM determines how much storage to allocate.

#### LOC=

- If WAREA= is specified, this paramter is ignored.
- If WAREA= is not specified, this parameter indicates where zVSAM is to allocate storage for the ACB or ACBs.

Supported keywords:
- BELOW = below 16M (addressable in Amode 24, 31, or 64)
- ANY   = below 2G  (requires Amode 31 or 64 to address)

#### Other keywords

All parameters supported by the [ACB macro](#acb-macro) are supported here as well.

#### MF=

Indicates the Macro Format.
If specified, the [label] subparameter is EQUated to the length of the CBMR.
See [MF= parameter](#mf-parameter) for details.

#### Return and Reason Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=9   | WAREA is too small                                                       |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

======================

### MODCB ACB macro

The MODCB macro with ACB=addr will modify an ACB according to the parameters specified on the macro invocation.
It is for this reason that all parameters and keywords of the ACB macro (as described above) are supported
on the MODCB macro when ACB=addr is specified.

The MODCB macro's function depends on the ZVSAM option in effect:

| Option   | Effect                   |
|----------|--------------------------|
| ZVSAM(0) | Error: zVSAM disabled    |
| ZVSAM(1) | MODCB1 macro is expanded |
| ZVSAM(2) | MODCB2 macro is expanded |

The structure and layout of the ACB are not part of the interface definition
and are therefore not shown in this chapter. For details please see the
[zACB description](zVSAM_V2_Design_Addenda.md#zacb-description) or the ACB2 macro in the mac folder.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the MODCB request to the CBMR handler
are not part of the interface and are therefore not shown in this chapter. For details please see the
[CBMR description](zVSAM_V2_Design_Addenda.md#cbmr-description) or the CBMR macro in the mac folder.

> [!NOTE]
> Direct access to subfields in the ACB or CBMR is strongly discouraged. Use GENCB BLK=ACB, SHOWCB ACB=,
> TESTCB ACB= and/or MODCB ACB= to generate, inspect, test, and/or modify the ACB's content.

All keywords on the MODCB ACB macro are optional. Except ACB= which is required.

The MODCB ACB macro can be coded as follows:

| Opcode        | Operand                   | Remarks                                             |
|---------------|---------------------------|-----------------------------------------------------|
| [label] MODCB | ACB=address               | Points MODCB to the ACB to be modified              |
|               | [AM=VSAM]                 | Optional, no other values allowed                   |
|               | **[other]**               | **Any parameter supported on the ACB macro**        |
|               | [MF=]                     | Use standard form of MODCB ACB; this is the default |
|               | [MF=L/MF=(L,addr,[label]] | Use list form of MODCB ACB                          |
|               | [MF=(E,addr)]             | Use execute form of MODCB ACB                       |
|               | [MF=(G,addr,[label])]     | Use generate form of MODCB ACB                      |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

#### ACB=

Required parameter; specify the address of the ACB to be modified.

#### AM=

VSAM is the default and the only supported value.

#### Other keywords

All parameters supported by the [ACB macro](#acb-macro) are supported here as well.

Please note: not supported are expressions like `(S,scon)` or `(*,scon)`

##### MACRF=

MACRF is a special case of the "other keywords". This paragrqaph clarifies how MACRF works.

All supported subparameters have their own bit in `CBMRACB_MACRF` (currently 16),
Conflicts are `MNOTE`d, eg. bits for `NIS` and `SIS` cannot both be on.

If MF=E is specified then the whole of `CBMRACB_MACRF` is replaced,

When the ACB is modified:
- For mutually exclusive parameters, the bit is turned on or off
- For each non-exclusive parameter the appropriate bit is turned on, therefore it isn't possible to turn a nonexclusive
  bit off using MODCB, this has to be done manually.
- eg. When an ACB has MACRF=(OUT) which allows read and write functions it is not possible to change
  the ACB to read-only using MODCB
- if this is needed code the instruction `NI ACBMACR1,255-ACBOUT`

> [!NOTE]
> I do not entirely agree with how Melvyn has set this up, although I do like his extensive early error detection proposal.
> There are basically two alternatives that I can see:
> 1. every MACRF option has a separate verb code, we generate as many verb codes as we need, no data is needed
> 2. We generate a single verb code for the MACRF modification, supplying two 2-byte masks in the data.
>    One mask to indicate affected postions, the other to indicate the desired bit values for the selected postions.

#### MF=

Indicates the Macro Format.
If specified, the [label] subparameter is EQUated to the length of the CBMR.
See [MF= parameter](#mf-parameter) for details.

#### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=12  | MODCB was attempted on an open ACB                                       |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

======================

### SHOWCB ACB macro

The SHOWCB macro with ACB=addr will return ACB-related fields according to the parameters specified
on the macro invocation in the order they are specified. Duplicates are permitted.

The SHOWCB macro's function depends on the ZVSAM option in effect:

| Option   | Effect                    |
|----------|---------------------------|
| ZVSAM(0) | Error: zVSAM disabled     |
| ZVSAM(1) | SHOWCB1 macro is expanded |
| ZVSAM(2) | SHOWCB2 macro is expanded |

The structure and layout of the ACB are not part of the interface definition
and are therefore not shown in this chapter. For details please see the
[zACB description](zVSAM_V2_Design_Addenda.md#zacb-description) or the ACB2 macro in the mac folder.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the SHOWCB request to the CBMR handler
are not part of the interface and are therefore not shown in this chapter. For details please see the
[CBMR description](zVSAM_V2_Design_Addenda.md#cbmr-description) or the CBMR macro in the mac folder.

> [!NOTE]
> Direct access to subfields in the ACB or CBMR is strongly discouraged. Use GENCB BLK=ACB, SHOWCB ACB=,
> TESTCB ACB= and/or MODCB ACB= to generate, inspect, test, and/or modify the ACB's content.

The SHOWCB ACB macro can be coded as follows:

| Opcode         | Operand                    | Remarks                                                       |
|----------------|----------------------------|---------------------------------------------------------------|
| [label] SHOWCB | ACB=address                | Points MODCB to the ACB to be queried                         |
|                | [AM=VSAM]                  | Optional, no other values allowed                             |
|                | AREA=addr                  | Address of return area                                        |
|                | LENGTH=nr                  | Size of return area in bytes                                  |
|                | [OBJECT=DATA/INDEX]        | For KSDS: select data or index component; DATA is the default |
|                | FIELDS=(keywd_list)        | List of keywords indicating which fields to return            |
|                | [MF=]                      | Use standard form of SHOWCB ACB; this is the default          |
|                | [MF=L/MF=(L,addr,[label]]  | Use list form of SHOWCB ACB                                   |
|                | [MF=(E,addr)]              | Use execute form of SHOWCB ACB                                |
|                | [MF=(G,addr,[label])]      | Use generate form of SHOWCB ACB                               |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

#### ACB=

Required parameter; specify the address of the ACB to be queried.

#### AM=

VSAM is the default and the only supported value.

#### AREA=

Required parameter; specify the address of the return area.

#### LENGTH=

Required parameter; specify the length of the return area.

### OBJECT=

Optional parameter; if specified must be `DATA`or `INDEX`. `DATA` is the default.

#### FIELDS=

Specifies a list of keywords. Each keyword specified returns a field of 4 or 8 bytes. These return values are stored consecutively in the return area specified in the `AREA`= and `LENGTH`= parameters.
Some keywords are valid only when the ACB is open. An error is returned when any of these keywords are used while the ACB is not open.

Defined options for the FIELDS parameter are listed below:

| Keyword  | Length | Remarks                                                                                      |
|----------|--------|----------------------------------------------------------------------------------------------|
| ACBLEN   | 4      | Size of ACB in bytes                                                                         |
| AVSPAC   | 4      | Available space in component                                                                 |
| BFRFND   | 4      | Nr of buffer hits for component (No I/O needed to satisfy read request)                      |
| BSTRNO   | 4      | Initial nr of strings                                                                        |
| BUFND    | 4      | Nr of data buffers specified in ACB                                                          |
| BUFNI    | 4      | Nr of index buffers specified in ACB                                                         |
| BUFNO    | 4      | Number of buffers in use for component                                                       |
| BUFNOL   | 4      | Number of data/index buffers allocated for LSR processing                                    |
| BUFRDS   | 4      | I/O count in buffers                                                                         |
| BUFSP    | 4      | Buffer space in bytes specified in ACB                                                       |
| BUFUSE   | 4      | Number of data/index buffers actually in use                                                 |
| CDTASIZE | 8      | Size of a compressed dataset (returns zero)                                                  |
| CINV     | 4      | Block size for component                                                                     |
| CIPCA    | 4      | CI's in CA (returns zero)                                                                    |
| DDNAME   | 8      | DDname specified in ACB                                                                      |
| ENDRBA   | 4      | PFXHLRA, recalculated to RBA value of Block's last byte                                      |
| ERROR    | 4      | Return code from last open/close operation                                                   |
| EXLLEN   | 4      | Length of EXLST in bytes                                                                     |
| EXLST    | 4      | Ptr to EXLST, foxes if no EXLST applies                                                      |
| FS       | 4      | Nr of free blocks per 100 blocks in the component. Derived from PFXFRBLK and PFXFRINT values |
| HALCRBA  | 4      | Highest valid XLRA in the component, recalculated to an RBA value                            |
| HLRBA    | 4      | For OBJECT=INDEX only, highest index block RBA.                                              |
| KEYLEN   | 4      | Length of key field                                                                          |
| LEVEL    | 8      | Address (4 bytes) and length (4 bytes) of field containing zVSAM version number              |
| LOKEY    | 8      | Ptr (4 bytes) to lowest key in the cluster + length (4 bytes) of key                         |
| LRECL    | 4      | Maximum record length; foxes if in excess of 4GB                                             |
| MAREA    | 4      | Ptr to message area, foxes if not relevant                                                   |
| MLEN     | 4      | Length of message area, foxes if not relevant                                                |
| NCIS     | 4      | Nr of Block splits in the data component. Foxes for index.                                   |
| NDELR    | 4      | Nr of deleted records from data component. Foxes for index.                                  |
| NEXCP    | 4      | Nr of I/O requests for the component                                                         |
| NEXT     | 4      | Nr of extents to the physical file. Foxes.                                                   |
| NINSR    | 4      | Nr of records inserted for the data component. Foxes for index.                              |
| NIXL     | 4      | Nr of index levels for index component. Foxes for data component.                            |
| NLOGR    | 4      | Nr of records in the component                                                               |
| NRETR    | 4      | Nr of records retrieved from the data component. Foxes for index.                            |
| NSSS     | 4      | Nr of control area splits. Foxes.                                                            |
| NUIW     | 4      | Nr of implicit write operations.                                                             |
| NUPDR    | 4      | Nr of updated records in the component                                                       |
| PASSWD   | 4      | Ptr to password, consisting of length (1 byte, binary) followed by actual password value     |
| RELEASE  | 8      | Address (4 bytes) and length (4 bytes) of field containing zVSAM version number              |
| RKP      | 4      | Relative Key Position, offset of key within logical record                                   |
| RMODE31  | 4      | 0=None, 1=Buff, 2=CB, 3=All.                                                                 |
| RPLLEN   | 4      | Length of RPL in bytes                                                                       |
| SDTASIZE | 8      | Data size.                                                                                   |
| SHRPOOL  | 4      | SHRPOOL number                                                                               |
| STMST    | 8      | System timestamp of last close                                                               |
| STRMAX   | 4      | Max nr of concurrently active strings                                                        |
| STRNO    | 4      | Max nr of allocated strings                                                                  |
| UIW      | 4      | Nr of explicit writes for component                                                          |
| XAVSPAC  | 8      | AVSPAC when value may exceed 4GB                                                             |
| XBFRFND  | 8      | BFRFND when value may exceed 4GB                                                             |
| XBUFNO   | 8      | BUFNO when value may exceed 4GB                                                              |
| XBUFRDS  | 8      | BUFRDS when value may exceed 4GB                                                             |
| XBUFUSE  | 8      | BUFUSE when value may exceed 4GB                                                             |
| XENDRBA  | 8      | ENDRBA when value may exceed 4GB                                                             |
| XHALCRBA | 8      | HALCRBA when value may exceed 4GB                                                            |
| XHLRBA   | 8      | HLRBA when value may exceed 4GB                                                              |
| XNCIS    | 8      | NCIS when value may exceed 4GB                                                               |
| XNDELR   | 8      | NDELR when value may exceed 4GB                                                              |
| XNEXCP   | 8      | NEXCP when value may exceed 4GB                                                              |
| XNINSR   | 8      | NINSR when value may exceed 4GB                                                              |
| XNLOGR   | 8      | NLOGR when value may exceed 4GB                                                              |
| XNRETR   | 8      | NRETR when value may exceed 4GB                                                              |
| XNUIW    | 8      | NNUIW when value may exceed 4GB                                                              |
| XNUPDR   | 8      | NUPDR when value may exceed 4GB                                                              |
| XSTRMAX  | 8      | STRMAX when value may exceed 4GB                                                             |
| XUIW     | 8      | UIW when value may exceed 4GB                                                                |

#### MF=

Indicates the Macro Format.
If specified, the [label] subparameter is EQUated to the length of the CBMR.
See [MF= parameter](#mf-parameter) for details.

#### Return and Reason Codes

| Return Code | Reason Code     | Meaning                                                                          |
|-------------|-----------------|----------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                       |
| R15=4       | Reason Code=1   | ACBPFX or ACBXPFX are zero                                                       |
|             |                 | (X)HLRBA requested and OBJECT=DATA                                               |
|             |                 | 4-byte version oif 8-byte field is requested but the 1st four bytes are not zero |
|             |                 | CTRLOKEY@ is foxes for: non-KSDS / KSDS index / KSDS data but empty              |
| R15=4       | Reason Code=4   | Invalid control block                                                            |
| R15=4       | Reason Code=9   | Length too small                                                                 |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created         |

======================

### TESTCB ACB macro

The TESTCB macro with ACB=addr will test ACB-related fields according to the parameters specified
on the macro invocation. Only a single test can be specified on each TESTCB invocation.
TESTCB returns a PSW condition code of 8=Equal when the specified test is met, 7=NotEqual otherwise.

The TESTCB macro's function depends on the ZVSAM option in effect:

| Option   | Effect                    |
|----------|---------------------------|
| ZVSAM(0) | Error: zVSAM disabled     |
| ZVSAM(1) | TESTCB1 macro is expanded |
| ZVSAM(2) | TESTCB2 macro is expanded |

The structure and layout of the ACB are not part of the interface definition
and are therefore not shown in this chapter. For details please see the
[zACB description](zVSAM_V2_Design_Addenda.md#zacb-description) or the ACB2 macro in the mac folder.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the TESTCB request to the CBMR handler
are not part of the interface and are therefore not shown in this chapter. For details please see the
[CBMR description](zVSAM_V2_Design_Addenda.md#cbmr-description) or the CBMR macro in the mac folder.

The TESTCB ACB macro can be coded as follows:

> [!NOTE]
> Direct access to subfields in the ACB or CBMR is strongly discouraged. Use GENCB BLK=ACB, SHOWCB ACB=,
> TESTCB ACB= and/or MODCB ACB= to generate, inspect, test, and/or modify the ACB's content.

The SHOWCB ACB macro can be coded as follows:

| Opcode         | Operand                   | Remarks                                                                       | Conditions returned |
|----------------|---------------------------|-------------------------------------------------------------------------------|---------------------|
| [label] TESTCB | ACB=address               | Points TESTCB to the ACB to be tested                                         | n/a                 |
|                | [AM=VSAM]                 | Optional, no other values allowed                                             | n/a                 |
|                | ERET=addr                 | Address of error handling routine                                             | n/a                 |
|                | [OBJECT=DATA/INDEX]       | For KSDS: select data or index component                                      | n/a                 |
|                | ACBLEN=nr                 | length of ACB in bytes                                                        | EQ LO HI            |
|                | ATRB=(keywd_list)         | List of keywords indicating attributes to test                                | keyword dependent   |
|                | AVSPAC=nr                 | Available space in data/index                                                 | EQ LO HI            |
|                | BFRFND=nr                 | Buffer hits for data/index including LSR                                      | EQ LO HI            |
|                | BSTRNO=nr                 | Initial value of strings for a path                                           | EQ LO HI            |
|                | BUFND=nr                  | Nr of data buffers                                                            | EQ LO HI            |
|                | BUFNI=nr                  | Nr of index buffers                                                           | EQ LO HI            |
|                | BUFNO=nr                  | Nr of I/O Buffers                                                             | EQ LO HI            |
|                | BUFRDS=nr                 | Nr of data/index buffer reads                                                 | EQ LO HI            |
|                | BUFSP=nr                  | Buffer space in bytes                                                         | EQ LO HI            |
|                | BUFUSE=nr                 | Number of data/index buffers actually in use                                  | EQ LO HI            |
|                | CINV=nr                   | Block size in bytes                                                           | EQ LO HI            |
|                | DDNAME=string             | DDname                                                                        | EQ LO HI            |
|                | ENDRBA=nr                 | High water mark XLRA                                                          | EQ LO HI            |
|                | ERROR=nr                  | Error code of last error                                                      | EQ LO HI            |
|                | EXLLEN=nr                 | EXLST length in bytes                                                         | EQ LO HI            |
|                | EXLST=adr                 | EXLST address                                                                 | EQ LO HI            |
|                | FS=nr                     | Free Block per 100                                                            | EQ LO HI            |
|                | HALCRBA=nr                | Highest allocated data/index RBA                                              | EQ LO HI            |
|                | HLRBA=nr                  | For OBJECT=INDEX only, highest index block RBA.                               | EQ LO HI            |
|                | KEYLEN=nr                 | Length of key field in bytes                                                  | EQ LO HI            |
|                | LRECL=nr                  | Logical Record Length                                                         | EQ LO HI            |
|                | MACRF=(keyword list)      | List of keywords for processing options. All subparms have to be true for EQ. | EQ NE=HI            |
|                | MAREA=adr                 | Message area address                                                          | ??                  |
|                | MLEN=nr                   | Length of message area in bytes                                               | ??                  |
|                | NCIS=nr                   | Nr of Block splits                                                            | EQ LO HI            |
|                | NDELR=nr                  | Nr of deleted records                                                         | EQ LO HI            |
|                | NEXCP=nr                  | Nr of I/O requests                                                            | EQ LO HI            |
|                | NEXT=nr                   | Nr of extents                                                                 | EQ LO HI            |
|                | NINSR=nr                  | Nr of records inserted                                                        | EQ LO HI            |
|                | NIXL=nr                   | Nr of index levels                                                            | EQ LO HI            |
|                | NLOGR=nr                  | Nr of records                                                                 | EQ LO HI            |
|                | NRETR=nr                  | Nr of records retrieved                                                       | EQ LO HI            |
|                | NSSS=nr                   | Nr of control area splits. Compares to zero.                                  | EQ LO HI            |
|                | NUIW=nr                   | Nr of non-user writes                                                         | EQ LO HI            |
|                | NUPDR=nr                  | Nr of updates applied                                                         | EQ LO HI            |
|                | OFLAGS=OPEN               | Opened successfully?                                                          | EQ NE=HI            |
|                | OPENOBJ=PATH/BASE/AIX     | ACB represents Path/Base/AIX?                                                 | EQ NE=HI            |
|                | PASSWD=adr                | Ptr to 1-byte length followed by password                                     | EQ LO HI            |
|                | RKP=nr                    | Offset of key field within record                                             | EQ LO HI            |
|                | RPLLEN=nr                 | RPL length in bytes                                                           | EQ LO HI            |
|                | SHRPOOL=nr                | SHRPOOL number                                                                | EQ LO HI            |
|                | SDTASZ=adr                | Data size                                                                     | EQ LO HI            |
|                | STMST=adr                 | Pointer to system timestamp field                                             | EQ LO HI            |
|                | STRMAX=nr                 | Max. value of concurrently active strings                                     | EQ LO HI            |
|                | STRNO=nr                  | Max. nr of parallel requests                                                  | EQ LO HI            |
|                | UIW=nr                    | value of user writes                                                          | EQ LO HI            |
|                | XAVSPAC=nr                | AVSPAC when value may exceed 4GB                                              | EQ LO HI            |
|                | XBFRFND=nr                | BFRFND when value may exceed 4GB                                              | EQ LO HI            |
|                | XBUFNO=nr                 | BUFNO when value may exceed 4GB                                               | EQ LO HI            |
|                | XBUFRDS=nr                | BUFRDS when value may exceed 4GB                                              | EQ LO HI            |
|                | XBUFUSE=nr                | BUFUSE when value may exceed 4GB                                              | EQ LO HI            |
|                | XENDRBA=nr                | ENDRBA when value may exceed 4GB                                              | EQ LO HI            |
|                | XHALCRBA=nr               | HALCRBA when value may exceed 4GB                                             | EQ LO HI            |
|                | XHLRBA=nr                 | HLRBA when value may exceed 4GB                                               | EQ LO HI            |
|                | XNCIS=nr                  | NCIS when value may exceed 4GB                                                | EQ LO HI            |
|                | XNDELR=nr                 | NDELR when value may exceed 4GB                                               | EQ LO HI            |
|                | XNEXCP=nr                 | NEXCP when value may exceed 4GB                                               | EQ LO HI            |
|                | XNEXT=nr                  | NEXT when value may exceed 4GB                                                | EQ LO HI            |
|                | XNINSR=nr                 | NINSR when value may exceed 4GB                                               | EQ LO HI            |
|                | XNLOGR=nr                 | NLOGR when value may exceed 4GB                                               | EQ LO HI            |
|                | XNRETR=nr                 | NRETR when value may exceed 4GB                                               | EQ LO HI            |
|                | XNUIW=nr                  | NNUIW when value may exceed 4GB                                               | EQ LO HI            |
|                | XNUPDR=nr                 | NUPDR when value may exceed 4GB                                               | EQ LO HI            |
|                | XSTRMAX=nr                | STRMAX when value may exceed 4GB                                              | EQ LO HI            |
|                | XUIW=nr                   | UIW when value may exceed 4GB                                                 | EQ LO HI            |
|                | [MF=]                     | Use standard form of SHOWCB ACB; this is the default                          | n/a                 |
|                | [MF=L/MF=(L,addr,[label]] | Use list form of SHOWCB ACB                                                   | n/a                 |
|                | [MF=(E,addr)]             | Use execute form of SHOWCB ACB                                                | n/a                 |
|                | [MF=(G,addr,[label])]     | Use generate form of SHOWCB ACB                                               | n/a                 |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

#### ACB=

Required parameter; specify the address of the ACB to be tested.

#### AM=

VSAM is the default and the only supported value.

#### ERET=

Optional address of error handling routine.

#### OBJECT=

Optional parameter; if specified must be `DATA`or `INDEX`. `DATA` is the default.

#### ATRB=

Defined options for the ATRB parameter are listed below:

| Keyword | Remarks                                 |
|---------|-----------------------------------------|
| COMPRESS| Compression on?                         |
| ESDS    | Component is an ESDS?                   |
| KSDS    | Component is a KSDS?                    |
| LDS     | Component is a LDS?                     |
| RRDS    | Component is a RRDS?                    |
| REPL    | Always false for zVSAM                  |
| SPAN    | Component may hold segmented records    |
| SSWD    | Always false for zVSAM.                 |
| UNQ     | Path is defined on unique key?          |
| VRRDS   | Variable-length RRDS?                   |
| VESDS   | Variable-length ESDS? (zVSAM extension) |
| WCK     | Always false for zVSAM                  |
| XADDR   | Extended format?                        |

> [!NOTE]
> All subparameters have to be true for EQ.
> The syntax rules for ATRB have been tightened in zVSAM V2:
> - ESDS, KSDS, LDS, RRDS and VRRDS are considered mutually exclusive
> - `PFXFFLGS` is tested.
> - For `SPAN` or `UNQ`, `PFXRFLGS` is tested.
> - If `COMPRESS`, `REPL`, `SSWD` or `WCK` are included then NE=HI is returned.

#### MACRF=

If any of the following keywords are used then NE=HI will be returned:
- `CNV`, `CFX`, `NFX`, `DDN`, `DSN`, `LEW`, `NLW`, `NRS`, `RST`, `RLS`, `NUB`, `UBF`, `NCI`, `ICI`.

#### Keyword=nr/adr

Remaining Keyword parameters specify a value, an address, or a keyword list to be tested.

> [!NOTE]
> Melvyn defined RBA values. We use LRSNs throughout. If we do not drop the RBA support,
> we'll need to find out why Melvyn introduced RBA values and how he plans to assign/maintain them.

> [!NOTE]
> Melvyn defined ERROR to return code from last open/close.
> I think it should be the error code from last operation that errorred.
> Whichever executable macro that might have been.

#### MF=

Indicates the Macro Format.
If specified, the [label] subparameter is EQUated to the length of the CBMR.
See [MF= parameter](#mf-parameter) for details.

### Return and Reason Codes

| Return Code | Reason Code     | Meaning                                                                                                |
|-------------|-----------------|--------------------------------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                                             |
| R15=4       | Reason Code=1   | This parameter requires the dataset to be open.                                                        |
|             |                 | (X)HLRBA requested and OBJECT=DATA                                                                     |
|             |                 | For fields that have 8-byte values the 4-byte version is requested but the 1st four bytes are not zero |
| R15=4       | Reason Code=4   | Invalid control block                                                                                  |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created                               |

======================

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
