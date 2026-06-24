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

The following macros are provided for assembler programs:

- ACB
- ACBD
- CBMR
- GENCB BLK=ACB
- MODCB ACB=
- SHOWCB ACB=
- TESTCB ACB=

**Note:** The ACB macro defines a statically allocated ACB.
This macro is primarily intended for use in non-reentrant programs.
GENCB BLK=ACB should be used to create an ACB in dynamically acquired storage,
or in private static storage. MODCB ACB= can be used to modify an existing ACB,
whereas SHOWCB ACB= can be used to query specific fields of an ACB
and TESTCB ACB= can be used to validate specific fields of an ACB.

- EXLST
- EXLSTD
- GENCB BLK=EXLST
- MODCB EXLST=
- SHOWCB EXLST=
- TESTCB EXLST=

**Note:** The EXLST macro defines a statically allocated EXLST.
This macro is primarily intended for use in non-reentrant programs.
GENCB BLK=EXLST should be used to create an EXLST in dynamically acquired storage,
or in private static storage. MODCB EXLST= can be used to modify an existing EXLST,
whereas SHOWCB EXLST= can be used to query specific fields of an EXLST
and TESTCB EXLST= can be used to validate specific fields of an EXLST.

- OPEN
- CLOSE

**Note:** OPEN and CLOSE macros can be used to open and close either sequential files represented by a DCB
and/or zVSAM files represented by an ACB.

A description of these interfaces as implemented for z390 and zVSAM is detailed in the next chapters.

### ACB macro

The ACB macro will generate an ACB and initialize it according to the parameters
specified on the macro invocation.

The structure and layout of the generated ACB are not part of the interface definition
and are therefore not shown in this chapter.
For details please see the [ACB chapter in the Addenda](zVSAM_V2_Design_Addenda.md#acb-macro-parameters).

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
MODCB ACB= to inspect, test, and/or modify the ACB's content.

All keywords on the ACB macro are optional. Before the cluster is opened,
all ACB values can be modified using MODCB ACB=, or by changing the ACB directly.
The latter is not recommended, as it is not guaranteed to be portable or compatible
with future versions of zVSAM.

The table below shows how the ACB macro can be coded:

| Opcode      | Operand                | Remarks                                                                                                                                            |
|-------------|------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| [label] ACB | [AM=VSAM]              | Designates this ACB as a zVSAM ACB; this is the default                                                                                            |
|             | [DDNAME=ddname]        | DDNAME: name of an environment variable in the host OS holding the name of the cluster to be processed                                            |
|             | [PASSWD=address]       | Address of password for the cluster. Points to a single byte length followed by the password eg. X'05',C'ABCDE'                                    |
|             | [EXLST=address]        | Address of an exit list. Please see the EXLST macro description for details                                                                        |
|             | [MACRF=(keyword list)] | List of keywords for processing options. See table below for valid keywords                                                                        |
|             | [BUFSP=value]          | Max amount of storage (in bytes) to use for buffers                                                                                                |
|             | [BUFND=value]          | Number of data buffers to allocate for this ACB. Specify a number between 1 and 65535                                                              |
|             | [BUFNI=value]          | Number of index buffers to allocate for this ACB. Specify a number between 1 and 65535                                                             |
|             | [RMODE31=keyword]      | Indicates whether buffers and/or control blocks can be allocated above the line                                                                    |
|             | [STRNO=value]          | Number of concurrent requests allowable for this ACB. Specify a number between 1 and 255. The default is 1                                         |
|             | [BSTRNO=value]         | Beginning number of concurrent requests allocated to this ACB when a path is opened. Only applies if MACRF=NSR. Specify a number between 0 and 255 |
|             | [MAREA=address]        | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                            |
|             | [MLEN=value]           | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                            |
|             | [RLSREAD=keyword]      | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                            |
|             | [SHRPOOL=value]        | Not supported yet – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                                            |

With the exception of the DDNAME= parameter explained below, all supported parameters are implemented
compatibly with IBM's VSAM implementation. For details, please refer to the relevant IBM manual.

#### DDNAME=

DDNAME is required before open is executed. If DDNAME is not supplied on the ACB macro, the label
used on the ACB macro is used as DDNAME. If neither is specified, a proper value must be supplied by
using MODCB ACB=.

In zVSAM the DDNAME refers to the name of an environment variable in the host OS. This variable in turn
should contain the path and qualified filename of the cluster to be opened. The qualifier is the name of an
environment variable in the host OS and is the path to the assembled catalog.
For more information on zVSAM catalogs, please refer to the
[zVSAM Catalog User Guide](../../user_guide/zVSAM/zVSAM_V1_Catalog_User_Guide.md).

#### BUFSP=

Maximum buffer space in virtual storage for this cluster.
This is the combined size in bytes of all buffers allocated for this cluster. If (BUFND + BUFNI) \* Block_size
exceeds the value specified for BUFSP, then BUFND and BUFNI will be reduced proportionally to keep the
total allocation below the limit specified in the BUFSP parameter.

#### RMODE31=

Specifies whether buffers and/or control blocks should be allocated below or above the 16M line:
- NONE Control Blocks and buffers below 16M
- CB Control Blocks above or below 16M, buffers below 16M
- BUFF Control Blocks below 16M, buffers above or below 16M
- ALL Control Blocks and buffers above 16M or below 16M

The default for RMODE31 is NONE.

### ACB MACRF keywords

Defined options for the MACRF parameter are listed below:

| Keyword subset    | Keyword | Remarks                                                                                                           |
|-------------------|---------|-------------------------------------------------------------------------------------------------------------------|
| [ADR/KEY/CNV]     |         | Only one of these allowed; ADR is the default                                                                     |
|                   | ADR     | Addressed access to ESDS by (X)RBA. Using (X)RBA to access a KSDS is not supported                                |
|                   | KEY     | Keyed access to a KSDS or RRDS                                                                                    |
|                   | CNV     | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |
| [DFR/NDF]         |         | Only one of these allowed; DFR is the default                                                                     |
|                   | DFR     | Allow writes to be deferred                                                                                       |
|                   | NDF     | Do not defer writes                                                                                               |
| [DIR]/[SEQ]/[SKP] |         | May be combined; SEQ is the default                                                                               |
|                   | DIR     | Direct access to ESDS, KSDS or RRDS                                                                               |
|                   | SEQ     | Sequential access to ESDS, KSDS or RRDS                                                                           |
|                   | SKP     | Skip sequential access to KSDS or RRDS. Only for keyed access. Allows the use of POINT                            |
| [IN]/[OUT]        |         | May be combined; IN is the default                                                                                |
|                   | IN      | Read access for ESDS, KSDS or RRDS                                                                                |
|                   | OUT     | Both read and write/delete access for ESDS, KSDS or RRDS                                                          |
| [NIS/SIS]         |         | Only one of these allowed; NIS is the default                                                                     |
|                   | NIS     | Normal Insert Strategy for KSDS                                                                                   |
|                   | SIS     | Sequential Insert Strategy for KSDS                                                                               |
| [NRM/AIX]         |         | Only one of these allowed; NRM is the default                                                                     |
|                   | NRM     | DDNAME indicates cluster to be processed                                                                          |
|                   | AIX     | DDNAME of a path to access an AIX directly, rather than using it to access records in the underlying base cluster |
| [NRS/RST]         |         | Only one of these allowed; NRS is the default                                                                     |
|                   | NRS     | Treat dataset as non-reusable                                                                                     |
|                   | RST     | Reset dataste during OPEN.                                                                                        |
| [NSR/LSR/GSR/RLS] |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |
| [NUB/UBF]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |
| [CFX/NFX]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |
| [DDN/DSN]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |
| [ICI/NCI]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |
| [LEW/NLW]         |         | Not supported. Keyword is flagged as ignored with a warning message (Level 4 Mnote)                               |

**Note 1:** Option RRN for access to RRDS was defined by Melvyn, but is not documented for IBM VSAM. Instead we use option KEY to indicate access by RRN to a RRDS.

**Note 2:** Options LSR and GSR were defined by Melvyn, but will not be implemented on z390.



