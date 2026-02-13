# zVSAM V2 - API for Assembler and zCobol programs

## ACB-based interfaces

The ACB is the primary interface for operations at the cluster level.
Each cluster is represented by an ACB.

- ACB.Open
- ACB.Close

The ACB interface consists of an ACB control block, possibly an Exit list Control Block,
and a set of macros to manage and manipulate the ACB and EXLST control blocks.
These macros can be used in your assembler programs. For zCobol and/or other higher-level languages,
these macros will be generated from specifications for the files as appropriate in the host language's syntax.

The following macros are provided for assembler programs:

- ACB
- GENCB ACB
- MODCB ACB
- SHOWCB ACB
- TESTCB ACB

Note: the ACB macro defines a statically allocated ACB. This macro is primarily intended for use
in non-re-entrant programs. GENCB ACB should be used to create an ACB in dynamically acquired storage,
or in private static storage. MODCB ACB can be used to modify a pre-existing ACB, whereas SHOWCB ACB
can be used to query specific fields of an ACB and TESTCB ACB can be used to validate specific fields of an ACB.

- EXLST
- GENCB EXLST
- MODCB EXLST
- SHOWB EXLST
- TESTCB EXLST

Note: the EXLST macro defines a statically allocated EXLST. This macro is primarily intended for use
in non-re-entrant programs. GENCB EXLST should be used to create an EXLST in dynamically acquired storage,
or in private static storage. MODCB EXLST can be used to modify a pre-existing EXLST, whereas SHOWCB EXLST
can be used to query specific fields of an EXLST and TESTCB EXLST can be used to validate specific fields of an EXLST.

- OPEN
- CLOSE

Note: Open and Close macros can be used to open and close either sequential files represented by a DCB
and/or zVSAM files represented by an ACB.

A description of these interfaces as implemented for z390 and zVSAM is detailed in the next chapters.

### ACB macro

The ACB macro will generate an ACB and initialize it according to the parameters specified on the macro invocation.

The structure and layout of the generated ACB are not part of the interface and are therefore not shown
in this chapter. Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB, TESTCB ACB and/or MODCB ACB
to inspect, test, and/or modify the ACB's content.

All keywords on the ACB macro are optional. Before the cluster is opened, all ACB values can be modified
using MODCB ACB, or by changing the ACB directly. The latter is not recommended, as it is not guaranteed
to be portable or compatible with future versions of zVSAM.

The table below shows how the ACB macro can be coded.

| Opcode      | Operand              | Remarks                                                                                                |
|-------------|----------------------|--------------------------------------------------------------------------------------------------------|
| [label] ACB | [AM=VSAM]            | Designates this ACB as a zVSAM ACB                                                                     |
|             | [DDNAME=ddname]      | DDNAME: name of an environment variable in the host OS holding the name of the cluster to be processed |
|             | [PASSWD=ptr]         | Pointer to password for the cluster                                                                    |
|             | [EXLST=ptr]          | Pointer to EXLST structure                                                                             |
|             | [MACRF=(keywd_list)] | List of keywords specifying processing options. See table below for valid keywords                     |
|             | [BUFSP=nr]           | Max amount of storage (in bytes) to use for buffers                                                    |
|             | [BUFND=nr]           | Number of data buffers to allocate for this ACB                                                        |
|             | [BUFNI=nr]           | Number of index buffers to allocate for this ACB                                                       |
|             | [RMODE31=keyword]    | Indicates whether buffers and/or control blocks can be allocated above the line                        |
|             | [STRNO=nr]           | Number of concurrent requests allowable for this ACB                                                   |
|             | [BSTRNO=nr]          | Beginning number of concurrent requests allowable for this ACB                                         |
|             | [_MAREA_=ptr]        | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)    |
|             | [_MLEN_=nr_]         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)    |
|             | [_RLSREAD_=keyword_] | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)    |
|             | [_SHRPOOL_=nr]       | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)    |

Supported options for the MACRF parameter are listed below:

| Keyword subset      | Keyword | Remarks                                                                                             |
|---------------------|---------|-----------------------------------------------------------------------------------------------------|
| [ADR/KEY]           | ADR     | Addressed access to ESDS                                                                            |
|                     | KEY     | Keyed access to KSDS or RRDS                                                                        |
|                     | _CNV_   | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [DFR/NDF]           | DFR     | Allow writes to be deferred                                                                         |
|                     | NDF     | Disallow deferred writes                                                                            |
| [DIR]/[SEQ]/[SKP]   | DIR     | Direct access to RRDS                                                                               |
|                     | SEQ     | Sequential access to ESDS, KSDS or RRDS                                                             |
|                     | SKP     | Skip sequential access to KSDS or RRDS                                                              |
| [IN/OUT]            | IN      | Read access for ESDS, KSDS or RRDS                                                                  |
|                     | OUT     | Both read and write access for ESDS, KSDS or RRDS                                                   |
| [NIS/SIS]           | NIS     | Normal Insert Strategy for KSDS                                                                     |
|                     | SIS     | Sequential Insert Strategy                                                                          |
| [NRM/AIX]           | NRM     | DDname indicates cluster to be processed                                                            |
|                     | AIX     | DDname indicates an AIX to be processed as a path into its base cluster                             |
| [_NRS/RST_]         |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_NSR/LSR/GSR/RLS_] |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_NUB/UBF_]         |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_CFX/NFX_]         |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_DDN/DSN_]         |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_ICI/NCI_]         |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_LEW/NLW_]         |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |

DDNAME= DDname is required before open is executed. If DDname is not supplied on the ACB
macro, the label used on the ACB macro is used as DDname. If neither is specified,
a proper value must be supplied by using MODCB ACB.
In zVSAM the DDname is to hold the name of an environment variable in the host
OS. This variable in turn should contain the path and qualified filename of the cluster
to be opened. The qualifier is the name of an environment variable in the host OS
and is the path to the assembled catalog. For more information on zVSAM catalogs,
please refer to the "z390_zVSAM_Catalog_User_Guide".

With the exception of the DDNAME parameter explained above, all supported parameters are implemented compatibly
with IBM's VSAM implementation. For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.




