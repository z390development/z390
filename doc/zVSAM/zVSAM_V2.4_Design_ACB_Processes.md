# zVSAM V2 - Logical processes for ACB-based requests

## API for Assembler and zCobol programs

There is much more syntax checking in zVSAM V2 than in IBM macros and this may result in unexpected
MNOTEs. All macros will continue processing after an MNOTE except for the most serious.
This may result in assembler errors. Once the reason for the MNOTE has been resolved the assembler errors
will be eliminated.

## ACB-based interfaces

The ACB is the primary interface for operations at the cluster level.
Each cluster is represented by an ACB.

The ACB interface consists of an ACB control block, possibly an Exit list Control Block, and a set of macros
to manage and manipulate the ACB and EXLST control blocks. These macros can be used in your assembler
programs. For zCobol and/or other higher-level languages, these macros will be generated from
specifications for the files as appropriate in the host language's syntax.

The following macros are provided for assembler programs:

- ACB
- ACBD
- CBMR
- GENCB BLK=ACB
- MODCB ACB=
- SHOWCB ACB=
- TESTCB ACB=

*Note:* The ACB macro defines a statically allocated ACB.
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

*Note:* The EXLST macro defines a statically allocated EXLST.
This macro is primarily intended for use in non-re-entrant programs.
GENCB BLK=EXLST should be used to create an EXLST in dynamically acquired storage,
or in private static storage. MODCB EXLST= can be used to modify an existing EXLST,
whereas SHOWCB EXLST= can be used to query specific fields of an EXLST
and TESTCB EXLST= can be used to validate specific fields of an EXLST.

- OPEN
- CLOSE

*Note:* OPEN and CLOSE macros can be used to open and close either sequential files represented by a DCB
and/or zVSAM files represented by an ACB.

A description of these interfaces as implemented for z390 and zVSAM is detailed in the next chapters

## ACB Macro

The ACB macro will generate an ACB and initialize it according to the parameters
specified on the macro invocation.

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
MODCB ACB= to inspect, test, and/or modify the ACB's content.

All keywords on the ACB macro are optional. Before the cluster is opened,
all ACB values can be modified using MODCB ACB=, or by changing the ACB directly.
The latter is not recommended, as it is not guaranteed to be portable or compatible
with future versions of zVSAM.

The table below shows how the ACB macro can be coded:

| Opcode      | Operand                | Remarks                                                                                                                                            |
|-------------|------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| [label] ACB | [AM=VSAM]              | Designates this ACB as a zVSAM ACB                                                                                                                 |
|             | [DDNAME=ddname]        | DDNAME: name of an environment variable in the host OS holding the name of the cluster to be processed                                             |
|             | [PASSWD=address]       | Address of password for the cluster. Points to a single byte length followed by the password eg. X'05',C'ABCDE'                                    |
|             | [EXLST=address]        | Address of an exit list. Please see the EXLST macro description for details                                                                        |
|             | [MACRF=(keyword list)] | List of keywords for processing options. See table below for valid keywords                                                                        |
|             | [BUFSP=value]          | Max amount of storage (in bytes) to use for buffers                                                                                                |
|             | [BUFND=value]          | Number of data buffers to allocate for this ACB. Specify a number between 1 and 65535                                                              |
|             | [BUFNI=value]          | Number of index buffers to allocate for this ACB. Specify a number between 1 and 65535                                                             |
|             | [RMODE31=keyword]      | Indicates whether buffers and/or control blocks can be allocated above the line                                                                    |
|             | [STRNO=value]          | Number of concurrent requests allowable for this ACB. Specify a number between 1 and 255. The default is 1                                         |
|             | [BSTRNO=value]         | Beginning number of concurrent requests allocated to this ACB when a path is opened. Only applies if MACRF=NSR. Specify a number between 0 and 255 |
|             | [MAREA=address]        | Not supported – future option. Keyword is flagged as ignored with a warning message                                                                |
|             | [MLEN=value]           | Not supported – future option. Keyword is flagged as ignored with a warning message                                                                |
|             | [RLSREAD=keyword]      | Not supported – future option. Keyword is flagged as ignored with a warning message                                                                |
|             | [SHRPOOL=value]        | LSR shared pool number – future option                                                                                                             |

### Notes

With the exception of the DDNAME parameter explained below, all supported parameters are implemented
compatibly with IBM's VSAM implementation. For details, please refer to the relevant IBM manual.

#### DDNAME=

DDNAME is required before open is executed. If DDNAME is not supplied on the ACB macro, the label
used on the ACB macro is used as DDNAME. If neither is specified, a proper value must be supplied by
using MODCB ACB=

In zVSAM the DDNAME refers to the name of an environment variable in the host OS. This variable in turn
should contain the path and qualified filename of the cluster to be opened. The qualifier is the name of an
environment variable in the host OS and is the path to the assembled catalog
For more information on zVSAM catalogs, please refer to the "z390_zVSAM_Catalog_User_Guide"
For more information on environment variables, please refer to the "z390_zVSAM_zREPRO_User_Guide"

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

The default for RMODE31 is NONE

### ACB MACRF keywords

| Keyword subset    | Keyword | Remarks                                                                                                           |
|-------------------|---------|-------------------------------------------------------------------------------------------------------------------|
| [ADR KEY]         | ADR     | Addressed access to ESDS by (X)RBA. Using (X)RBA to access a KSDS is not supported                                |
|                   | KEY     | Keyed access to a KSDS                                                                                            |
|                   | RRN     | access to an RRDS                                                                                                 |
|                   | CNV     | Not supported. Keyword is flagged with a warning message                                                          |
| [DFR NDF]         | DFR     | Allow writes to be deferred                                                                                       |
|                   | NDF     | Do not defer writes                                                                                               |
| [DIR SEQ SKP]     | DIR     | Direct access to ESDS, KSDS or RRDS                                                                               |
|                   | SEQ     | Sequential access to ESDS, KSDS or RRDS                                                                           |
|                   | SKP     | Skip sequential access to KSDS or RRDS. Only for keyed access. Allows the use of POINT                            |
| [IN OUT]          | IN      | Read only access for ESDS, KSDS or RRDS                                                                           |
|                   | OUT     | Both read and write access for ESDS, KSDS or RRDS                                                                 |
| [NIS SIS]         | NIS     | Normal Insert Strategy for KSDS                                                                                   |
|                   | SIS     | Sequential Insert Strategy for KSDS                                                                               |
| [NRM AIX]         | NRM     | DDNAME indicates cluster to be processed                                                                          |
|                   | AIX     | DDNAME of a path to access an AIX directly, rather than using it to access records in the underlying base cluster |
| [NRS RST]         |         | Not supported. Keyword is flagged with a warning message                                                          |
| [LSR GSR NSR RLS] |         | Local, Global or no Shared Buffers. RLS is not supported                                                          |
| [NUB/UBF]         |         | Not supported. Keyword is flagged with a warning message                                                          |
| [CFX/NFX]         |         | Not supported. Keyword is flagged with a warning message                                                          |
| [DDN/DSN]         |         | Not supported. Keyword is flagged with a warning message                                                          |
| [ICI/NCI]         |         | Not supported. Keyword is flagged with a warning message                                                          |
| [LEW/NLW]         |         | Not supported. Keyword is flagged with a warning message                                                          |

## OPEN macro

A cluster needs to be opened before it can be processed. The open macro is used to open one or more clusters
and/or one or more sequential files in a single call.

| Opcode        | Operand            | Remarks                                                                                               |
|---------------|--------------------|-------------------------------------------------------------------------------------------------------|
| [label] OPEN  | (entry[,entry]...) | Each cluster or file requires an entry of two parameters                                              |
|               | [MODE=24/31]       | Residency mode of all control blocks involved. Specify 31 if any resides above the line               |
| Entry format: | address,(options)  | Address of ACB or DCB, followed by a list of options (for DCB only). For ACB omit the list of options |
|               | [MF=I or omitted]  | Use standard form of OPEN                                                                             |
|               | [MF=L]             | Use list form of OPEN                                                                                 |
|               | [MF=(E,address)]   | Use execute form of OPEN                                                                              |

All supported parameters are implemented compatibly with IBM's VSAM implementation. For details,
please refer to the relevant IBM manual

### OPEN macro parameters

| Parameter       | Explanation                                                                                                                                                                                                                                  |
|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| entry           | The OPEN macro accepts a list of entries. Each entry consists of two consecutive parameters: an address and an optional list of options                                                                                                      |
| address         | The address can be specified as an A-type address or as a register. If a register is coded the register number or name must be enclosed in parentheses. The address can be either the address of a DCB or the address of an ACB              |
| options         | For a DCB options may be encoded according to the z390_File_Access_Method_Guide. For an ACB the options list is ignored and should be coded as an omitted parameter. Any options (e.g. IN/OUT) are taken from the ACB, not the open parmlist |
| MF=I or omitted | An open parmlist is generated inline, plus a call to the OPEN SVC using the parmlist                                                                                                                                                         |
| MF=L            | An open parmlist is generated inline                                                                                                                                                                                                         |
| MF=(E,address)  | Code to modify/populate the open parameter list at the indicated address, which may be a relocatable constant or a (register), plus a call to the OPEN SVC using the parmlist                                                                |

*Note:* IBMs OPEN allows an MF=L to be built which can then be overwritten with an MF=E
containing more entries than were originally created, thus causing a storage violation.
zVSAM V2 will return R15=8 if this is attempted.

IBMs OPEN allows an MF=E to modify an MF=L with a different mode causing damage to the list.
zVSAM V2 will return R15=8 if this is attempted

### OPEN logic

Open logic has two major components: the open macro and the actual run-time logic to execute a request to
open a file or a number of files.

Open parameter list entries have two different formats depending on the MODE parameter.
- When MODE=24 then each entry is one fullword
- When MODE=31 then each entry is two fullwords

Only one SVC 19 is generated for each OPEN macro (MF=I or E)

A header precedes the list as follows:
- DC C'zOPC'
- DC AL2(no. of entries) The senior bit is off for MODE=24 and on for MODE=31

The list format and input to OPEN depend on MODE=

R1 points to the header

- MODE=24 AL1(option),AL3(DCB/ACB address)
- MODE=31 AL1(option),XL3'00',AL4(DCB/ACB address)

- option=X'40' INPUT
- option=X'20' OUTPUT
- option=X'60' UPDATE

The last entry has the X'80' bit on in option.
The option is ignored when opening an ACB.

### OPEN execution logic

OPEN execution logic is implemented as a Java routine.
This logic consists of the following elements:

| Action                                      | Details                                                  |
|---------------------------------------------|----------------------------------------------------------|
| Determine type of parameter list            | no.of entries senior bit off, MODE=24                    |
|                                             | no.of entries senior bit on, MODE=31                     |
| Determine if zVSAM V1 or V2                 | 1st 4 bytes <> C'zOPC'=> ZVSAM V1                        |
|                                             | 1st 4 bytes = C'zOPC'=> ZVSAM V2                         |
| loop over all entries in the parameter list | End-of-list is indicated in the option byte of the entry |
| - check address:                            | ACB or DCB First byte = X'A0' => ACB V1                  |
|                                             | First four bytes = C'zACB' => ACB V2                     |
|                                             | First four bytes = C'DCBV' => DCB                        |
|                                             | Otherwise => Error                                       |
| - if DCB invoke DCB open routine            | OPEN logic for DCB is beyond the scope of this document  |
| - if ACB validate ACB                       | ACBID <> X'A0' => Error                                  |
|                                             | ACBSTYP <> X'10' => Error                                |
|                                             | ACBVER <> X'02' => Error                                 |
|                                             | ACB V1/V2 <> ZVSAM(n) parm => Error                      |
| - if ACB valid invoke VSAM open routine     |                                                          |
| - next entry or end-of-loop                 | If bit 0 of an entry is on, terminate loop               |

OPEN logic for ACB handles a single ACB and proceeds as follows:

| Action                                      | Details                                                                              |
|---------------------------------------------|--------------------------------------------------------------------------------------|
| Check ACB status                            | If ACB already open, issue error and fail open                                       |
|                                             | Set ACBIOSFG                                                                         |
| Copy ACB to newly created FCB               | FCB is the java-equivalent of the ACB                                                |
| Extract DDNAME                              | Copy ACBDDNM field from ACB/FCB                                                      |
| Find actual file name                       | Retrieve host variable with name matching ACBDDNM                                    |
|                                             | If not available: issue error and fail open                                          |
| Validate against catalog                    | Find the file name in the catalog. If missing: issue error                           |
| Issue OS open against file                  | Read-only if ACB specifies MACRF=IN                                                  |
|                                             | Update/extend otherwise                                                              |
|                                             | If unsuccessful issue error and fail open                                            |
| Allocate buffer for prefix block            | Save buffer address in FCB                                                           |
| Read first 4096 bytes into buffer           | If read fails, issue error                                                           |
| Validate block header and footer            | If BHDREYE <> C'HDR' issue error                                                     |
|                                             | If BFTREYE <> C'FTR' issue error                                                     |
|                                             | If BHDRSEQ# <> BFTRSEQ# issue error                                                  |
|                                             | If BHDRVER <> X'02' issue error                                                      |
|                                             | If BHDRSELF <> foxes issue error                                                     |
|                                             | If BHDRPREV <> foxes issue error                                                     |
|                                             | If BHDRNEXT <> foxes issue error                                                     |
|                                             | If BHDRFLGS <> X'80' issue error                                                     |
| Validate prefix area                        | if PFXEYE <> C'zPFX' issue error                                                     |
|                                             | if filename <> PFXDNAM issue error                                                   |
|                                             | if file's path <> PFXDPAT issue error                                                |
|                                             | if PFX_INDX is on issue error                                                        |
| Validate counters area                      | if CTREYE <> C'zCTR' issue error                                                     |
| Validate prefix against catalog             | Only if no errors detected thus far:                                                 |
|                                             | compare cluster type                                                                 |
|                                             | compare lrecl                                                                        |
|                                             | compare blocksize                                                                    |
|                                             | compare key offset                                                                   |
|                                             | compare key length                                                                   |
| Fail open on error                          | If any error was detected:                                                           |
|                                             | - request OS to close the file                                                       |
|                                             | - free the prefix buffer                                                             |
|                                             | - set buffer address in FCB to zeros                                                 |
|                                             | - fail the open request                                                              |
| Issue OS open against index file            | If PFXXNAM@ is non-zero then open the indicated index file                           |
|                                             | readonly if ACB MACRF=IN for input/update/extend otherwise                           |
|                                             | Read index header block and repeat all validations with the following modifications: |
|                                             | - if PFX_INDX is off rather than on issue an error                                   |
| Fail open on error                          | If any error was detected:                                                           |
|                                             | - request OS to close the files                                                      |
|                                             | - free the prefix buffers                                                            |
|                                             | - set buffer address in FCB to zeroes                                                |
|                                             | - fail the open request                                                              |
| Create data buffers                         | Based on ACBBUFND                                                                    |
| Create index buffers                        | At least ACBBUFNI in total                                                           |
|                                             | Exactly one for the root block                                                       |
|                                             | At least 4 for each other index level                                                |
| Open component                              | What is opened depends on what type of component the ACB points to                   |
|                                             | A path may imply opening of the base cluster and/or AIXs                             |
|                                             | Repeat the open process for each component                                           |
|                                             | File names and other info to be gathered from the catalog                            |
|                                             | The table on the next page has the permutations of component types                   |
| Update ACB on OPEN completion               | Set ACBIOSFG off                                                                     |
|                                             | Set ACBOPEN on                                                                       |
|                                             | Set addresses for ACBPFX, ACBXPFX, ACBBUFD, ACBBUFI                                  |
|                                             | Set ACBDTYPE:                                                                        |
|                                             | - If MACRF=AIX is specified, then ACB_AIX and ACB_BASE are set on                    |

*Note:* The environment variables take the following form (in a Windows environment)
SET ddname=drive:\path\catalog.filename \
SET catalog=drive:\path \
The ddname variable may only contain one dot

#### Implied OPEN table

This table has the permutations of component types, indented entries are implied processing.

    | Open component          | MACRF=IN                                       | MACRF=OUT                                      |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | Base                    | Opened for input                               | Opened for in/out                              |
    | - AIXs (UPGRADE=NO)     | Not opened                                     | Not opened                                     |
    | - AIXs (UPGRADE=YES)    | Not opened                                     | Opened for in/out                              |
    |                         |                                                | See note 3                                     |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (NOUPDATE) to Base | Opened for input; No error if already open     | Opened for in/out; No error if already open    |
    | - AIXs (UPGRADE=NO)     | Not opened                                     | Not opened                                     |
    | - AIXs (UPGRADE=YES)    | Not opened                                     | Not opened                                     |
    |                         |                                                | See note 1                                     |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (UPDATE) to Base   | Opened for input; No error if already open     | Opened for in/out; No error if already open    |
    | - AIXs (UPGRADE=NO)     | Not opened                                     | Not opened                                     |
    | - AIXs (UPGRADE=YES)    | Not opened                                     | Opened for in/out                              |
    |                         |                                                | See note 3                                     |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (NOUPDATE) to AIX  | Implied open of Base; No error if already open | Implied open of Base; No error if already open |
    | See Note 4              | See Note 2                                     | See Note 2                                     |
    | - AIXs (UPGRADE=NO)     | AIX opened for input                           | AIX opened for input                           |
    | - AIXs (UPGRADE=YES)    | Not opened; See Note 1                         | Not opened; See Note 1                         |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (UPDATE) to AIX    | Implied open of Base; No error if already open | Implied open of Base; No error if already open |
    | See Note 4              | See Note 2                                     | See Note 2                                     |
    | - AIXs (UPGRADE=NO)     | AIX opened for input                           | AIX opened for input                           |
    | - AIXs (UPGRADE=YES)    | Opened for in/out; See Note 3                  | Opened for in/out; See Note 3                  |
    |-------------------------|------------------------------------------------|------------------------------------------------|

*Notes:*
1. A `NOUPDATE PATH` means that the structures for AIXs on the upgrade set are not created
2. The Base is opened by zVSAM for input but has no associated ACB as it hasn't been opened by the application
3. All AIXs on the upgrade set are opened for in/out by zVSAM and may be updated
4. A PATH to an AIX ignores MACRF=IN/OUT

## EXLST macro

The EXLST macro will generate an Exit List control block and initialize it according to the parameters
specified on the macro invocation.

The structure and layout of the generated EXLST are not part of the interface and are therefore not shown in
this chapter. Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST=, TESTCB
EXLST= and/or MODCB EXLST= to inspect, test, and/or modify the EXLST's content.

All keywords on the EXLST macro are optional. Before the cluster is opened, all EXLST values can be
modified using MODCB EXLST=, or by changing the EXLST directly. The latter is not recommended, as it
is not guaranteed to be portable or compatible with future versions of zVSAM.

The table below shows how the EXLST macro can be coded:

| Opcode        | Operand                   | Remarks                                                  |
|---------------|---------------------------|----------------------------------------------------------|
| [label] EXLST | [AM=VSAM]                 | Designates this EXLST as a zVSAM EXLST                   |
|               | [EODAD=(address[,mod]])   | End-of-data exit routine                                 |
|               | [LERAD=(address[,mod]])   | Logical error analysis routine                           |
|               | [SYNAD=(address[,mod]])   | Physical error analysis routine                          |
|               | [JRNAD=(address[,mod]])   | Not supported. Keyword is flagged with a warning message |
|               | [UPAD=(address[,mod]])    | Not supported. Keyword is flagged with a warning message |
|               | [RLSWAIT=(address[,mod]]) | Not supported. Keyword is flagged with a warning message |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For GENCB MF=I, L or G, a missing address will generate zero and no error, whereas IBM displays an error.
It is assumed that the address will be made valid by a MODCB EXLST= macro invocation.
A missing mod will generate A

For GENCB MF=E, a missing address or mod means don't modify that parameter in the CBMR.

*Note:* Although a null address may be set in the EXLST, you cannot change an address to null with MODCB.

### EXLST macro parameters

| Parameter | Explanation                                                                                                               |
|-----------|---------------------------------------------------------------------------------------------------------------------------|
| EODAD=    | Optional parameter to specify the entry address of an exit that handles an end-of-data condition during sequential access |
|           | The routine address may be followed by a modifier. For details, please see below                                          |
|           | The AMODE for the routine is encoded in the address using the common convention                                           |
| LERAD=    | Optional parameter to specify the entry address of an exit that handles logic errors.                                     |
|           | The routine address may be followed by a modifier. For details, please see below                                          |
|           | The AMODE for the routine is encoded in the address using the common convention                                           |
| SYNAD=    | Optional parameter to specify the entry address of an exit that handles physical errors.                                  |
|           | The routine address may be followed by a modifier. For details, please see below                                          |
|           | The AMODE for the routine is encoded in the address using the common convention                                           |
| mod       | modifier, can optionally be specified after each routine address                                                          |
|           | Values: A or N for Active or Not-active. These are mutually exclusive                                                     |
|           | As long as the routine is not active it will not be called by zVSAM                                                       |
|           | The secondary modifier of L (for Load from Linklib) is not supported                                                      |

### Exit logic

This logic is only entered if any of the following conditions are raised:
- End-of-data (EODAD)
- Logical error (LERAD)
- Physical error (SYNAD)

| Action                             | Details               |
|------------------------------------|-----------------------|
| ACBEXLST has an address            | No action if zero     |
| Check that the exit is active      | No action if inactive |
| Check that the address is not zero | No action if zero     |
| Branch to the exit address         |                       |

## CLOSE macro

A cluster needs to be closed after it has been processed. The close macro is used to close one or more clusters
and/or one or more sequential files in a single call.

| Opcode        | Operand            | Remarks                                                                                |
|---------------|--------------------|----------------------------------------------------------------------------------------|
| [label] CLOSE | (entry[,entry]...) | Each cluster or file requires an entry of two parameters                               |
|               | [MODE=24/31]       | Residency mode of all control blocks involved. Specify 31 if any reside above the line |
|               | [TYPE=T]           | Not supported – future option. Keyword is flagged as ignored with a warning message    |
| Entry format: | address,,          | Address of ACB or DCB, followed by two commas to show that options are omitted         |
|               | [MF=I or omitted]  | Use standard form of CLOSE                                                             |
|               | [MF=L]             | Use list form of CLOSE                                                                 |
|               | [MF=(E,address)]   | Use execute form of CLOSE                                                              |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manua.l

### CLOSE macro parameters

For ease of access a short summary follows here:

| Parameter       | Explanation                                                                                                                                                                     |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| entry           | The CLOSE macro accepts a list of entries. Each entry consists of two consecutive parameters: an address and an optional list of options                                        |
| address         | The address can be specified as an A-type address or as a register.                                                                                                             |
|                 | If a register is coded the register number or name must be enclosed in parentheses.                                                                                             |
|                 | The address can be either the address of a DCB or the address of an ACB                                                                                                         |
| options         | Code as an omitted parameter                                                                                                                                                    |
| MF=I or omitted | If the MF parameter is omitted a close parmlist is generated inline, plus a call to the CLOSE SVC using the parmlist.                                                           |
| MF=L            | With MF=L a close parmlist is generated inline                                                                                                                                  |
| MF=(E,address)  | Code to modify/populate the close parameter list at the indicated address, which may be a relocatable constant or a (register), plus a call to the CLOSE SVC using the parmlist |

*Note:* IBMs CLOSE allows an MF=L to be built which can then be overwritten with an MF=E containing
more entries than were originally created, thus causing storage violation.
zVSAM V2 will return R15=8 if this is attempted.

IBMs CLOSE allows an MF=E to modify an MF=L with a different mode causing damage to the list.
zVSAM V2 will return R15=8 if this is attempted.

### CLOSE logic

The close macro generates a close parameter list and/or an SVC 20 instruction to invoke the close routine.

The macro generates the following code:

| MF variant      | Generated Code                                                                                                                     |
|-----------------|------------------------------------------------------------------------------------------------------------------------------------|
| MF=L            | Close parameter list data only                                                                                                     |
| MF=(E,address)  | 1) Code to modify/populate the close parameter list at the indicated address, which may be a relocatable constant or a (register). |
|                 | 2) Code to invoke the close routine                                                                                                |
| MF=I or omitted | 1) Close parameter list data (inline)                                                                                              |
|                 | 2) Code to invoke the close routine                                                                                                |

Close parameter list entries have two different formats depending on the MODE parameter.
- When MODE=24 then each entry is one fullword
- When MODE=31 then each entry is two fullwords

Only one SVC 20 is generated for each CLOSE macro (MF=I or E)

A header precedes the list as follows:
- DC C'zOPC'
- DC AL2(no. of entries) The senior bit is off for MODE=24 and on for MODE=31

The list format and input to CLOSE depend on MODE=

R1 points to the header
- MODE=24 AL1(option),AL3(DCB/ACB address)
- MODE=31 AL1(option),XL3'00',AL4(DCB/ACB address)
option=0 except for the last entry when option=X'80'

### CLOSE execution logic

Close involves lock and buffer management and may involve the closure of associated AIXs.

CLOSE execution logic is implemented as a Java routine.
This logic consists of the following elements:

| Action                                      | Details                                                  |
|---------------------------------------------|----------------------------------------------------------|
| Determine type of parameter list            | no.of entries senior bit off, MODE=24                    |
|                                             | no.of entries senior bit on, MODE=31                     |
| Determine if zVSAM V1 or V2                 | 1st 4 bytes <> C'zOPC'=> ZVSAM V1                        |
|                                             | 1st 4 bytes = C'zOPC'=> ZVSAM V2                         |
| loop over all entries in the parameter list | End-of-list is indicated in the option byte of the entry |
| - check : ACB or DCB                        | First byte = X'A0' => ACB V1                             |
|                                             | First four bytes = C'zACB' => ACB V2                     |
|                                             | First four bytes = C'DCBV' => DCB                        |
|                                             | Otherwise => Error                                       |
| - if DCB invoke DCB close routine           | CLOSE logic for DCB is beyond the scope of this document |
| - if ACB valid invoke VSAM close routine    |                                                          |
| - next entry or end-of-loop                 | If bit 0 of an entry is on, terminate loop               |

CLOSE logic for ACB handles a single ACB and proceeds as follows:

| Action                                      | Details                                                                                            |
|---------------------------------------------|----------------------------------------------------------------------------------------------------|
| Check ACB status                            | If ACB already closed, issue error and fail close                                                  |
| Check lock status                           | If any blocks in this dataset or any associated AIX are locked then wait until the locks are freed |
|                                             | ???may need a timeout mechanism                                                                    |
| Check buffer status                         | Free any read buffers                                                                              |
|                                             | Write any buffers marked as 'pending write' and then free them                                     |
| Issue OS close against file                 | If unsuccessful issue error and fail close                                                         |

