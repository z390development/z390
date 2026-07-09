# zVSAM V2 - RPL-based interfaces

The RPL is the primary interface for operations at the record level.
A program can use multiple RPLs.
An RPL must always point to an open ACB in order to specify a valid operation.

## RPL macro

The RPL macro will generate an RPL and initialize it according to the parameters specified on the macro invocation.

Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL=, TESTCB RPL= and/or MODCB
RPL= to inspect, test, and/or modify the RPL's content.

All keywords on the RPL macro are optional. Before a request is issued, all RPL values can be modified
using MODCB RPL=, or by changing the RPL directly. The latter is not recommended, as it is not
guaranteed to be portable or compatible with future versions of zVSAM.

The table below shows how the RPL macro can be coded

| Opcode      | Operand                | Remarks                                                                                 |
|-------------|------------------------|-----------------------------------------------------------------------------------------|
| [label] RPL | [AM=VSAM]              | Designates this RPL as a zVSAM RPL                                                      |
|             | [ACB=address]          | Address of an open ACB                                                                  |
|             | [AREA=address]         | Address of a record area                                                                |
|             |                        | - In Move mode the record is read into the area                                         |
|             |                        | - In Locate mode a to the record is moved into the area                                 |
|             | [AREALEN=value]        | Length of record area or record                                                         |
|             | [ARG=address]          | Address of the search argument. This is a key, a relative record number, or an RBA.     |
|             | [ECB=address]          | Address of an ECB. Used with asynchronous requests. See note below.                     |
|             | [KEYLEN=value]         | Length of key value in ARG when a generic key search is requested                       |
|             | [MSGAREA=address]      | Address of message area                                                                 |
|             | [MSGLEN=value]         | Length of message area                                                                  |
|             | [NXTRPL=address]       | Address of the next RPL in the chain.                                                   |
|             |                        | RPLs can be chained together to request a series ofoperations in a single call to zVSAM |
|             | [OPTCD=(keyword list)] | List of keywords specifying processing options. See table below for valid keywords      |
|             | [RECLEN=value]         | Record length. Required when updating or adding records                                 |
|             | [TRANSID=value]        | Not supported – future option. Keyword is flagged as ignored with a warning message     |

*ECB note:*
RPLECB is an internal ECB if ECB= is not specified, or an external one if it is.
The bit RPLOPT2_ECB is set for an external ECB.

Supported options for the OPTCD parameter are listed below:

| Keyword subset    | Keyword | Remarks                                                                             |
|-------------------|---------|-------------------------------------------------------------------------------------|
| [ADR | KEY]       | ADR     | Addressed access to ESDS or KSDS (under review)                                     |
|                   | KEY     | Keyed access to KSDS or RRDS                                                        |
|                   | CNV     | Not supported – future option. Keyword is flagged as ignored with a warning message |
| [DIR | SEQ | SKP] | DIR     | Direct access to ESDS, KSDS, RRDS                                                   |
|                   | SEQ     | Sequential access to ESDS, KSDS or RRDS                                             |
|                   | SKP     | Skip sequential access to KSDS or RRDS                                              |
| [ARD | LRD]       | ARD     | Access user-defined record location                                                 |
|                   | LRD     | Access last record in the cluster                                                   |
| [FWD | BWD]       | FWD     | Forward processing                                                                  |
|                   | BWD     | Backward processing                                                                 |
| [SYN | ASY]       | SYN     | Synchronous request                                                                 |
|                   | ASY     | Asynchronous request                                                                |
| [NUP | UPD | NSP] | NUP     | Not for update                                                                      |
|                   | UPD     | For update                                                                          |
|                   | NSP     | Retain positioning for next sequential access                                       |
| [KEQ | KGE]       | KEQ     | Locate record with exact key match                                                  |
|                   | KGE     | Locate record with exact key match, or next higher value                            |
| [FKS | GEN]       | FKS     | Full key search                                                                     |
|                   | GEN     | Generic key search. KEYLEN required                                                 |
| [MVE | LOC]       | MVE     | Move mode                                                                           |
|                   | LOC     | Locate mode                                                                         |
| [RBA | XRBA]      | RBA     | 4-byte RBA values                                                                   |
|                   | XRBA    | 8-byte extended RBA values                                                          |
| [NWAITX/WAITX]    |         | Not supported – future option. Keyword is flagged as ignored with a warning message |
| [CR/NRI]          |         | Not supported – future option. Keyword is flagged as ignored with a warning message |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

## POINT macro

## GET macro

## PUT macro

## ERASE macro

## CHECK macro

## ENDREQ macro

## VERIFY macro

## GENCB, MODCB, TESTCB and SHOWCB macros

### GENCB, MODCB, TESTCB and SHOWCB use of the CBMR

### GENCB, MODCB, TESTCB and SHOWCB use of MF=<a id="MFdetails" />

All forms except MF=L generate executable code.

| Parameter            | Explanation                                        |
|----------------------|----------------------------------------------------|
| MF=I or omitted      | Generates CBMR and invokes ZVSAM19C to process it  |
| MF=L                 | Generates CBMR inline                              |
| MF=(L,address)       | Generates CBMR inline and then moves it to address |
| MF=(L,address,label) | as above and generates label equ size              |
| MF=(E,address)       | Modifies the CBMR at address                       |
|                      | Invokes ZVSAM19C to process the CBMR               |
| MF=(G,address)       | Generates CBMR inline and then moves it to address |
|                      | Invokes ZVSAM19C to process the CBMR               |
| MF=(G,address,label) | as above and generates label equ size              |

address can be label or reg, reg cannot be 0, 1, 14 or 15. reg is not permitted for MF=L

### GENCB, MODCB, TESTCB and SHOWCB parameter types

For abs expression (called value in the macro definitions)

| Parameter type        | For MF=I/G/L                   | For MF=E                       |
|-----------------------|--------------------------------|--------------------------------|
| n                     | Permitted                      | Permitted                      |
| EQUated numeric value | Permitted, but not for LENGTH= | Permitted, but not for LENGTH= |

For address

| Parameter type               | For MF=I/G/L                        | For MF=E                                |
|------------------------------|-------------------------------------|-----------------------------------------|
| n                            | Permitted. See note 1               | Permitted, but not for ERET= See note 1 |
|                              |                                     | When n=0 see note 2                     |
| EQUated numeric value        | Permitted, but not for LENGTH=      | Permitted, but not for LENGTH=          |
|                              | See note 1                          | See note 1 here                         |
| ADCON-type address           | Permitted                           | Permitted                               |
| Register form (reg)          | Permitted, but not regs 0,1,14,15   | Permitted, but not regs 0,1,14,15       |
| Indirect form with ADCON     | Permitted for certain 8-byte fields | Permitted for certain 8-byte fields     |
| (\*,address)                 | See Note 3                          | See Note 3                              |
| Indirect form with disp(reg) | Permitted for certain 8-byte fields | Permitted for certain 8-byte fields     |
| (\*,n(reg))                  | reg cannot be 0,1,14,15. See Note 3 | reg cannot be 0,1,14,15. See Note 3     |

*Note 1:* The use of numeric values instead of an address may lead to accessing low storage and
should be avoided

*Note 2:* An exception is TESTCB EODAD, JRNAD, LERAD and SYNAD where zero instead of an
address means 'don't test the address'

*Note 3:* The following fields only support the indirect form in TESTCB:
`SDTASZ`, `STMST` and all `X*` fields.
The lack of proper syntax checking in the IBM macro can cause access to low storage or
environmental destruction, so the following syntaxes are not allowed: `(*,*)` and `(*,n)`.

## GENCB BLK=EXLST macro

The GENCB macro with BLK=EXLST will generate or manipulate Exit Lists for use with ACBs and
initialize or change them according to the parameters specified on the macro invocation. It is for this reason
that all supported parameters and keywords of the EXLST macro (as described above) are supported on the
GENCB macro when BLK=EXLST is specified.

Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST=, TESTCB EXLST= and/or
MODCB EXLST= to inspect, test, and/or modify the EXLST's content.

Direct access to subfields in the CBMR is strongly discouraged.

The GENCB EXLST macro can be coded as follows:

| Opcode        | Operand           | Remarks                                                                                                                         |
|---------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------|
| [label] GENCB | BLK=EXLST         | Instructs GENCB to generate one or more EXLSTs                                                                                  |
|               | [AM=VSAM]         | Optional, no other values allowed                                                                                               |
|               | [COPIES=1]        | The number of identical EXLSTs to generate. Specify a number between 1 and 65535                                                |
|               | [WAREA=address]   | The work area where the EXLSTs are to be constructed                                                                            |
|               | [LENGTH=value]    | Length of the work area in bytes. If WAREA/LENGTH are omitted then storage is dynamically acquired and LOC=BELOW is the default |
|               | [LOC=BELOW | ANY] | Where GENCB is to allocate dynamically acquired storage if needed                                                               |
|               | **[other]**       | **Any parameter supported on the EXLST macro**                                                                                  |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |

All supported parameters are implemented compatibly with IBM's VSAM implementation
For details, please refer to the relevant IBM manual.

### WAREA=

- When WAREA is specified, LENGTH must be specified too
- When WAREA is not specified, the CBMR handler allocates an area of storage
- The address of this area whether via GETMAIN or WAREA is returned in R1
- The length of the generated EXLST(s) is returned in R0

### LENGTH=

Length in bytes of the area indicated by WAREA.
When LENGTH is specified, WAREA must be specified as well

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=9   | WAREA is too small                                                       |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

## GENCB BLK=RPL macro

The GENCB BLK=RPL macro generates or manipulates RPLs and initializes or changes them according to
the parameters specified on the macro invocation. It is for this reason that all supported parameters and
keywords of the RPL macro (as described above) are supported on the GENCB macro.

Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL=, TESTCB RPL= and/or MODCB
RPL= to inspect, test, and/or modify the RPL's content.

Direct access to subfields in the CBMR is strongly discouraged.

The GENCB RPL macro can be coded as follows:

| Opcode        | Operand           | Remarks                                                                                                                         |
|---------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------|
| [label] GENCB | BLK=RPL           | Instructs GENCB to generate 1 or more RPLs                                                                                      |
|               | [AM=VSAM]         | Optional, no other values allowed                                                                                               |
|               | [COPIES=1]        | The number of identical RPLs to generate. Specify a number between 1 and 65535.                                                 |
|               | [WAREA=address]   | The work area where the RPLs are to be constructed                                                                              |
|               | [LENGTH=value]    | Length of the work area in bytes. If WAREA/LENGTH are omitted then storage is dynamically acquired and LOC=BELOW is the default |
|               | [LOC=BELOW | ANY] | Where GENCB is to allocate dynamically acquired storage if needed                                                               |
|               | **[other]**       | **Any parameter supported on the RPL macro**                                                                                    |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |

All supported parameters are implemented compatibly with IBM's VSAM implementation
For details, please refer to the relevant IBM manual.

### WAREA=

- When WAREA is specified, LENGTH must be specified too
- When WAREA is not specified, the CBMR handler allocates an area of storage
- The address of this area whether via GETMAIN or WAREA is returned in R1
- The length of the generated RPL(s) is returned in R0

### LENGTH=

Length in bytes of the area indicated by WAREA.
When LENGTH is specified, WAREA must be specified as well

## Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=9   | WAREA is too small                                                       |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

## MODCB EXLST= macro

The MODCB macro with EXLST=address will modify an EXLST according to the parameters specified on
the macro invocation. It is for this reason that all parameters and keywords of the EXLST macro (as
described above) are supported on the MODCB macro when EXLST=address is specified.

Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST=, TESTCB EXLST= and/or
MODCB EXLST= to inspect, test, and/or modify the EXLST's content.

Direct access to subfields in the CBMR is strongly discouraged.

The MODCB EXLST macro can be coded as follows:

| Opcode        | Operand           | Remarks                                                                                                                         |
|---------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------|
| [label] MODCB | EXLST=address     | Points MODCB to the EXLST to be modified                                                                                        |
|               | [AM=VSAM]         | Optional, no other values allowed                                                                                               |
|               | **[other]**       | **Any parameter supported on the EXLST macro**                                                                                  |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=4   | EXLST= does not point to an EXLST                                        |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

## MODCB RPL= macro

The MODCB macro with RPL=address will modify an RPL according to the parameters specified on the
macro invocation. It is for this reason that all parameters and keywords of the RPL macro (as described
above) are supported on the MODCB macro when RPL=address is specified.

Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL=, TESTCB RPL= and/or MODCB
RPL= to inspect, test, and/or modify the RPL's content.

Direct access to subfields in the CBMR is strongly discouraged.

The MODCB RPL macro can be coded as follows:

| Opcode        | Operand           | Remarks                                                                                                                         |
|---------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------|
| [label] MODCB | RPL=address       | Points MODCB to the RPL to be modified                                                                                          |
|               | [AM=VSAM]         | Optional, no other values allowed                                                                                               |
|               | **[other]**       | **Any parameter supported on the RPL macro**                                                                                    |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### ECB=

ECB= can be modified to zero or an address
- If it's zero then RPLOPT2_ECB is reset (internal ECB)
- If it's non-zero then RPLOPT2_ECB is set (external ECB)

### OPTCD=

To clarify how OPTCD works...

All supported subparameters have their own bit in `CBMRRPL_OPTCD` (currently 22),
Conflicts are `MNOTEd`, eg. bits for FWD and BWD cannot both be on.

If MF=E is specified then the whole of `CBMRRPL_OPTCD` is replaced.

When the RPL is modified, then for each subset, RPLOPTn bits are turned on or off as appropriate

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

## SHOWCB with no specified block type macro

The SHOWCB macro without a block will return length fields according to the parameters specified on the
macro invocation in the order they are specified. Duplicates are permitted.

| Opcode         | Operand               | Remarks                                                  |
|----------------|-----------------------|----------------------------------------------------------|
| [label] SHOWCB | [AM=VSAM]             | Optional, no other values allowed                        |
|                | AREA=address          | Address of return area                                   |
|                | LENGTH=value          | Size of return area in bytes                             |
|                | FIELDS=(keyword list) | List of keywords indicating which fields to return       |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |

Supported options for the FIELDS parameter are listed below:

| Keyword | Length | Remarks                    |
|---------|--------|----------------------------|
| ACBLEN  | 4      | Length of ACB in bytes     |
| EXLLEN  | 4      | Length of EXLST in bytes   |
| RPLLEN  | 4      | Length of RPL in bytes     |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=9   | Length too small                                                         |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

## SHOWCB EXLST= macro

The SHOWCB macro with EXLST=address will return EXLST-related fields according to the parameters
specified on the macro invocation in the order they are specified. Duplicates are permitted

Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST=, TESTCB= EXLST and/or
MODCB EXLST= to inspect, test, and/or modify the EXLST's content.

Direct access to subfields in the CBMR is strongly discouraged.

The SHOWCB EXLST= macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  |
|----------------|-----------------------|----------------------------------------------------------|
| [label] SHOWCB | EXLST=address         | Points SHOWCB to the EXLST to be queried                 |
|                | [AM=VSAM]             | Optional, no other values allowed                        |
|                | AREA=address          | Address of return area                                   |
|                | LENGTH=value          | Size of return area in bytes                             |
|                | FIELDS=(keyword list) | List of keywords indicating which fields to return       |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |

Supported options for the FIELDS parameter are listed below:

| Keyword | Length | Remarks                                                                                                                         |
|---------|--------|---------------------------------------------------------------------------------------------------------------------------------|
| ACBLEN  | 4      | Length of ACB in bytes                                                                                                          |
| EODAD   | 4      | End-of-data exit routine address                                                                                                |
| EXLLEN  | 4      | Length of EXLST in bytes                                                                                                        |
| JRNAD   | 4      | Supported here, but as it's not supported by other macros, zero is returned                                                     |
| LERAD   | 4      | Logical error analysis routine address                                                                                          |
| RPLLEN  | 4      | Length of RPL in bytes                                                                                                          |
| SYNAD   | 4      | Physical error analysis routine address                                                                                         |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                          |
|-------------|-----------------|----------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                       |
| R15=4       | Reason Code=4   | Invalid control block                                                            |
| R15=4       | Reason Code=9   | Length too small                                                                 |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created         |

## SHOWCB RPL= macro

The SHOWCB macro with RPL=address will return RPL-related fields according to the parameters specified
on the macro invocation in the order they are specified. Duplicates are permitted.

Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL=, TESTCB RPL= and/or MODCB
RPL= to inspect, test, and/or modify the RPL's content.

Direct access to subfields in the CBMR is strongly discouraged.

The SHOWCB RPL= macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  |
|----------------|-----------------------|----------------------------------------------------------|
| [label] SHOWCB | RPL=address           | Points SHOWCB to the RPL to be queried                   |
|                | [AM=VSAM]             | Optional, no other values allowed                        |
|                | AREA=address          | Address of return area                                   |
|                | LENGTH=value          | Size of return area in bytes                             |
|                | FIELDS=(keyword list) | List of keywords indicating which fields to return       |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |

Supported options for the FIELDS parameter are listed below:

| Keyword | Length | Remarks                                                                                                                         |
|---------|--------|---------------------------------------------------------------------------------------------------------------------------------|
| ACB     | 4      | Address of ACB                                                                                                                  |
| ACBLEN  | 4      | Length of ACB in bytes                                                                                                          |
| AIXPC   | 4      | Alternate index count. Derived from `PFXAIXN`                                                                                   |
| AREA    | 4      | Address of record buffer                                                                                                        |
| AREALEN | 4      | Size of record buffer in bytes                                                                                                  |
| ARG     | 4      | Address of search argument field                                                                                                |
| ECB     | 4      | Address of ECB                                                                                                                  |
| EXLLEN  | 4      | Length of EXLST in bytes                                                                                                        |
| FDBK    | 4      | Feedback code for the last request                                                                                              |
| FTNCD   | 4      | Function code                                                                                                                   |
| KEYLEN  | 4      | Length of key, for use with OPTCD=GEN                                                                                           |
| MSGAREA | 4      | Address of message area (returns foxes)                                                                                         |
| MSGLEN  | 4      | Length of message area (returns zero)                                                                                           |
| NXTRPL  | 4      | Address of next RPL                                                                                                             |
| RBA     | 4      | 4-byte RBA of last record processed (ESDS ony, otherwise zero)                                                                  |
| RECLEN  | 4      | Length of current record                                                                                                        |
| RPLLEN  | 4      | Length of RPL in bytes                                                                                                          |
| TRANSID | 4      | Transaction id (returns foxes)                                                                                                  |
| XRBA    | 8      | 8-byte RBA of last record processed (ESDS only, otherwise zero)                                                                 |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                          |
|-------------|-----------------|----------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                       |
| R15=4       | Reason Code=1   | AIXPC or RPLDACB are zero                                                        |
| R15=4       | Reason Code=4   | Invalid control block                                                            |
| R15=4       | Reason Code=9   | Length too small                                                                 |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created         |

## TESTCB Introduction

Only a single test can be specified on each TESTCB invocation.
In all cases the value or address supplied in the macro is compared to a constant or a value in a control block.
Many parameters have been added in zVSAM V2 to bring it in line with SHOWCB, these are in **bold**.

An extra column is provided in the charts below to indicate the type of condition code that could be returned,
the notation NE=LO (etc) is used to indicate that although LO may be returned the preferred test is for EQ/NE.

Where a parm may have subparameters, then all must be true for an EQ to be returned, if unsupported parms
or subparameters are specified then NE=LO is returned, there are more details against the parm itself.

It is highly recommended that a branch table is placed after the TESTCB to capture any error conditions, the
condition code is unpredictable if an error does occur.

Example:
```
         TESTCB ACB=MYACB,NCIS=20,MF=I
         B     *+4(R15)
         J     OK
         J     ERR04
         J     ERR08
OK       DS    0H
```

IBMs TESTCB is very badly syntax checked, zVSAM V2 has tightened the rules.
It's very unlikely that imported code will result in unexpected errors, the z390 team would like to know of any.

### More details against specific parms.

### TESTCB with no specified block type macro

The TESTCB without a block macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  | Conditions returned |
|----------------|-----------------------|----------------------------------------------------------|---------------------|
| [label] TESTCB | [AM=VSAM]             | Optional, no other values allowed                        |                     |
|                | [ERET=address]        | Address of error handling routine                        |                     |
|                | ACBLEN=value          | ACB length                                               | EQ LO HI            |
|                | RPLLEN=value          | RPL length                                               | EQ LO HI            |
|                | EXLLEN=value          | EXLST length                                             | EQ LO HI            |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |                     |

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                          |
|-------------|-----------------|----------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                       |
| R15=4       | Reason Code=4   | Invalid control block                                                            |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created         |

## TESTCB EXLST= macro

If mod L is specified then NE=LO is returned.

The TESTCB EXLST macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  | Conditions returned |
|----------------|-----------------------|----------------------------------------------------------|---------------------|
| [label] TESTCB | EXLST=address         | Points TESTCB to the EXLST to be tested                  |                     |
|                | [AM=VSAM]             | Optional, no other values allowed                        |                     |
|                | ERET=address          | Address of error handling routine                        |                     |
|                | ACBLEN=value          | ACB length                                               | EQ LO HI            |
|                | **RPLLEN=value**      | RPL length                                               | EQ LO HI            |
|                | **EXLLEN=value**      | EXLST length                                             | EQ LO HI            |
|                | EODAD=(address[,mod]) | End-of-data exit address                                 |                     |
|                |                       | If address is zero or omitted and no mod                 | EQ                  |
|                |                       | If address is zero and mod (only mod is tested)          | EQ NE=LO            |
|                |                       | If address is not zero and no mod                        | EQ LO HI            |
|                |                       | If address is.not zero and mod                           | EQ NE=LO            |
|                | JRNAD=                | Journal exit address (allowed but not supported)         | NE=LO               |
|                | LERAD=(address[,mod]) | Logical error analysis address                           |                     |
|                |                       | If address is zero or omitted and no mod                 | EQ                  |
|                |                       | If address is zero and mod (only mod is tested)          | EQ NE=LO            |
|                |                       | If address is not zero and no mod                        | EQ LO HI            |
|                |                       | If address is.not zero and mod                           | EQ NE=LO            |
|                | SYNAD=(address[,mod]) | Physical error analysis address                          |                     |
|                |                       | If address is zero or omitted and no mod                 | EQ                  |
|                |                       | If address is zero and mod (only mod is tested)          | EQ NE=LO            |
|                |                       | If address is not zero and no mod                        | EQ LO HI            |
|                |                       | If address is.not zero and mod                           | EQ NE=LO            |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |                     |

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                                                |
|-------------|-----------------|--------------------------------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                                             |
| R15=4       | Reason Code=4   | Invalid control block                                                                                  |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created                               |

## TESTCB RPL= macro

The TESTCB RPL macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  | Conditions returned          |
|----------------|-----------------------|----------------------------------------------------------|------------------------------|
| [label] TESTCB | RPL=address           | Points TESTCB to the RPL to be tested                    |                              |
|                | [AM=VSAM]             | Optional, no other values allowed                        |                              |
|                | ERET=address          | Address of error handling routine                        |                              |
|                | ACBLEN=value          | ACB length                                               | EQ LO HI                     |
|                | **RPLLEN=value**      | RPL length                                               | EQ LO HI                     |
|                | **EXLLEN=value**      | EXLST length                                             | EQ LO HI                     |
|                | ACB=address           | ACB address                                              | EQ LO HI                     |
|                | AIXFLAG=AIXPKP        | AIX pointer type. From `RPLAIXID`                        | EQ is RBA; NE=HI is Key      |
|                | AIXPC=value           | no. of AIXs in upgrade set. From `PFXAIXN`               | EQ LO HI                     |
|                | AREA=address          | address of record area. From `RPLAREA`                   | EQ LO HI                     |
|                | AREALEN=value         | length of record area. From `RPLAREAL`                   | EQ LO HI                     |
|                | ARG=address           | Address of ARG. From `RPLARG`                            | EQ LO HI                     |
|                | ECB=address           | Address of ECB. From `RPLECB`                            | EQ LO HI                     |
|                | FDBK=value            | Feedback code of the last request. From `RPLERRCD`       | EQ LO HI                     |
|                | FTNCD=value           | Function code. From `RPLCMPON`                           | EQ LO HI                     |
|                | IO=COMPLETE           | I/O is complete. From `RPLECB`,`RPLPOST`                 | EQ is complete; NE=LO is not |
|                | KEYLEN=value          | Length of key field                                      | EQ LO HI                     |
|                | MSGAREA=address       | Address of message area. From `RPLMSGAR`                 | EQ LO HI                     |
|                | MSGLEN=value          | Length of message area. From `RPLMSGLN`                  | EQ LO HI                     |
|                | NXTRPL=address        | Address of next RPL. From `RPLNXTRP`                     | EQ LO HI                     |
|                | OPTCD=(keyword list)  | List of keywords indicating attributes to test           |                              |
|                |                       | All subparameters have to be true for EQ. From `RPLOPTn` | EQ NE=HI                     |
|                | RBA=value             | Current RBA (last 4 bytes). From `RPLCXRBA`              | EQ LO HI                     |
|                | RECLEN=value          | Record Length. From `RPLRECLN`                           | EQ LO HI                     |
|                | TRANSID=value         | Allowed but not supported                                | EQ                           |
|                | **XRBA=value**        | Current RBA. From `RPLCXRBA`                             | EQ LO HI                     |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |                              |

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                                                           |
|-------------|-----------------|-------------------------------------------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                                                        |
| R15=4       | Reason Code=1   | This parameter requires the dataset to be open.                                                                   |
|             |                 | For fields that have 8-byte values (eg. XRBA) the 4-byte version is requested but the 1st four bytes are not zero |
| R15=4       | Reason Code=4   | Invalid control block                                                                                             |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created                                          |

## Catalog management

This is where all meta-data about the zVSAM components are kept and where the relations between zVSAM
components are defined. Catalogs are currently created as static assembled modules.
Extended catalogs contained in datasets will be considered in a future release.

The catalog will hold at least:
- file name
- pointer to index file
- pointers to all related AIX clusters
- LRECL
- record type (F, V, FS, VS)
- type of component (ESDS, KSDS, RRDS, AIX)
- freeblocks (during load, between blocks)
- freespace (during load, within blocks)
- Physical Block size (aka CI-size, 512 bytes to 16MB)

For a complete list of catalog components please see the
z390_zVSAM_Catalog_User_Guide.

