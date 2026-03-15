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

A CBMR is generated for all forms of these macros.

Direct access to subfields in the CBMR is discouraged. Use SHOWCB, TESTCB and/or MODCB to inspect,
test, and/or modify the content of an ACB, EXLST, or RPL. Use the appropriate MF= parameter on any of
these macros to modify and/or use a CBMR.

The CBMR consists of three parts: a header, a body, and a tail. The header has a fixed layout. The body
consists of request-dependent fields and a list of verb codes. The tail contains all the data fields that go with
the verb codes. Data fields can be 0, 4 or 8 bytes in length.

Verb codes X'01'-X'DF' have a 4-byte data field
Verb codes X'E0'-X'FF' have an 8-byte data field

All data fields in the tail are allocated consecutively, in the same order as the verbs that define their meaning

#### CBMR – header

The CBMR header identifies the type (ACB, EXLST, RPL, GENCB, MODCB, SHOWCB or TESTCB)

It also has details of any work area needed and a count of verbs in CBMRVRBS

#### CBMR – body

Its length is determined by the CBMRVRBS fields in the CBMR header.
It contains one verb code for each specified parameter.

#### CBMR – tail

The body is directly followed by the tail.
It contains a data field of 4 or 8 bytes for each verb coded in the body, in the same sequence.

The starting point of the tail can be found by adding the CBMRVRBS value to the end of the CBMR header.
Its length can be calculated from the CBMRSIZE field, by subtracting both the header length and the CBMRVRBS field.

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

## GENCB BLK=ACB macro

This GENCB macro will generate ACBs and initialize or change them according to the parameters specified
on the macro invocation. It is for this reason that all supported parameters and keywords of the ACB macro
(as described above) are supported on the GENCB macro.

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
MODCB ACB= to inspect, test, and/or modify the ACB's content.

Direct access to subfields in the CBMR is strongly discouraged.

The GENCB ACB macro can be coded as follows:

| Opcode        | Operand           | Remarks                                                                                                                         |
|---------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------|
| [label] GENCB | BLK=ACB           | Instructs GENCB to generate 1 or more ACBs                                                                                      |
|               | [AM=VSAM]         | Optional, no other values allowed                                                                                               |
|               | [COPIES=1]        | The number of identical ACBs to generate. Specify a number between 1 and 65535                                                  |
|               | [WAREA=address]   | The work area where the ACBs are to be constructed                                                                              |
|               | [LENGTH=value]    | Length of the work area in bytes. If WAREA/LENGTH are omitted then storage is dynamically acquired and LOC=BELOW is the default |
|               | [LOC=BELOW | ANY] | Where GENCB is to allocate dynamically acquired storage if needed                                                               |
|               | **[other]**       | **Any parameter supported on the ACB macro**                                                                                    |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### WAREA=

- When WAREA is specified, LENGTH must be specified too.
- When WAREA is not specified, the CBMR handler allocates an area of storage
- The address of this area whether via GETMAIN or WAREA is returned in R1
- The length of the generated ACB(s) is returned in R0

### LENGTH=

Length in bytes of the area indicated by WAREA.
When LENGTH is specified, WAREA must be specified as well.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=9   | WAREA is too small                                                       |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

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

## MODCB ACB= macro

The MODCB macro with ACB=address will modify an ACB according to the parameters specified on the
macro invocation. It is for this reason that all parameters and keywords of the ACB macro (as described
above) are supported on the MODCB macro when ACB=address is specified.

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
MODCB ACB= to inspect, test, and/or modify the ACB's content.

Direct access to subfields in the CBMR is strongly discouraged.

The MODCB ACB macro can be coded as follows:

| Opcode        | Operand           | Remarks                                                                                                                         |
|---------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------|
| [label] MODCB | ACB=address       | Points MODCB to the ACB to be modified                                                                                          |
|               | [AM=VSAM]         | Optional, no other values allowed                                                                                               |
|               | **[other]**       | **Any parameter supported on the ACB macro**                                                                                    |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |

All supported parameters are implemented compatibly with IBM's VSAM implementation. For details,
please refer to the relevant IBM manual.

### MACRF=

To clarify how MACRF works...

All supported subparameters have their own bit in `CBMRACB_MACRF` (currently 16),
Conflicts are `MNOTE`d, eg. bits for NIS and SIS cannot both be on.

If MF=E is specified then the whole of `CBMRACB_MACRF` is replaced

When the ACB is modified:
- For mutually exclusive parameters, the bit is turned on or off
- For each non-exclusive parameter the appropriate bit is turned on, therefore it isn't possible to turn a nonexclusive
  bit off using MODCB, this has to be done manually.
- eg. When an ACB has MACRF=(OUT) which allows read and write functions it is not possible to change
  the ACB to read-only using MODCB
- if this is needed code the instruction `NI ACBMACR1,255-ACBOUT`

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=12  | MODCB was attempted on an open ACB                                       |
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

## SHOWCB ACB= macro

The SHOWCB macro with ACB=address will return ACB-related fields according to the parameters
specified on the macro invocation in the order they are specified. Duplicates are permitted.

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB ACB= and/or
MODCB ACB= to inspect, test, and/or modify the ACB's content.

Direct access to subfields in the CBMR is strongly discouraged.

The SHOWCB ACB macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  |
|----------------|-----------------------|----------------------------------------------------------|
| [label] SHOWCB | ACB=address           | Points SHOWCB to the ACB to be queried                   |
|                | [AM=VSAM]             | Optional, no other values allowed                        |
|                | AREA=address          | Address of return area                                   |
|                | LENGTH=value          | Size of return area in bytes                             |
|                | [OBJECT=DATA/INDEX]   | For KSDS: select data or index component                 |
|                | FIELDS=(keyword list) | List of keywords indicating which fields to return       |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |

Supported options for the FIELDS parameter are listed below:

| Keyword  | Length | Remarks                                                                                                                         |
|----------|--------|---------------------------------------------------------------------------------------------------------------------------------|
| ACBLEN   | 4      | Length of ACB in bytes                                                                                                          |
| AVSPAC   | 4      | Available space in data/index (last 4 bytes). Derived from `CTRAVSPAC`                                                          |
| BFRFND   | 4      | value of buffer hits for data/index including LSR (last 4 bytes) Derived from `CTRNBFRFND`                                      |
| BSTRNO   | 4      | Initial value of strings for a path. Derived from `ACBBSTNO`                                                                    |
| BUFND    | 4      | value of data buffers specified in ACB. Derived from `ACBBUFND`                                                                 |
| BUFNI    | 4      | value of index buffers specified in ACB. Derived from `ACBBUFNI`                                                                |
| BUFNO    | 4      | Number of data/index buffers allocated (last 4 bytes) Derived from `CTRNBUFNO`                                                  |
| BUFNOL   | 4      | Number of data/index buffers allocated for LSR processing (returns zero)                                                        |
| BUFRDS   | 4      | Number of data/index buffer reads (last 4 bytes). Derived from `CTRNBUFRDS`                                                     |
| BUFSP    | 4      | Buffer space in bytes specified in ACB. Derived from `ACBBUFSP`                                                                 |
| BUFUSE   | 4      | Number of data/index buffers actually in use (last 4 bytes) Derived from `CTRNBUFUSE`                                           |
| CDTASIZE | 8      | Size of a compressed dataset (returns zero)                                                                                     |
| CINV     | 4      | Block size for data/index. Derived from `PFXBLKSZ`                                                                              |
| CIPCA    | 4      | CI's in CA (returns zero)                                                                                                       |
| DDNAME   | 8      | DDNAME specified in ACB. Derived from `ACBDDNM`                                                                                 |
| ENDRBA   | 4      | Highest used RBA (last 4 bytes). Derived from `CTRENDRBA`                                                                       |
| ERROR    | 4      | Return code from last open/close operation. Derived from `ACBERFLG`                                                             |
| EXLLEN   | 4      | Length of EXLST in bytes                                                                                                        |
| EXLST    | 4      | address of EXLST, zero if none. Derived from `ACBEXLST`                                                                         |
| FS       | 4      | returns zero                                                                                                                    |
| HALCRBA  | 4      | Highest allocated data/index RBA (last 4 bytes). Derived from `CTRHALCRBA`                                                      |
| HLRBA    | 4      | For OBJECT=INDEX only, highest index block RBA. Derived from `CTRHLRBA`                                                         |
| KEYLEN   | 4      | Length of key field. Derived from `PFXKYLEN`                                                                                    |
| LEVEL    | 8      | Address (4 bytes) and length (4 bytes) of field containing zVSAM version. Derived from `ACBVER`                                 |
| LOKEY    | 8      | Address (4 bytes) of lowest key in the cluster + length (4 bytes) of key. Derived from `CTRLOKEY@` and `PFXKYLEN`               |
|          |        | Some requests may result in an error, see the list below.                                                                       |
| LRECL    | 4      | Maximum data/index record length. Derived from `PFXRCLEN`                                                                       |
| MAREA    | 4      | Message area (returns foxes)                                                                                                    |
| MLEN     | 4      | Message length (returns zero)                                                                                                   |
| NCIS     | 4      | value of Block splits in the data component (last 4 bytes) Zero for OBJECT=INDEX. Derived from `CTRNCIS`                        |
| NDELR    | 4      | value of deleted records from the data component (last 4 bytes) Zero for OBJECT=INDEX. Derived from `CTRNDELR`                  |
| NEXCP    | 4      | value of I/O requests for the data/index components (last 4 bytes) Derived from `CTRNEXCP`                                      |
| NEXT     | 4      | value of extents of the data/index components (returns 1)                                                                       |
| NINSR    | 4      | value of records inserted for the data component (last 4 bytes) Zero for OBJECT=INDEX. Derived from `CTRNINSR`                  |
| NIXL     | 4      | value of index levels for index component. Zero for OBJECT=DATA. Derived from highest non-foxes `PFXBLVLn`                      |
| NLOGR    | 4      | value of records in the data/index (last 4 bytes). Derived from `CTRNLOGR`                                                      |
| NRETR    | 4      | value of records retrieved from the data component (last 4 bytes). Zero for OBJECT=INDEX. Derived from `CTRNRETR`               |
| NSSS     | 4      | value of control area splits for the data/index (returns zero)                                                                  |
| NUIW     | 4      | value of non-user writes (last 4 bytes). Derived from `CTRNNUIW`                                                                |
| NUPDR    | 4      | value of updated records in the data/index components (last 4 bytes). Derived from `CTRNUPDR`                                   |
| PASSWD   | 4      | address to password, consisting of length (1 byte, binary) followed by the actual password value. Derived from `ACBPASSW`       |
| RELEASE  | 8      | Address (4 bytes) and length (4 bytes) of field containing zVSAM version. Derived from `ACBVER`. Same as LEVEL                  |
| RKP      | 4      | Relative Key Position, offset of key within logical record. Derived from `PFXKYOFF`                                             |
| RMODE31  | 4      | 0=None, 1=Buff, 2=CB, 3=All. Derived from `ACBOFLGS`                                                                            |
| RPLLEN   | 4      | Length of RPL in bytes                                                                                                          |
| SDTASIZE | 8      | Data size. Derived from `CTRSDTASZ`                                                                                             |
| SHRPOOL  | 4      | SHRPOOL number. Derived from `ACBSHRP`                                                                                          |
| STMST    | 8      | STCK of last close. Derived from `CTRSTMST`                                                                                     |
| STRMAX   | 4      | Max value of concurrently active strings (last 4 bytes). Derived from `CTRSTRMAX`                                               |
| STRNO    | 4      | Max value of allocated strings. Derived from `ACBSTRNO`                                                                         |
| UIW      | 4      | value of user writes for data/index (last 4 bytes). Derived from `CTRNUIW`                                                      |
| XAVSPAC  | 8      | AVSPAC when value may exceed 4GB                                                                                                |
| XBFRFND  | 8      | BFRFND when value may exceed 4GB                                                                                                |
| XBUFNO   | 8      | BUFNO when value may exceed 4GB                                                                                                 |
| XBUFRDS  | 8      | BUFRDS when value may exceed 4GB                                                                                                |
| XBUFUSE  | 8      | BUFUSE when value may exceed 4GB                                                                                                |
| XENDRBA  | 8      | ENDRBA when value may exceed 4GB                                                                                                |
| XHALCRBA | 8      | HALCRBA when value may exceed 4GB                                                                                               |
| XHLRBA   | 8      | HLRBA when value may exceed 4GB                                                                                                 |
| XNCIS    | 8      | NCIS when value may exceed 4GB                                                                                                  |
| XNDELR   | 8      | NDELR when value may exceed 4GB                                                                                                 |
| XNEXCP   | 8      | NEXCP when value may exceed 4GB                                                                                                 |
| XNINSR   | 8      | NINSR when value may exceed 4GB                                                                                                 |
| XNLOGR   | 8      | NLOGR when value may exceed 4GB                                                                                                 |
| XNRETR   | 8      | NRETR when value may exceed 4GB                                                                                                 |
| XNUIW    | 8      | NNUIW when value may exceed 4GB                                                                                                 |
| XNUPDR   | 8      | NUPDR when value may exceed 4GB                                                                                                 |
| XSTRMAX  | 8      | STRMAX when value may exceed 4GB                                                                                                |
| XUIW     | 8      | UIW when value may exceed 4GB                                                                                                   |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.

### Return (R15) and Reason (R0) Codes

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

## TESTCB ACB= macro

The TESTCB ACB macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                                            | Conditions returned |
|----------------|-----------------------|------------------------------------------------------------------------------------|---------------------|
| [label] TESTCB | ACB=address           | Points TESTCB to the ACB to be tested                                              |                     |
|                | [AM=VSAM]             | Optional, no other values allowed                                                  |                     |
|                | [ERET=address]        | Address of error handling routine                                                  |                     |
|                | [OBJECT=DATA/INDEX]   | Select data or index component                                                     |                     |
|                | ACBLEN=value          | ACB length                                                                         | EQ LO HI            |
|                | **RPLLEN=value**      | RPL length                                                                         | EQ LO HI            |
|                | **EXLLEN=value**      | EXLST length                                                                       | EQ LO HI            |
|                | ATRB=(keyword list)   | List of keywords indicating attributes to test                                     |                     |
|                |                       | All subparameters have to be true for EQ. See [Note 1](#Note1)                     | EQ NE=HI            |
|                | AVSPAC=value          | Available space in data/index (last 4 bytes) From `CTRAVSPAC`                      | EQ LO HI            |
|                | **BFRFND=value**      | Buffer hits for data/index including LSR (last 4 bytes). From `CTRNBFRFND`         | EQ LO HI            |
|                | BSTRNO=value          | Initial value of strings for a path From `ACBBSTNO`                                | EQ LO HI            |
|                | BUFND=value           | value of data buffers. From `ACBBUFND`                                             | EQ LO HI            |
|                | BUFNI=value           | value of index buffers. From `ACBBUFNI`                                            | EQ LO HI            |
|                | BUFNO=value           | value of I/O Buffers (last 4 bytes) From `CTRNBUFNO`                               | EQ LO HI            |
|                | **BUFRDS=value**      | Number of data/index buffer reads (last 4 bytes). From `CTRNBUFRDS`                | EQ LO HI            |
|                | BUFSP=value           | Buffer space in bytes. From `ACBBUFSP`                                             | EQ LO HI            |
|                | **BUFUSE=value**      | Number of data/index buffers actually in use (last 4 bytes). From `CTRNBUFUSE`     | EQ LO HI            |
|                | CINV=value            | Block size in bytes. From `PFXBLKSZ`                                               | EQ LO HI            |
|                | DDNAME=string         | DDNAME. From `ACBDDNM`                                                             | EQ LO HI            |
|                | ENDRBA=value          | Highest used RBA (last 4 bytes). From `CTRENDRBA`                                  | EQ LO HI            |
|                | ERROR=value           | Return code from last open/close operation From `ACBERFLG`                         | EQ LO HI            |
|                | EXLST=address         | EXLST address. From `ACBEXLST`                                                     | EQ LO HI            |
|                | FS=value              | Compares to zero                                                                   | EQ LO HI            |
|                | **HALCRBA=value**     | Highest allocated data/index RBA (last 4 bytes). From `CTRHALCRBA`                 | EQ LO HI            |
|                | **HLRBA=value**       | For OBJECT=INDEX only, highest index block RBA. From `CTRHLRBA`                    | EQ LO HI            |
|                | KEYLEN=value          | Length of key field. From `PFXKYLEN`                                               | EQ LO HI            |
|                | LRECL=value           | Logical Record Length. From `PFXRCLEN`                                             | EQ LO HI            |
|                | MACRF=(keyword list)  | List of keywords for processing options.                                           |                     |
|                |                       | All subparameters have to be true for EQ. From `ACBMACRn`. See [Note 2](#Note2)    | EQ NE=HI            |
|                | NCIS=value            | value of block splits (last 4 bytes)                                               |                     |
|                |                       | Compares to zero for OBJECT=INDEX From `CTRNCIS`                                   | EQ LO HI            |
|                | NDELR=value           | value of deleted records (last 4 bytes)                                            |                     |
|                |                       | Compares to zero for OBJECT=INDEX From `CTRNDELR`                                  | EQ LO HI            |
|                | NEXCP=value           | value of I/O requests (last 4 bytes) From `CTRNEXCP`                               | EQ LO HI            |
|                | NEXT=value            | value of extents (last 4 bytes) From `CTRNEXT` (always 1)                          | EQ LO HI            |
|                | NINSR=value           | value of inserted records (last 4 bytes)                                           |                     |
|                |                       | Compares to zero for OBJECT=INDEX From `CTRNINSR`                                  | EQ LO HI            |
|                | NIXL=value            | value of index levels Compares to zero for OBJECT=DATA From `PFXBLVLn`             | EQ LO HI            |
|                | NLOGR=value           | value of records (last 4 bytes) From `CTRNLOGR`                                    | EQ LO HI            |
|                | NRETR=value           | value of records retrieved (last 4 bytes)                                          |                     |
|                |                       | Compares to zero for OBJECT=INDEX From `CTRNRETR`                                  | EQ LO HI            |
|                | NSSS=value            | Compares to zero                                                                   | EQ LO HI            |
|                | **NUIW=value**        | value of non-user writes (last 4 bytes). From `CTRNNUIW`                           | EQ LO HI            |
|                | NUPDR=value           | value of updated records (last 4 bytes) From `CTRNUPDR`                            | EQ LO HI            |
|                | OFLAGS=OPEN           | Successful OPEN. From `ACBOFLGS`                                                   | EQ NE=HI            |
|                | OPENOBJ=PATH|BASE|AIX | ACB represents Path, Base or AIX From `ACBDTYPE`                                   | EQ NE=HI            |
|                | PASSWD=address        | address of 1-byte length, password From `ACBPASSW`                                 | EQ LO HI            |
|                | RKP=value             | offset of key field within record From `PFXKYOFF`                                  | EQ LO HI            |
|                | SHRPOOL=value         | SHRPOOL number. From `ACBSHRP`                                                     | EQ LO HI            |
|                | **SDTASZ=address**    | Data size. From `CTRSDTASZ`                                                        | EQ LO HI            |
|                | STMST=address         | Address of system timestamp field From `CTRSTMST`                                  | EQ LO HI            |
|                | **STRMAX=value**      | Max. value of concurrently active strings (last 4 bytes). Derived from `CTRSTRMAX` | EQ LO HI            |
|                | STRNO=value           | Max. value of parallel requests From `ACBSTRNO`                                    | EQ LO HI            |
|                | **UIW=value**         | value of user writes (last 4 bytes). From `CTRNUIW`                                | EQ LO HI            |
|                | **XAVSPAC=value**     | AVSPAC when value may exceed 4GB                                                   | EQ LO HI            |
|                | **XBFRFND=value**     | BFRFND when value may exceed 4GB                                                   | EQ LO HI            |
|                | **XBUFNO=value**      | BUFNO when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XBUFRDS=value**     | BUFRDS when value may exceed 4GB                                                   | EQ LO HI            |
|                | **XBUFUSE=value**     | BUFUSE when value may exceed 4GB                                                   | EQ LO HI            |
|                | **XENDRBA=value**     | ENDRBA when value may exceed 4GB                                                   | EQ LO HI            |
|                | **XHALCRBA=value**    | HALCRBA when value may exceed 4GB                                                  | EQ LO HI            |
|                | **XHLRBA=value**      | HLRBA when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNCIS=value**       | NCIS when value may exceed 4GB                                                     | EQ LO HI            |
|                | **XNDELR=value**      | NDELR when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNEXCP=value**      | NEXCP when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNEXT=value**       | NEXT when value may exceed 4GB                                                     | EQ LO HI            |
|                | **XNINSR=value**      | NINSR when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNLOGR=value**      | NLOGR when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNRETR=value**      | NRETR when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNUIW=value**       | NNUIW when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XNUPDR=value**      | NUPDR when value may exceed 4GB                                                    | EQ LO HI            |
|                | **XSTRMAX=value**     | STRMAX when value may exceed 4GB                                                   | EQ LO HI            |
|                | **XUIW=value**        | UIW when value may exceed 4GB                                                      | EQ LO HI            |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                                           |                     |

**Note 1:** <a id="Note1" />
The syntax rules for ATRB have been tightened in zVSAM V2:
- ESDS, KSDS, LDS, RRDS and VRRDS are considered mutually exclusive
- `PFXFFLGS` is tested.
- For `SPAN` or `UNQ`, `PFXRFLGS` is tested.
- If `COMPRESS`, `REPL`, `SSWD` or `WCK` are included then NE=HI is returned.

**Note 2:** <a id="Note2" />
If any of the following keywords are used then NE=HI will be returned:
- `CNV`, `CFX`, `NFX`, `DDN`, `DSN`, `LEW`, `NLW`, `NRS`, `RST`, `RLS`, `NUB`, `UBF`, `NCI`, `ICI`.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                                                |
|-------------|-----------------|--------------------------------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                                             |
| R15=4       | Reason Code=1   | This parameter requires the dataset to be open.                                                        |
|             |                 | (X)HLRBA requested and OBJECT=DATA                                                                     |
|             |                 | For fields that have 8-byte values the 4-byte version is requested but the 1st four bytes are not zero |
| R15=4       | Reason Code=4   | Invalid control block                                                                                  |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created                               |

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

