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

### GENCB BLK=ACB macro

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
|               | [other]           | Any parameter supported on the ACB macro                                                                                        |
|               | [MF=]             | See the [description of MF=](#MFdetails)                                                                                        |













