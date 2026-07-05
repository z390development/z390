# zVSAM V2 - Addenda

This document describes Macro parameters and control blocks used in the zVSAM V2 implementation.

## zACB description

The zACB is the internal structure (control block, object) that constitutes the ACB.

The structure and layout of the zACB are not formally part of the interface and may change in future releases.
Therefore the zACB layout for zVSAM V1 is not included and the zACB layout for zVSAM V2 is shown here only for the sake of completeness.
Direct access to subfields in the zACB is discouraged. Use SHOWCB ACB, TESTCB ACB and/or MODCB ACB to inspect, test, and/or modify the zACB's content.
Accessing subfields of the zACB directly may adversely impact portability of your programs.

| Label    | Equate       | Designation | Remarks                                                         |
|----------|--------------|-------------|-----------------------------------------------------------------|
| IHAACB   |              | DSECT       |                                                                 |
| IFGACB   |              | DSECT       | Synonym of IFGACB                                               |
| ACBEYE   |              | CL4         | Eye catcher                                                     |
|          | ACBZACB      | =C'zACB'    | Fixed value                                                     |
| ACBID    |              | XL1         | Identifier                                                      |
|          | ACBIDVAL     | =X'A0'      | ACB                                                             |
| ACBSTYP  |              | XL1         | Subtype                                                         |
| ACBSTYPE |              | XL1         | Synonym of ACBSTYP                                              |
| ACBTYPE  |              | XL1         | Synonym of ACBSTYP                                              |
|          | ACBVSAM      | =X'10'      | VSAM ACB                                                        |
| ACBLENG  |              | H           | Length of this ACB in bytes                                     |
| ACBLENG2 |              |             | Synonym of ACBLENG                                              |
| ACBLEN   |              |             | Synonym of ACBLENG                                              |
| ACBLEN2  |              |             | Synonym of ACBLENG                                              |
| ACBDDNM  |              | CL8         | Name of host variable holding cluster name                      |
| ACBDDNAM |              |             | Synonym of ACBDDNM                                              |
| ACBMACRF |              |             | ACBMACR1 + ACBMACR2                                             |
| ACBMACR1 |              | BL1         | Option bits                                                     |
|          | ACBKEY       | =X'80'      | Indexed access by logical key                                   |
|          | ACBMACR1_KEY |             | Synonym of ACBKEY                                               |
|          | ACBADDR      | =X'40'      | Non-indexed access by address                                   |
|          | ACBADD       |             | Synonym of ACBADDR                                              |
|          | ACBMACR1_ADR |             | Synonym of ACBADDR                                              |
|          | ACBCNV       | =X'20'      | Reserved                                                        |
|          | ACBMACR1_CNV |             | Synonym of ACBCNV                                               |
|          | ACBSEQ       | =X'10'      | Sequential access                                               |
|          | ACBMACR1_SEQ |             | Synonym of ACBSEQ                                               |
|          | ACBDIR       | =X'08'      | Direct access                                                   |
|          | ACBMACR1_DIR |             | Synonym of ACBDIR                                               |
|          | ACBIN        | =X'04'      | Get allowed                                                     |
|          | ACBMACR1_IN  |             | Synonym of ACBIN                                                |
|          | ACBGET       |             | Synonym of ACBIN                                                |
|          | ACNOUT       | =X'02'      | Get / Put / Erase allowed                                       |
|          | ACBMACR1_OUT |             | Synonym of ACBOUT                                               |
|          | ACBPUT       |             | Synonym of ACBOUT                                               |
|          | ACBUBF       | =X'01'      | Reserved                                                        |
|          | ACBMACR1_UBF |             | Synonym of ACBUBF                                               |
| ACBMACR2 |              | BL1         | More option bits                                                |
|          | ACBSKP       | =X'10'      | Skip-sequential access                                          |
|          | ACBMACR2_SKP |             | Synonym of ACBSKP                                               |
|          | ACBRST       | =X'04'      | Dataset Reset opn open                                          |
|          | ACBMACR2_RST |             | Synonym of ACBRST                                               |
|          | ACBAIX       | =X'01'      | Access through AIX                                              |
|          | ACBMACR2_AIX |             | Synonym of ACBAIX                                               |
|          | ACBAIXP      |             | Synonym of ACBAIX                                               |
| ACBMACR3 |              | BL1         | Additional option bits                                          |
|          | ACBNLW       | =X'80'      | Reserved                                                        |
|          | ACBLSR       | =X'40'      | Reserved                                                        |
|          | ACBMACR3_LSR |             | Synonym of ACBLSR                                               |
|          | ACBGSR       | =X'20'      | Reserved                                                        |
|          | ACBMACR3_GSR |             | Synonym of ACBGSR                                               |
|          | ACBDFR       | =X'08'      | Deferred writes allowed                                         |
|          | ACBMACR3_DFR |             | Synonym of ACBDFR                                               |
|          | ACBSIS       | =X'04'      | Sequential insert strategy                                      |
|          | ACBMACR3_SIS |             | Synonym of ACBSIS                                               |
|          | ACBMODE      | =X'01'      | Buffers allowed above the line                                  |
| ACBMACR4 |              | BL1         | Reserved                                                        |
| ACBBUFND |              | XL2         | Nr of data buffers                                              |
| ACBBUFNI |              | XL2         | Nr of index buffers                                             |
| ACBBUFSP |              | F           | Max buffer space in bytes                                       |
| ACBLRECL |              | XL2         | Record length                                                   |
| ACBPASSW |              | A           | Password pointer                                                |
| ACBEXLST |              | A           | EXLST ptr                                                       |
| ACBUEL   |              |             | Synonym of ACBEXLST                                             |
| ACBINFLG |              |             | ACBINFL1 + ACBINFL2                                             |
| ACBINFL  |              |             | Synonym of ACBINFLG                                             |
| ACBINFL1 |              | BL1         | Indicator flags                                                 |
|          | ACBCAT       | =X'10'      | Reserved                                                        |
| ACBINFL2 |              | BL1         | More indicator flags                                            |
|          | ACBSWARN     | =X'80'      | Suppress open warning                                           |
|          | ACBSHROP     | =X'03'      | Reserved                                                        |
|          | ACBSHR02     | =X'02'      | Reserved                                                        |
|          | ACBSHR01     | =X'01'      | Reserved                                                        |
| ACBOFLGS |              | BL1         | Open / Close flags                                              |
|          | ACBR31B      | =X'80'      | 31-bit addressing for buffers                                   |
|          | ACBR31C      | =X'40'      | 31-bit addressing for control blocks                            |
|          | ACBEOV       | =X'20'      | Reserved                                                        |
|          | ACBOPEN      | =X'10'      | ACB currently open                                              |
|          | ACBDSERR     | =X'08'      | Error – ACB must be closed                                      |
|          | ACBRECOV     | =X'04'      | Open for recovery                                               |
|          | ACBEXFG      | =X'02'      | Off when user exit in progress                                  |
|          | ACBLOCK      |             | Synonym of ACBEXFG                                              |
|          | ACBIOSFG     | =X'01'      | Open / close in progress                                        |
|          | ACBBUSY      |             | Synonym of ACBIOSFG                                             |
| ACBERFLG |              | BL1         | Error flags                                                     |
|          | ACBOALR      | =X'04'      | Already opened / closed                                         |
|          | ACBCALR      |             | Synonym of ACBOALR                                              |
| ACBBSTNO |              | XL1         | Reserved                                                        |
| ACBSTRNO |              | XL1         | Reserved                                                        |
| ACBSHRP  |              | XL1         | Reserved                                                        |
| ACBVER   |              | XL1         | zACB layout version                                             |
|          | ACBV2        | =X'02'      | zACB V2 version                                                 |
|----------|--------------|-------------|-----------------------------------------------------------------|
| ACBPFX   |              | AL4         | Prefix Block in buffer                                          |
| ACBXPFX  |              | AL4         | Prefix Block of index; zeroes if no index open                  |
| ACBBUFD  |              | AL4         | Address of data buffers                                         |
| ACBBUFI  |              | AL4         | Address of index buffers                                        |
| ACBDTYPE |              | X           | Type of dataset opened                                          |
|          | ACB_PATH     | EQU   X'80' | Path                                                            |
|          | ACB_AIX      | EQU   X'20' | Aix                                                             |
|          | ACB_BASE     | EQU   X'10' | Base                                                            |
|----------|--------------|-------------|-----------------------------------------------------------------|
|          |              |             | used for alignment                                              |
|          | ACBEND       | EQU   \*    |                                                                 |
|          | ACB_LEN      | EQU         | ACBEND-IHAACB                                                   |

> [!NOTE]
> Fields ACBPFX through ACBDTYPE are dataset-level fields and do not belong in the zACB,
> which is intended for cluster-level information. These fields need to be moved to another structure.

### CBMR description

The structure and layout of the CBMR are not formally part of the interface and may change in future releases.
Therefore the CBMR layout is shown here only for the sake of completeness. Direct access to subfields in the CBMR is discouraged.
Use SHOWCB, TESTCB and/or MODCB to inspect, test, and/or modify the content of an ACB, EXLST, or RPL.
Use the appropriate MF= parameter on any of these macros to modify and/or use a CBMR.

The CBMR consists of three parts: a header, a body, and a tail. The header has a fixed layout.
The body consists of request-dependent fields and a list of verb codes.
The tail contains all the data fields that go with the verb codes. Data fields can be 0, 4, or 8 bytes in length.

- Verb codes X'00'-X'5F' have a zero-length data field (i.e. no data field).
- Verb codes '60'-X'DF' have a 4-byte data field.
- Verb codes X'E0'-X'FF' have an 8-byte data field.
- For valid verb values and their data field lengths, please refer to the relevant CBMR body chapter.

All data fields in the tail are allocated consecutively, in the same order as the verbs that define their meaning.

Direct access to subfields in the CBMR is discouraged. Use `SHOWCB`, `TESTCB` and/or `MODCB` to inspect,
test, and/or modify the content of an `ACB`, `EXLST`, or `RPL`. Use the appropriate MF= parameter on any of
these macros to modify and/or use a CBMR.

### CBMR – header

The CBMR header identifies the type: `ACB`, `EXLST`, or `RPL` and `GENCB`, `MODCB`, `SHOWCB` or `TESTCB`.

It also has details of any work area(s) needed and a count of verbs in `CBMRVRBS`.

| Label    | Equate       | Designation | Remarks                                                         |
|----------|--------------|-------------|-----------------------------------------------------------------|
| CBMR     |              | DSECT       |                                                                 |
| CBMREYE  |              | CL4         | Eye catcher                                                     |
|          | CBMRCBMR     | =C'CBMR'    | Fixed value                                                     |
| CBMRREQ  |              | XL1         | Request type                                                    |
|          | CBMRTEST     | =X'80'      | TESTCB request                                                  |
|          | CBMRSHOW     | =X'40'      | SHOWCB request                                                  |
|          | CBMRMOD      | =X'20'      | MODCB request                                                   |
|          | CBMRGEN      | =X'10'      | GENCB request                                                   |
|          | -            | =X'08'      | Reserved                                                        |
|          | CBMRRPL      | =X'04'      | RPL request                                                     |
|          | CBMRXLST     | =X'02'      | EXLST request                                                   |
|          | CBMRACB      | =X'01'      | ACB request                                                     |
| CBMRRMOD |              | XL1         | Request modifier                                                |
|          | -            | =X'C0'      | Reserved                                                        |
|          | CBMROBJI     | =X'20'      | Object=Index                                                    |
|          | CBMROBJD     | =X'10'      | Object=Data                                                     |
|          | -            | =X'0C'      | Reserved                                                        |
|          | CBMRLOCA     | =X'02'      | Work area below 2G                                              |
|          | CBMRLOCB     | =X'01'      | Work area below 16M                                             |
| CBMRVRBS |              | AL1         | Nr of verbs in body                                             |
| –        |              | XL1         | Reserved                                                        |
| CBMRWORK |              | AL4         | Workarea pointer                                                |
| CBMRWLEN |              | AL2         | Workarea length                                                 |
| CBMRSIZE |              | AL2         | CBMR size                                                       |
| CBMRBODY |              | Depends     | List of verb codes                                              |
| CBMRTAIL |              | Depends     | Data fields                                                     |

Remarks:
- `CBMRLOCA` `CBMRLOCB`: when both bits are off the CBMR handler will not allocate a work area, rather it will use `CBMRWORK`/`CBMRWLEN` instead.
- `CBMRVRBS`: The number of verbs in the body of the CBMR. This number is always a multiple of 4.
- `CBMRSIZE`: length in bytes of CBMR header and tail combined. Tail follows header directly

### CBMR – body

The length of the CBMR body is determined by the `CBMRVRBS` field in the CBMR header.
It contains one verb code for each specified parameter.

There is no mapping - the body is always allocated in fullwords.
Unused position are marked with a no-op request verb; they must be set to `CBMRNB_NULL` (zero).

Applicable verb codes are defined below.

### CBMR – tail

The CBMR tail directly follows the CBMR body.
It contains a data field of 0, 4 or 8 bytes for each verb coded in the body, in the same sequence.

The starting point of the tail can be found by adding the `CBMRVRBS` value to the start of the CBMR body at `CBMRBODY`.
Its length can be calculated from the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

There is no mapping for the tail. Invoking programs must calculate the address of each field to access its contents.

### CBMR - body and tail for ACB

The CBMR body for ACB is applicable only when the CBMR header's `CBMRACB` bit is on.
Its length is determined by the `CBMRVRBS` fields in the CBMR header. This is always a multiple of 4
to allow word alignment of the tail area that directly follows the body section.
Any unused/unneeded verb bytes should be set to zero, indicating a no-operation.

The tail follows the body directly, but is absent if it has a zero length.
It contains a data field of 0, 4, or 8 bytes for each verb code in the body, in the same sequence.
The starting point of the tail can be found by adding the `CBMRVRBS` value to the start of the CBMR body at `CBMRBODY`.
Its length can be deduced form the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

**!! Review Note: !!** The below table significantly differs from what Melvyn implemented. This is to be revisited
after we define the interfaces for GENCB/MODCB/SHOWCB/TESTCB for the ACB.

| Label         | Equate           | Designation | Remarks                                                         |
|---------------|------------------|-------------|-----------------------------------------------------------------|
| CBMR          |                  | DSECT       |                                                                 |
| CBMRBODY      |                  |             |                                                                 |
| CBMRACB_VERBS |                  | XL1         | Verb code, repeats                                              |
|               | CBMRACB_NOOP     | =X'00'      | Filler, no-operation                                            |
|               | CBMRACB_ADR      | =X'01'      | RBA-addressed                                                   |
|               | CBMRACB_KEY      | =X'02'      | Key addressed                                                   |
|               | CBMRACB_CNV      | =X'03'      | Control Interval                                                |
|               | CBMRACB_DFR      | =X'04'      | Deferred writes                                                 |
|               | CBMRACB_NDF      | =X'05'      | Immediate writes                                                |
|               | CBMRACB_DIR      | =X'06'      | Direct                                                          |
|               | CBMRACB_SEQ      | =X'07'      | Sequential                                                      |
|               | CBMRACB_SKP      | =X'08'      | Skip-sequential                                                 |
|               | CBMRACB_IN       | =X'09'      | Input                                                           |
|               | CBMRACB_OUT      | =X'0A'      | Output                                                          |
|               | CBMRACB_NIS      | =X'0B'      | Normal inserts                                                  |
|               | CBMRACB_SIS      | =X'0C'      | Sequential inserts                                              |
|               | CBMRACB_NRM      | =X'0D'      | Normal, non-path                                                |
|               | CBMRACB_AIX      | =X'0E'      | Path                                                            |
|               | CBMRACB_NRS      | =X'0F'      | Not reusable                                                    |
|               | CBMRACB_RST      | =X'10'      | Reset on open                                                   |
|               | CBMRACB_NSR      | =X'11'      | Nonshared resources                                             |
|               | CBMRACB_LSR      | =X'12'      | Local shared resources                                          |
|               | CBMRACB_GSR      | =X'13'      | Global shared resources                                         |
|               | CBMRACB_RLS      | =X'14'      | Record-level sharing                                            |
|               | CBMRACB_NUB      | =X'15'      | No user buffering                                               |
|               | CBMRACB_UBF      | =X'16'      | User buffering                                                  |
|               | CBMRACB_CFX      | =X'17'      | Buffers fixed                                                   |
|               | CBMRACB_NFX      | =X'18'      | Buffers fixed                                                   |
|               | CBMRACB_DDN      | =X'19'      | Share by DDName                                                 |
|               | CBMRACB_DSN      | =X'1A'      | Share by DSN                                                    |
|               | CBMRACB_ICI      | =X'1B'      | Improved CI processing                                          |
|               | CBMRACB_NCI      | =X'1C'      | Normal CI processing                                            |
|               | CBMRACB_LEW      | =X'1D'      | LSR enqueue wait                                                |
|               | CBMRACB_NLW      | =X'1E'      | No lock wait                                                    |
|               | CBMRACB_ESDS     | =X'20'      | Cluster is ESDS                                                 |
|               | CBMRACB_KSDS     | =X'21'      | Cluster is KSDS                                                 |
|               | CBMRACB_LDS      | =X'22'      | Cluster is LDS                                                  |
|               | CBMRACB_RRDS     | =X'23'      | Cluster is RRDS                                                 |
|               | CBMRACB_REPL     | =X'24'      | Index is replicated                                             |
|               | CBMRACB_SPAN     | =X'25'      | Spanned                                                         |
|               | CBMRACB_SSWD     | =X'26'      | Sequence set with data                                          |
|               | CBMRACB_VRRDS    | =X'27'      | Variable RRDS                                                   |
|               | CBMRACB_WCK      | =X'28'      | Write checks                                                    |
|               | CBMRACB_CMPRS    | =X'29'      | Compression                                                     |
|               | CBMRACB_UNQ      | =X'2A'      | Unique key                                                      |
|               | CBMRACB_XADDR    | =X'2B'      | 8-byte RBAs                                                     |
|               | CBMRACB_OPEN     | =X'2C'      | ACB is open                                                     |
|               | CBMRACB_PATH     | =X'2D'      | ACB uses a path                                                 |
|               | CBMRACB_BASE     | =X'2E'      | ACB uses base cluster                                           |
|               | CBMRACB_AIX      | =X'2F'      | ACB uses AIX                                                    |
|               | CBMR_ACB_VESDS   | =X'30'      | Variable ESDS                                                   |
|               | CBMRACB_COPIES   | =X'60'      | Nr of copies                                                    |
|               | CMBRACB_ERET     | =X'61'      | Error return address                                            |
|               | CBMRACB_PASSWD   | =X'62'      | Password pointer                                                |
|               | CBMRACB_EXLST    | =X'63'      | EXLST pointer                                                   |
|               | CBMRACB_BUFSP    | =X'64'      | Buffer space                                                    |
|               | CBMRACB_BUFND    | =X'65'      | Nr of data buffers                                              |
|               | CBMRACB_BUFNI    | =X'66'      | Nr of index buffers                                             |
|               | CBMRACB_STRNO    | =X'67'      | Parallel threads                                                |
|               | CBMRACB_BSTRNO   | =X'68'      | Nr of strings                                                   |
|               | CBMRACB_MAREA    | =X'69'      | Messagearea                                                     |
|               | CBMRACB_MLEN     | =X'6A'      | Message area length                                             |
|               | CBMRACB_RLSREAD  | =X'6B'      | RLS/CR/NORD                                                     |
|               | CBMRACB_SHRPL    | =X'6C'      | Sharepool number                                                |
|               | CBMRACB_ACBLEN   | =X'6D'      | Length of ACB                                                   |
|               | CBMRACB_AVSPAC   | =X'6E'      | Available space                                                 |
|               | CBMRACB_BFRND    | =X'6F'      | Buffer found count                                              |
|               | CBMRACB_BUFNO    | =X'70'      | Nr of I/O buffers                                               |
|               | CBMRACB_BUFRDS   | =X'71'      | Buffer read count                                               |
|               | CBMRACB_CINV     | =X'72'      | Block size                                                      |
|               | CBMRACB_ENDRBA   | =X'73'      | End RBA                                                         |
|               | CBMRACB_ERROR    | =X'74'      | Open/close error code                                           |
|               | CBMRACB_FS       | =X'75'      | Free Space                                                      |
|               | CBMRACB_HALCRBA  | =X'76'      | High allocated RBA                                              |
|               | CBMRACB_KEYLEN   | =X'77'      | Key length                                                      |
|               | CBMRACB_LRECL    | =X'78'      | (max) record length                                             |
|               | CBMRACB_NCIS     | =X'79'      | Nr of Block splits                                              |
|               | CBMRACB_NDELR    | =X'7A'      | Nr. of deletes                                                  |
|               | CBMRACB_NEXCP    | =X'7B'      | Nr of I/O requests                                              |
|               | CBMRACB_NEXT     | =X'7C'      | Nr. of file extents                                             |
|               | CBMRACB_NINSR    | =X'7D'      | Nr of inserts                                                   |
|               | CBMRACB_NIXL     | =X'7E'      | Nr of index levels                                              |
|               | CBMRACB_NLOGR    | =X'7F'      | Nr of records                                                   |
|               | CBMRACB_NRETR    | =X'80'      | Nr of retrievals                                                |
|               | CBMRACB_NSSS     | =X'81'      | Nr of CA splits                                                 |
|               | CBMRACB_NUIW     | =X'82'      | Nr of VSAM writes                                               |
|               | CBMRACB_NUPDR    | =X'83'      | Nr of updates                                                   |
|               | CBMRACB_RKP      | =X'84'      | Relative key position                                           |
|               | CBMRACB_SDTASZ   | =X'85'      | Uncompressed data size                                          |
|               | CBMRACB_STRMAX   | =X'86'      | Max. nr of strings                                              |
|               | CBMRACB_UIW      | =X'87'      | User initiated writes                                           |
|               | CBMRACB_DDNM     | =X'E0'      | DDname                                                          |
|               | CBMRACB_CDTASZ   | =X'E1'      | Compressed data size                                            |
|               | CBMRACB_LEVEL    | =X'E2'      | zVSAM level                                                     |
|               | CBMRACB_LOKEY    | =X'E3'      | Lowest valid key ptr                                            |
|               | CBMRACB_RELEASE  | =X'E4'      | zVSAM level                                                     |
|               | CBMRACB_STMST    | =X'E5'      | Last close timestamp                                            |
|               | CBMRACB_XAVCSPC  | =X'E6'      | 8-byte AVCSPAC                                                  |
|               | CBMRACB_XENDRBA  | =X'E7'      | 8-byte ENDRBA                                                   |
|               | CBMRACB_XHALCRBA | =X'E8'      | 8-byte HALXRBA                                                  |

The validity of verb codes differ per macro. Whether any given verb code is valid is determined by the syntax definition of the GENCB, MODCB, SHOWCB, TESTCB macros.
Please refer to the appropriate chapter to check which parameters are supported.

For SHOWCB and TESTCB some keywords are applicable only after open. If they are issued before the ACB has been opened, an error is returned.

Additional notes:

- `CBMRACB_NRS` – TESTCB only, always true
- `CBMRACB_RST` – TESTCB only, always false
- `CBMRACB_NSR` – TESTCB only, always true
- `CBMRACB_LSR` – TESTCB only, always false
- `CBMRACB_GSR` – TESTCB only, always false
- `CBMRACB_RLS` – TESTCB only, always false
- `CBMRACB_NUB` – TESTCB only, always true
- `CBMRACB_UBF` – TESTCB only, always false
- `CBMRACB_CFX` – TESTCB only, always false
- `CBMRACB_NFX` – TESTCB only, always false
- `CBMRACB_DDN` – TESTCB only, always false
- `CBMRACB_DSN` – TESTCB only, always false
- `CBMRACB_ICI` – TESTCB only, always false
- `CBMRACB_NCI` – TESTCB only, always true
- `CBMRACB_LEW` – TESTCB only, always true
- `CBMRACB_NLW` – TESTCB only, always false
- `CBMRACB_REPL` – TESTCB only, always false
- `CBMRACB_SSWD` – TESTCB only, always false
- `CBMRACB_WCK` – TESTCB only, always false
- `CBMRACB_CMPRS` – TESTCB only, always false
- `CBMRACB_XADDR` – TESTCB only, always true
- `CBMRACB_COPIES` – GENCB only. If not specified, the CBMR handler assumes 1.
- `CBMRACB_PASSWD` – pointer to a one-byte length field, followed by the password
- `CBMRACB_MAREA` – TESTCB only, always 0
- `CBMRACB_MLEN` – TESTCB only, always 0
- `CBMRACB_SHRPL` – TESTCB only, always 0
- `CBMRACB_ENDRBA` – ending RBA of the component, derived from ending XLRA.
- `CBMRACB_FS` – Nr of free blocks per 100
- `CBMRACB_HALCRBA` – High allocated RBA, derived from highest allocated XLRA.
- `CBMRACB_NEXT` – for zVSAM the value is always 1.
- `CBMRACB_NSSS` – always 0
- `CBMRACB_LEVEL` – 4-byte address followed by 4-byte length of level info field
- `CBMRACB_LOKEY` – 4-byte address followed by 4-byte length of key field
- `CBMRACB_RELEASE` – 4-byte address followed by 4-byte length of level info field

### CBMR - body and tail for EXLST

The CBMR body for EXLST is applicable only when the CBMR header's `CBMRXLST` bit is on.
Its length is determined by the `CBMRVRBS` fields in the CBMR header. This is always a multiple of 4
to allow word alignment of the tail area that directly follows the body section.
Any unused/unneeded verb bytes should be set to zero, indicating a no-operation.

The tail follows the body directly, but is absent if it has a zero length.
It contains a data field of 0, 4, or 8 bytes for each verb code in the body, in the same sequence.
The starting point of the tail can be found by adding the `CBMRVRBS` value to the start of the CBMR body at `CBMRBODY`.
Its length can be deduced form the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

**!! Review Note: !!** The below table significantly differs from what Melvyn implemented. This is to be revisited
after we define the interfaces for GENCB/MODCB/SHOWCB/TESTCB for the EXLST.

| Label         | Equate           | Designation | Remarks                                                         |
|---------------|------------------|-------------|-----------------------------------------------------------------|
| CBMR          |                  | DSECT       |                                                                 |
| CBMRBODY      |                  |             |                                                                 |
| CBMRXL_VERBS  |                  | XL1         | Verb code, repeats                                              |
|               | CBMRXL_NOOP      | =X'00'      | Filler, no-operation                                            |
|               | CBMRXL_COPIES    | =X'60'      | Nr of copies                                                    |
|               | CMBRXL_ERET      | =X'61'      | Error return address                                            |
|               | CBMRXL_XLSTLEN   | =X'6D'      | Length of EXLST                                                 |
|               | CBRMXL_NEODAD    | =X'A0'      | No EODAD routine                                                |
|               | CBMRXL_EODAD     | =X'A1'      | End-of-data routine                                             |
|               | CBMRXL_NEOD_A    | =X'A2'      | (,A)                                                            |
|               | CBMRXL_EOD_A     | =X'A3'      | (address,A)                                                     |
|               | CBMRXL_NEOD_N    | =X'A4'      | (,N)                                                            |
|               | CBMRXL_EOD_N     | =X'A5'      | (address,N)                                                     |
|               | CBMRXL_NEOD_L    | =X'A8'      | (,L)                                                            |
|               | CBMRXL_EOD_L     | =X'A9'      | (address,L)                                                     |
|               | CBMRXL_NEOD_AL   | =X'AA'      | (,A,L)                                                          |
|               | CBMRXL_EOD_AL    | =X'AB'      | (address,A,L)                                                   |
|               | CBMRXL_NEOD_NL   | =X'AC'      | (,N,L)                                                          |
|               | CBMRXL_EOD_NL    | =X'AD'      | (address,N,L)                                                   |
|               | CBRMXL_NLERAD    | =X'B0'      | No LERAD routine                                                |
|               | CBMRXL_LERAD     | =X'B1'      | Logical error routine                                           |
|               | CBMRXL_NLER_A    | =X'B2'      | (,A)                                                            |
|               | CBMRXL_LER_A     | =X'B3'      | (address,A)                                                     |
|               | CBMRXL_NLER_N    | =X'B4'      | (,N)                                                            |
|               | CBMRXL_LER_N     | =X'B5'      | (address,N)                                                     |
|               | CBMRXL_NLER_L    | =X'B8'      | (,L)                                                            |
|               | CBMRXL_LER_L     | =X'B9'      | (address,L)                                                     |
|               | CBMRXL_NLER_AL   | =X'BA'      | (,A,L)                                                          |
|               | CBMRXL_LER_AL    | =X'BB'      | (address,A,L)                                                   |
|               | CBMRXL_NLER_NL   | =X'BC'      | (,N,L)                                                          |
|               | CBMRXL_LER_NL    | =X'BD'      | (address,N,L)                                                   |
|               | CBRMXL_NSYNAD    | =X'C0'      | No SYNAD routine                                                |
|               | CBMRXL_SYNAD     | =X'C1'      | Physical error routine                                          |
|               | CBMRXL_NSYN_A    | =X'C2'      | (,A)                                                            |
|               | CBMRXL_SYN_A     | =X'C3'      | (address,A)                                                     |
|               | CBMRXL_NSYN_N    | =X'C4'      | (,N)                                                            |
|               | CBMRXL_SYN_N     | =X'C5'      | (address,N)                                                     |
|               | CBMRXL_NSYN_L    | =X'C8'      | (,L)                                                            |
|               | CBMRXL_SYN_L     | =X'C9'      | (address,L)                                                     |
|               | CBMRXL_NSYN_AL   | =X'CA'      | (,A,L)                                                          |
|               | CBMRXL_SYN_AL    | =X'CB'      | (address,A,L)                                                   |
|               | CBMRXL_NSYN_NL   | =X'CC'      | (,N,L)                                                          |
|               | CBMRXL_SYN_NL    | =X'CD'      | (address,N,L)                                                   |
|               | CBRMXL_NJRNAD    | =X'D0'      | No JRNAD routine                                                |
|               | CBMRXL_JRNAD     | =X'D1'      | Journaling routine                                              |
|               | CBMRXL_NJRN_A    | =X'D2'      | (,A)                                                            |
|               | CBMRXL_JRN_A     | =X'D3'      | (address,A)                                                     |
|               | CBMRXL_NJRN_N    | =X'D4'      | (,N)                                                            |
|               | CBMRXL_JRN_N     | =X'D5'      | (address,N)                                                     |
|               | CBMRXL_NJRN_L    | =X'D8'      | (,L)                                                            |
|               | CBMRXL_JRN_L     | =X'D9'      | (address,L)                                                     |
|               | CBMRXL_NJRN_AL   | =X'DA'      | (,A,L)                                                          |
|               | CBMRXL_JRN_AL    | =X'DB'      | (address,A,L)                                                   |
|               | CBMRXL_NJRN_NL   | =X'DC'      | (,N,L)                                                          |
|               | CBMRXL_JRN_NL    | =X'DD'      | (address,N,L)                                                   |

The validity of verb codes differ per macro.
Whether any given verb code is valid is determined by the syntax definition of the GENCB, MODCB, SHOWCB, TESTCB macros.
Please refer to the appropriate chapter to check which parameters are supported.

### CBMR - body and tail for RPL

The CBMR body for RPL is applicable only when the CBMR header's `CBMRRPL` bit is on.
Its length is determined by the `CBMRVRBS` fields in the CBMR header. This is always a multiple of 4
to allow word alignment of the tail area that directly follows the body section.
Any unused/unneeded verb bytes should be set to zero, indicating a no-operation.

The tail follows the body directly, but is absent if it has a zero length.
It contains a data field of 0, 4, or 8 bytes for each verb code in the body, in the same sequence.
The starting point of the tail can be found by adding the `CBMRVRBS` value to the start of the CBMR body at `CBMRBODY`.
Its length can be deduced form the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

**!! Review Note: !!** The below table significantly differs from what Melvyn implemented. This is to be revisited
after we define the interfaces for GENCB/MODCB/SHOWCB/TESTCB for the RPL.

| Label         | Equate          | Designation | Remarks                                                         |
|---------------|-----------------|-------------|-----------------------------------------------------------------|
| CBMR          |                 | DSECT       |                                                                 |
| CBMRBODY      |                 |             |                                                                 |
| CBMRRPL_VERBS |                 | XL1         | Verb code, repeats                                              |
|               | CBMRRPL_NOOP    | =X'00'      | Filler, no-operation                                            |
|               | CBMRRPL_ADR     | =X'01'      | RBA-addressed                                                   |
|               | CBMRRPL_KEY     | =X'02'      | Key addressed                                                   |
|               | CBMRRPL_CNV     | =X'03'      | Control Interval                                                |
|               | CBMRRPL_DIR     | =X'06'      | Direct                                                          |
|               | CBMRRPL_SEQ     | =X'07'      | Sequential                                                      |
|               | CBMRRPL_SKP     | =X'08'      | Skip-sequential                                                 |
|               | CBMRRPL_ARD     | =X'30'      | Argument-determined                                             |
|               | CBMRRPL_LRD     | =X'31'      | Last Record                                                     |
|               | CBMRRPL_FWD     | =X'32'      | Forward                                                         |
|               | CBMRRPL_BWD     | =X'33'      | Backward                                                        |
|               | CBMRRPL_ASY     | =X'34'      | Asynchronous                                                    |
|               | CBMRRPL_SYN     | =X'35'      | Synchronous                                                     |
|               | CBMRRPL_NSP     | =X'36'      | Next Sequential Position                                        |
|               | CBMRRPL_NUP     | =X'37'      | Not for update                                                  |
|               | CBMRRPL_UPD     | =X'38'      | For update                                                      |
|               | CBMRRPL_KEQ     | =X'39'      | Exact key match                                                 |
|               | CBMRRPL_KGE     | =X'3A'      | Exact match or next greater key                                 |
|               | CBMRRPL_FKS     | =X'3B'      | Full key search                                                 |
|               | CBMRRPL_GEN     | =X'3C'      | Generic                                                         |
|               | CBMRRPL_LOC     | =X'3D'      | Locate mode                                                     |
|               | CBMRRPL_MVE     | =X'3E'      | Move mode                                                       |
|               | CBMRRPL_PKP     | =X'3F'      | AIXFLAG=AIXPKP                                                  |
|               | CBMRRPL_COMPL   | =X'40'      | IO=COMPLETE                                                     |
|               | CBMRRPL_COPIES  | =X'60'      | Nr of copies                                                    |
|               | CMBRRPL_ERET    | =X'61'      | Error return address                                            |
|               | CBMRRPL_MAREA   | =X'69'      | Message area                                                    |
|               | CBMRRPL_MLEN    | =X'6A'      | Message area length                                             |
|               | CBMRRPL_RPLLEN  | =X'6D'      | Length of ACB                                                   |
|               | CBMRRPL_KEYLEN  | =X'77'      | Key length                                                      |
|               | CBMRRPL_RECLEN  | =X'78'      | record length                                                   |
|               | CBMRRPL_ACB     | =X'90'      | ACB pointer                                                     |
|               | CBMRRPL_AREA    | =X'91'      | Record area                                                     |
|               | CBMRRPL_AREALEN | =X'92'      | Area length                                                     |
|               | CBMRRPL_ARG     | =X'93'      | Argument pointer                                                |
|               | CBMRRPL_ECB     | =X'94'      | ECB pointer                                                     |
|               | CBMRRPL_NXTRPL  | =X'95'      | Next RPL in chain                                               |
|               | CBMRRPL_TRANSID | =X'96'      | Transaction ID                                                  |
|               | CBMR_RPL_AIXPC  | =X'97'      | Nr of AIX pointers                                              |
|               | CBMRRPL_FDBK    | =X'98'      | Feedback code                                                   |
|               | CBMRRPL_FTNCD   | =X'99'      | Function code                                                   |
|               | CBMRRPL_CRBA    | =X'9A'      | Current RBA                                                     |
|               | CBMRTPL_XRBA    | =X'F0'      | 8-byte Current RBA                                              |

The validity of verb codes differ per macro.
Whether any given verb code is valid is determined by the syntax definition of the GENCB, MODCB, SHOWCB, TESTCB macros.
Please refer to the appropriate chapter to check which parameters are supported.

For SHOWCB and TESTCB some keywords are applicable only when no I/O is in progress.
If they are issued before the ECB has been posted, unpredictable results may occur.

Additional notes:
- `CBMRRPL_CNV` – TESTCB only, always false
- `CBMRRPL_TRANSID` – Always foxes

