# zVSAM V2 - Addenda

This document describes Macro parameters and control blocks used in the zVSAM V2 implementation.

### zACB description

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

