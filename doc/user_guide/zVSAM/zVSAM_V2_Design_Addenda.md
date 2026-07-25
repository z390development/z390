### GENCB RPL macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `BLK`=RPL           | required to indicate the RPL-supporting logic of the macro is being invoked                                                                                                                                             |
| `COPIES`=           | Number of identical copies to generate. Specify a number between 1 and 65535. The default is 1.                                                                                                                         |
| `WAREA`=            | Address of a work area where the RPL/RPLs is/are to be constructed. When `WAREA` is specified, `LENGTH must` be specified too.                                                                                          |
|                     | When WAREA is not specified, the CBMR handler allocates an area of storage. The address of this area is returned in R1; its length in R0.                                                                               |
| `LENGTH`=           | Length in bytes of the area indicated by `WAREA`. When `LENGTH` is specified, `WAREA` must be specified as well.                                                                                                        |
| `LOC`=              | Where a work area for constructing the RPL/RPLs is to be allocated. Used only when `WAREA` and `LENGTH` are not specified. Supports the keywords `BELOW` and `ANY`, with `BELOW` being the default                      |
| - `BELOW`           | the work area is to be allocated below the line                                                                                                                                                                         |
| - `ANY`             | the work area is to be allocated above the line if possible, below the line otherwise.                                                                                                                                  |
| *other*             | Any parameters and/or keywords supported by the RPL macro. Please see the description of the RPL macro for details.                                                                                                     |
|                     | Supported parameters and keywords on the RPL macro are supported on GENCB RPL as well. Likewise, unsupported parameters and keywords on the RPL macro are not supported on GENCB RPL either.                            |
|                     | How the parameters can be specified differs per parameter.                                                                                                                                                              |
|                     | For a complete list of options, please see the IBM manual “DFSMS Macro Instructions for Data Sets” or equivalent for the operating system and version that you are porting to/from.                                     |
| Please note:        | not supported are expressions like (S,scon) or (\*,scon)                                                                                                                                                                |
| `MF`=               | When omitted, specifies the standard form of the GENCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                               |
| `MF=L`              | Specifies the list form of the GENCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                              |
| `MF=(L,addr)`       | Specifies the list form of the GENCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                               |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the GENCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                    |
| `MF=(G,addr)`       | Specifies the generate form of the GENCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                        |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

### MODCB RPL macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `RPL`=addr          | required to indicate the RPL to be modified                                                                                                                                                                             |
| *other*             | Any parameters and/or keywords supported by the RPL macro. Please see the description of the RPL macro for details.                                                                                                     |
|                     | Supported parameters and keywords on the RPL macro are supported on MODCB RPL as well. Likewise, unsupported parameters and keywords on the RPL macro are not supported on MODCB RPL either.                            |
|                     | How the parameters can be specified differs per parameter.                                                                                                                                                              |
|                     | For a complete list of options, please see the IBM manual “DFSMS Macro Instructions for Data Sets” or equivalent for the operating system and version that you are porting to/from.                                     |
| Please note:        | not supported are expressions like (S,scon) or (\*,scon)                                                                                                                                                                |
| `MF`=               | When omitted, specifies the standard form of the MODCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                               |
| `MF=L`              | Specifies the list form of the MODCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                              |
| `MF=(L,addr)`       | Specifies the list form of the MODCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                               |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the MODCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                    |
| `MF=(G,addr)`       | Specifies the generate form of the MODCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                        |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

### SHOWCB RPL macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `RPL`=addr          | required to indicate the RPL to be queried                                                                                                                                                                              |
| `FIELDS`=           | specifies a list of keywords. Each keyword specified returns a field of 4 or 8 bytes. These return values are stored consecutively in the return area specified in the `AREA`= and `LENGTH`= parameters.                |
|                     | Some keywords are valid only when the RPL's reqeuest is not in progress. Unpredictable results may occur when any of these keywords are used on a SHOWCB RPL request that is still processing.                          |
|                     | For the following keywords there are a few things to keep in mind:                                                                                                                                                      |
| - `AIXPC`           | What info is this indicating? Value will be taken from `PFXAIXN` (to be defined)?                                                                                                                                       |
| - `RBA`/`XRBA`      | How to determine??? zVSAM supports these keywords only for ESDS. For any other type of cluster a value of foxes will be returned by default.                                                                            |
| - `TRANSID`         | Always returns foxes.                                                                                                                                                                                                   |
| `MF`=               | When omitted, specifies the standard form of the SHOWCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                              |
| `MF=L`              | Specifies the list form of the SHOWCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                             |
| `MF=(L,addr)`       | Specifies the list form of the SHOWCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                              |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the SHOWCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                   |
| `MF=(G,addr)`       | Specifies the generate form of the SHOWCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                       |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

Review notes:
- `AIXPC` - What info is this indicating? Value will be taken from `PFXAIXN` (to be defined)? Need to validate this decision.
- `RBA`/`XRBA` - How to determine??? zVSAM supports these keywords only for ESDS. For any other type of cluster a value of foxes will be returned by default. Need to validate this decision.

### TESTCB RPL macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `RPL`=addr          | required to indicate the RPL to be tested                                                                                                                                                                               |
| `FTNCD`=nr          | Values used for `FTNCD` and their meaning can be found in the IBM manual “DFSMS Macro Instructions for Datasets”, chapter “Return and Reason Codes”, section “Component Codes”                                          |
| `RBA`=nr            | zVSAM supports this keyword only for ESDS. For any other type of cluster a value of foxes will be assumed by default.                                                                                                   |
| `MF`=               | When omitted, specifies the standard form of the TESTCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                              |
| `MF=L`              | Specifies the list form of the TESTCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                             |
| `MF=(L,addr)`       | Specifies the list form of the TESTCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                              |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the TESTCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                   |
| `MF=(G,addr)`       | Specifies the generate form of the TESTCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                       |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

### POINT macro parameters

### GET macro parameters

### PUT macro parameters

### ERASE macro parameters

### CHECK macro parameters

### ENDREQ macro parameters

### VERIFY macro parameters

## List of changes

This list of changes starts after the meeting between Abe Kornelis, Melvyn Maltz, and Hugh Sweeney
where earlier drafts were corroborated and finalized.

| Date       | Author       | Description                                                                                                             |
|------------|--------------|-------------------------------------------------------------------------------------------------------------------------|
| 2018-09-16 | Abe Kornelis | Remove SPX from VS records that have only a single segment.                                                             |
|            |              | Change order and numbering of chapters                                                                                  |
|            |              | Move macro parameter descriptions to addendum                                                                           |
|            |              | Expand chapter on compatibility                                                                                         |
|            |              | In the addendum for GENCB ACB add explanation on MF usage                                                               |
| 2018-09-18 | Abe Kornelis | Various small changes as suggested by Hugh Sweeney                                                                      |
|            |              | Moved zACB and zEXLST layout paragraphs to the addenda.                                                                 |
| 2018-09-20 | Abe Kornelis | Various small changes as suggested by Melvyn. See mail dated 2018-09-19 22:19                                           |
| 2018-09-27 | Abe Kornelis | Added content for MODCB ACB, including addendum.                                                                        |
| 2018-09-29 | Abe Kornelis | Added content for SHOWCB ACB, including addendum                                                                        |
| 2018-10-01 | Abe Kornelis | Added content for TESTCB ACB, including addendum                                                                        |
| 2018-10-07 | Abe Kornelis | Added comment on CBMR layout to chapters on GENCB ACB, MODCB ACB, SHOWCB ACB and TESTCB ACB.                            |
|            |              | Parm AM=VSAM added to GENCB ACB chapter.                                                                                |
|            |              | Added content for GENCB EXLST, including addendum                                                                       |
| 2018-10-08 | Abe Kornelis | Added content for MODCB EXLST, including addendum                                                                       |
|            |              | Added content for SHOWCB EXLST, including addendum                                                                      |
|            |              | Added content for TESTCB EXLST, including addendum                                                                      |
| 2018-10-09 | Abe Kornelis | CBMR split into header and separate tail sections                                                                       |
|            |              | CBMR header description added                                                                                           |
| 2018-10-10 | Abe Kornelis | Minor changes as suggested by Melvyn's mail dd 2018-10-09 23:40                                                         |
|            |              | Addition of chapter titles for RPL-based interfaces to addenda.                                                         |
| 2018-10-11 | Abe Kornelis | Added CBMR description – body for ACB                                                                                   |
| 2018-10-13 | Abe Kornelis | Added CBMR description – body for EXLST                                                                                 |
|            |              | Added RPL macro description, including addendum                                                                         |
|            |              | Added GENCB RPL macro description, including addendum                                                                   |
|            |              | Added MODCB RPL macro description, including addendum                                                                   |
|            |              | Added SHOWCB RPL macro description, including addendum                                                                  |
|            |              | Added TESTCB RPL macro description, including addendum                                                                  |
| 2018-10-15 | Abe Kornelis | Added ACBPFX pointer to zACB layout                                                                                     |
| 2018-10-16 | Abe Kornelis | Added CBMR description – body for RPL                                                                                   |
|            |              | Changed ACBTYPE to ACBSTYPE                                                                                             |
|            |              | Removed ACBMACR3_NLW and ACBMACR3_MODE                                                                                  |
|            |              | Changed ACBCUEL to ACBUEL                                                                                               |
|            |              | Changed ACBOCK to ACBLOCK                                                                                               |
| 2018-10-21 | Abe Kornelis | ACB ADR/KEY improved keyword description in addendum                                                                    |
|            |              | ACB IN/OUT improved keyword description in addendum                                                                     |
|            |              | ACB DDNAME improved keyword description in ACB macro chapter and the addendum                                           |
|            |              | Unsupported parameters and keywords on ACB, EXLST, RPL changed from “flagged as error” to “ignored”                     |
| 2018-10-22 | Abe Kornelis | Add description of prefix block, including counters area.                                                               |
|            |              | Updated addendum for SHOWCB/TESTCB with reference to source of data for each keyword.                                   |
|            |              | Added prefix field PFXIXLVL.                                                                                            |
|            |              | Added instructions for RPL-based operations on how to maintain prefix counter fields.                                   |
|            |              | Added description of spacemap block.                                                                                    |
| 2018-10-23 | Abe Kornelis | Specify that SHOWCB/TESTCB for RBA/XRBA is supported for ESDS only. Foxes for any other component.                      |
| 2018-10-24 | Abe Kornelis | Add description for block header, block, footer, record pointer list                                                    |
| 2018-10-26 | Abe Kornelis | Added description of open macro logic                                                                                   |
| 2018-10-27 | Abe Kornelis | Added eyecatcher to the prefix area, moved record length and key info fields to beginning of prefix area                |
|            |              | BHDRPREV/NEXT on prefix block documented as being foxes                                                                 |
| 2018-10-28 | Abe Kornelis | Added ACBVER to zACB layout                                                                                             |
|            |              | Added Area to Terminology chapter                                                                                       |
|            |              | Max. block size reduced from 2G to 16MB                                                                                 |
|            |              | Added ACBXPFX to zACB layout                                                                                            |
|            |              | Added description of open execution logic                                                                               |
| 2018-11-21 | Abe Kornelis | In API on macro interfaces improved wording for handling of (as yet) unsupported macro parameters.                      |
|            |              | Add ATRB=VESDS for TESTCB ACB                                                                                           |
|            |              | Improved picture and text on spacemap block layout                                                                      |
| 2018-11-22 | Abe Kornelis | BHDRPREV/NEXT details expanded                                                                                          |
|            |              | Removed PFXBSEG/PFXESEG                                                                                                 |
| 2018-11-25 | Abe Kornelis | Spacemap area structure. Segmented records were missing. Added.                                                         |
| 2018-11-29 | Abe Kornelis | Added diagrams to chapter on block header structure.                                                                    |
| 2018-12-02 | Abe Kornelis | Added alternative diagram for chaining segments. Preferred solution not yet determined                                  |
|            |              | And added drawings for chaining index blocks                                                                            |
| 2018-12-09 | Abe Kornelis | Structure of Physical files: ELIX added to the list of block types                                                      |
|            |              | Block Header Structure: BHDRFLGS changed to BHDRFLG1 and added BHDRFLG2 with BHDR_ELX                                   |
| 2018-12-17 | Abe Kornelis | Put PFXBSEG/PFXESEG back in                                                                                             |
|            |              | Corrected typos in drawings for explaining BHDRNEXT/PREV                                                                |
|            |              | RPTR_END no longer all foxes, foxes only for RPTRREC@                                                                   |
|            |              | Added 4 date fields to the prefix structure for creation and last update timestamps for both data and index component.  |
|            |              | MF=omitted changed to MF= in various locations                                                                          |
| 2019-01-06 | Abe Kornelis | Added various fields to RPL                                                                                             |

The end of this change section marks the point where Melvyn Maltz took over maintenance of the document.
