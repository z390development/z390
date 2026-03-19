# zVSAM V2 - Addenda

## API for RPL-based interfaces

## POINT macro parameters

## GET macro parameters

## PUT macro parameters

## ERASE macro parameters

## CHECK macro parameters

## ENDREQ macro parameters

## VERIFY macro parameters


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

| Date       | Author       | Description                                                                                                             |
|------------|--------------|-------------------------------------------------------------------------------------------------------------------------|
| 2019-01-23 | Melvyn Maltz | Version 2.0                                                                                                             |
|            |              | Took over the D&L, not documenting spelling, syntax, bad references and other trivia                                    |
|            |              | Added hyperlinks                                                                                                        |
|            |              | Amended CBMRACB fields                                                                                                  |
|            |              | Amended CBMRRPL fields                                                                                                  |
|            |              | Removed RPL TIMEOUT...VTAM only                                                                                         |
|            |              | Updated Prefix Block DSECT                                                                                              |
| 2019-02-17 | Melvyn Maltz | Version 2.1                                                                                                             |
|            |              | Corrected diagram “Segmented Data Block Structure”                                                                      |
|            |              | CBMR RPL – Added GEN MOD SHOW TEST column                                                                               |
|            |              | CBMR ACB – Added GEN MOD SHOW TEST column                                                                               |
|            |              | CBMRACB RMODE31 – Added description                                                                                     |
| 2019-02-21 | Melvyn Maltz | Corrections to RPL DSECT. Added RPLFEEDB                                                                                |
|            |              | Note added to AIXPC                                                                                                     |
| 2019-02-22 | Melvyn Maltz | Corrections and updates to ACB DSECT                                                                                    |
| 2019-02-25 | Melvyn Maltz | Added note to MODCB ACB about turning off MACRF=OUT                                                                     |
| 2019-02-26 | Melvyn Maltz | Revised the OPEN Execution Logic table to distinguish V1 from V2                                                        |
| 2019-03-02 | Melvyn Maltz | Revised the EXLST DSECT                                                                                                 |
| 2019-03-03 | Melvyn Maltz | Added section EXLST Macro Logic                                                                                         |
|            |              | CBMR EXLST – Added GEN MOD SHOW TEST column                                                                             |
|            |              | Added \_MODS in preparation for MODCB                                                                                   |
| 2019-03-10 | Melvyn Maltz | Removed sections on Logical Processes that don't involve calls to Java eg. MODCB. These are already well described      |
| 2019-03-12 | Melvyn Maltz | Added Return and Reason Codes to MODCB ACB, RPL and EXLST                                                               |
| 2019-03-17 | Melvyn Maltz | Revised all sections on OPEN and CLOSE                                                                                  |
|            |              | Added “CLOSE Macro Logic”                                                                                               |
|            |              | Removed OPCL DSECT, replaced with list formats                                                                          |
| 2019-03-18 | Melvyn Maltz | Added Data Block diagram                                                                                                |
|            |              | Most references and diagrams for LDS removed, too long term                                                             |
| 2019-04-15 | Melvyn Maltz | Added hyperlinks to EXLST CBMR Modifiers                                                                                |
| 2019-04-17 | Melvyn Maltz | Added UPAD= and RLSWAIT= - Although not supported, they need to exist                                                   |
|            |              | Amended the EXLST CBMR keyword values to fit them in                                                                    |
|            |              | Added WAREA, LENGTH and LOC to EXLST CBMR                                                                               |
| 2019-05-19 | Melvyn Maltz | Added WAREA, LENGTH and LOC to ACB CBMR                                                                                 |
|            |              | Amended the ACB CBMR keyword values to fit them in                                                                      |
| 2019-06-26 | Melvyn Maltz | Added WAREA, LENGTH and LOC to RPL CBMR                                                                                 |
|            |              | Added Return and Reason Codes to GENCB ACB, RPL and EXLST                                                               |
| 2019-06-29 | Melvyn Maltz | Changed PFXMAPOF from 3 to 4 bytes, adjusted following offsets                                                          |
| 2019-06-30 | Melvyn Maltz | Version 2.2                                                                                                             |
|            |              | Open Execution Logic, removed references to OC24/OC31/OCPL                                                              |
| 2019-07-06 | Melvyn Maltz | RPLACB changed to RPLDACB to match IBM                                                                                  |
| 2019-07-07 | Melvyn Maltz | CBMR EXLST:                                                                                                             |
|            |              | Added CBMRXL_AREA and \_EXLST                                                                                           |
|            |              | Marked fields used for SHOWCB                                                                                           |
| 2019-07-08 | Melvyn Maltz | zVSAM V2 compatibility with zVSAM V1                                                                                    |
|            |              | Added item 4 about re-assembling modules with OPEN                                                                      |
| 2019-08-17 | Melvyn Maltz | EXLST Macro corrected                                                                                                   |
|            |              | Extra comments added to clarify missing parms for GENCB                                                                 |
| 2019-08-24 | Melvyn Maltz | AM=VSAM added to Macros                                                                                                 |
|            |              | SHOWCB EXLST, UPAD and RLSWAIT removed, IBM doesn't support them. JRNAD will return zero                                |
| 2019-10-09 | Melvyn Maltz | Added subfields to RPLFEEDB                                                                                             |
|            |              | Added RPLCXRBA                                                                                                          |
| 2019-10-13 | Melvyn Maltz | Added CBMRACB_ACB and CBMRRPL_RPL for SHOWCB                                                                            |
|            |              | Renamed CBMRRPL_CRBA to CBMRRPL_RBA                                                                                     |
| 2019-10-20 | Melvyn Maltz | Added CBMRRPL_RECAREA to resolve conflict between SHOWCB RPL AREA= and FIELDS=AREA                                      |
|            |              | Renumbered CBMRRPL_RPLLEN to X'6E' to avoid conflict with CBMRXL_XLSTLEN                                                |
|            |              | Added CBMRRPL_TRANSID for SHOWCB                                                                                        |
| 2019-10-27 | Melvyn Maltz | Corrected syntax to all forms of MF=L                                                                                   |
|            |              | SHOWCB, ACB, EXLST and RPL have all 3 lengths: ACBLEN, EXLLEN and RPLLEN                                                |
|            |              | New section added “No block type specified” and SHOWCB for extracting lengths only                                      |
|            |              | New section added “CBMR description-body for no block specified”                                                        |
| 2019-11-13 | Melvyn Maltz | SHOWCB ACB FIELDS revised                                                                                               |
|            |              | Added several X~ fields as SHOWCB counters expect 4 bytes and the CTR fields are 8 bytes                                |
| 2020-01-26 | Melvyn Maltz | SHOWCB ACB revisions                                                                                                    |
|            |              | CBMRACB_RMODE31 and CBMRACB_SHRPL codes changed to avoid conflict with CBMRRPL_RPLLEN and CBMRXL_XLSTLEN                |
| 2020-02-24 | Melvyn Maltz | CBMRACB_SDTASZ added as an 8-byte field                                                                                 |
|            |              | CTRSDTA renamed to CTRSDTASZ                                                                                            |
|            |              | References to a value of zero returned removed                                                                          |
| 2020-03-01 | Melvyn Maltz | Corrections made to the description of CTRLOKEY@                                                                        |
|            |              | This is an offset to LL+key                                                                                             |
| 2020-03-02 | Melvyn Maltz | Added CBMRACB_AREA                                                                                                      |
| 2020-04-28 | Melvyn Maltz | Reconstructed the Macro description sections to be more like that of the VSAM Macro manual                              |
|            |              | Separate chapter for the xCB Macro MF= with hyperlinks to it from each of them, duplicate MF= descriptions removed      |
| 2020-05-28 | Melvyn Maltz | Added diagrams for Data and Index (L0 and Ln) blocks                                                                    |
| 2020-05-29 | Melvyn Maltz | Version 2.3                                                                                                             |
|            |              | Removing individual sections on Fixed, Variable etc. and ESDS, KSDS and RRDS                                            |
|            |              | Removing Concepts                                                                                                       |
|            |              | Replaced with all 12 dataset type diagrams with descriptions                                                            |
|            |              | Removed Displaced Record structure                                                                                      |
|            |              | Added SPX format                                                                                                        |
|            |              | Added AIX structures and formats                                                                                        |
|            |              | Added ELIX structures and formats                                                                                       |
|            |              | Removed Counters area values, these are referenced in the macros themselves                                             |
|            |              | Added Implied OPEN table                                                                                                |
|            |              | Added Close execution logic (incomplete)                                                                                |
|            |              | All OPEN-related, EXLST/Exit-related and CLOSE-related doc now in single chapters                                       |
|            |              | Addenda items moved to the appropriate chapter or deleted                                                               |
|            |              | Deleted all DSECTs, too difficult to maintain both the DSECT and the doc                                                |
|            |              | The DSECTs are fully commented and offsets can be seen in an assembly                                                   |
|            |              | CBMR details moved to the xCB chapter and simplified                                                                    |
| 2020-08-18 | Melvyn Maltz | Version 2.4                                                                                                             |
|            |              | Added paragraph in MODCB RPL= to clarify the OPTCD= parameter                                                           |
| 2020-08-22 | Melvyn Maltz | Revised the xCB return codes                                                                                            |
|            |              | Revised the paragraph in MODCB ACB= about the MACRF= parameter                                                          |
| 2020-09-25 | Melvyn Maltz | Improved the wording of MF= for the xCB macros                                                                          |
|            |              | Clarified what is returned in R0 and R1 for the xCB macros                                                              |
| 2020-10-10 | Melvyn Maltz | RPL and MODCB have notes about internal/external ECBs and the bit RPLOPT2_ECB                                           |
| 2020-10-19 | Melvyn Maltz | Updated OPEN execution logic to include fields and bits in the ACB which must be set on a successful OPEN               |
| 2020-11-08 | Melvyn Maltz | Added a paragraph about syntax checking in API for Assembler...                                                         |
| 2020-11-23 | Melvyn Maltz | Added section “GENCB, MODCB, TESTCB and SHOWCB parameter types”                                                         |
| 2020-12-01 | Melvyn Maltz | Revised all sections for TESTCB                                                                                         |
|            |              | Removed Addenda, chapter on “API for ACB-based interfaces”. Info is incorporated into the macro description             |
|            |              | CBMR verb codes amended to X'01'-X'DF' as 4-byte fields                                                                 |
|            |              | So few 0-byte fields that it wasn't worthwhile having a separate category                                               |
| 2020-12-17 | Melvyn Maltz | SHOWCB LOKEY, added comment and hyperlink to errors                                                                     |
| 2021-01-25 | Melvyn Maltz | Note added in Terminology to clarify the meaning of XRBA                                                                |
| 2021-05-11 | Melvyn Maltz | Revised OPEN and CLOSE sections to include the zOPC header, storage violation attempt and V1 vs V2 detection            |
| 2021-05-13 | Melvyn Maltz | Revised OPEN and CLOSE sections to include the MODE bit as the senior bit of no. of entries,                            |
|            |              | code to prevent MF=E modifying an MF=L with a different mode                                                            |
| 2021-06-19 | Melvyn Maltz | Added new section “Counters Area and its maintenance “                                                                  |

