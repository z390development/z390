# zVSAM V2 - Addenda

This document describes Macro parameters and control blocks used in the zVSAM V2 implementation.
It contains the following major chapters:

1. [API for ACB-based interfaces](#API-for-ACB-based-interfaces)
    1. [ACB macro parameters](#ACB-macro-parameters)
    2. [GENCB ACB macro parameters](#GENCB-ACB-macro-parameters)
    3. [MODCB ACB macro parameters](#MODCB-ACB-macro-parameters)
    4. [SHOWCB ACB macro parameters](#SHOWCB-ACB-macro-parameters)
    5. [TESTCB ACB macro parameters](#TESTCB-ACB-macro-parameters)
    6. [EXLST macro parameters](#EXLST-macro-parameters)
    7. [GENCB EXLST macro parameters](#GENCB-EXLST-macro-parameters)
    8. [MODCB EXLST macro parameters](#MODCB-EXLST-macro-parameters)
    9. [SHOWCB EXLST macro parameters](#SHOWCB-EXLST-macro-parameters)
   10. [TESTCB EXLST macro parameters](#TESTCB-EXLST-macro-parameters)
   11. [OPEN macro parameters](#OPEN-macro-parameters)
   12. [CLOSE macro parameters](#CLOSE-macro-parameters)
   13. [zACB description](#zACB-description)
   14. [zEXLST description](#zEXLST-description)
   15. [CBMR description](#CBMR-description)
   16. [CBMR description – body for ACB](#CBMR-description-–-body-for-ACB)
   17. [CBMR description – body for EXLST](#CBMR-description-–-body-for-EXLST)
2. [RPL-based interfaces](#RPL-based-interfaces)
    1. [RPL macro parameters](#RPL-macro-parameters)
    2. [GENCB RPL macro parameters](#GENCB-RPL-macro-parameters)
    3. [MODCB RPL macro parameters](#MODCB-RPL-macro-parameters)
    4. [SHOWCB RPL macro parameters](#SHOWCB-RPL-macro-parameters)
    5. [TESTCB RPL macro parameters](#TESTCB-RPL-macro-parameters)
    6. [POINT macro parameters](#POINT-macro-parameters)
    7. [GET macro parameters](#GET-macro-parameters)
    8. [PUT macro parameters](#PUT-macro-parameters)
    9. [ERASE macro parameters](#ERASE-macro-parameters)
   10. [CHECK macro parameters](#CHECK-macro-parameters)
   11. [ENDREQ macro parameters](#ENDREQ-macro-parameters)
   12. [VERIFY macro parameters](#VERIFY-macro-parameters)
   13. [zRPL description](#zRPL-description)
   14. [CBMR description – body for RPL](#CBMR-description-–-body-for-RPL)
3. [List of changes](#List-of-changes)

## API for ACB-based interfaces

### ACB macro parameters

With the exception of the DDNAME parameter, all supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `AM`=               | Optional parameter. AM=VSAM is the default. No other values are supported.                                                                                                                                              |
| `DDNAME`=           | DDname is required before open is executed. If DDname is not supplied on the ACB macro, the label used on the ACB macro is used as DDname. If neither is specified, a proper value must be supplied by using MODCB ACB. |
|                     | In zVSAM the DDname is to hold the name of an environment variable in the host OS. This variable in turn should contain the path and qualified filename of the cluster to be opened.                                    |
|                     | The qualifier is the name of another environment variable in the host OS and is the path to the assembled catalog. For more information on zVSAM catalogs, please refer to the "z390_zVSAM_Catalog_User_Guide".         |
| `PASSWD`=           | Supply the address of the password, consisting of a single byte with the password's length (1-8 characters) followed by the password value.                                                                             |
| `EXLST`=            | Pointer to an exit list. Please see the EXLST macro description for details.                                                                                                                                            |
| `BUFSP`=            | Maximum buffer space in virtual storage for this cluster. This is the combined size in bytes of all buffers allocated for this cluster.                                                                                 |
|                     | If (`BUFND` + `BUFNI`) \* Block_size exceeds the value specified for `BUFSP`, then `BUFND` and `BUFNI` will be reduced proportionally to keep total allocation below the limit specified in the `BUFSP` parameter.      |
| `BUFND`=            | Number of data buffers to allocate. Specify a number up to 65535. When over-allocating (see `BUFSP` parameter above) fewer data buffers will be allocated than requested.                                               |
| `BUFNI`=            | Number of index buffers to allocate. Specify a number up to 65535. When over-allocating (see `BUFSP` parameter above) fewer index buffers will be allocated than requested.                                             |
| `RMODE31`=          | Specifies whether buffers and/or control blocks should be allocated below the 16M line, or may be allocated above the 16M line. The default for RMODE31 is NONE. The following keywords are supported:                  |
| - `NONE`            | Control Blocks and buffers below 16M                                                                                                                                                                                    |
| - `CB`              | Control Blocks above or below 16M, buffers below 16M                                                                                                                                                                    |
| - `BUFF`            | Control Blocks below 16M, buffers above or below 16M                                                                                                                                                                    |
| - `ALL`             | Control Blocks and buffers above 16M or below 16M                                                                                                                                                                       |
| `STRNO`=            | Maximum number of strings (concurrent requests) for this ACB. If specified requires a number between 1 and 255 inclusive.                                                                                               |
| `BSTRNO`=           | Beginning (or initial) number of strings (concurrent requests) allocated to this ACB when a path is opened. If specified requires a number between 1 and 255 inclusive.                                                 |
| `MACRF`=            | List of keywords specifying how the cluster will be processed after open. The following keywords are supported:                                                                                                         |
| - `ADR/KEY`         | Non-exclusive `MACRF` keywords indicating whether the cluster may be accessed by address or by key.                                                                                                                     |
|                     | ADR can be used only with ESDS or KSDS to access records by RBA or XRBA.                                                                                                                                                |
|                     | KEY can be used with KSDS to access records by key                                                                                                                                                                      |
|                     | KEY can be used with RRDS to access records by relative record number.                                                                                                                                                  |
| - `DFR/NDF`         | Mutually exclusive `MACRF` keywords indicating whether or not buffer changes need to be written out to the file immediately.                                                                                            |
| - `DFR`             | allows zVSAM to defer writing and keep changes in the buffer. When multiple changes are combined, fewer I/Os are needed which should improve program performance.                                                       |
| - `NDF`             | disallows zVSAM to defer writing, forcing a buffer write for every single change to the buffer.                                                                                                                         |
| - `DIR`,`SEQ`,`SKP` | can be coded in any combination. If none of the three is specified `SEQ` is used as a default.                                                                                                                          |
| - `DIR`             | `MACRF` keyword indicating that the cluster will be processed directly. `DIR` can be used with ESDS, KSDS, or RRDS to access data randomly.                                                                             |
| - `SEQ`             | `MACRF` keyword indicating that the cluster will be processed sequentially. `SEQ` can be used with ESDS, KSDS, or RRDS to access data sequentially.                                                                     |
| - `SKP`             | `MACRF` keyword to allow skip-sequential access. I.e. specifying this keyword allows usage of the POINT macro to position the file to a specific position to access data randomly.                                      |
|                     | `SKP` can be used with KSDS or RRDS to randomly position the file to a specific key or RRN prior to sequential access                                                                                                   |
| - `IN`/`OUT`        | Non-exclusive `MACRF` keywords indicating whether the cluster will be processed for input only or for both input and output. `OUT` implies `IN`.                                                                        |
| - `IN`              | data in the cluster can be read but not changed in any way.                                                                                                                                                             |
| - `OUT`             | data in the cluster can be read, updated, inserted, or deleted.                                                                                                                                                         |
| - `NIS`/`SIS`       | Mutually exclusive `MACRF` keywords indicating how zVSAM inserts new records into the cluster. Relevant only for KSDS clusters.                                                                                         |
| - `NIS`             | Normal insert strategy: zVSAM will insert records optimizing for inserts that are dispersed randomly across the data set                                                                                                |
| - `SIS`             | Sequential insert stragegy: zVSAM will insert records optimizing for inserts that are (generally, mostly) packed together in a sequential manner.                                                                       |
| - `NRM`/`AIX`       | Mutually exclusive `MACRF` keywords indicating how zVSAM is to process accesses to an AIX. Relevant only when the DDname specifies a path.                                                                              |
| - `NRM`             | Normal mode: zVSAM treats the AIX data as a normal KSDS. This allows direct access to the AIX's data records.                                                                                                           |
| - `AIX`             | AIX mode: zVSAM will use the AIX to access records in the underlying base cluster.                                                                                                                                      |

### GENCB ACB macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `BLK`=ACB           | required to indicate the ACB-supporting logic of the macro is being invoked                                                                                                                                             |
| `COPIES`=           | Number of identical copies to generate. Specify a number between 1 and 65535. The default is 1.                                                                                                                         |
| `WAREA`=            | Address of a work area where the ACB/ACBs is/are to be constructed.                                                                                                                                                     |
|                     | When `WAREA` is specified, `LENGTH` must be specified too.                                                                                                                                                              |
|                     | When `WAREA` is not specified, the CBMR handler allocates an area of storage.                                                                                                                                           |
|                     | The address of this area is returned in R1; its length in R0.                                                                                                                                                           |
| `LENGTH`=           | Length in bytes of the area indicated by `WAREA`.                                                                                                                                                                       |
|                     | When `LENGTH` is specified, `WAREA` must be specified as well.                                                                                                                                                          |
| `LOC`=              | Where a work area for constructing the ACB/ACBs is to be allocated. Used only when `WAREA` and `LENGTH` are not specified. Supports the keywords `BELOW` and `ANY`, with `BELOW` being the default                      |
| - `BELOW`           | the work area is to be allocated below the line                                                                                                                                                                         |
| - `ANY`             | the work area is to be allocated above the line if possible, below the line otherwise.                                                                                                                                  |
| *other*             | Any parameters and/or keywords supported by the ACB macro. Please see the description of the ACB macro for details.                                                                                                     |
|                     | Supported parameters and keywords on the ACB macro are supported on GENCB ACB as well. Likewise, unsupported parameters and keywords on the ACB macro are not supported on GENCB ACB either.                            |
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

### MODCB ACB macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ACB`=addr          | required to indicate the ACB to be modified                                                                                                                                                                             |
| *other*             | Any parameters and/or keywords supported by the ACB macro. Please see the description of the ACB macro for details.                                                                                                     |
|                     | Supported parameters and keywords on the ACB macro are supported on MODCB ACB as well. Likewise, unsupported parameters and keywords on the ACB macro are not supported on MODCB ACB either.                            |
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

### SHOWCB ACB macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ACB`=addr          | required to indicate the ACB to be queried                                                                                                                                                                              |
| `FIELDS`=           | specifies a list of keywords. Each keyword specified returns a field of 4 or 8 bytes. These return values are stored consecutively in the return area specified in the `AREA`= and `LENGTH`= parameters.                |
|                     | Some keywords are valid only when the ACB is open. An error is returned when any of these keywords are used while the ACB is not open.                                                                                  |
|                     | For the following keywords there are a few things to keep in mind:                                                                                                                                                      |
| - `AVSPAC`          | Count of available space in bytes – taken from prefix counter field `CTRAVSPAC`                                                                                                                                         |
| - `BFRFND`          | nr of times since this ACB was opened that a get/read request for this ACB was satisfied from a buffer, without doing any I/O.                                                                                          |
| - `BUFRDS`          | nr or times since this ACB was opened an I/O was needed for this ACB to read a block into a buffer.                                                                                                                     |
| - `CDTASIZE`        | Compressed data size. Since zVSAM does not support compression, this is the same as `SDTASIZE`. Taken from prefix counter field `CTRSDTA`.                                                                              |
| - `ENDRBA`          | high water mark of the component in bytes, discounting the prefix block. Taken from prefix counter field `CTRENDRBA`.                                                                                                   |
|                     | If the last block in the component is free, it's the starting XRBA of that block. If the last block holds the last record, it's the XRBA of that record's last byte.                                                    |
|                     | Otherwise, it's the XRBA of the last byte of the last record on the block.                                                                                                                                              |
| - `FS`              | For data component `PFXFRBLK` / (`PFXFRBLK` + `PFXFRINT`) \* 100. Foxes for index.                                                                                                                                      |
| - `HALCRBA`         | XRBA of the last byte of the last record. Taken from prefix counter field `CTRHALCRBA`.                                                                                                                                 |
| - `KEYLEN`          | length of key field. For KSDS this is the length of the key field. For RRDS/ESDS the value is always 8. Taken from prefix field `PFXKEYLN`.                                                                             |
| - `NCIS`            | nr of times a block was split for this component. Taken from prefix counter field `CTRNCIS`.                                                                                                                            |
| - `NDELR`           | nr of times a record was deleted for this component. Taken from prefix counter field `CTRNDELR`.                                                                                                                        |
| - `NEXCP`           | nr of times an I/O was issued for this component. Taken from prefix counter field `CTRNEXCP`.                                                                                                                           |
| - `NEXT`            | nr of extents. For zVSAM this is the number of files used to store the component. Taken from prefix counter field `CTRNEXT`.                                                                                            |
| - `NINSR`           | nr of times a record was inserted for this component. Taken from prefix counter field `CTRNINSR`.                                                                                                                       |
| - `NIXL`            | nr of index levels. Taken from prefix field `PFXIXLVL`.                                                                                                                                                                 |
| - `NLOGR`           | nr of records in the component. Taken from prefix counter field `CTRNLOGR`.                                                                                                                                             |
| - `NRETR`           |  nr of times a record was retrieved for this component. Taken from prefix counter field `CTRNRETR`.                                                                                                                     |
| - `NSSS`            | nr of CA splits. Always foxes.                                                                                                                                                                                          |
| - `NUIW`            | nr of times a block was written for this component by zVSAM rather than the user program. Taken from prefix counter field `CTRNNUIW`.                                                                                   |
| - `NUPDR`           | nr of times a record was updated for this component. Taken from prefix counter field `CTRNUPDR`.                                                                                                                        |
| - `RKP`             | relative key position. Taken from prefix field `PFXKYOFF`.                                                                                                                                                              |
| - `SDTASIZE`        | Total nr of data bytes currently stored in the component. Sum of all record lengths. Taken from prefix counter field `CTRSDTA`.                                                                                         |
| - `SHRPOOL`         | where do we take this value from?                                                                                                                                                                                       |
| - `STMST`           | system timestamp of last close operation on the component. Taken from prefix counter field `CTRSTMST`.                                                                                                                  |
| - `UIW`             | nr of times a block was written for this component by the user program rather than zVSAM. Taken from prefix counter field `CTRNUIW`.                                                                                    |
| `MF`=               | When omitted, specifies the standard form of the SHOWCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                              |
| `MF=L`              | Specifies the list form of the SHOWCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                             |
| `MF=(L,addr)`       | Specifies the list form of the SHOWCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                              |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the SHOWCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                   |
| `MF=(G,addr)`       | Specifies the generate form of the SHOWCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                       |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

Review notes:
1. ENDRBA - current last sentence seems superfluous. Left-over from a prior version? Double-check and remove or rephrase.
2. NSSS should be zero, rather than foxes - we never do CA splits.
3. SHRPOOL - Is it an attribute of a cluster, of a component, or of an ACB? i.e. is it always the same for a given cluster,
   for a given component, or can the user freely choose a pool number when building the ACB?

### TESTCB ACB macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `ACB`=addr          | required to indicate the ACB to be tested
| *other*             | All other keywords function the same way that they do on a SHOWCB ACB request. Please see the preceding chapter for details.
| `MF`=               | When omitted, specifies the standard form of the TESTCB to generate an inline CBMR and an inline call to the CBMR handler.
| `MF=L`              | Specifies the list form of the TESTCB macro which generates an inline CBMR but no call to the CBMR handler.
| `MF=(L,addr)`       | Specifies the list form of the TESTCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.
| `MF=(E,addr)`       | Specifies the execute form of the TESTCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.
| `MF=(G,addr)`       | Specifies the generate form of the TESTCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR

### EXLST macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `AM`=               | Optional parameter. AM=VSAM is the default. No other values are supported.                                                                                                                                              |
| `EODAD`=            | Optional parameter to specify the entry address of an exit that handles an end-of-data condition during sequential access.                                                                                              |
|                     | The routine address may be followed by a modifier. For details, please see below. The amode for the routine is encoded in the address using the common convention.                                                      |
| `LERAD`=            | Optional parameter to specify the entry address of an exit that handles logic errors.                                                                                                                                   |
|                     | The routine address may be followed by a modifier. For details, please see below. The amode for the routine is encoded in the address using the common convention.                                                      |
| `SYNAD`=            | Optional parameter to specify the entry address of an exit that handles physical errors.                                                                                                                                |
|                     | The routine address may be followed by a modifier. For details, please see below. The amode for the routine is encoded in the address using the common convention.                                                      |
| *mod*               | modifier, can optionally be specified after each routine address. Values: A or N for Active or Not-active.                                                                                                              |
|                     | As long as the routine is not active it will not be called by zVSAM.                                                                                                                                                    |
|                     | The secondary modifier of L (for Load from Linklib) is not supported.                                                                                                                                                   |

Review notes:
1. We have no linklib, but we might load a module anyway using our existing support for SVC 6 (Load macro)

### GENCB EXLST macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `BLK=EXLST`         | required to indicate the EXLST-supporting logic of the macro is being invoked                                                                                                                                           |
| `COPIES`=           | Number of identical copies to generate. Specify a number between 1 and 65535. The default is 1.                                                                                                                         |
| `WAREA`=            | Address of a work area where the EXLST/EXLSTs is/are to be constructed.                                                                                                                                                 |
|                     | When `WAREA` is specified, `LENGTH` must be specified too.                                                                                                                                                              |
|                     | When `WAREA` is not specified, the CBMR handler allocates an area of storage. The address of this area is returned in R1; its length in R0.                                                                             |
| `LENGTH`=           | Length in bytes of the area indicated by `WAREA`. When `LENGTH` is specified, `WAREA` must be specified as well.                                                                                                        |
| `LOC=`              | Where a work area for constructing the EXLST/EXLSTs is to be allocated. Used only when `WAREA` and `LENGTH` are not specified. Supports the keywords `BELOW` and `ANY`, with `BELOW` being the default                  |
| - `BELOW`           | the work area is to be allocated below the line                                                                                                                                                                         |
| - `ANY`             | the work area is to be allocated above the line if possible, below the line otherwise.                                                                                                                                  |
| *other*             | Any parameters and/or keywords supported by the EXLST macro. Please see the description of the EXLST macro for details.                                                                                                 |
|                     | Supported parameters and keywords on the EXLST macro are supported on GENCB EXLST as well. Likewise, unsupported parameters and keywords on the EXLST macro are not supported on GENCB EXLST either.                    |
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

### MODCB EXLST macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `EXLST`=addr        | required to indicate the EXLST to be modified                                                                                                                                                                           |
| *other*             | Any parameters and/or keywords supported by the EXLST macro. Please see the description of the EXLST macro for details.                                                                                                 |
|                     | Supported parameters and keywords on the EXLST macro are supported on MODCB EXLST as well. Likewise, unsupported parameters and keywords on the EXLST macro are not supported on MODCB EXLST either.                    |
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

### SHOWCB EXLST macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `EXLST=addr`        | required to indicate the EXLST to be queried                                                                                                                                                                            |
| `FIELDS`=           | specifies a list of keywords. Each keyword specified returns a field of 4 or 8 bytes. These return values are stored consecutively in the return area specified in the `AREA`= and `LENGTH`= parameters.                |
| `MF`=               | When omitted, specifies the standard form of the SHOWCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                              |
| `MF=L`              | Specifies the list form of the SHOWCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                             |
| `MF=(L,addr)`       | Specifies the list form of the SHOWCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                              |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the SHOWCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                   |
| `MF=(G,addr)`       | Specifies the generate form of the SHOWCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                       |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

### TESTCB EXLST macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `EXLST=addr`        | required to indicate the EXLST to be tested                                                                                                                                                                             |
| *mod*               | modifier, can optionally be specified after each routine address. Values: A or N for Active or Not-active.                                                                                                              |
|                     | When this modifier is specified, only Equal or Not-Equal condition can be returned.                                                                                                                                     |
|                     | The secondary modifier of L (for Load from Linklib) is not supported.                                                                                                                                                   |
| `MF`=               | When omitted, specifies the standard form of the TESTCB to generate an inline CBMR and an inline call to the CBMR handler.                                                                                              |
| `MF=L`              | Specifies the list form of the TESTCB macro which generates an inline CBMR but no call to the CBMR handler.                                                                                                             |
| `MF=(L,addr)`       | Specifies the list form of the TESTCB macro to generate a remote CBMR at the indicated location. No call to the CBMR handler is generated.                                                                              |
| `MF=(L,addr,label)` | Same as `MF=(L,addr)` but label will be equated to the length of the CBMR.                                                                                                                                              |
| `MF=(E,addr)`       | Specifies the execute form of the TESTCB macro to generate code that will dynamically modify the CBMR at the indicated address according to the parameters specified before calling the CBMR handler.                   |
| `MF=(G,addr)`       | Specifies the generate form of the TESTCB macro to generates code to modify the indicated CBMR as specified by the other parameters and to call the CBMR handler.                                                       |
| `MF=(G,addr,label)` | Same as `MF=(G,addr)` but label will be equated to the length of the CBMR                                                                                                                                               |

### OPEN macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *entry*             | The OPEN macro accepts a list of entries. Each entry consists of two consecutive parameters: an address and an optional list of options.                                                                                |
| *address*           | The address can be specified as an A-type address or as a register. If a register is coded the register number or name must be enclosed in parentheses.                                                                 |
|                     | The address can be either the address of a DCB or the address of an ACB.                                                                                                                                                |
| *options*           | For a DCB options may be encoded according to the z390_File_Access_Method_Guide.                                                                                                                                        |
|                     | For an ACB the options list is ignored and should be coded as an omitted parameter. Any options (e.g. `IN`/`OUT`) are taken from the ACB, not the open parmlist.                                                        |
| `MF`=               | If the MF parameter is omitted an open parmlist is generated inline, plus a call to the open SVC using the parmlist.                                                                                                    |
| `MF=L`              | With MF=L an open parmlist is generated inline                                                                                                                                                                          |
| `MF=(L,addr)`       | Code is generated to construct the open parmlist at run-time, rather than at assembly time, at the indicated address.                                                                                                   |
|                     | If the address is specified within parentheses, it is assumed to indicate a register pointing to the desired address.                                                                                                   |
| `MF=(E,addr)`       | Code is generated to call the open SVC using the parmlist at the indicated address.                                                                                                                                     |
|                     | If the address is specified within parentheses, it is assumed to indicate a register pointing to the desired address.                                                                                                   |

### CLOSE macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| *entry*             | The CLOSE macro accepts a list of entries. Each entry consists of two consecutive parameters: an address and an optional list of options.                                                                               |
| *address*           | The address can be specified as an A-type address or as a register. If a register is coded the register number or name must be enclosed in parentheses.                                                                 |
|                     | The address can be either the address of a DCB or the address of an ACB.                                                                                                                                                |
| *options*           | For a DCB options may be encoded according to the z390_File_Access_Method_Guide.                                                                                                                                        |
|                     | For an ACB the options list is ignored and should be coded as an omitted parameter.                                                                                                                                     |
| `MF`=               | If the MF parameter is omitted a close parmlist is generated inline, plus a call to the close SVC using the parmlist.                                                                                                   |
| `MF=L`              | With MF=L a close parmlist is generated inline                                                                                                                                                                          |
| `MF=(L,addr)`       | Code is generated to construct the close parmlist at run-time, rather than at assembly time, at the indicated address.                                                                                                  |
|                     | If the address is specified within parentheses, it is assumed to indicate a register pointing to the desired address.                                                                                                   |
| `MF=(E,addr)`       | Code is generated to call the close SVC using the parmlist at the indicated address.                                                                                                                                    |
|                     | If the address is specified within parentheses, it is assumed to indicate a register pointing to the desired address.                                                                                                   |

### zACB description

The structure and layout of the zACB are not formally part of the interface and may change in future releases.
Therefore the zACB layout is shown here only for the sake of completeness. Direct access to subfields in the zACB is discouraged.
Use SHOWCB ACB, TESTCB ACB and/or MODCB ACB to inspect, test, and/or modify the zACB's content.
Accessing subfields of the zACB directly may adversely impact portability of your programs.

| Label    | Equate       | Designation | Remarks                                                         |
|----------|--------------|-------------|-----------------------------------------------------------------|
| IFGACB   |              | DSECT       |                                                                 |
| IHAACB   |              | DSECT       | Synonym of IFGACB                                               |
| ACBEYE   |              | CL4         | Eye catcher                                                     |
|          | ACBZACB      | =C'zACB'    | Fixed value                                                     |
| ACBID    |              | XL1         | Identifier                                                      |
|          | ACBIDVAL     | =X'A0'      | ACB                                                             |
| ACBSTYP  |              | XL1         | Subtype                                                         |
| ACBSTYPE |              | XL1         | Synonym of ACBSTYP                                              |
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
| ACBMACR2 |              | BL1         | More option bits                                                |
|          | ACBSKP       | =X'10'      | Skip-sequential access                                          |
|          | ACBMACR2_SKP |             | Synonym of ACBSKP                                               |
|          | ACBRST       | =X'04'      | Reserved                                                        |
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
|          |              | H           | Reserved                                                        |
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
| ACBPFX   |              | AL4         | Prefix Block in buffer                                          |
| ACBXPFX  |              | AL4         | Prefix Block of index; zeroes if no index open                  |

### zEXLST description
The structure and layout of the zEXLST are not formally part of the interface and may change in future releases.
Therefore the zEXLST layout is shown here only for the sake of completeness. Direct access to subfields in the zEXLST is discouraged.
Use SHOWCB EXLST, TESTCB EXLST and/or MODCB EXLST to inspect, test, and/or modify the zEXLST's content.
Accessing subfields of the zEXLST directly may adversely impact portability of your programs.

| Label    | Equate       | Designation | Remarks                                                         |
|----------|--------------|-------------|-----------------------------------------------------------------|
| IFGEXLST |              | DSECT       |                                                                 |
| EXLEYE   |              | CL4         | Eye catcher                                                     |
|          | EXLZLST      | =C'zLST'    | Fixed value                                                     |
| EXLLEN   |              | H           | Length of exit list                                             |
| EXLLEN2  |              |             | Synonym of EXLLEN                                               |
| EXLSTYP  |              | XL1         | Subtype                                                         |
|          | EXLSVSAM     | =X'10'      | zVSAM                                                           |
| EXLEODF  |              | XL1         | Eodad routine flags                                             |
|          | EXLEODS      | =X'80'      | Present                                                         |
|          | EXLEODA      | =X'40'      | Active                                                          |
| EXLLERF  |              | XL1         | Lerad routine flags                                             |
|          | EXLLERS      | =X'80'      | Present                                                         |
|          | EXLLERA      | =X'40'      | Active                                                          |
| EXLSYNF  |              | XL1         | Synad routine flags                                             |
|          | EXLSYNS      | X'80'       | Present                                                         |
|          | EXLSYNA      | X'40'       | Active                                                          |
| EXLEODP  |              | AL4         | EODAD address                                                   |
| EXLLERP  |              | AL4         | SYNAD address                                                   |
| EXLSYNP  |              | AL4         | LERAD address                                                   |

### CBMR description

The structure and layout of the CBMR are not formally part of the interface and may change in future releases.
Therefore the CBMR layout is shown here only for the sake of completeness. Direct access to subfields in the CBMR is discouraged.
Use SHOWCB, TESTCB and/or MODCB to inspect, test, and/or modify the content of an ACB, EXLST, or RPL.
Use the appropriate MF= parameter on any of these macros to modify and/or use a CBMR.

The CBMR consists of three parts: a header, a body, and a tail. The header has a fixed layout.
The body consists of request-dependent fields and a list of verb codes.
The tail contains all the data fields that go with the verb codes. Data fields can be 0, 4, or 8 bytes in length.
Verb codes X'00'-X'5F' have a zero-length data field (i.e. no data field).
Verb codes '60'-X'DF' have a 4-byte data field. Verb codes X'E0'-X'FF' have an 8-byte data field.
All data fields in the tail are allocated consecutively, in the same order as the verbs that define their meaning.

For valid verb values and their data field lengths, please refer to the relevant CBMR body chapter.

| Label    | Equate       | Designation | Remarks                                                         |
|----------|--------------|-------------|-----------------------------------------------------------------|
| CBMR     |              | DSECT       |                                                                 |
| CBMREYE  |              | CL4         | Eye catcher                                                     |
|          | CBMRCBMR     | =C'CBMR'    | Fixed value                                                     |
| CBMRREQ  |              | XL1         | Request type                                                    |
|          | CBMRACB      | =X'01'      | ACB request                                                     |
|          | CBMRXLST     | =X'02'      | EXLST request                                                   |
|          | CBMRRPL      | =X'04'      | RPL request                                                     |
|          | CBMRGEN      | =X'10'      | GENCB request                                                   |
|          | CBMRMOD      | =X'20'      | MODCB request                                                   |
|          | CBMRSHOW     | =X'40'      | SHOWCB request                                                  |
|          | CBMRTEST     | =X'80'      | TESTCB request                                                  |
| CBMRRMOD |              | XL1         | Request modifier                                                |
|          | CBMRLOCB     | =X'01'      | Work area below 16M                                             |
|          | CBMRLOCA     | =X'03'      | Work area below 2G                                              |
|          | CBMROBJD     | =X'10'      | Object=Data                                                     |
|          | CBMROBJI     | =X'20'      | Object=Index                                                    |
| CBMRVRBS |              | AL1         | Nr of verbs in body                                             |
| –        | –            | XL1         | Reserved                                                        |
| CBMRWORK |              | AL4         | Workarea pointer                                                |
| CBMRWLEN |              | AL2         | Workarea length                                                 |
| CBMRSIZE |              | AL2         | CBMR size                                                       |
| CBMRBODY |              | Depends     | List of verb codes                                              |
| CBMRTAIL |              | Depends     | Data fields                                                     |

Remarks:
- `CBMRLOCA`: when both bits are off the CBMR handler will not allocate a work area, rather it will use `CBMRWORK`/`CBMRWLEN` instead.
- `CBMRVRBS`: The number of verbs in the body of the CBMR. This number is always a multiple of 4.
- `CBMRSIZE`: length in bytes of CBMR header and tail combined. Tail follows header directly

### CBMR description – body for ACB

The CBMR header is described in the addenda, [CBMR description](#CBMR-description).

The CBMR tail for ACB is applicable only when the CBMR header's CBMRACB bit is on.
It's length is determined by the `CBMRVRBS` fields in the CBMR header. This is always a multiple of 4
to allow word alignment of the tail area that directly follows the body section.
Any unused/unneeded verb bytes should be set to zero, indicating a no-operation.

The body is directly followed by the tail, if applicable.
It contains a data field of 0, 4, or 8 bytes for each verb code in the body, in the same sequence.
The starting point of the tail can be found by adding the `CBMRVRBS` value to the end of the CBMR header.
Its length can be deduced form the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

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

### CBMR description – body for EXLST

The CBMR header is described in the addenda, [CBMR description](#CBMR-description).

The CBMR tail for EXLST is applicable only when the CBMR header's `CBMRXLST` bit is on.
It's length is determined by the `CBMRVRBS` fields in the CBMR header.
This is always a multiple of 4 to allow word alignment of the tail area that directly follows the body section.
Any unused/unneeded verb bytes should be set to zero, indicating a no-operation.

The body is directly followed by the tail, if applicable.
It contains a data field of 0, 4, or 8 bytes for each verb code in the body, in the same sequence.
The starting point of the tail can be found by adding the `CBMRVRBS` value to the end of the CBMR header.
Its length can be deduced form the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

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

## RPL-based interfaces

### RPL macro parameters

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

| Keyword             | Usage and implementation in zVSAM                                                                                                                                                                                       |
|---------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `AM`=               | Optional parameter. AM=VSAM is the default. No other values are supported.
| `ACB`=              | Pointer to an open ACB that represents the clusteer to be accessed
| `AREA`=             | Record area. In Move mode reading a record implies moving the record into this area. In locate mode a pointer to the record is moved into the area instead.
| `AREALEN`=          | Length of record area
| `ARG`=              | Pointer to search argument. This is a key, a relative record number, or a RBA.
| `KEYLEN`=           | Length of key value specified in `ARG` when a generic key search is requested
| `ECB`=              | Pointer to ECB. Used with Asynchronous requests.
| `MSGAREA`=          | Pointer to a message area where error information may be returned
| `MSGLEN`=           | Length of messagea area
| `NXTRPL`=           | Pointer to next RPL in the chain. RPLs can be chained together to request a series of operations in a single call to zVSAM.
| `RECLEN`=           | Record length. Required when updating or adding records.
|                     | When updating a record that has not changed its length, the parameter can be omitted if the immediately preceding operation on the RPL was the read for the record being updated.
| `OPTCD`=            | List of keywords specifying how the request is to be handled. Please see below for the list of supported keywords and their meaning.
| - `ADR`/`KEY`       | Mutually exclusive `OPTCD` keywords indicating whether the cluster is to be accessed by address or by key.
| - `ADR`             | can be used only with ESDS to access records by RBA or XRBA.
| - `KEY`             | can be used with KSDS to access records by key, or with RRDS to access records by relative record number.
| - `DIR`/`SKP`/`SEQ` | Mutually exclusive `OPTCD` keywords on the RPL macro. When none of the three is specified, a default of `SEQ` will be used.
| - `DIR`             | `OPTCD` keyword indicating that the cluster is to be processed directly.
|                     | can be used with ESDS, KSDS or RRDS to access data randomly
| - `SEQ`             | `OPTCD` keyword indicating that the cluster is to be processed sequentially.
|                     | can be used with ESDS, KSDS, or RRDS to access data sequentially
| - `SKP`             | `OPTCD` keyword to request skip-sequential access.
|                     | can be used with KSDS to randomly position the file to a specific key prior to sequential access
|                     | can be used with RRDS to randomly position the file to a specific RRN prior to sequential access
| - `ARD`/`LRD`       | Mutually exclusive `OPTCD` keywords indicating whether the cluster is to be accessed as specified by the user/current position, or just the very last record.
| - `FWD`/`BWD`       | Mutually exclusive `OPTCD` keywords indicating whether the cluster is to be accessed in a forward or backward direction
| - `SYN`/`ASY`       | Synchronous or asynchronous handling of the request.
| - `NUP`/`UPD`/`NSP` | Mutually exclusive `OPTCD` keywords indicating whether or not the record is to be locked for update. Updates are allowed if the cluster was opened with the `OUT` option specified.
|                     | `NSP` is used only with `OPTCD`=`DIR`: retain file position, to enable sequential access from this point.
| - `KEQ`/`KGE`       | Mutually exclusive `OPTCD` keywords indicating whether the key has to match exactly (`KEQ`) or – if an exact match cannot be found – the next higher value is acceptable too.
| - `FKS`/`GEN`       | Mutually exclusive `OPTCD` keywords indicating whether we're doing a full key search, or we're specifying only a partial key value.
|                     | In the latter case `KEYLEN` has to be specified to indicate how many key positions we're specifying.
| - `MVE`/`LOC`       | Mutually exclusive OPTCD keywords indicating whether we're working in move mode (zVSAM moves record between user record buffer and zVSAM buffer)
|                     | or in locate mode (record is not moved, data are processed in the zVSAM buffer, zVSAM provides a pointer only)
| - `RBA`/`XRBA`      | Mutually exclusive `OPTCD` keywords indicating whether we're using 4-byte RBA values or 8-byte extended RBA values.

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

### zRPL description

The structure and layout of the zRPL are not formally part of the interface and may change in future releases.
Therefore the zRPL layout is shown here only for the sake of completeness. Direct access to subfields in the zRPL is discouraged.
Use SHOWCB RPL, TESTCB RPL and/or MODCB RPL to inspect, test, and/or modify the zRPL's content.
Accessing subfields of the zRPL directly may adversely impact portability of your programs.

| Label     | Equate       | Designation | Remarks                                                         |
|-----------|--------------|-------------|-----------------------------------------------------------------|
| IFGRPL    |              | DSECT       |    
| IHARPL    |              |             | Synonym of IFGRPL 
| RPLEYE    |              | CL4         | Eye catcher
|           | RPLZRPL      | =C'zRPL'    | Fixed value
| RPLACB    |              | AL4         | Pointer to ACB
| RPLAREA   |              | AL4         | Pointer to record area
| RPLAREAL  |              | XL4         | Length of record area
| RPLARG    |              | AL4         | Pointer to argument
| RPLECB    |              | AL4         | Pointer to ECB
| RPLMSGAR  |              | AL4         | Pointer to message area
| RPLRPL    |              | AL4         | Pointer to chained RPL
| RPLRECLN  |              | XL4         | Length of record read or of record to be written
| RPLMSGLN  |              | XL2         | Length of message area
| RPLKEYLN  |              | XL1         | Key length
|           |              | XL1         | Reserved
| RPLOPTCD  |              | 0XL2        | Option codes
| RPLOPTCD1 |              | XL1         | Option byte 1
|           | RPLOPT_KEY   | =X'80'      | 0: OPTCD=ADR 1: OPTCD=KEY
|           | RPLOPT_SEQ   | =X'40'      | 0: OPTCD=DIR 1: OPTCD=SEQ
|           | RPLOPT_SKP   | =X'20'      | 0: OPTCD=SEQ/DIR 1: OPTCD=SKP
|           | RPLOPT_ARD   | =X'10'      | 0: OPTCD=LRD 1: OPTCD=ARD
|           | RPLOPT_FWD   | =X'08'      | 0: OPTCD=BWD 1: OPTCD=FWD
|           | RPLOPT_SYN   | =X'04'      | 0: OPTCD=ASY 1: OPTCD=SYN
|           | RPLOPT_NUP   | =X'02'      | 0: OPTCD=UPD 1: OPTCD=NUP
|           | RPLOPT_NSP   | =X'01'      | 0: OPTCD=NUP/UPD 1: OPTCD=NSP
| RPLOPTCD2 |              | XL1         | Option byte 2
|           | RPLOPT_KEQ   | =X'80'      | 0: OPTCD=KGE 1: OPTCD=KEQ
|           | RPLOPT_FKS   | =X'40'      | 0: OPTCD=GEN 1: OPTCD=FKS
|           | RPLOPT_MVE   | =X'20'      | 0: OPTCD=LOC 1: OPTCD=MVE
|           | RPLOPT_RBA   | =X'10'      | 0: OPTCD=XRBA 1: OPTCD=RBA
| RPLID     | ?            | XL1         | Identifier
|           | ??           |             | Fixed value for RPL
| RPLSTYPE  | ?            | XL1         | RPL Subtype
|           | ??           |             | Fixed value for VSAM
| RPLFEEDB  |              | XL4         | Feedback code
|           | ???          |             | Which codes do we support?
| RPLNEXT   |              | A           | Ptr to next RPL
| RPLLXRBBA |              | XL8         | XRBA of last record
| RPLCXRBA  |              | XL8         | XRBA of current record
| RPLOPENC  |              | F           | Unique ACB Open count
| RPLFLAG   |              | 0XL4        | Processing flags
| RPLFLG1   |              | XL1         | Processing flags
|           | RPLF1GOK     | =X'80'      | Previous GET ok
|           | RPLF1GNF     | =X'40'      | Previous GET not found
| RPLFLG2   |              | XL1         | Processing flags
| RPLFLG3   |              | XL1         | Processing flags
| RPLFLG4   |              | XL1         | Processing flags
|           | RPLEND       | \*          | End of RPL marker
|           | RPLLEN       | \*-RPLEYE   | Length of RPL

Review notes:
- `RPLID` and next entry - Reason for ? in column 2 not clear. Need to double check.
- `RPLSTYPE` and next entry - Reason for ? in column 2 not clear. Need to double check.
- `RPLFEEDB` and next entry - Determine feedback codes we need to support and document them here.
- `RPLNEXT` - Describe purpose of this chain.

### CBMR description – body for RPL

The CBMR header is described in the addenda, [CBMR description](#CBMR-description).

The CBMR tail for RPL is applicable only when the CBMR header's `CBMRRPL` bit is on.
Its length is determined by the `CBMRVRBS` fields in the CBMR header.
This is always a multiple of 4 to allow word alignment of the tail area that directly follows the body section.
Any unused/unneeded verb bytes should be set to zero, indicating a no-operation.

The body is directly followed by the tail, if applicable.
It contains a data field of 0, 4, or 8 bytes for each verb code in the body, in the same sequence.
The starting point of the tail can be found by adding the `CBMRVRBS` value to the end of the CBMR header.
Its length can be deduced form the `CBMRSIZE` field, by subtracting both the header length and the `CBMRVRBS` field.

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
