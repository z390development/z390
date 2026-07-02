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
