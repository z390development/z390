# zVSAM V2 - Addenda

This document describes Macro parameters and control blocks used in the zVSAM V2 implementation.

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
| - `NRS`/`RST`       | NoReSet / ReSeT processing                                                                                                                                                                                              |
| - `NRS`             | After OPEN the data in the dataset are available                                                                                                                                                                        |
| - `RST`             | During OPEN the high water mark is reset effectively deleting all the data in the dataset                                                                                                                               |

