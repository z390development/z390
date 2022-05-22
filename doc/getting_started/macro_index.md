# Mainframe OS compatible macros

z390 provides some mainframe OS compatible macros (MVS, Z/OS, Z/VM) for use in your programs.

SVC calls are not compatible. Z390 implements its own OS services which are different to the mainframe.

If your program needs to access OS services, then your programs should use the provided OS based macros.


| macro       | description                                                                                                                                                                           |
|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ABEND       | abort program with specified code                                                                                                                                                     |
| ACB         | VSAM access control block                                                                                                                                                             |
| ACBD[^1]    | VSAM ACB DSECT                                                                                                                                                                        |
| BLDL        | Search for one or more 390 load modules in memory or in SYS390 load library directories.                                                                                              |
| CALL        | Call subroutine using standard linkage                                                                                                                                                |
| CHECK       | Check completion of READ/WRITE                                                                                                                                                        |
| CLOSE       | Close DCB or ACB file                                                                                                                                                                 |
| CMDPROC     | Control command processing tasks                                                                                                                                                      |
| CVTD        | DSECT of limited CVT fields supported                                                                                                                                                 |
| DCB         | Data control block for file (supports 31 bit DCBLRECLF field as extension to DCBLRECL.   See SUPERZAP utility for example of how to read or write up to 2 GB file with 1 I/O operation.) |
| DCBD        | DSECT of limited DCB fields                                                                                                                                                           |
| DCBE        | Define EODAD and SYNAD for DCB                                                                                                                                                        |
| DECBD[^1]   | DSECT of limited DECB fields                                                                                                                                                          |
| DEFINE[^1]  | Define VSAM Cluster Definition Table                                                                                                                                                  |
| DEQ[^1]     | Release a resource lock set by ENQ                                                                                                                                                    |
| DELETE      | Freemain 390 load module if use count 0                                                                                                                                               |
| ENQ[^1]     | request lock on a resource                                                                                                                                                            |
| ERASE       | VSAM delete record                                                                                                                                                                    |
| ESPIE       | set program interruption handler                                                                                                                                                      |
| ESTAE       | set program abend handler                                                                                                                                                             |
| FREEMAIN    | release memory area                                                                                                                                                                   |
| GENCB       | generate VSAM ACB or RPL                                                                                                                                                              |
| GET         | read record from QSAM or VSAM file                                                                                                                                                    |
| GETENV      | get value of environment variable                                                                                                                                                     |
| GETMAIN     | allocate memory area                                                                                                                                                                  |
| IHAEPIE[^1] | DSECT for ESPIE exits                                                                                                                                                                 |
| IHASDWA[^1] |  DSECT for SDWA ESTAE exits                                                                                                                                                           |
| LINK        | load and execute 390 load module                                                                                                                                                      |
| LOAD        | load 390 load module                                                                                                                                                                  |
| MODCB       | modify ACB or RPL control block field                                                                                                                                                 |
| OPEN        | open DCB or ACB file                                                                                                                                                                  |
| POINT       | position to relative record or RBA in file                                                                                                                                            |
| POST        | post ECB as completed                                                                                                                                                                 |
| PUT         | write record to QSAM or VSAM file                                                                                                                                                     |
| READ        | read block from file at current position                                                                                                                                              |
| RETURN      | restore saved registers and return                                                                                                                                                    |
| RPL         | VSAM request parameter list                                                                                                                                                           |
| RPLD[^1]    | VSAM RPL DSECT                                                                                                                                                                        |
| SAVE        | save specified registers                                                                                                                                                              |
| SETRP[^1]   | set register options for ESTAE exits                                                                                                                                                  |
| SHOWCB      | move ACB or RPL field to user area                                                                                                                                                    |
| SNAP        | dump selected area of memory and/or dump file information from TIOT/DCB’s, general and floating point registers, program information from CDE, and memory allocation totals           |
| STIMER      | wait specified interval of time                                                                                                                                                       |
| STORAGE     | obtain or release main storage                                                                                                                                                        |
| TESTCB      | test current value of ACB or RPL field                                                                                                                                                |
| TGET        | read from GUI TN3270 interface                                                                                                                                                        |
| TIME        | get time and date in requested format                                                                                                                                                 |
| TTIMER      | test and/or cancel STIMER event                                                                                                                                                       |
| TPUT        | write to GUI TN3270 display                                                                                                                                                           |
| VCDTD[^1]   | VSAM Catalog Definition Table DSECT                                                                                                                                                   |
| WAIT        | wait for 1 or more ECB’s to be posted                                                                                                                                                 |
| WRITE       | write block to file at current position                                                                                                                                               |
| WTO         | write text message to operator console                                                                                                                                                |
| WTOR        | write to operator with reply                                                                                                                                                          |
| XCTL        | transfer control and delete prior pgm                                                                                                                                                 |
| XLATE       | translate area to/from EBCDIC/ASCII                                                                                                                                                   |

[^1]:Requires documentation.

