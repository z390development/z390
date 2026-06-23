# zCics - introduction

## z390 EXEC CICS Assembler and COBOL Support

Z390 CICS assembler and COBOL application transaction manager support
was developed by Melvyn Maltz and is distributed as open source in the
z390\cics directory.

The z390 CICS compatible transaction manager includes a server supporting
multiple local and remote sessions connected via a TCP/IP network. The
transaction server can be started by running `cics\bat\Z390CICG.BAT`
or `cics/bash/z390cicg` which also starts local terminal sessions.

Remote clients can be started by running `cics\bat\Z390KCPL.BAT`
or `cics/bash/z390kcpl` for multiple remotes.
To start a single remote for tracing and testing, use
`cics\bat\Z390KCPR.BAT` or `cics/bash/z390kcpr`.

For SOA client server messaging the z390 TCPIO macro and
the associated SVC for runtime support are used to request zCICS
functions from the server and to pass data and status back
to the client on a TCP/IP network.

For release highlights, please see the [zCics History](zcics_history.md)
overview.

## z390 Support

Programs written in COBOL are converted into macro statements.

The mz390 macro processor parses `EXEC CICS` statements and maps them
into standard assembler macro calls. All the `EXEC CICS` support macros are in
the cics directory.

The mz390 macro processor replaces standard `DFHRESP(error code)` and
`DFHVALUE(cvda)` references with the corresponding standard fullword literal
constant.

The ez390 runtime GUAM user interface dialog is used by each client to
support TN3270 screen and keyboard I/O via TPUT with `FULLSCR` option and
TGET with `ASIS` option for TN3270 standard data streams.




