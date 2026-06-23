# zVSAM v1 User Guide

## Overview

z390 VSAM support enables z390 application assembler programs running on
Windows or Linux to access fixed length or variable length record data files (data sets)
in key sequence (`KSDS`) which is the default, relative record sequence (`RRDS`/`VRRDS`),
entry sequence (`ESDS`), or linear (`LDS`) control interval access.

z390 VSAM file access from assembler programs is performed using VSAM macros
such as `GENCB`, `OPEN`, `CLOSE`, `POINT`, `GET`, `PUT`, `MODCB`, `SHOWCB`, or
`TESTCB` referencing `ACB` and `RPL` control blocks which define the type of data
access to be performed.

The advantages of z390 VSAM over QSAM and BSAM access methods are:
1. Z390 VSAM supports random and skip sequential access to fixed and
   variable length records by key index fields, by relative record number, or
   by 32 bit or 64 bit relative byte address.
2. Z390 VSAM supports sequential and random access to both fixed length
   and variable length records up to 2 GB.
3. Z390 VSAM supports any number of alternate key indexes with offsets and
   lengths up to 2 GB. Note only random and skip sequential primary key
   browse currently supported.
4. VSAM maximizes performance by utilizing a VSAM Cache Buffering
   (`VCB`) option to buffer all I/O for records less than 4096. Writes are
   always performed immediately where as reads may come from file or
   cache. The cache buffers are reused on least recently used basis.

z390 VSAM files are defined in VSAM Catalog Definition Tables (`VCDT`s) which
are defined using `DEFINE` macro calls to define any number of base clusters,
alternate indexes, and paths. The macro calls are assembled and linked into
loadable table. When an `ACB` is opened, the DDNAME points to the `VCDT` catalog
to be accessed. An optional dot suffix defining the name of the catalog entry to open
can be specified or the name of the `ACB` will be used as default entry name. The
`DEFINE` macro checks all the entries for consistency and then passes global data to
`ZDEFINE` macro to generate the table if no errors are found. See the copybook
`ZDEFINE.CPY` for the global `VCDT` data definitions. For examples see
`vsam\demo\DEMOCAT.MLC` which defines all the demo VSAM data sets, and see
`vsam\test\TESTCAT.MLC` which defines all the regression test VSAM data sets.

## IDCAMS type VSAM utilities

The `IDCAMS` utility on the mainframe is used to create VSAM catalog base cluster,
alternate index, and path definitions. The following z390 tools are used in place of
`IDCAMS` utility:
1. `DEFINE` macro to define base cluster, alternate index, and path entries in
   a z390 VSAM Catalog Definition Table (`VCDT`)
2. `REPRO` assembler utility to load or unload z390 VSAM cluster files from
   or to QSAM file. When VSAM files are loaded or unloaded, deleted
   records are removed and the base cluster data records are stored in
   physical primary key sequence. Note inserts and updates can cause
   records to be added to the end of the base cluster leaving dead space and
   causing the file to grow so periodic reorganizations may be necessary.

The following `DEFINE` macro calls can be used to create
a z390 VSAM Catalog Definition Table:

```lang[asm]
*
* DEFINE VCDT CATALOG FOR VSAM DEMOS IN vsam\demo
*
         DEFINE CATALOG,NAME=DEMOCAT
         DEFINE CLUSTER,NAME=ESF1,                                     X
               RECORDSIZE=80,INDEX=NONINDEXED
         DEFINE CLUSTER,NAME=ESV1,RECORDSIZE=(200,300),                X
               INDEX=NONINDEXED
         DEFINE CLUSTER,NAME=RRF1,                                     X
               RECORDSIZE=80,INDEX=NUMBERED
         DEFINE CLUSTER,NAME=RRV1,                                     X
               RECORDSIZE=(200,400),INDEX=NUMBERED
         DEFINE CLUSTER,NAME=KSF1NAME,                                 X
               RECORDSIZE=70,KEYS=(20,0)
         DEFINE ALTERNATEINDEX,NAME=KSF1ADDR,                          X
               RELATE=KSF1NAME,KEYS=(20,20)
         DEFINE ALTERNATEINDEX,NAME=KSF1CITY,                          X
               RELATE=KSF1NAME,KEYS=(20,40)
         DEFINE ALTERNATEINDEX,NAME=KSF1STAT,                          X
               RELATE=KSF1NAME,KEYS=(5,60)
         DEFINE ALTERNATEINDEX,NAME=KSF1ZIP,                           X
               RELATE=KSF1NAME,KEYS=(5,65)
         DEFINE PATH,NAME=NAMELIST,ENTRY=KSF1NAME,UPDATE=NOUPDATE
         DEFINE PATH,NAME=ADDRLIST,ENTRY=KSF1ADDR,UPDATE=NOUPDATE
         DEFINE PATH,NAME=CITYLIST,ENTRY=KSF1CITY,UPDATE=NOUPDATE
         DEFINE PATH,NAME=STATLIST,ENTRY=KSF1STAT,UPDATE=NOUPDATE
         DEFINE PATH,NAME=ZIPLIST,ENTRY=KSF1ZIP,UPDATE=NOUPDATE
         DEFINE CLUSTER,NAME=ESF1CI2K,CONTROLINTERVALSIZE=2048,        X
               RECORDSIZE=200,INDEX=NONINDEXED
         DEFINE CLUSTER,NAME=LDS1CI2K,CONTROLINTERVALSIZE=2048,        X
               INDEX=LINEAR
         DEFINE END
         END
```

The above macros can be assembled and linked into loadable table that is
referenced by the `ACB` `DDNAME=` parameter when opening a VSAM file. The file
specification can have path and must have the file name `DEMOCAT`. Optionally a
specific catalog entry can be specified such as `DEMOCAT.ESF1`. If no entry name
is specified, the name of the `ACB` is used to search catalog for entry. At open time
`DCB`s are dynamically allocated for the physical data file and any index files
required.

An `IDCAMS` type utility `REPRO.390` can be used to load or unload VSAM data
files to or from QSAM using `INFILE` and `OUTFILE` `DDNAME`s. Note `REPRO`
can only be used if the record length is less than 32760 due to QSAM limits.
VSAM files must be periodically unloaded and reloaded to free dead space created by
updating records with different lengths. Reloading also optimizes file structure by
moving inserted KSDS records from insert balanced tree structures to direct access
primary indexes structures.

## VSAM macros for use within z390 assembler

1. `ACB` – Access Control Block defining access method and VSAM cluster files
2. `ACBD` – `ACB` `IHAACB` DSECT
3. `CLOSE` – Close VSAM cluster files
4. `DEFINE` – define VSAM catalog entries and load global data tables
5. `ENDREQ` – end request to release record locking for update
6. `ERASE` – delete VSAM record (not supported yet)
7. `GENCB` – generate `ACB` or `RPL` control block (not supported yet)
8. `GET` – Get VSAM record
9. `MODCB` – modify `ACB` or `RPL` control block field. Currently `AREA` and `RECLEN` for `RPL` are supported. See `linklib\REPRO.MLC` for example.
10. `OPEN` – Open VSAM cluster files
11. `POINT` – Start sequential access at specified key position (not supported yet)
12. `PUT` – Put VSAM record
13. `RPL` – Request Processing List
14. `RPLD` – `RPL` `IHARPL` DSECT
15. `SHOWCB` – move `ACB` or `RPL` field to user area. Currently `AREA`, `RECLEN`, `FDBK`, `RBA`, and `XRBA` are supported for `RPL`. See `linklib\REPRO.MLC` and `vsam\test` regression test programs.
16. `TESTCB` – test value of field in `ACB` or `RPL`. Currently `RECLEN` and `FDBK` fields in `RPL` are supported. See `linklib\REPRO.MLC` for example usage of `TESTCB` to check `FDBK` reason code for logical end or data error.
17. `VCDTD` – VSAM Catalog Definition Table containing the following DSECTS:
    1. `IHAVCDT` – VSAM Catalog DSECT with pointers to `VCLR`, `VAIX`, and `VPTH` (Applications should only use `SHOWCB`, `TESTCB`, and `MODCB` for compatibility. See `linklib\REPRO.MLC` for example `VCDTD` usage).
    2. `IHAVCLR` – VSAM Base Cluster entry
    3. `IHAVAIX` – VSAM Alternate Index entry
    4. `IHAVPTH` – VSAM Path entry
18. `ZDEFINE.MAC` – generate VSAM catalog from global data tables
19. `ZDEFINE.CPY` – copybook with global data tables for catalog
20. `ZGENACB` – create `ACB` control block for `GENCB` or `ACB`
21. `ZGENMACF` – create `ACB` `MACRF` field for `ZGENACB` or `MODCB`
22. `ZGENOPTD` – create `RPL` `OPTCD` field for `ZGENRPL` or `MODCB`
23. `ZGENRPL` – create `RPL` control block for `GENCB` or `RPL`
24. `ZMODCB` – generate `MODCB` code
25. `ZSHOWCB` – generate `SHOWCB` code
26. `ZTESTCB` – generate `TESTCB` code

## VSAM Cluster Definition Table (`VCDT`) and Z390 VSAM physical files

A z390 VSAM catalog is created by assembling calls to `DEFINE` macro. When an
`ACB` is opened, the `DDNAME` points to loadable catalog file and either user
specified catalog entry name or `ACB` name is used to find specific VSAM cluster to
be opened. The following `DCB`s for physical files are dynamically allocated at `ACB`
open time using base cluster, alternate index, and path information from catalog:
1. Base cluster `NAME.VES` – ESDS data file containing all data records.
   Variable length records are preceded by 4 byte length which is not
   included in logical record length. The individual fixed or variable length
   records can be up to 2 GB long. The maximum `ESDS` file size is `2**63` or
   about `10**18`. The user can override default path and name of `VES` file
   using `VESDSN=` parm on `DEFINE CLUSTER` macro.
2. Base cluster `NAME.VX0` – `VRRDS`/`ESDS` primary index file containing 8
   byte `RBA` addresses to each record in the base cluster `VES` data file.
   `ESDS`, fixed `RRDS`, and fixed `LDS` file types do not use `VX0`. `XRBA`
   index values are +1 with 0 indicating not written yet. Negative values are
   `KSDS` pointers to inserted record structures. The user can override
   default path and name of `VX0` file using `VX0DSN=` parm on `DEFINE CLUSTER`
   macro.
3. Alternate index `NAME.VXN` – `KSDS` alternate index files containing 8
   byte `RBA` addresses plus alternate key field to each index entry in the
   `VX0` primary index file. Any number of alternate keys can be defined
   with lengths and offsets up to 2 GB. The default `UPGRADE` mode for
   each alternate index can be changed either by `UPGRADE` parm on
   alternate index definition or by turning off all alternate index ugrades via
   use of `DEFINE PATH` with `UPDATE=NOUPDATE` option. The user can
   override the default path and name of `VXN` file using `VXNDSN=` parm
   on `DEFINE ALTERNATEINDEX` macro.
