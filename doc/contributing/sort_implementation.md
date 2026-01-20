# zCOBOL SORT Implementation

*Implemented by Zane Hambly, December 2025*

## PR: Implement COBOL SORT per FIPS PUB 21-2

**Closes 17-year TODO** - The SORT.MAC stub has been pending implementation since 2008.

### Changes

- **4 new files**: `ZCSD.MAC`, `ZC_SD_FIND.MAC`, `GEN_SORT.MAC`, `ZC_SD.CPY`
- **3 modified macros**: `SORT.MAC`, `RELEASE.MAC`, `RETURN.MAC`
- **Test suite**: 2 test programmes with input data (LINE SEQUENTIAL and fixed-format)
- **Documentation**: This file, citing the federal standard FIPS PUB 21-2

All tests green ✓

---

This document describes the implementation of the COBOL SORT statement in zCOBOL.

## Source Reference

This implementation follows **FIPS PUB 21-2** "COBOL Programming Language" which 
incorporates **ANSI X3.23-1985** and **ISO 1989-1985**, the official COBOL 85 standard.

The SORT statement specification can be found in **Section XI (Sort-Merge Module)** 
of the standard. This document is publicly available from the NIST archives and can 
be used to verify the implementation against the authoritative specification.

## Supported Syntax

### SORT Statement

```cobol
SORT file-name-1
     ON {ASCENDING|DESCENDING} KEY {data-name-1}...
     [WITH DUPLICATES IN ORDER]
     [COLLATING SEQUENCE IS alphabet-name-1]
     {INPUT PROCEDURE IS proc-1 [THRU proc-2] | USING {file-name-2}...}
     {OUTPUT PROCEDURE IS proc-3 [THRU proc-4] | GIVING {file-name-3}...}
```

### SD (Sort Description) Entry

```cobol
SD sort-file-name
    [RECORD CONTAINS integer CHARACTERS]
    [DATA RECORD IS record-name].
01 sort-record-name.
    05 sort-key-1    PIC X(10).
    05 sort-data     PIC X(70).
```

### RELEASE Statement

Used within an INPUT PROCEDURE to pass records to the sort:

```cobol
RELEASE record-name-1 [FROM identifier-1]
```

### RETURN Statement

Used within an OUTPUT PROCEDURE to retrieve sorted records:

```cobol
RETURN file-name-1 RECORD [INTO identifier-1]
    AT END imperative-statement-1
    [NOT AT END imperative-statement-2]
[END-RETURN]
```

## Implementation Details

The zCOBOL SORT implementation generates code that uses the existing z390 ZSORT 
macro infrastructure (SVC X'A1'). This provides a consistent and tested sorting 
mechanism.

### Files Modified/Created

| File | Purpose |
|------|---------|
| `zcobol/mac/ZCSD.MAC` | Parses SD entries and stores sort file metadata |
| `zcobol/mac/SORT.MAC` | Parses SORT statement and generates sort code |
| `zcobol/mac/GEN_SORT.MAC` | Generates HLASM code using ZSORT |
| `zcobol/mac/RELEASE.MAC` | Implements RELEASE for INPUT PROCEDURE |
| `zcobol/mac/RETURN.MAC` | Extended to support SORT RETURN |
| `zcobol/mac/ZC_SD_FIND.MAC` | Helper to look up sort files by name |
| `zcobol/cpy/ZC_SD.CPY` | New file for sort-specific global variables |

Note: The original `zcobol/cpy/ZC_WS.CPY` was intentionally left unchanged to avoid
potential compatibility issues with existing z390 infrastructure.

### Key Type Mapping

COBOL PIC types are mapped to ZSORT key types as follows:

| COBOL PIC Type | ZSORT Type | Description |
|----------------|------------|-------------|
| X, A | CH | EBCDIC character |
| 9 (DISPLAY) | ZD | Zoned decimal |
| 9 COMP-3 | PD | Packed decimal |
| 9 COMP (H/F/G) | FI | Signed binary |

## Current Limitations

The following optional features from the COBOL 85 standard are not yet implemented:

- `WITH DUPLICATES IN ORDER` phrase
- `COLLATING SEQUENCE IS` phrase  
- Multiple USING files
- Multiple GIVING files
- MERGE statement

These limitations are documented in the generated code as warning messages (MNOTE 4).

## File Encoding Note

z390 uses **EBCDIC** encoding internally for standard file I/O. Input and output files 
used with SORT must be in EBCDIC format, not ASCII. This is consistent with mainframe 
behaviour where EBCDIC is the native character set.

For LINE SEQUENTIAL files (text files), z390 handles ASCII-to-EBCDIC translation 
automatically. However, for fixed-record files without ORGANIZATION IS LINE SEQUENTIAL, 
the files must be pre-converted to EBCDIC.

## Test Programme

A test programme is provided at `zcobol/test/TESTSRT1.CBL` demonstrating basic 
SORT usage with USING/GIVING files.

## Example Usage

### Simple File-to-File Sort

```cobol
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC.
           05  INPUT-KEY     PIC X(10).
           05  INPUT-DATA    PIC X(70).
       FD  OUTPUT-FILE.
       01  OUTPUT-REC.
           05  OUTPUT-KEY    PIC X(10).
           05  OUTPUT-DATA   PIC X(70).
       SD  SORT-FILE.
       01  SORT-REC.
           05  SORT-KEY      PIC X(10).
           05  SORT-DATA     PIC X(70).
       
       PROCEDURE DIVISION.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               USING INPUT-FILE
               GIVING OUTPUT-FILE.
```

### Sort with INPUT/OUTPUT PROCEDURE

```cobol
       PROCEDURE DIVISION.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY
               INPUT PROCEDURE IS INPUT-PROC
               OUTPUT PROCEDURE IS OUTPUT-PROC.
           STOP RUN.
       
       INPUT-PROC.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO SORT-REC
               AT END GO TO INPUT-DONE.
           PERFORM UNTIL END-OF-FILE
               RELEASE SORT-REC
               READ INPUT-FILE INTO SORT-REC
                   AT END SET END-OF-FILE TO TRUE
           END-PERFORM.
       INPUT-DONE.
           CLOSE INPUT-FILE.
       
       OUTPUT-PROC.
           OPEN OUTPUT OUTPUT-FILE.
           RETURN SORT-FILE RECORD INTO OUTPUT-REC
               AT END GO TO OUTPUT-DONE.
           PERFORM UNTIL END-OF-SORT
               WRITE OUTPUT-REC
               RETURN SORT-FILE RECORD INTO OUTPUT-REC
                   AT END SET END-OF-SORT TO TRUE
           END-PERFORM.
       OUTPUT-DONE.
           CLOSE OUTPUT-FILE.
```

## Contributing

When extending this implementation, please ensure changes comply with FIPS PUB 21-2.
The standard document should be consulted for any ambiguous behaviour. If you need this document please feel free to reach out :-) 

