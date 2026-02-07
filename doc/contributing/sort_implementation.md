# zCOBOL SORT Implementation

**Author:** Zane Hambly
**Date:** December 2025
**Standard:** FIPS PUB 21-2 (ANSI X3.23-1985, ISO 1989-1985), Section XI

---

## Abstract

`SORT.MAC` has said `MNOTE 8,'SORT NOT SUPPORTED YET'` since November 2008.
Seventeen years is a long time to leave records unsorted. The stub has been
replaced with a working implementation that parses the COBOL SORT statement
and generates HLASM targeting the existing z390 ZSORT infrastructure
(SVC X'A1').

Both USING/GIVING and INPUT/OUTPUT PROCEDURE forms are supported.
Multi-key sorts with mixed ASCENDING/DESCENDING work. MERGE does not,
but Rome wasn't built in a day and neither was COBOL.

---

## How It Works

The SORT macro parses the statement at assembly time, looks up the SD
(Sort Description) entry to find record layout and key positions, then
calls `GEN_SORT.MAC` to emit the actual HLASM. The generated code
delegates to ZSORT, which does the heavy lifting via SVC X'A1'.

For INPUT/OUTPUT PROCEDURE, the macro generates PERFORM-style branch
logic around the user's procedure sections. RELEASE writes records into
the sort, RETURN reads them back out.

```
COBOL source --> SORT.MAC (parse) --> ZC_SD_FIND (lookup) --> GEN_SORT (emit) --> ZSORT (SVC)
```

## Files

| File                          | What It Does                                         |
|-------------------------------|------------------------------------------------------|
| `zcobol/mac/SORT.MAC`        | Parses SORT statement - replaces the 2008 stub       |
| `zcobol/mac/GEN_SORT.MAC`    | Generates HLASM code targeting ZSORT                 |
| `zcobol/mac/ZCSD.MAC`        | Parses SD entries, stores sort file metadata          |
| `zcobol/mac/ZC_SD_FIND.MAC`  | Looks up sort files by name                          |
| `zcobol/cpy/ZC_SD.CPY`       | Global variables for sort file tracking              |
| `zcobol/mac/RELEASE.MAC`     | Implements RELEASE for INPUT PROCEDURE               |
| `zcobol/mac/RETURN.MAC`      | Extended to support SORT RETURN                      |

`ZC_WS.CPY` was intentionally left alone. Touching shared copybooks to add
sort variables felt like defusing a bomb with a hammer.

## Key Type Mapping

COBOL PIC types map to ZSORT key types:

| COBOL PIC          | ZSORT | Notes                  |
|--------------------|-------|------------------------|
| X, A               | CH    | EBCDIC character       |
| 9 (DISPLAY)        | ZD    | Zoned decimal          |
| 9 COMP-3           | PD    | Packed decimal         |
| 9 COMP (H/F/G)     | FI    | Signed binary          |

## File Encoding

SORT expects EBCDIC internally. For LINE SEQUENTIAL files, z390 handles
ASCII-EBCDIC translation automatically. For fixed-record files, you need
EBCDIC input or your output will be sorted with great confidence and zero
correctness.

## Limitations

Not yet implemented (flagged via MNOTE 4 at assembly time):

- `WITH DUPLICATES IN ORDER`
- `COLLATING SEQUENCE IS`
- Multiple USING/GIVING files
- MERGE statement

## Tests

| Test                          | Covers                                    |
|-------------------------------|-------------------------------------------|
| `zcobol/tests/TESTSRT1.CBL`  | LINE SEQUENTIAL sort with USING/GIVING    |
| `zcobol/tests/TESTSRT2.CBL`  | Fixed-format EBCDIC sort                  |

Both tests use DD name environment variables (`INFILE`, `OUTFILE`) for
file assignment, consistent with JCL conventions:

```bash
export INFILE=zcobol/tests/TESTSRT1.TF1
export OUTFILE=zcobol/tests/TESTSRT1.OUT
bash/cblclg zcobol/tests/TESTSRT1
```

## Normative Reference

> **FIPS PUB 21-2**, *COBOL*, Federal Information Processing Standards Publication,
> U.S. Department of Commerce, National Institute of Standards and Technology, 1985.
> Incorporates ANSI X3.23-1985 and ISO 1989-1985.

Relevant sections:

| Section | Title                     |
|---------|---------------------------|
| XI      | Sort-Merge Module         |
| XI-2    | The SORT Statement        |
| XI-3    | The RELEASE Statement     |
| XI-4    | The RETURN Statement      |
| XI-5    | The SD Entry              |

If you need a copy of the standard, ask.
