# zCOBOL STRING Statement Implementation

**Author:** Zane Hambly  
**Date:** December 2025  
**Standard:** FIPS PUB 21-2 (ANSI X3.23-1985, ISO 1989-1985)

---

## Abstract

This document describes the implementation of the COBOL STRING statement in z390 
zCOBOL, closing a 17-year TODO item (stub created 11/17/2008). The implementation
provides basic STRING functionality with `DELIMITED BY SIZE`, following the Federal 
Information Processing Standards Publication 21-2.

---

## Normative Reference

This implementation conforms to:

> **FIPS PUB 21-2**, *COBOL*, Federal Information Processing Standards Publication,
> U.S. Department of Commerce, National Institute of Standards and Technology, 1985.
> Incorporates ANSI X3.23-1985 and ISO 1989-1985.

The following sections of FIPS PUB 21-2 were consulted:

| Section | Title | Page Reference |
|---------|-------|----------------|
| VI-6.25 | The STRING Statement | **VI-131** to VI-133 |
| VI-6.25.1 | Function | VI-131 |
| VI-6.25.2 | General Format | VI-131 |
| VI-6.25.3 | Syntax Rules | VI-131 |
| VI-6.25.4 | General Rules | VI-132 |

The standard document was obtained from NIST archives and verified against the
official specification text.

---

## What Was Implemented

### Basic STRING with DELIMITED BY SIZE

```cobol
STRING source-field DELIMITED BY SIZE
       INTO target-field.
```

This copies the entire contents of the source field into the target field, starting at position 1 (or the current pointer position if WITH POINTER is used).

### Files Created/Modified

| File | Type | Description |
|------|------|-------------|
| `zcobol/lib/STRING.MLC` | New | Runtime library routine |
| `zcobol/mac/STRING.MAC` | Modified | COBOL statement parser |
| `zcobol/mac/GEN_STRING.MAC` | New | Code generator (simplified) |
| `zcobol/lib/ZC390CVT.CPY` | Modified | Added ZCVT_STRING entry |
| `zcobol/cpy/ZC390CVT.CPY` | Modified | Added ZCVT_STRING entry |
| `bat/BLDCBLLIB.BAT` | Modified | Include STRING in library build |
| `zcobol/test/TESTSTR1.CBL` | New | Test program |

---

## Technical Details

### Implementation Approach

The STRING statement is implemented using inline code generation rather than a separate GEN_STRING macro call, due to HLASM macro line continuation issues discovered during development.

The runtime (`STRING.MLC`) uses **MVCL** for the copy operation rather than EX/MVC, as the EX instruction exhibited unexpected behaviour in z390's emulation during testing.

### Parameter Passing

Parameters are passed via `ZCVT_WORKAREA`:

| Offset | Content | Description |
|--------|---------|-------------|
| 0 | A(source) | Address of source field |
| 4 | L'source | Length of source field |
| 8 | A(delimiter) | Address of delimiter (0 if SIZE) |
| 12 | L'delimiter | Length of delimiter (0 if SIZE) |
| 16 | A(target) | Address of target field |
| 20 | L'target | Length of target field |
| 24 | A(pointer) | Address of pointer field (0 if none) |
| 28 | flags | Reserved |

### Return Values

- R0 = New pointer position (1-based)
- R1 = Overflow flag (0 = no overflow, 1 = overflow)

---

## What Remains to be Implemented

### STRING Statement

| Feature | Priority | Complexity | Notes |
|---------|----------|------------|-------|
| Multiple source fields | Medium | Medium | Loop through sources |
| DELIMITED BY literal | Medium | Easy | e.g., `DELIMITED BY SPACE` |
| DELIMITED BY identifier | Low | Easy | Use field value as delimiter |
| WITH POINTER clause | Medium | Easy | Placeholder exists |
| ON OVERFLOW clause | Medium | Medium | Conditional branching |
| NOT ON OVERFLOW clause | Low | Medium | Conditional branching |
| END-STRING scope | Low | Easy | Already handled by parser |

### UNSTRING Statement

The UNSTRING statement is significantly more complex than STRING:

```cobol
UNSTRING source-field
    DELIMITED BY [ALL] delimiter-1 [OR [ALL] delimiter-2]...
    INTO dest-1 [DELIMITER IN delim-1] [COUNT IN count-1]
         dest-2 [DELIMITER IN delim-2] [COUNT IN count-2]...
    [WITH POINTER pointer-field]
    [TALLYING IN tally-field]
    [ON OVERFLOW statement]
    [NOT ON OVERFLOW statement]
    [END-UNSTRING]
```

| Feature | Complexity | Reason |
|---------|------------|--------|
| Basic UNSTRING | High | Requires delimiter scanning algorithm |
| Multiple delimiters | High | OR logic with multiple patterns |
| ALL keyword | Medium | Treat consecutive delimiters as one |
| DELIMITER IN | Medium | Store matched delimiter |
| COUNT IN | Medium | Store character count |
| TALLYING IN | Medium | Count fields filled |
| Multiple outputs | High | Variable number of target fields |

**Estimated effort:** 8+ hours for full implementation

---

## Testing

### Test Program: TESTSTR1.CBL

```cobol
STRING WS-FIRST-NAME DELIMITED BY SIZE
       INTO WS-RESULT.
```

**Expected output:**
```
TESTSTR1 - COBOL STRING TEST STARTED
RESULT: [ JOHN                      ]
TEST 1: PASS - STRING COPIED CORRECTLY
TESTSTR1 - STRING TEST COMPLETED
TESTSTR1 - TEST ENDED OK
```

### Running Tests

```batch
bat\CBLCLG.BAT zcobol\test\TESTSTR1
```

---

---

## References

1. **FIPS PUB 21-2** (1985). *COBOL*. Federal Information Processing Standards 
   Publication. U.S. Department of Commerce, National Institute of Standards 
   and Technology. Incorporates ANSI X3.23-1985 and ISO 1989-1985.
   - Section VI-6.25: The STRING Statement (pp. VI-131 to VI-133)

2. **ANSI X3.23-1985** (1985). *American National Standard for Information Systems - 
   Programming Language - COBOL*. American National Standards Institute.

3. **ISO 1989:1985** (1985). *Programming languages - COBOL*. International 
   Organization for Standardization.

4. **z390 Portable Mainframe Assembler and Emulator**. z390 Assembler LLC.
   - zcobol/lib/ZC390CVT.CPY - Runtime vector table

---

## Changelog

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | December 2025 | Zane Hambly | Initial implementation of basic STRING with DELIMITED BY SIZE |

---

*Document version: 1.0 | Last updated: December 2025*
