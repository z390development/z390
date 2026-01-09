# zCOBOL REPLACE Statement Implementation

**Author:** Zane Hambly  
**Date:** December 2025  
**Standard:** FIPS PUB 21-2 (ANSI X3.23-1985, ISO 1989-1985)

---

## Abstract

This document describes the implementation of the COBOL REPLACE compiler-directing 
statement in z390 zCOBOL, closing a 16-year TODO item (stub created 08/18/2009). 
The implementation strictly follows the Federal Information Processing Standards 
Publication 21-2.

---

## Normative Reference

This implementation conforms to:

> **FIPS PUB 21-2**, *COBOL*, Federal Information Processing Standards Publication,
> U.S. Department of Commerce, National Institute of Standards and Technology, 1985.
> Incorporates ANSI X3.23-1985 and ISO 1989-1985.

The following sections of FIPS PUB 21-2 were consulted:

| Section | Title | Page Reference |
|---------|-------|----------------|
| XII | Source Text Manipulation Module | XII-1 to XII-8 |
| XII-3 | The REPLACE Statement | **XII-6** |
| XII-3.1 | Function | XII-6 |
| XII-3.2 | General Format | XII-6 |
| XII-3.3 | Syntax Rules | XII-6 |
| XII-3.4 | General Rules | XII-7 |

The standard document was obtained from NIST archives and verified against the
official specification text.

## Syntax

### Format 1: Enable Text Substitution
```cobol
REPLACE ==pseudo-text-1== BY ==pseudo-text-2== ...
```

### Format 2: Disable Text Substitution  
```cobol
REPLACE OFF
```

## Implementation Details

### Architecture

REPLACE is implemented as a **compiler directive**, not a runtime statement. The text
substitution occurs during the zCOBOL-to-HLASM translation phase in `zc390.java`, 
before the source reaches the macro assembler.

### Modified Files

| File | Changes |
|------|---------|
| `src/zc390.java` | Added REPLACE parsing and text substitution logic |
| `zcobol/mac/REPLACE.MAC` | Updated stub to note pre-processor handling |

### New Test Files

| File | Purpose |
|------|---------|
| `zcobol/test/TESTREPL.CBL` | Test program for REPLACE functionality |

### Key Changes in zc390.java

1. **Global Variables** (lines 187-194):
   - `zc_replace_active` - Flag indicating if REPLACE is active
   - `zc_replace_count` - Number of replacement pairs
   - `zc_replace_lit1[]` - Array of patterns to find
   - `zc_replace_lit2[]` - Array of replacement text

2. **Array Initialization** (lines 351-352):
   - Arrays initialized in `init_zc390()` for 100 replacement pairs

3. **REPLACE Recognition** (line 1023-1024):
   - Added check in `find_next_token()` to intercept REPLACE keyword
   - Calls `process_replace()` when detected

4. **process_replace() Method** (lines 1760-1866):
   - Parses REPLACE statement syntax
   - Handles both Format 1 (replacement pairs) and Format 2 (OFF)
   - Stores patterns and replacements in global arrays

5. **Text Substitution** (lines 877-884):
   - Applied in `get_next_zc_line()` before tokenisation
   - Uses Java String.replace() for pattern matching
   - Supports multiple simultaneous replacement pairs

## Testing

### Test Case: TESTREPL.CBL

```cobol
       REPLACE ==AAA== BY ==BBB==.
       01  WS-AAA-FIELD     PIC X(20) VALUE 'TEST VALUE 1'.
       
       REPLACE OFF.
       01  WS-AAA-KEPT      PIC X(15) VALUE 'NOT REPLACED'.
```

### Expected Behaviour

1. `WS-AAA-FIELD` is replaced with `WS-BBB-FIELD`
2. After REPLACE OFF, `WS-AAA-KEPT` remains unchanged

### Running the Test

```batch
bat\CBLCLG.BAT zcobol\test\TESTREPL
```

### Expected Output

```
TESTREPL - COBOL REPLACE TEST STARTED
TEST 1:  TEST VALUE 1
TEST 2:  TEST VALUE 2  
TEST 3:  NOT REPLACED
TESTREPL - ALL TESTS COMPLETED
TESTREPL - TEST ENDED OK
```

## Limitations

1. **Maximum Replacement Pairs**: 100 pairs per REPLACE statement
2. **Pseudo-text Format**: Must use `==text==` delimiters
3. **Single-statement Scope**: Each REPLACE statement replaces the previous one

## Compliance

This implementation follows the COBOL 85 standard specification. Key compliance points:

- REPLACE operates on source text before compilation
- REPLACE OFF deactivates all replacements
- A new REPLACE statement replaces (not extends) prior replacements
- Pseudo-text must be delimited by `==` markers

## Building

After modifying `zc390.java`:

```bash
# Compile Java sources
javac -d bin src/*.java

# Rebuild JAR (include z390.properties)
copy z390.properties bin\
jar cvf z390.jar -C bin .
```

---

## References

1. **FIPS PUB 21-2** (1985). *COBOL*. Federal Information Processing Standards 
   Publication. U.S. Department of Commerce, National Institute of Standards 
   and Technology. Incorporates ANSI X3.23-1985 and ISO 1989-1985.
   - Section XII: Source Text Manipulation Module (pp. XII-1 to XII-8)
   - Section XII-3: The REPLACE Statement (pp. XII-6 to XII-7)

2. **ANSI X3.23-1985** (1985). *American National Standard for Information Systems - 
   Programming Language - COBOL*. American National Standards Institute.

3. **ISO 1989:1985** (1985). *Programming languages - COBOL*. International 
   Organization for Standardization.

4. **z390 Portable Mainframe Assembler and Emulator**. z390 Assembler LLC.
   - src/zc390.java - COBOL to HLASM translator

---

*Document version: 1.0 | Last updated: December 2025*

