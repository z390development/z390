# Clauses of the Cobol WRITE verb

The information in this document was derived from the IBM Language Reference manual
for Enterprise Cobol 6.5. IBM document code `SC27-8713-03`

The `WRITE` verb as defined in the Cobol language supports:
- `BEFORE ADVANCING`
- `AFTER ADVANCING`
- `AT END-OF-PAGE`
- `NOT AT END-OF-PAGE`
- `INVALID KEY`
- `NOT INVALID KEY`

The `BEFORE ADVANCING` and `AFTER ADVANCING` clauses are mutually exclusive on a single `WRITE`
statement. Each `WRITE` statement for any file can use either `BEFORE ADVANCING` or `AFTER ADVANCING`
or omit the `ADVANCING` clause altogether. Thus, from a file perspective, `ADVANCING` clauses can
be a mixture of `BEFORE` and `AFTER` with omitted `ADVANCING` clauses defaulting to `AFTER 1`.

The other four clauses are not mutually exclusive, so at most 5 of the 6 defined clauses may be present
on any given `WRITE` statement.

> [!NOTE]
> 1. The `BEFORE`/`AFTER` `ADVANCING` clauses interact with the `LINAGE` clause on the `FD` definition
>    for the file being written.
> 2. When no `LINAGE` is defined, advancing a `PAGE` is considered to be the same as 1 line.

Limitations / Questions:
- According to IBM, the `ADVANCING` options can be used only on QSAM files.
  - Question 1: in z390 both QSAM and BSAM are supported. Do we also want to supprt VSAM-ESDS?
- According to IBM, the `INVALID KEY` clause applies, when applied to a sequential file or
  line sequential file, applies to exceeding file size limits only.
  - Question 2: in z390 we have no implementation yet. Do we want to include the option to flag error
    when a VSAM-ESDS is written such that an overlying Unique AIX detects a duplicate key?
- IBM does not mention any exceptions, yet it seems to make sense to suppress default spacing
  of `AFTER 1 LINE` when no `ADVANCING` clause is specified on the first `WRITE` operation to the file.
  - Question 3a: Do we want to suppress the `AFTER 1 LINE` if the first `WRITE` does not specify an `ADVANCING` clause?
  - Question 3b: Also, a comparable problem arises when a `BEFORE PAGE` is followed by an `WRITE` without `ADVANCING` clause.
    How do we resolve that?

## FD LINAGE clause details

The FD allows a LINAGE clause with the following 4 elements:
- `LINAGE`
  - Specifies the total nr of lines on each page
  - including top margin, bottom margin, and footing area
  - causes the `LINAGE-COUNTER` special register to be created for the file
- `WITH FOOTING AT`
  - Specifies the first line of the footing area within the page body (i.e. disregarding top margin)
  - The default is the last line of the page body
- `LINES AT TOP`
  - unprintable lines at top of page
  - the default is zero
- `LINES AT BOTTOM`
  - unprintable lines at bottom of page
  - the default is zero

Graphically:
```
      |---------------------------------|-------------------
      |                                 |                 ^
      | Lines At Top (top margin)       |                 |
      |                                 |                 |
      |---------------------------------|-----            |
      |                                 |   ^             |
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   | Page Body   | Logical Page Depth
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   |             |
      |                                 |   |             |
      |------ with footing value--------|   |             |
      |                                 |   |             |
      | Footing Area                    |   |             |
      |                                 |   V             |
      |---------------------------------|-----            |
      |                                 |                 |
      | Lines At Bottom (bottom margin) |                 |
      |                                 |                 V
      |---------------------------------|-------------------
```

> [!NOTE]
> 1. Lines can be written into the footing area, but doing so causes an end-of-page condition to be raised.
> 2. An end-of-page condition is handled **after** the record has been written.
> 3. When an attempt is made to write a record beyond the page body an automatic page overflow condition is raised.
> 4. An automatic page overflow condition is handled **before** the record is written.

### Oddities to be resolved

On page 193, under the "LINAGE clause" heading the manual states:
> The LINAGE clause specifies the depth of a logical page in number of lines. Optionally, it also specifies
> the line number at which the footing area begins and the top and bottom margins of the logical page. (The
> logical page and the physical page cannot be of the same dimension.)

It says: LINAGE specifies "depth of a logical page". According to the associated figure (on page 194)
this comprises the full logical page: lines-at-top + page-body + lines-at-bottom.

In the next paragraph, however, it says:
> The number of lines that can be written or spaced on this logical page. The area of the page that these
> lines represent is called the page body. The value must be greater than zero.

So the first sentence speaks of "logical page" which probably intends to be equivalent to the "logical page depth"
as shown in the figure (on page 194). This implies that the LINAGE value **includes** the top and bottom margins.
This is in agreement with the statement in the preceding paragraph cited above.

However, the second sentence equates the LINAGE value specifically to the "page body". Which would **exclude** 
the top and bottom margins.

Finally, the fist paragraph following the figure on page 194 states:
> The logical page dimension specified in the LINAGE clause is the sum of all values specified in each
> phrase except the FOOTING phrase.

This, again, defines the LINAGE value as **including** the top and bottom margins.

In z390 we will implement the LINAGE value as being the logical page depth, that is:
**including** not only the page body, but also the top and bottom margins.

## Different types of control characters

### ANSI or ASA control characters

On the mainframe, typically, ASA control characters are used.
The ASA control character is written **before** the first printable character of each record.
The ASA control characters are defined as follows: 

| Code  | Meaning                           |
|-------|-----------------------------------|
| space | Space 1 line before printing      |
| `0`   | Space 2 lines before printing     |
| `-`   | Space 3 lines before printing     |
| `+`   | Space 0 lines before printing     |
| `1`   | Skip to next page before printing |

The ASA control character indicates spacing to be applied **before** printing a record.
Therefore, it is the natural equivalent of the Cobol `WRITE` verb with `AFTER ADVANCING` clause.

When a Cobol program issues a `WRITE` verb with `BEFORE ADVANCING` clause,
the requested spacing must be kept pending in order to be expressed on the next record's control character.

### Windows / Linux control characters

On Windows and Linux, typically, Ascii control characters are used.
The Ascii control characters are written **after** the last printable character of each record.
The Ascii control characters are defined as follows: 

| Code | Meaning         | Effect                           |
|------|-----------------|----------------------------------|
| CR   | Carriage Return | Move print head to column 1      |
| LF   | Line Feed       | Space 1 line after printing      |
| FF   | From Feed       | Skip to next page after printing |

- On Windows, two control characters are normally used: CR-LF or CR-FF.
- Linux and MacOs, a single control character is normally used: LF or FF.

The Ascii control characters indicate spacing to be applied **after** printing a record.
Therefore, it is the natural equivalent of the Cobol `WRITE` verb with `BEFORE ADVANCING` clause.

When a Cobol program issues a `WRITE` verb with `AFTER ADVANCING` clause,
the record to be written must be kept pending in order to be terminated with the next record's spacing,
if the next `WRITE` has a `AFTER ADVANCING` clause.

Under normal Windows/Linux conventions, there is no option to specify `ADVANCE 0 LINES`.
We could use CR or CR-CR to implement that, but I'm not sure that's a good plan.

Question 4: Do we want to support `ADVANCE 0 LINES`? If yes, how can we encode that in the file being written?

## Other complexities

### Selecting the correct form of ADVANCING control

When the program is being compiled, the compiler is unable to tell what the run-time requirements will be:
- ASA can be enabled by specifying an `A` in the `RECFM` parameter on the DD statement or host variable
  specifying the exact file to be written.
- To enable Windows- or Unix-style output z390 employs the `T` on the `RECFM` parameter, but there is as yet
  no provision to select Windows-style of Unix-style line endings, other than checking the host OS at run-time.

Question 5: It is unclear whether the control characters - whether ASA or Ascii - are defined in the record lay-out.
If they are, there is no option to dynamically switch at run-time, and an abend is due when an `OPEN` is attempted
for the file, specifying an incorrect type of printing control characters.

Question 6: Can we derive meaningful information form the `TARGET-COMPUTER` paragraph?
It seems this paragraph is  mostly left unspecified in the NIST test suite. I doubt we would want to
create a zCobol-specific extension.

### ADVANCING without LINAGE

When `LINAGE` is not specified on the `FD` no page information is available. Using the `PAGE` keyword on the
`ADVANCING` clause, in this case, defaults to simply advancing 1 line.

On the other hand, a sequential or line sequential file without `LINAGE` may or may not have to support
`ADVANCING` clauses on applicable `WRITE` statements. For files without a `LINAGE` declaration, whether or not
`ADVANCING` logic needs to be generated on `WRITE` statements is known only after the entire `PROCEDURE DIVISION`
has been assembled.

Question 7: do we need to be able to support defaulted `ADVANCING` control on files that have no `LINAGE` clause
on their `FD` yet do have the `ADVANCING` option on some, but not all, of their `WRITE` operations?

Question 8: if Yes, then how can we implement `ADVANCING` logic on `WRITE` statements that occur **before**
the compiler knows that `ADVANCING` logic is needed? In other words: if the first `WRITE` to a file without a
`LINAGE` clause on its `FD` contains no `ADVANCING` clause, then no `ADVANCING` support appears to be needed.
Yet when a subsequent `WRITE` to the same file does specify an `ADVANCING` clause, then how do we modify the
earlier expansion of the `WRITE` macro to change from not having `ADVANCING` support to actually having
`ADVANCING` support?

Potential solution: do not perform the write in-line. Instead, perform an out-of-line subroutine which is
generated at the end of the `PROCEDURE DIVISION`.

### Record and Block formats

- For `RECFM=V*` the `RDW` needs to be built before writing the record; for `RECFM=F*` no `RDW` applies.
- For files using `LBI` (Large Block Interface) a different layout of `RDW` or `BDW` is required.
  - RDW/BDW = LL00 with high bit off supports up to 32K
  - RDW/BDW = LLLL with high bit on supports up to 2GB
- For `RECFM=*T*` Ebcdic-Ascii translation is needed.

Question 9: which of these actions are performed by the z390 access methods, and which need to be
implemented as application code in the compiled program?

## Generalized solution

The compiler must either know all the specifics of the file to be written in order to generate the
required logic. Or the compiler can be built to create generic code that is capable of adapting to the
specifics of the file to be written, whatever those specifics may be at run-time.

Finally, as a trade-off, the compiler might require some details - but not all - to be known at compile-time.
The remaining details can then still be picked up when the file is opened.

Question 10: How generic do we want to make the generated code for writing sequential and line sequential files?

### DCB and ADVANCING support

- When the DCB is generated, we add a couple of fields for the four `LINAGE` values:
  - LINAGE
  - FOOTING
  - TOP
  - BOTTOM
- We need to add the following fields for `ADVANCING` support:
  - LINAGE_COUNTER
  - ADVANCE_FLAGS
    - ADVANCING_ENABLED
    - FIRST_RECORD_WRITTEN
    - BUFFER_HAS_RECORD
    - PENDING_PAGE
  - PENDING_NEWLINES
  - RECORD_BUFFER_PTR
- The flag field which indicates whether or not `ADVANCING` support is enabled defaults to yes
  if the `FD` has a `LINAGE` clause, otherwise the default is no.
- If any `WRITE` is detected that uses an `ADVANCING` clause, the flag is turned on during `OPEN` processing.







