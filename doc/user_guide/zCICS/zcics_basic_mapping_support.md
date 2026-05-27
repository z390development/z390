# zCICS Basic Mapping Support

## Introduction

This document outlines the extensions and facilities implemented in zCICS.
Please refer to the IBM Manuals on BMS usage.

The documentation for `EXEC CICS SEND MAP`, `RECEIVE MAP` and `SEND CONTROL`
are in the [zCICS Application Programming Guide](zcics_app_prog_guide.md).

The following BMS facilities are not yet implemented:
- Support for devices outside the 3270 family Printers
- Extended Attributes (other than colour) and alternate screen size
- Dynamic Map Positioning and ACCUM processing
- Partitioning
- Paging

## zCICS Basic Mapping Support

### BMS Macros

#### DFHMSD -- Mapset definition and termination

Although `DFHMSD` is optional it is recommended that one is coded. A default set of
values will be internally generated from the `DFHMDI` macro if missing.

| Name   | Operation | Operands              |
|--------|-----------|-----------------------|
| mapset | DFHMSD    | TYPE=MAP/DSECT/FINAL  |
|        |           | MODE=IN/OUT/INOUT     |
|        |           | LANG=ASM/COBOL        |
|        |           | CTRL=                 |
|        |           | CURSLOC=YES/NO        |
|        |           | DSATTS=               |
|        |           | EXTATT=YES/NO/MAPONLY |
|        |           | MAPATTS=              |
|        |           | STORAGE=AUTO          |
|        |           | TIOAPFX=YES/NO        |
|        |           | TERM=                 |

> [!NOTE]
> `TERM=` is supported but discarded

##### Errors:
- A MAXIMUM OF 3 OPERANDS ALLOWED IN CTRL
- CANNOT HAVE MORE THAN ONE DFHMSD MACRO
- DSATTS IS SPECIFIED WITHOUT MAPATTS
- EXTATT CONFLICTS WITH DSATTS/MAPATTS
- EXTATT NOT NO, MAPONLY OR YES
- INVALID CURSLOC -- cursloc
- INVALID LANG -- lang
- INVALID MODE -- mode
- INVALID STORAGE -- storage
- INVALID TIOAPFX -- tioapfx
- INVALID TYPE -- type
- MAPSET NAME IS MISSING
- POSITIONAL OPERAND xxxx INVALID

#### DFHMDI -- Map definition

| Name    | Operation | Operands          |
|---------|-----------|-------------------|
| mapname | DFHMDI    | TYPE=MAP/DSECT    |
|         |           | MODE=IN/OUT/INOUT |
|         |           | LANG=ASM/COBOL    |
|         |           | CTRL=             |
|         |           | CURSLOC=YES/NO    |
|         |           | DSATTS=           |
|         |           | EXTATT=           |
|         |           | MAPATTS=          |
|         |           | SIZE=(24,80)      |
|         |           | LINE=1            |
|         |           | COLUMN=1          |
|         |           | STORAGE=AUTO      |
|         |           | TIOAPFX=YES/NO    |
|         |           | JUSTIFY=          |
|         |           | NOLABEL=          |

> [!NOTE]
> `TYPE` and `STORAGE` are required here only if the `DFHMSD` macro is omitted.
> 
> Parameters `TYPE`, `MODE`, `LANG`, `CTRL`, `CURSLOC`, `DSATTS`, `EXTATT`, `MAPATTS`,
> `STORAGE` and `TIOAPFX` can all be omitted if the settings specified in `DFHMSD`
> are correct for this map.
> 
> MODE may be specified as an override to `DFHMSD`. This facility is an extension.
> 
> Any option(s) specified in CTRL will override all the options specified in `DFHMSD`.
> 
> For `LINE` and `COLUMN`, only numeric parameters are supported.
> 
> `JUSTIFY` is discarded.
> 
> `NOLABEL` is a special parameter for zCICS maps.
> It suppresses the label indicated and speeds up the map assembly.
> When used on the `DFHMDI` macro it applies to all labelled `DFHMDF` macros
> that follow. When specified on the `DFHMDF` macro it overrides any
> specification on the `DFHMDI` macro.
> 
> The labels that can be suppressed are as follows:
> - A (attribute)
> - C (colour)
> - F (flag)
> - L (length)
> - O (output)
> - eg. `NOLABEL=ACF`

##### Errors:
- A MAXIMUM OF 3 OPERANDS ALLOWED IN CTRL
- COLUMN CANNOT EXCEED nn
- DSATTS is specified without MAPATTS
- EXTATT conflicts with MAPATTS/DSATTS
- EXTATT not omitted, NO, MAPONLY or YES
- INVALID CTRL OPTION - ctrl
- INVALID CURSLOC -- cursloc
- INVALID MODE -- mode
- INVALID SIZE -- size
- INVALID TIOAPFX -- tioapfx
- LINE CANNOT EXCEED nn
- MAPNAME IS MISSING
- MSD AND MDI TYPE MISMATCH
- POSITIONAL OPERAND xxxx INVALID

#### DFHMDF -- Field definition

| Name    | Operation | Operands    |
|---------|-----------|-------------|
| fldname | DFHMDF    | POS=n/(n,n) |
|         |           | LENGTH=     |
|         |           | ATTRB=      |
|         |           | COLOR=      |
|         |           | JUSTIFY=    |
|         |           | INITIAL=    |
|         |           | XINIT=      |
|         |           | PICIN=      |
|         |           | PICOUT=     |
|         |           | OCCURS=     |
|         |           | GRPNAME=    |
|         |           | NOLABEL=    |

> [!NOTE]
> fldname may now use long names.
> 
> POS does not have to be in sequence.
> 
> If `ATTRB` is missing then `ATTRB=(ASKIP,NORM)` is assumed.
> If protection or intensity parameters are missing then a sensible default is assumed.
> 
> A field specified as `ATTRB=(UNPROT,NUM)` will only accept digits 0-9.
> Some 3270 models also allowed dots and commas.
> 
> Extensive cross-checking is made, such that any 'unusual' map structure is
> flagged as an error. A peruse of the errors below give a good indication of
> these types of map structure.

##### Extensions:

- `ATTRB=(ALPHA)` has been added as the opposite of `NUM`
- `XINIT=FF..`<br />
  The dots are replaced by a hex code of a fill character.<br />
  This will generate an `RA` order to fill this field for its whole length.<br />
  e.g. Instead of coding `INITIAL='__________'` code `XINIT=FF6D`
- `PICIN` and `PICOUT`<br />
  These parameters are not well-described in the IBM Manuals.
- `PICIN (COBOL)`<br />
  A symbolic reference to the input data, the data cannot be changed.<br />
  ie. `PICIN='$$$9'` does not make any sense.<br />
  `PICIN='99V99'` tells COBOL where the decimal point is.
- `PICOUT (COBOL)`<br />
  The data is subject to an editting process.<br />
  ie. `PICOUT='$$$9'` the data is editted<br />
  `PICOUT='99V99'` is the same as `'9(4)'`
- `PICIN` (Assembler) extension<br />
  The incoming data IS subjected to an editting process. Described below.
- `PICOUT` (Assembler) extension<br />
  The outgoing data is subjected to an editting process. Described below.
- `PICIN` and `PICOUT` (Assembler)<br />
  The parameter is (almost) a standard edit word with some restraints.

Currently only numeric data can be subjected to `PICIN`/`PICOUT`.

- `PICOUT`<br />
  Operates for a `SEND MAP`, but not if `MAPONLY` is specified.<br />
  Data is taken from the map structure and subjected to the edit word before being sent to the screen.
- `PICIN`<br />
  Operates for a `RECEIVE MAP`.<br />
  The data received is subjected to the edit word and then passed to the application in the map structure.<br />
  Note that the resulting data may no longer be numeric.
- The edit word has some strict rules at present. Suggested extensions are most welcome.
  The first hex pair must be 40 (blank), 5C (asterisk), 5B (dollar) or 4D (open bracket).<br />
  Blank and asterisk are fill characters, dollar and open bracket are float characters.
- The remaining hex pairs can be 20 (digit select), 21 (significancestarter), 4B (dot) or 6B (comma).
  The number of 20 and 21's must be odd.

##### Examples

- `PICOUT=4020216B202020`<br />
  Data is `12345`, displayed as `12,345`
- `PICOUT=5B20216B202020`<br />
  Data is `1234`, displayed as `$1,234`
- `PICOUT=5C20216B202020`<br />
  Data is `89`, displayed as `*****89`

`PICIN` is no different, just that it edits incoming data.

If the data to be processed is not numeric after being PACKed, a MAPFAIL/8 condition is raised.

##### IMG file

For each `TYPE=MAP` generation, a mapname.IMG file is generated.
This gives a symbolic picture of the resulting screen and can be used for
planning map changes.
See the file `GUI6.IMG` for a good example.

##### NOLABEL

`NOLABEL` is a special parameter for zCICS maps.

It suppresses the label indicated and speeds up the map assembly.
When used on the `DFHMDI` macro it applies to all labelled `DFHMDF`
macros that follow. When specified on the `DFHMDF` macro it overrides
any specification on the `DFHMDI` macro.

The labels that can be suppressed are as follows:
A (attribute), C (colour), F (flag), L (length), O (output)<br />
eg. `NOLABEL=ACF`

##### Errors:
- A MAXIMUM OF 2 OPERANDS ALLOWED IN JUSTIFY
- ADJACENT ATTRIBUTE AT POS nnn
- ATTRIBUTE CONFLICT - ALPHA AND NUM
- ATTRIBUTE CONFLICT - ASKIP AND ALPHA
- ATTRIBUTE CONFLICT - ASKIP AND NUM
- ATTRIBUTE CONFLICT - ASKIP AND PROT
- ATTRIBUTE CONFLICT - ASKIP AND UNPROT
- ATTRIBUTE CONFLICT - BRT AND DRK
- ATTRIBUTE CONFLICT - BRT AND NORM
- ATTRIBUTE CONFLICT - NORM AND DRK
- ATTRIBUTE CONFLICT - PROT AND UNPROT
- BAD LENGTH - length
- BOTH INITIAL AND XINIT ARE SPECIFIED
- CONFLICTING JUSTIFY PARMS
- GRPNAME AND OCCURS ARE SPECIFIED
- GRPNAME SPECIFIED WITHOUT FLDNAME
- IC ATTRIBUTE HAS OCCURRED IN ANOTHER MDF MACRO
- INITIAL/XINIT IS GREATER THAN LENGTH
- INITIAL/XINIT IS INVALID WITH LENGTH=0
- INVALID ATTRB OPTION – attrb
- INVALID COLOR
- INVALID JUSTIFY OPTION - justify
- INVALID OCCURS - occurs
- INVALID POS - pos
- JUSTIFY CONFLICT - BLANK AND ZERO
- JUSTIFY CONFLICT - LEFT AND RIGHT
- LENGTH=0 IS INVALID FOR NAMED FIELD
- PICIN/PICOUT CAN ONLY BE SPECIFIED FOR A NAMED FIELD
- PICIN/PICOUT HAS ODD NUMBER OF HEX DIGITS
- PICIN/PICOUT HEX LENGTH NOT EQUAL TO LENGTH
- PICIN/PICOUT IS INVALID
- PICIN/PICOUT: 1ST PAIR NOT 40, 5C, 5B, 4D
- PICIN/PICOUT: NUMBER OF 20/21 IS NOT ODD
- POS nnn OVERLAPS WITH ANOTHER FIELD
- POS COLUMN+LENGTH EXCEEDS DEFINED MAP SIZE
- POS EXCEEDS DEFINED MAP SIZE
- POS HAS TOO MANY PARAMETERS
- POSITIONAL OPERAND xxxx INVALID
- SINGLE POS EXCEEDS DEFINED MAP SIZE
- SPECIAL FORMAT XINIT CANNOT BE SPECIFIED IF LENGTH IS LESS THAN 8
- UNNAMED FIELD HAS UNPROT OR FSET, INPUT DATA CANNOT BE MAPPED
- XINIT HAS ODD NUMBER OF HEX DIGITS

The following errors occur owing to internal limits.
Please report them.
- AN INTERNAL ERROR HAS OCCURRED<br />
  ERROR IN ATTRIBUTE TABLE nnnn
- AN INTERNAL ERROR HAS OCCURRED<br />
  MAPSIZE TOO LARGE - nnnn
- AN INTERNAL ERROR HAS OCCURRED<br />
  PICIN/PICOUT LENGTH GREATER THAN 50 BYTES

## Map generation

See the sample generation for `MAP01` embedded in `DFHALL.BAT`.
This generates both the `TYPE=DSECT` creating `MAP01.CPY`
and the `TYPE=MAP` creating `MAP01.390`.
For `LANG=COBOL`, the structure will have a `.CPZ` suffix.
This suffix is the default for the zCOBOL `COPY` statement.
As the program name is the same, the `TYPE=DSECT` assembler output is
renamed `mapset.PR1`.

## Change Summary

| February 1, 2012  | Comment on long fldname support added                                               |
|-------------------|-------------------------------------------------------------------------------------|
| November 1, 2010  | Colour support, EXTATT, DSATTS, MAPATTS and COLOR parameters added and extra MNOTEs |
|                   | DFHMDI and DFHMDF new NOLABEL parameter                                             |
| August 1, 2009    | Update to PICIN and PICOUT doc.                                                     |
| February 21, 2009 | Added LANG=COBOL and mention of the .CPZ suffix                                     |
|                   | Added commentary separating PICIN/PICOUT by language                                |
| November 24, 2008 | Added comment about POS                                                             |
|                   | TIOAPFX is fully supported                                                          |
|                   | TERM is supported but discarded                                                     |
|                   | Extra error checking for LINE/COLUMN                                                |
