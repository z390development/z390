# zCOBOL System Programmer's Guide

!!! Warning
    There are still a number of missing functions in zCOBOL, and until such time
    as zCOBOL successfully passes the NIST ANSI COBOL 1985 Test Suite, zCOBOL
    should be considered to still be in a beta test state.

## zCOBOL internals

The zCOBOL compiler translates COBOL source language programs into executable code
using the following 3 major components:

* The java program `zc390.class` in z390.jar reads COBOL language source program
  with file extension CBL and generates a z390 HLASM compatible mainframe assembler
  source program with MLC extension.

* Each COBOL verb becomes a macro call opcode and all the words following up to
  the next verb or period become positional parameters for the macro call.
* Periods generate a PERIOD macro call to terminate all structures which may be
  missing the optional END-IF type words.
* All dashes in words are converted to underscores unless in quotes.
* The level numbers in data division statements are mapped to WS macro call with
  level as first positional operand.

* The macros in COBOL verb macro library `zcobol/*.mac` parse the parameters
  for each verb, access global macro symbol table, and call code generation macros
  to generated executable code. For example the IF macro issues calls to GEN_COMP
  macro to generate executable source code to compare two fields,
  and issues call to GEN_BC to generate executable source code to branch on condition.
* There are currently 4 optional zCOBOL executable code generation macro libraries.
  The `zcobol/z390` HLASM code generation library is the primary focus currently.
  There is a COBOL demo program `zcobol/demo/HELLO.CBL` which can be
  compiled and executed in all 4 different target language environments using
  the initial zCOBOL release. The following libraries are available:
    - `zcobol/z390` - zCOBOL code generation macros for HLASM native z9/10 code
    - `zcobol/java` - zCOBOL code generation macros for J2SE Java
    - `zcobol/vce` - zCOBOL code generation macros for MS Visual Express C++
    - `zcobol/i586` - zCOBOL code generation macros for HLA and MASM native Intel code

Once the z390 HLASM code generation macros are complete and all the
[NIST COBOL 1985 standards tests](https://www.itl.nist.gov/div897/ctg/cobol_form.htm)
have been completed successfully as a first milestone, then these macros can
be copied to the other libraries and modified to replace HLASM source code
model statements with the other target language statements.

## zc390 Translator

The zc390 translator is a java regular expression based parser which reads
COBOL source program and generates HLASM compatible mainframe assembler
source program in one pass. 

Each recognized COBOL verb starts a new assembler macro call statement with all 
the parameters up to the next verb, period, or paragraph label passed as positional 
parameters. Periods generate a separate macro call to PERIOD to generate end to all 
the structures in the previous sentence.

Paragraph and section labels generate call to LABEL with the name and type of
label to generate. All hyphens in names are translated to underscores for
HLASM compatibility.

## COBOL Language Verb Macros

All the macros for the COBOL language verbs and section headings are stored
in the macro library `zcobol/mac`. These macros parse the parameters, validate
them for any syntax errors, and issue calls to generation macros in separate
directory as described below. For example, the `zcobol/mac/IF.MAC` macro generates
multiple calls to the generation macros `GEN_COMP`, `GEN_BC`,  `GEN_B`, and `GEN_LABEL`.

There are no language specific code generation macros in the zCOBOL directory
so it is shared across multiple target language environments. All the macros
are written in structured form using the z390 ZSTRMAC SPE structured programming
extensions such as AIF, AELSEIF, AELSE, AEND, AWHILE, etc. As a result there are
no explicit AGO or AIF labels in these macros. 
See [Structured Macro Entensions documentation](/user_guide/z390/structured_macro) for more details.

## COMPUTE Statement Example

The COBOL compute statement is a good example to study to understand how the zCOBOL 
compiler works. The steps followed to compile the following MOVE and COMPUTE 
statements are as follows:

```cobol
77 FLT-SRT USAGE FLOAT-SHORT OCCURS 2.
MOVE 1.1 TO FLT-SRT(2).
COMPUTE FLT-SRT(2) = FLT-SRT(2)+2.2.
```

1. zc390 translator generates the following 2 zCOBOL verb macro call statements:
```cobol
MOVE  1.1,'TO',FLT_SRT,"(',2,')'`   
COMPUTE FLT_SRT,'(',2,')',=,FLT_SRT,'(",2,')',+,2.2
```
2. The MOVE macro uses shared copybook routine GET_FIELD_PARM to parse the two
fields for MOVE and store resulting field name and symbol table index.
For the literal 1.1 the index is 0, for the subscripted field, the name is set
to explicit register reference including length offset(length,register)
and the code is generated to set the register to address of the subscripted field.
3. The MOVE macro next issues call to GEN_MOVE with the source and target
field names and system table indexes.
4. The GEN_MOVE macro checks the type of each field and generates appropriate
code to move value from source to target field. In this case it uses LARL
to set register to address of DFP short value of 1.1 in literal table and then
generates MVC to move the literal to the target subscripted field.
5. The COMPUTE uses GET_FIELD_PARM to obtain name and index of target field
and then extracts parms in expression following the = and then calls
ZC_CALC macro to generate code for expression and store result in specified
target field. This macro can be used by IF and other verb macros to calculate
expression for loop etc.

The ZC_CALC macro parses the expression parameters into Backus Normal Form
using two stacks. One stack has the operators in expression and the other has
the field parm index pointers. Following the rules of precedence, the operators
and associated parameter pointers are removed from the stacks and stored
sequentially in an operation table containing the operators, 2 operands,
and the target field for each operation.   

Temporary storage fields are represented using negative indexes instead of
position and a table of temporary fields created along with their type
is maintained. A queue of free temporary fields is maintained and once
a temporary field has been used in an operation, that temporary field is
on the free queue for reuse rather than allocating a new temporary storage field.

Once the expression has been parsed and all the operation table entries
have been generated, the last target field is replaced with the result field
passed to ZC_CALC and then the operation table is scanned and the generation
macros for each operation are called to generate code to perform the operation.

Just prior to generating code for an operation, the two input parameter types
are used to determine the required type of result to minimize any loss
of precision during the calculations. A call to GEN_MOVE is made to move
the first operand field to the target field prior to performing
add, subtract, etc. on the target field for operation. If the first operand is
the same as the target field, the move can be omitted but that is not always
possible to determine in the case of subscripting and indexing where
different variables may just happen to have the same value.

The called generation macros GEN_ADD, GEN_SUB, GEN_MPY, and GEN_DIV check
the field types and perform the necessary conversion when types do not match.

See demo added in `zcobol/demo/callcomp` directory which contains CALLCALC.MLC
main assembler program which calls subroutine COMPSUM.CBL which uses
COMPUTE statement to calculate sum of 15 different numeric data field types
and returns sum as packed decimal for editing and display by calling program.

There is a paper about this demo here:
[zcobol_COMPUTE.pdf -- Dead link](http://www.z390.org/zcobol/demo/callcomp/zcobol_COMPUTE.pdf)

## zCOBOL Target Source Language Generation Macros

All the target source language generation macros called by the COBOL verb macros
in z390/zcobol are stored in the following directories by target language:

| Directory   | Notes                                                        |
|-------------|--------------------------------------------------------------|
| zcobol/z390 | Generate HLASM compatible mainframe assembler source program |
| zcobol/java | Generate J2SE java compatible source program                 |
| zcobol/vce  | Generate MS Visual Express C compatible source program       |
| zcobol/i586 | Generate HLA/MASM Intel assembler compatible source program  |

Current only the z390 HLASM compatible source generation macros are being
fully developed along with the required runtime support functions stored
in the `zcobol/lib/ZC390LIB.390` dynamically loaded runtime module.

zCOBOL demos include a hello world COBOL program which can be compiled
and executed in each of the target environments form the same `zcobol/demo/HELLO.CBL`
source program.   

The following commands generate the corresponding source language equivalent
and executable:

| Command                       | Generated Source Code Target | Generated Executable Code | Notes                                             |
|-------------------------------|------------------------------|---------------------------|---------------------------------------------------|
| cblclg zcobol/demo/HELLO      | zcobol/demo/HELLO.MLC/BAL    | zcobol/demo/HELLO.390     | requires z390 and J2SE on Windows/Linux           |
| ZCJAVCLG zcobol/demo/HELLO    | zcobol/demo/HELLO.java       | zcobol/demo/HELLO.class   | requires J2SE on Windows/Linux                    |
| ZCVCECLG zcobol/demo/HELLO    | zcobol/demo/HELLO.ccp        | zcobol/demo/HELLO.exe     | requires MS VCE runtime on Windows                |
| ZC586CLG zcobol/demo/HELLO    | zcobol/demo/HELLO.HLA/ASM    | zcobol/demo/HELLO.exe     | requires HLA, MASM, and MS VCE runtime on Windows |

If you are interested in joining in the open source zCOBOL development effort
in any of the 4 target language environments or want to add another target
language environment, join the zcobol development email discussion group
and make your interests known. Melvyn Maltz is currently developing additional
EXEC CICS support for zCOBOL programs.

## ZC390LIB Runtime Library

The `zcobol\lib` code generation macro directory also contains all the
source code and the ZC390CVT.CPY copybook required to build
the `linklib\ZC390LIB.390` runtime load module which is dynamically loaded
by all generated z390 zCOBOL programs. 

This module contains the following components:

* The ZC390CVT.CPY copybook is used in every zCOBOL generated program
  to define the DSECT addressed by register 9.
* The same copybook is also used in ZC390LIB.MLC to generate
  the CVT at the beginning of the ZC390LIB.390 runtime load module
  with addresses of all the entries followed by work areas used by
  the code generation macros.

| Library Element | Notes                                                                                                                                                        |
|-----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ZC390LIB.MLC    | Contains ZC390LIB CSECT and COPY ZC390CVT to include all object modules following the CVT at the beginning                                                   |
| ZC390NUC.MLC    | Included module with system function routines such as CALL, GOBACK, STOPRUN, PERFORM, and PMCHECK to check for end of current performed paragraph or section |
| ABORT.MLC       | Contains module called to abort execution with reason code                                                                                                   |
| ACCEPT.MLC      | Contains support for ACCEPT date, time, day of week                                                                                                          |
| DISPLAY.MLC     | Display any type field or literal                                                                                                                            |
| INSPECT.MLC     | Inspect field tallying, replacing, or transforming                                                                                                           |

## Base Free Code Generation

The zCOBOL code generation macros in `zcobol/lib` generate base free code
for the procedure division using relative instructions for both
branch addressing and for literal addressing as required.

The only address constants generated in zCOBOL programs are for statically
linked CALL's to other zCOBOL or assembler programs. The only limit on
the combined size of working storage and the procedure division is 16MB.  
In order to use relative addressing for literals, all odd length literals
are padded to even lengths.   

The LARL instruction is used to set address of data field or literal field
as required for use in following RX type instructions. To address working storage
and linkage section data fields, conventional base registers are dynamically
allocated as required for use in RX type instructions.   

Since R13 always points to the beginning of working-storage, no dynamic
base registers are required for access to data items in the first 4K of
working storage.

## zCOBOL EXEC CICS Support

When the option CICS is specified on the command line for ZC390C, ZC390CL,
or ZC390CLG, then the zcobol\ZCOBOL MAC global option &ZC_CICS is set on
and the following changes in code generation are made:

1. The CICS option will generate call to DFHEIENT to initialize CICS
prior to executing user code starting at the first program CSECT.
2. A DFHEISTG DSECT is generated at the beginning of working-storage
instead of WSLOC LOCTR and warnings are generated for any data VALUE clauses
defined in working-storage section.

## zCOBOL Data Types

1. The zCOBOL option FLOAT(HEX/BINARY/DECIMAL) can be used to change the
default from DECIMAL to HEX or BINARY for the generic types FLOAT-SHORT,
FLOAT-LONG, and FLOAT-EXTENDED.
2  COMP-3 packed and also zoned decimal are limited to 18 digits per COBOL
standard unless option EXTEND is set allowing up to 31 digits for both packed
decimal and zoned decimal fields.

| USAGE            | PICTURE        | Z390 Assembler Type | Description                                                        |
|------------------|----------------|---------------------|--------------------------------------------------------------------|
| COMP             | S9(4)          | H                   | 16 bit binary                                                      |
| COMP             | S9(9)          | F                   | 32 bit binary                                                      |
| COMP             | S9(18)         | G                   | 64 bit binary                                                      |
| COMP             | S9(39)         | Q                   | 128 bit binary                                                     |
| FLOAT-HEX-7      | COMP-1         | EH                  | HFP short 7 digits                                                 |
| FLOAT-HEX-15     | COMP-2         | DH                  | HFP long - 15 digits                                               |
| FLOAT-HEX-30     |                | LH                  | HFP extended - 30 digits                                           |
| FLOAT-BINARY-7   |                | EB                  | BFP short 7 digits                                                 |
| FLOAT-BINARY-16  |                | DB                  | BFP long - 16 digits                                               |
| FLOAT-BINARY-34  |                | LB                  | BFP extended - 34 digits                                           |
| FLOAT-DECIMAL-7  | FLOAT-SHORT    | EB                  | DFP short 7 digits                                                 |
| FLOAT-DECIMAL-16 | FLOAT-LONG     | DB                  | DFP long - 16 digits                                               |
| FLOAT-DECIMAL-34 | FLOAT-EXTENDED | LB                  | DFP extended - 34 digits                                           |
| FLOAT-DECIMAL-7  | FLOAT-SHORT    | EB                  | DFP short 7 digits                                                 |
| FLOAT-DECIMAL-16 | FLOAT-LONG     | DB                  | DFP long - 16 digits                                               |
| FLOAT-DECIMAL-34 | FLOAT-EXTENDED | D                   | LB                                                                 | DFP extended - 34 digits                                      |
| COMP-3           | S9(31)         | P(3)                | Packed decimal up to 31 digits with option EXTEND                  |
|                  | S9(31)         | Z(3)                | Zoned Decimal up to 31 digits with option EXTEND (uses PD support) |
|                  | X              | X                   | Characters                                                         |
| FLOAT-SHORT      |                | EH,EB,ED            | Use option FLOAT(HFP/BFP/DFP)                                      |
| FLOAT-LONG       |                | DH,DB,DD            | Use option FLOAT(HFP/BFP/DFP)                                      |
| FLOAT-EXTENDED   |                | LH,LB,LD            | Use option FLOAT(HFP/BFP/DFP)                                      |

## Command Line options for zCOBOL Compiler

Options are passed to the zCOBOL macro stage via CBL macro call with the
options defined as positional parameters.

To turn off an option that is on, prefix the option name with NO on command line
or in OPT options file.
