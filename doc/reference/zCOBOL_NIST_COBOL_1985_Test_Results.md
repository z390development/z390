# zCOBOL NIST COBOL 1985 Test Results

**v1.5.06**

*Automated Software Tools Corporation*

[Overview]

[Summary Statistics]

[Translation to zcobol macro calls]

[Generated HLASM Assembler Instructions]

[Pending Priority List to complete test suite for HLASM target language]

[Trademarks](#Trademarks)

[Credits](#Credits)

== *(C) Copyright 2009 Automated Software Tools Corporation.
This is part of z390 distributed under open source GPL License* ==

## Overview (#Overview)

The [NIST COBOL Tests](https://www.itl.nist.gov/div897/ctg/cobol_form.htm)
have been downloaded and are being used as an initial test
of zCOBOL compliance with the COBOL 1985 Standard.  There are 459 COBOL test
programs of which 409 are currently being tested with zcoboL.
See the [summary statistics](???)
showing translation, assembly, and execution by 2 character program name prefix.
Currently they all translate without errors.

See the [translation statistics](???)
showing the frequency of each generated zCOBOL macro call listed in descending
order by frequency followed by alphabetical list. The zCOBOL translator program
zc390.java/class in z390.jar consists of 1625 lines of java code and uses
regular expression parsing support to translate COBOL programs into equivalent
HLASM compatible macro assembler calls.

It's interesting to note that out of about 440,000 lines of COBOL code,
the following make up over 80% of all verbs:
PERIOD, MOVE, WS, PERFORM, LABEL, GO, IF, ADD, ELSE, and EXIT.
- PERIOD is the name of the macro generated to process end of sentence closing
  structures etc.
- WS is the name of the generated macro to handle all data definitions in
  working-storage and linkage section with unique qualified names.
- LABEL is the generated macro to handle creating unique qualified paragraph names.
The translator also maps dashes in names to underscores,
double quotes to single quotes, START to ZCSTART, and END DECLARATIVES to
END_DECLARATIVES to avoid assembler conflicts.

See the [Generated HLASM Assembler Instructions]()
showing the frequency of each generated HLASM assembler instruction listed
in descending order by frequency followed by alphabetical list.
The zCOBOL directory currently contains 119 macros corresponding to COBOL verbs
and reserved section names plus 9 copybooks totaling 7470 lines of conditional
structured macro code.

The zcobol directory macros in turn call code generation macros for the target
language environment. The zcobol\z390 directory contains 44 code generation macros
and 4 copybooks totaling 13,250 lines of conditional structured macro code.
Of the million plus lines of generated HLASM assembler code generated,
over 80% consist of the following:
DS, LARL, EQU, L, MVC, BASR, DC, MVI, LAY, USING, ORG, DROP, and BRC.
- The LARL is used to address single literal pool without requiring base register.

Much work has been done and the zCOBOL compiler does compile and execute programs
including a number of EXEC CICS COBOL programs and also a number of demo and
zCOBOL regression test COBOL programs.
However, there is still much work to do to complete the NIST COBOL 1985 test suite.
See the [pending priority list](???) and join the
[zCOBOL user email group](???)
to participate in future direction of zCOBOL and the setting of priorities
going forward.

The HLASM generation macros could now be copied to other target language
directories for Java, C, and MASM and then modified to generate the appropriate
equivalent source code.
However, the current focus will remain on HLASM and since the zCOBOL generic
language macros which call the code generation macros are still subject to change,
delaying the start of other target language macro libraries is recommended.
There are existing example generation macros already included which support
compiling the zcobol\demo\HELLO.CBL program into executable J2SE Java,
Microsoft Visual C, and Intel MASM assembler.

## Summary Statistics (#Summary)

The following summary report shows the current progress toward translating,
assembling, and executing 409 NIST test programs for COBOL 1985 using zCOBOL
and z390 version v1.5.01.
This report was generated automatically using the following set of
ZPAR utility commands:
    ``` dos
    call zpar\ZPARGEN  zpar\ZCCLG \work\nist\src\*.CBL
    call zpar\ZPARSUM2            \work\nist\src\*.ERR
    ```

|      |     |XLAT |XLAT    |ASM      |ASM  |ASM     |EXEC |EXEC    |
|ID    |TOT  |RC=0 |RC&gt;0 |RC&lt;=4 |RC=8 |RC&gt;8 |RC=0 |RC&gt;0 |
|------|-----|-----|--------|---------|-----|--------|-----|--------|
|CM    |  7  |   7 |      0 |       0 |   7 |      0 |   0 |      0 |
|DB    | 10  |  10 |      0 |       0 |  10 |      0 |   0 |      0 |
|EX    |  1  |   1 |      0 |       0 |   1 |      0 |   0 |      0 |
|IC    | 35  |  35 |      0 |      26 |   9 |      0 |   7 |      7 |
|IF    | 42  |  42 |      0 |       0 |  42 |      0 |   0 |      0 |
|IX    | 38  |  38 |      0 |       0 |  38 |      0 |   0 |      0 |
|NC    | 92  |  92 |      0 |      18 |  72 |      0 |   2 |     18 |
|OB    |  9  |   9 |      0 |       3 |   6 |      0 |   0 |      2 |
|RL    | 32  |  32 |      0 |       0 |  32 |      0 |   0 |      0 |
|RW    |  4  |   4 |      0 |       0 |   4 |      0 |   0 |      0 |
|SG    | 10  |  10 |      0 |       0 |  10 |      0 |   0 |      0 |
|SM    |  7  |   7 |      0 |       1 |   6 |      0 |   0 |      1 |
|SQ    | 82  |  82 |      0 |       0 |  82 |      0 |   0 |      0 |
|ST    | 39  |  39 |      0 |       0 |  39 |      0 |   0 |      0 |
|------|-----|-----|--------|---------|-----|--------|-----|--------|
|Total |408  | 408 |      0 |      48 | 358 |      0 |   9 |     28 |

Notes:
1. *The total NIST COBOL 1985 test suite includes 459 programs.*
2. SM201A and SM206A have been removed pending completion of RPI 1068 support
   for split pseudo literal for COPY REPLACING.
3. IC223a thru IC237A (11) have been removed pending RPI 1070 to add support
   for batch compiles from single CBL source with END PROGRAM statements.
4. Removed 31 programs of the form IDNNNM which intentionally raise flags to test
   for error checking.
5. Removed SM201A through SM208A until RPI 1006 is implemented to support
   the REPLACE verb.
6. All of the remaining 408 programs translate and assemble without any abnormal
   terminations in 30 minutes which is an average of about 4 seconds per program
   on Dell 3 GHZ duo core Windows Vista system..
7. The next step will be to add missing functions identified by MNOTEs in order
   to assemble and execute successfully.

## Translation to zcobol Macro Calls (#Translation)

All of the generated MLC files for the 409 NIST COBOL programs were read
to produce this report using the command:
    ``` dos
    zpar\ZPARMLC \work\NIST\SRC\*.MLC
    ```

| Statistics                               |
|------------------------------------------|
| ZPARMLC TOTAL OPCODES = 99               |
| ZPARMLC TOTAL LINES = 777169             |
| ZPARMLC CURRENT DATE=08/22/09 TIME=06.11 |

| Frequency | OPCODE           |
|-----------|------------------|
|    339044 | @_COMMENT        |
|    129509 | PERIOD           |
|     78200 | MOVE             |
|     70231 | WS               |
|     46035 | PERFORM          |
|     43839 | LABEL            |
|     20826 | GO               |
|     14095 | IF               |
|      7932 | ADD              |
|      5752 | ELSE             |
|      2053 | EXIT             |
|      1717 | WRITE            |
|      1198 | CLOSE            |
|      1196 | OPEN             |
|       997 | READ             |
|       932 | SET              |
|       932 | DATA             |
|       917 | PROCEDURE        |
|       883 | SELECT           |
|       842 | FD               |
|       661 | COMPUTE          |
|       487 | STOP             |
|       474 | END              |
|       466 | IDENTIFICATION   |
|       462 | ENVIRONMENT      |
|       451 | ZCOBOL           |
|       451 | PROGRAM_ID       |
|       451 | CONFIGURATION    |
|       427 | FILE             |
|       422 | INPUT_OUTPUT     |
|       422 | FILE_CONTROL     |
|       418 | WORKING_STORAGE  |
|       409 | COPY             |
|       342 | SUBTRACT         |
|       296 | WHEN             |
|       296 | REWRITE          |
|       269 | ACCEPT           |
|       219 | DIVIDE           |
|       218 | NEXT             |
|       186 | ZCSTART          |
|       154 | NOT              |
|       152 | MULTIPLY         |
|       150 | RETURN           |
|       147 | SEARCH           |
|       134 | USE              |
|       125 | CALL             |
|       111 | INSPECT          |
|       103 | DISPLAY          |
|        96 | ALTER            |
|        95 | EVALUATE         |
|        94 | CONTINUE         |
|        90 | END_DECLARATIVES |
|        64 | SEND             |
|        47 | END_IF           |
|        42 | SORT             |
|        40 | ZCSD             |
|        37 | RELEASE          |
|        34 | UNSTRING         |
|        34 | STRING           |
|        31 | DELETE           |
|        30 | I_O_CONTROL      |
|        30 | END_DIVIDE       |
|        29 | ENABLE           |
|        28 | SPECIAL_NAMES    |
|        22 | LINKAGE          |
|        22 | @ERROR@          |
|        21 | END_READ         |
|        20 | ALPHABET         |
|        19 | DISABLE          |
|        19 | CD               |
|        18 | END_SUBTRACT     |
|        18 | END_ADD          |
|        14 | GENERATE         |
|        14 | CLASS            |
|        13 | RECEIVE          |
|        12 | END_MULTIPLY     |
|        11 | INITIALIZE       |
|        11 | COMMUNICATION    |
|        11 | CANCEL           |
|         9 | END_EVALUATE     |
|         7 | END_PERFORM      |
|         6 | TERMINATE        |
|         6 | SYMBOLIC         |
|         6 | REPORT           |
|         6 | RD               |
|         6 | MERGE            |
|         6 | INITIATE         |
|         3 | DATE_COMPILED    |
|         2 | SOURCE_COMPUTER  |
|         2 | OBJECT_COMPUTER  |
|         2 | DECIMAL_POINT    |
|         2 | CURRENCY         |
|         1 | SECURITY         |
|         1 | SD               |
|         1 | ORDER            |
|         1 | INSTALLATION     |
|         1 | DATE_WRITTEN     |
|         1 | AUTHOR           |
|         1 | @_BLANK          |

| OPCODE           | Frequency |
|------------------|-----------|
| @_BLANK          |      1    |
| @_COMMENT        | 339044    |
| @ERROR@          |     22    |
| ACCEPT           |    269    |
| ADD              |   7932    |
| ALPHABET         |     20    |
| ALTER            |     96    |
| AUTHOR           |      1    |
| CALL             |    125    |
| CANCEL           |     11    |
| CD               |     19    |
| CLASS            |     14    |
| CLOSE            |   1198    |
| COMMUNICATION    |     11    |
| COMPUTE          |    661    |
| CONFIGURATION    |    451    |
| CONTINUE         |     94    |
| COPY             |    409    |
| CURRENCY         |      2    |
| DATA             |    932    |
| DATE_COMPILED    |      3    |
| DATE_WRITTEN     |      1    |
| DECIMAL_POINT    |      2    |
| DELETE           |     31    |
| DISABLE          |     19    |
| DISPLAY          |    103    |
| DIVIDE           |    219    |
| ELSE             |   5752    |
| ENABLE           |     29    |
| END              |    474    |
| END_ADD          |     18    |
| END_DECLARATIVES |     90    |
| END_DIVIDE       |     30    |
| END_EVALUATE     |      9    |
| END_IF           |     47    |
| END_MULTIPLY     |     12    |
| END_PERFORM      |      7    |
| END_READ         |     21    |
| END_SUBTRACT     |     18    |
| ENVIRONMENT      |    462    |
| EVALUATE         |     95    |
| EXIT             |   2053    |
| FD               |    842    |
| FILE             |    427    |
| FILE_CONTROL     |    422    |
| GENERATE         |     14    |
| GO               |  20826    |
| I_O_CONTROL      |     30    |
| IDENTIFICATION   |    466    |
| IF               |  14095    |
| INITIALIZE       |     11    |
| INITIATE         |      6    |
| INPUT_OUTPUT     |    422    |
| INSPECT          |    111    |
| INSTALLATION     |      1    |
| LABEL            |  43839    |
| LINKAGE          |     22    |
| MERGE            |      6    |
| MOVE             |  78200    |
| MULTIPLY         |    152    |
| NEXT             |    218    |
| NOT              |    154    |
| OBJECT_COMPUTER  |      2    |
| OPEN             |   1196    |
| ORDER            |      1    |
| PERFORM          |  46035    |
| PERIOD           | 129509    |
| PROCEDURE        |    917    |
| PROGRAM_ID       |    451    |
| RD               |      6    |
| READ             |    997    |
| RECEIVE          |     13    |
| RELEASE          |     37    |
| REPORT           |      6    |
| RETURN           |    150    |
| REWRITE          |    296    |
| SD               |      1    |
| SEARCH           |    147    |
| SECURITY         |      1    |
| SELECT           |    883    |
| SEND             |     64    |
| SET              |    932    |
| SORT             |     42    |
| SOURCE_COMPUTER  |      2    |
| SPECIAL_NAMES    |     28    |
| STOP             |    487    |
| STRING           |     34    |
| SUBTRACT         |    342    |
| SYMBOLIC         |      6    |
| TERMINATE        |      6    |
| UNSTRING         |     34    |
| USE              |    134    |
| WHEN             |    296    |
| WORKING_STORAGE  |    418    |
| WRITE            |   1717    |
| WS               |  70231    |
| ZCOBOL           |    451    |
| ZCSD             |     40    |
| ZCSTART          |    186    |

Notes:

1. @COMMENT@ counts comment lines. Note unless NOCOMMENT option is specified
   every COBOL line is also included as assembler comment.
2. @BLANK@ counts blank lines.
3. @ERROR@ counts any lines without valid assembler opcode.
4. LABEL counts macro to generate unique procedure labels.
5. PERIOD counts macro to end sentence.
6. WS counts macro to define working-storage and linkage section.
7. ZCSD counts SD sort definition sections (SD is an assembler instuction)
8. ZCSTART counts START macro (START is an assembler control instruction)

# Generated HLASM Assembler Instructions (#generated)

The following report showing all the generated HLASM opcodes for all 409 of the
NIST COBOL programs assembled using option BAL was generated using the following
command:
    ``` dos
    zpar\ZPARMLC \work\NIST\src\*.BAL
    ```

| Statistics                               |
|------------------------------------------|
| ZPARMLC TOTAL OPCODES = 142              |
| ZPARMLC TOTAL LINES = 1588292            |
| ZPARMLC CURRENT DATE=08/22/09 TIME=06.16 |

| Frequency | OPCODE           |
|-----------|------------------|
| 433584    | @_COMMENT        |
| 199445    | DS               |
| 158158    | LARL             |
| 126601    | EQU              |
| 106991    | L                |
|  92823    | MVC              |
|  92819    | BASR             |
|  70291    | DC               |
|  24827    | MVI              |
|  24727    | LAY              |
|  22338    | USING            |
|  22170    | ORG              |
|  20563    | DROP             |
|  19095    | BRC              |
|  18763    | PACK             |
|  11704    | MNOTE            |
|  11028    | CLC              |
|   9423    | LA               |
|   9151    | UNPK             |
|   8656    | ST               |
|   8143    | OI               |
|   7681    | AP               |
|   7067    | ZAP              |
|   6951    | ED               |
|   6490    | SVC              |
|   6412    | JNM              |
|   6296    | MVHI             |
|   5911    | AFI              |
|   5809    | CFI              |
|   5290    | LR               |
|   2220    | LOCTR            |
|   2150    | LH               |
|   1583    | IILF             |
|   1558    | CLI              |
|   1387    | DSECT            |
|   1367    | PRINT            |
|   1353    | BRAS             |
|   1333    | CSECT            |
|   1322    | LTR              |
|   1241    | SR               |
|   1162    | CP               |
|   1120    | STH              |
|   1001    | BR               |
|    973    | LHI              |
|    934    | MSFI             |
|    900    | LTORG            |
|    884    | LGFI             |
|    871    | BRZ              |
|    841    | CVB              |
|    787    | BCTR             |
|    735    | CVD              |
|    701    | STM              |
|    687    | LG               |
|    603    | STG              |
|    565    | JNE              |
|    551    | SP               |
|    527    | LRL              |
|    525    | ASI              |
|    521    | PFPO             |
|    469    | LMG              |
|    459    | END              |
|    451    | PUSH             |
|    451    | POP              |
|    451    | JZ               |
|    451    | CNOP             |
|    451    | BNE              |
|    442    | CXSTR            |
|    409    | COPY             |
|    304    | A                |
|    291    | AR               |
|    274    | SGR              |
|    262    | MVHHI            |
|    259    | AHI              |
|    247    | LGR              |
|    236    | CVBG             |
|    195    | EDMK             |
|    182    | STMG             |
|    180    | DP               |
|    166    | AG               |
|    165    | CHI              |
|    149    | CVDG             |
|    141    | MVGHI            |
|    117    | SGFR             |
|    116    | D                |
|    114    | SRP              |
|    114    | LT               |
|    110    | SRL              |
|     95    | CG               |
|     75    | AGF              |
|     69    | MVCL             |
|     67    | CGR              |
|     61    | LGF              |
|     52    | AGFR             |
|     44    | JM               |
|     33    | S                |
|     27    | MP               |
|     26    | IILL             |
|     26    | IILH             |
|     25    | SLL              |
|     25    | JL               |
|     25    | JH               |
|     25    | B                |
|     24    | CLCL             |
|     23    | LGH              |
|     22    | @ERROR@          |
|     21    | SRAG             |
|     21    | DXTR             |
|     16    | MVZ              |
|     14    | ALG              |
|     14    | ALCG             |
|     13    | LTG              |
|     13    | AH               |
|     11    | MEXIT            |
|     11    | MEND             |
|     11    | MACRO            |
|     11    | CD               |
|     10    | LPR              |
|     10    | LD               |
|     10    | JNO              |
|     10    | AGFI             |
|      9    | DR               |
|      8    | CH               |
|      8    | C                |
|      7    | SLGR             |
|      7    | SLBGR            |
|      6    | MSG              |
|      5    | DSGR             |
|      4    | TM               |
|      4    | STD              |
|      4    | SH               |
|      4    | SG               |
|      4    | MS               |
|      4    | LDR              |
|      4    | CLGR             |
|      2    | MSR              |
|      1    | SD               |
|      1    | MXTR             |
|      1    | LPGR             |
|      1    | JO               |
|      1    | JNL              |
|      1    | AGSI             |
|      1    | @_BLANK          |

| OPCODE           | Frequency |
|------------------|-----------|
| @_BLANK          |      1    |
| @_COMMENT        | 433584    |
| @ERROR@          |     22    |
| A                |    304    |
| AFI              |   5911    |
| AG               |    166    |
| AGF              |     75    |
| AGFI             |     10    |
| AGFR             |     52    |
| AGSI             |      1    |
| AH               |     13    |
| AHI              |    259    |
| ALCG             |     14    |
| ALG              |     14    |
| AP               |   7681    |
| AR               |    291    |
| ASI              |    525    |
| B                |     25    |
| BASR             |  92819    |
| BCTR             |    787    |
| BNE              |    451    |
| BR               |   1001    |
| BRAS             |   1353    |
| BRC              |  19095    |
| BRZ              |    871    |
| C                |      8    |
| CD               |     11    |
| CFI              |   5809    |
| CG               |     95    |
| CGR              |     67    |
| CH               |      8    |
| CHI              |    165    |
| CLC              |  11028    |
| CLCL             |     24    |
| CLGR             |      4    |
| CLI              |   1558    |
| CNOP             |    451    |
| COPY             |    409    |
| CP               |   1162    |
| CSECT            |   1333    |
| CVB              |    841    |
| CVBG             |    236    |
| CVD              |    735    |
| CVDG             |    149    |
| CXSTR            |    442    |
| D                |    116    |
| DC               |  70291    |
| DP               |    180    |
| DR               |      9    |
| DROP             |  20563    |
| DS               | 199445    |
| DSECT            |   1387    |
| DSGR             |      5    |
| DXTR             |     21    |
| ED               |   6951    |
| EDMK             |    195    |
| END              |    459    |
| EQU              | 126601    |
| IILF             |   1583    |
| IILH             |     26    |
| IILL             |     26    |
| JH               |     25    |
| JL               |     25    |
| JM               |     44    |
| JNE              |    565    |
| JNL              |      1    |
| JNM              |   6412    |
| JNO              |     10    |
| JO               |      1    |
| JZ               |    451    |
| L                | 106991    |
| LA               |   9423    |
| LARL             | 158158    |
| LAY              |  24727    |
| LD               |     10    |
| LDR              |      4    |
| LG               |    687    |
| LGF              |     61    |
| LGFI             |    884    |
| LGH              |     23    |
| LGR              |    247    |
| LH               |   2150    |
| LHI              |    973    |
| LMG              |    469    |
| LOCTR            |   2220    |
| LPGR             |      1    |
| LPR              |     10    |
| LR               |   5290    |
| LRL              |    527    |
| LT               |    114    |
| LTG              |     13    |
| LTORG            |    900    |
| LTR              |   1322    |
| MACRO            |     11    |
| MEND             |     11    |
| MEXIT            |     11    |
| MNOTE            |  11704    |
| MP               |     27    |
| MS               |      4    |
| MSFI             |    934    |
| MSG              |      6    |
| MSR              |      2    |
| MVC              |  92823    |
| MVCL             |     69    |
| MVGHI            |    141    |
| MVHHI            |    262    |
| MVHI             |   6296    |
| MVI              |  24827    |
| MVZ              |     16    |
| MXTR             |      1    |
| OI               |   8143    |
| ORG              |  22170    |
| PACK             |  18763    |
| PFPO             |    521    |
| POP              |    451    |
| PRINT            |   1367    |
| PUSH             |    451    |
| S                |     33    |
| SD               |      1    |
| SG               |      4    |
| SGFR             |    117    |
| SGR              |    274    |
| SH               |      4    |
| SLBGR            |      7    |
| SLGR             |      7    |
| SLL              |     25    |
| SP               |    551    |
| SR               |   1241    |
| SRAG             |     21    |
| SRL              |    110    |
| SRP              |    114    |
| ST               |   8656    |
| STD              |      4    |
| STG              |    603    |
| STH              |   1120    |
| STM              |    701    |
| STMG             |    182    |
| SVC              |   6490    |
| TM               |      4    |
| UNPK             |   9151    |
| USING            |  22338    |
| ZAP              |   7067    |

Notes:

1. @COMMENT@ count comments.
2. @MAC_COMMENT@ count macro comments.
3. @BLANK@ count blank lines.
4. @ERROR@ count any lines without valid assembler opcode.

## Pending Priority List for zcobol Development (#pending)

The following priorities for the open source zCOBOL portable COBOL compiler
are based on user feedback. Special thanks to Bill Klein for starting the process.

- Nucleus
    + Data Division (fix a few remaining unsupported clauses such as JUSTIFIED RIGHT)
    + IF (add support for expressions using ZC_CALC shared with COMPUTE)
    + MOVE CORRESPONDING
    + Functions
        - Date functions
        - Ordering  (MIN, MAX, ORD-MIN, and ORD-MAX)
        - Misc ORD, CHAR, UPPER-/LOWER-CASE
        - Math functions (SQRT, SIN, COS, etc.)
- Debugging
    + EXHIBIT (READY/RESET TRACE already done)
- File Access
    + Sequential (ESDS, QSAM,LINE SEQUENTIAL already done)
    + Random (RRDS, BDAM)
    + Indexed (KSDS)
- Source text management
    + REPLACE (COPY REPLACING already supported)
- SORT and MERGE
- Last priority after everything else is done
    + Segmentation
    + Communications

To submit your priorities, join the [zCOBOL user email group](???)
and post your requests:


## Trademarks {#Trademarks}

IBM, CICS, HLASM, MVS, OS/390, VSAM, z9, z10, and z/OS
are registered trademarks  of International Business Machines Corporation
 
Dell, Windows, Java, and MASM are registered trademarks of their respective owners.

## Credits {#Credits}

Author : Don Higgins
