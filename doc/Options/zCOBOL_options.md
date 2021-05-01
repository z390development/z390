# zCOBOL options

Option          | Default | Description
----------------|---------|------------
@file           | NO      | Retrieve additional options from free form text file with default suffix OPT.  Options can be specified delimited by spaces on as many lines as required.  All characters on a line following * are ignored as comments. The @file option can be nested.  The default path is the program path.
CICS            | NO      | Parse COBOL EXEC CICS commands into z390 EXEC CICS compatible macro calls and also rename working storage to DFHEISTG.
COMMENT         | YES     | Generate MLC comments showing original COBOL statement preceding each macro call statement.
EXTEND          | YES     | Support up to 31 digits for DISPLAY (Z) and COMP-3 (P) type data items rather than limiting precision to ANSI 1985 standard of 18.
FLOAT(DECIMAL)  | YES     | Set type of floating point for usage FLOAT-SHORT, FLOAT-LONG, and FLOAT-EXTENDED.  The choices are FLOAT(HEX) for Hexadecimal Floating Point (HFP) like COMP-1 and COMP-2, FLOAT(BINARY) for Binary Floating Point (BFP), or the default FLOAT(DECIMAL) for Decimal Floating Point (DFP).
R64             | YES     | Generate 64 bit instructions for the 16 GPR registers where appropriate.  NOR64 restricts code generation to only use lower 32 bits of 16 GPR registers as required by z/VSE and some other operating environments.  (Note option TRUNC and NOR64 results in use of DXR instead of DGR which is more efficient.)
TRACE           | NO      | Generate WTO display of paragraph name at entry to each new paragraph in procedure division.  This provides high level trace as opposed to using the z390 TRACE(E) option which generates instruction level trace.
TRUNC           | NO      | Truncate binary data types F, G, and H to specified number of digits in PICTURE.
WARN            | YES     | Generate level 4 MNOTE warnings from zCOBOL macros.
