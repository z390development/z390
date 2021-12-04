# Structured programming macros

Structured Programming Macros (SPM) are provided for use in coding assembler 
programs compatible with HLASM.

The macros provide a compatible subset of the macros that IBM supplies as part 
of its licensed HLASM Toolkit product.

These macros are based on the original 1978 public domain structure programming
macros distributed as SHARE CBT tape #177


## Macro reference

IF, ELSEIF, ELSE, ENDIF - alternate selection
DO, ENDDO - iteration
SELECT, WHEN, OTHRWISE, ENDSEL - selection by value using compares (v1403b first release)
CASENTRY, CASE, ENDCASE - selection by value using branch table (v1403b first release)
PM, PENTRY, PEXIT - perform block of code and return to next instruction
ASMMSP.CPY  - copybook for compatibility with HLASM programs that require this copybook
ZSTRGBL.CPY - copybook included in each SPM with shared global variables
ZSTREQU - macro called from ZSTRGBL to define shared EQU's for condition code operands

These macros have been updated from the original public domain macros in the SHARE CBT tape #177 dated 1978.  All these macros have been updated to use SPE structured programming macro extensions to eliminate all macro labels and use of AGO's.  Also included are translated versions of all the structured SPM's in the z390\mac\spm directory.  The translated version are for use on systems with HLASM or other mainframe assemblers which do not yet support the SPE's.

The following regression test programs are included:

rt\test\TESTSPM1.MLC - test IF, DO, SELECT, and CASENTRY structures

The following z390 utilities are written in structured form using these SPM's:

* linklib\REPRO.MLC - VSAM load and unload utility
* linklib\SUPERZAP.MLC - dump and path file utility
* linklib\FPCONMFC.MLC - interface between z390 BFP/DFP/HFP regression tests and the mainframe compatible  external format conversion routine linklib\FPCONVRT.MAC contributed by David Bond.
The ZSTRMAC1.MLC bootstrap conditional macro code program is the last unstructured macro code program I ever intend to write with 169 explicit macro labels.  The structured version has no labels, and I would submit that it is significantly easier to read and maintain using ACALL to reduce the logic down into small logical blocks.  Any and all feedback is welcome.  Send comments and suggestions to Don Higgins.


