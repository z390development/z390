# Structured programming macros

Structured Programming Macros (SPM) provide a compatible subset of the macros 
that IBM supplies as part of its licensed HLASM Toolkit product.

These macros are based on the original 1978 public domain structure programming
macros distributed as SHARE CBT tape #177. They have been enhanced to use 
[structured macro extensions](../user_guide/z390/structured_macro.md).


## Macro reference

* IF, ELSEIF, ELSE, ENDIF - alternate selection
* DO, ENDDO - iteration
* SELECT, WHEN, OTHRWISE, ENDSEL - selection by value using compares
* CASENTRY, CASE, ENDCASE - selection by value using branch table
* PM, PENTRY, PEXIT - perform block of code and return to next instruction

* ASMMSP.CPY  - copybook for compatibility with HLASM Toolkit programs that 
  require this copybook
* ZSTRGBL.CPY - copybook included in each structured programming macros (SPM) with 
  shared global variables
* ZSTREQU - macro called from ZSTRGBL to define shared EQU's for condition code 
  operands


## HLASM compatible versions

Also included are translated versions of all the structured SPM's in the 
z390\mac\spm directory. The translated version are for use on systems with 
HLASM or other mainframe assemblers which do not yet support the SPE's.

The following regression test programs are included:

* rt\test\TESTSPM1.MLC - test IF, DO, SELECT, and CASENTRY structures

The following z390 utilities are written in structured form using these 
structured macro:

* linklib\REPRO.MLC - VSAM load and unload utility
* linklib\SUPERZAP.MLC - dump and path file utility
* linklib\FPCONMFC.MLC - interface between z390 BFP/DFP/HFP regression tests and 
  the mainframe compatible  external format conversion routine 
  linklib\FPCONVRT.MAC.
