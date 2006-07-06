z390 v1.1.00f PTF 07/06/06
Copyright 2006 Automated Software Tools Corporation


The enclosed z390.jar contains following cumulative fixes for v1.1.00:

1. RPI 313 - correct mz390 and az390 errors:
a. Fix comma delimited continuation statements with L' type 
   operators appearing in operands.
b. Set positional operand count to 0 when single , used to
   allow comments on statement.
c. Fix expression parser to allow unary operator before (...).
d. Show original opcode on BAL/PRN when OPSYN used.
e. Set az390 return code to max MNOTE but do not issue error
   which forces return code 16.
f. Replace duplcate code in mz390 and az390 for statement parsing
   and continuation reformating using precompiled regular expression
   parser versus less efficient string split method.

2. RPI 314 - add alternate conditional macro opcodes AIFB and AGOB.

3. RPI 315 - add compond AIF/AIFB and computed AGO/AGOB support.
Improve mz390 speed using key indexing for macro labels and increase
key hash table size to reduce duplicate hash key compares.  Add new
option REFORMAT to align opcode at position 10 and operands at position
16.  The default is off to allow indendation of source for structured
programs.

4. RPI 316 - add ZSTRMAC directory with z390 structured programming
macros for DO WHILE=, DO UNTIL=, ENDDO, IF, ELSEIF, ELSE, and ENDIF.
See documentation in ZSTRMAC\ZSTRDOC.TXT and see demo program
DEMO\DEMOSTR1.MLC which can be compiled and executed with the command:
ASMLG DEMO\DEMOSTR1 SYSMAC(MAC+ZSTRMAC) SYSCPY(MAC+ZSTRMAC)

5. RPI 227 - add TN3270 extended data stream support as follows:
a.  Replace character based screen with Graphics 2D grid screen
which can support APL characters including line drawing, color, etc.
b.  Add support for graphics escape order X'08' followed by APL 
graphics character.  See DEMOAPL1.MLC for display of all APL characters.
c.  Add support for SFE and SA orders to set extended data stream 
attriute, highlighting, and color.  Only color is currently fully 
implemented.  See DEMOAPL1.MLC which changes default screen background
and text colors from yellow on black to black on green vis GUAM SCREEN
command and also set display title text color to blue using SFE command.

6. RPI 317 update DFHEIA0 and DFHEIA1 to allow assembly of non EXEC CICS
   programing using the CICS option. It no longer tries to gen DSA.

7. RPI 319 update TESTCMD2 to wait for both CMDPROC processes to end.

8. RPI 321 correct ESTAE and ESPIE MNOTE messages.

9. RPI 322 support CALL macro MF=L format with VL.

10.RPI 323 correct TEST to stop on opcode when first in exit.

11.RPI 324 correct GETMAIN A= and STORAGE ADDR= options to storge
   address in storage.

12.RPI 327 issue error if duplication factor is negative.

13.RPI 329 remove file suffix from all &SYS...._MEMBER global 
   variables for mainframe compatibility. 

14.RPI 330 add all MNOTE's with severity > 0 to ERR log file.

15.RPI 331 expand macro for opcode which has been cancelled by
   OPSYN and correct repeated execution of macro with inline macro.

16.RPI 334 move the structured macros to main MAC directory to remove
   requirement for extra macro library.

17.RPI 336 correct parsing error on D' operator and add support for
   evalution of ordinary symbol value in SET expressions.

18.RPI 337 correct z390 GUI interface to correctly pass long spacey file
   name paths without truncating first character of parm.

19.RPI 338 correct mz390 to handle duplication factor for string.

20.RPI 339 ignore unused operands on PRINT such as NOMCALL and UHEAD.

21.RPI 340 add mz390 support for all A-Z plus user defined ordinary
   symbol types and support all 3 operands on EQU.

22.RPI 341 correct error on ORG statement with no operands when used
   where it is not required since there has been no change in location.

23.RPI 342 correct mz390 parsing of prefix operators such as D', L', T'
   within parenthesis.

24.RPI 343 correct mz390 N'&array to return highest subscript stored
   instead of max.

25.RPI 344 change mz390 to issue MNOTE 4 warnings for undefined macro
   labels.  Error only occurs if branch is attempted.
   
26.RPI 345 correct mz390 to correctly handle store into subscripted
   array using subscripted array subscript.
   
27.RPI 346 correct mz390 to evaluate self defining character constant
   in either lower or upper case.  Unlike az390, it was forcing upper 
   case.     
   
28.RPI 349 correct loop in mz390 when EQU has expression error.

29.RPI 350 change TRACEALL to show source during loading. Note
   TRACEM does not show source during loading and only shows result
   of set statements.
   
30.RPI 325 Correct KEB and KED instrucutions to allow exact zero.

31.RPI 326 Add support for TCEB, TCDB, and TCXB data class instr.

33.RPI 333 Add SRNM and correct rounding mode instructions.

34.RPI 335 Correct mask trace for RRF2 type instructions.
      
35.RPI 347 Add TEST mode address stop break support using A addr,
T addr, or G addr to set and S command to clear all breaks.

36.RPI 348 correct trace to only show 2 bytes for half word instr.
add add blank line to trace after each successful branch.

37.RPI 351 correct OPSYN support to replace opcodes used previously.

38.RPI 352 correct DCB support to allow 4 byte alignment versus 8.

39.RPI 353 correct mz390 expression parser to support prefix operator
such as UPPER followed by (...) type expression.
 
40.RPI 356 correct az390 to prevent trap on CVD with no operands.

To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

ASMMSP.CPY - COPYBOOK IN MAC TO INCLUDE ZSTRMAC.CPY MACROS          
DO.MAC       
ELSE.MAC
ELSEIF.MAC
ENDDO.MAC
ENDIF.MAC
IF.MAC
ZSTRDOC.TXT - DOCUMENTATION ON ZSTRMAC MACROS
ZSTRMAC.CPY - COPYBOOK TO INCLUDE MACROS AND COPYBOOKS
ZSTREQU.CPY - COPYBOOK WITH TEST CONDITION MASK EQUATES E,NE, ETC.

3.  Copy the updated CICS directory to z390\cics directory.

DFHEIA0
DFHEIA1

4.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

5.  The following regression test sources
    are included:

TESTASM1 - updated with unary ops test and continue tests
TESTCAL2 - add test of CALL MF=L
TESTCMD2 - test waiting on both CMDPROC processes to complete
DEMOAPL1 - display TN3270 GE APL codes and use of color options
DEMOSTR1 - demo program using ZSTRMAC structured macros

Visit www.z390.org for more information on latest updates.
