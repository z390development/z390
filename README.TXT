z390 v1.1.00c PTF 05/24/06
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

      
To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

ASMMSP.CPY - COPYBOOK IN MAC TO INCLUDE ZSTRMAC.CPY MACROS          

3.  Copy the updated structured programming macro directory zstrmac to 
the z390\zstrmac direcotry.

DO.MAC       
ELSE.MAC
ELSEIF.MAC
ENDDO.MAC
ENDIF.MAC
IF.MAC
ZSTRDOC.TXT - DOCUMENTATION ON ZSTRMAC MACROS
ZSTRMAC.CPY - COPYBOOK TO INCLUDE MACROS AND COPYBOOKS
ZSTREQU.CPY - COPYBOOK WITH TEST CONDITION MASK EQUATES E,NE, ETC.


4.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

5.  The following regression test sources
    are included:

TESTASM1 - updated with unary ops test and continue tests
DEMOAPL1 - display TN3270 GE APL codes and use of color options
DEMOSTR1 - demo program using ZSTRMAC structured macros

Visit www.z390.org for more information on latest updates.
