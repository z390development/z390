z390 v1.0.13e PTF 04/09/06
Copyright 2006 Automated Software Tools Corporation


The enclosed z390.jar contains following cumulative fixes for v1.0.13:

1. RPI 232 - support integer constants with exponents such as 1E3 = 1000

2. RPI 236 - support running z390 from CD or read only server directory

3. RPI 242 - correct mz390 not initializing LCLA and GBLC variables in 
some cases.

4. RPI 250 - correct mz390 to correctly handle keyword macro values with
special characters within (...) without quotes.

5.  RPI 237 - Correct DROP for labeled USING causing error.  See updated
TESTUSE1.

6.  RPI 251 - Fix bug in TEST trace option involving 3 flavors of RRF format
instructions.  See updated TESTTST1 which now includes trace to break on CGXR.
See TESTTST1.SVG for expected log output used to verify batch TEST session.

7.  RPI 252 - Publish updated Guide for Program Services and Storage Services
from Melvyn Maltz to document new STIMER and STORAGE macros.

8.  RPI 254 - Support blank COM, CSECT, DSECT, LOCTR, RSECT, or START.  
See TESTASM1.

9.  RPI 256 - Allow macro labels to start with ._ as well as A-Z@#$.  
Also issue error if az390 label has invalid characters.  
See updated TESTMAC1 and TESTERR2.

10. RPI 257 - Correct mz390 to accept * as second operand in substring 
expression indicating remaining length of string.  See updated TESTMAC1.

11. RPI 258 - Correct az390 to process , delimiter on ORG statement.  
See updated TESTASM1.

12. RPI 260 - Improve error message for expression parsing errors and 
add documentation to user guide on how expressions are parsed using 
variable stack and operator stack.  See updated TESTERR2.

13. RPI 235 - fix z390 CD command path display and support CD commands
    without requiring space separator like Windons (i.e. CD..)

14. RPI 264 - improve error handling as followsL
    a) Abort with error if source files contain non-ascii characters
    b) If not TEXT option, abort if source line > 80
    c) Allow up to 3 passes in az390 to reduce errors
    
15. RPI 265 - Correct az390 to support DC fields with no data for 
    alignment.  See updated TESTDC1. 

16. RPI 266 - Correct mz380 to support inline macros within macro files
    as well as open code.  See new TESTMAC8.

17. RPI 267 - Correct pz390 to only flush extended floating point cache
    registers if both registers have not been changed and prevent TEST
    mode trap when tracing to invalid instruction causing 0C1.

18. RPI 268 - Add option TEXT to enable mz390 to read and write text
    files with any line length without continuation formatting.  Also
    add DSNAME= as well as DDNAME= extentions to AREAD and PUNCH to 
    enable reading and writing multiple source files in one 
    execution.  See new TESTTXT1 which creates 2 HTML text files
    from sinle eecution of testTXT1.MLC with option TEXT. 

19. RPI 270 add extended DS/DC types CA/CE/AD/FD/VD and 8 byte RLD 
support.

20. RPI 271 correct interactive TEST dump format for last line.

21. RPI 272 correct MR instruction to only use low 32 bits of r1+1
and r2 to form 64 bit product stored in r1 and r1+1.  It was using
all 64 bits of r1 and r1+1.


22. RPI 259 add remaining &SYS global and local system variables for
compatiblity. See TESTSYS1.

23. RPI 273 correct BAL line number references in error messages.
See updated TESTERR1.

24. RPI 274 correct macro labels on MEND, C type self defining terms 
with embedded quotes, cumplex AIF expression with OR(' sequence, 
non specific USING reference to prior labeled USING, and OPSYN , error.
See TESTMAC8, TESTMAC1, TESTMAC2, TESTUSE1, and TESTSYN1.

25, RPI 275 correct ML and MLR to only use low 32 bits of r1+1.
See updated TESTINS2.
 
To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  Copy the updated macro directory mac to z390\mac directory.

3.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

none

3.  The following regression test sources
    are included:

TESTASM1.MLC - test blank sections, labels starting with _, ORG , 
TESTERR2.MLC - test invalid label errors
TESTDC1.MLC  - test DC support of fields without data for alignment
TESTINS2.MLC - test instructions
TESTMAC1.MLC - test macro labels starting with _  and * as substring len
TESTMAC2.MLC - test complex AIF expression with OR(' sequence
TESTMAC6.MLC - test updated to test keyword with spec. characters
TESTMAC8.MLC - test inline macros in open source and macro files
TESTMAC8.MAC - test inline macros in macro files
TESTSDT1.MLC - test updated to test integer constants with exponents
TESTSET1.MLC - test updated to test LCLA and GBLC init error
TESTTST1.TF1 - test the interactive TEST mode using batch driver file 
TESTTST1.MLC - test the interactive TEST mode using batch driver file 
TESTTXT1.MLC - test TEXT option to create 2 HTML souce files
TESTUSE1.MLC - test USING and DROP
TESTSYN1.MLC - test OPSYN with , to cancel opcode

Visit www.z390.org for more information on latest updates.
