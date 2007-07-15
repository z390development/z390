z390 VSE Support readme as of 03/13/07 with v1.3.02b

z390 supports the assembly and execution of VSE
compatible mainframe assembler application programs
using macros in z30\vse\mac concatenated with z390\mac.

For example the demo VSE "Hello World" program vse\demo\DEMOVSE1.MLC 
can be assembled linked and executed with the command:

asmlg vse\demo\DEMOVSE1 sysmac(vse\mac+mac)

Additional demos and regression tests include use of CDLOAD and 
CDDLETE, DTF file I/O, and GETVIS and FREEVIS dynamic memory management.

To regression test all the vse demo and test programs run RTVSE.BAT

See the z390 VSE User Guide for more information on VSE support:

http://www.AutomatedSoftwareTools.com/z390/z390_VSE_User_Guide.pdf

For more information on z390 and VSE support visit www.z390.org
