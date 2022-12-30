# zCOBOL User Guide

!!! Warning
    There are still a number of missing functions in zCOBOL, and until such time
    as zCOBOL successfully passes the NIST ANSI COBOL 1985 Test Suite, zCOBOL
    should be considered to still be in a beta test state.

## Overview

zCOBOL is an open source portable mainframe COBOL compiler available as part of
the z390 open source portable mainframe assembler.

The zCOBOL compiler has been developed as a new flexible tool for testing and
modernizing COBOL applications without requiring rewriting existing programs.

## Getting Started

First, you will need to [install z390](/getting_started/install)

Next, try the [zCOBOL Quickstart](/getting_started/quickstart#hello-zcobol) which walks through 
how to create and run your first zCOBOL program.

You can compile and run the provided `HELLO` COBOL program by using the following command:

=== "Windows"
    ```batch
    cblclg zcobol\demo\HELLO
    ```

=== "MacOS/Unix"
    ```bash
    cblclg zcobol/demo/HELLO
    ```

The above command uses the zCOBOL to HLASM compile, link, and execute command
to compile the COBOL hello world program `HELLO.CBL` into an
executable HLASM compatible assembler program `HELLO.MLC`
which is assembled using zCOBOL macro libraries zcobol+zcobol\z390 and linked
into z390 executable load module `HELLO.390` which is then executed to generate
WTO display of "Hello World" on the display log and on the console log file
`HELLO.LOG`.

## Running your program

You can compile, link, and execute a COBOL program in any directory by
specifying the path and name of the program in the `cblclg` command.

The source COBOL program must be in ASCII format and have the extension of `CBL`.

Use the compiler command `cblc` to compile to relocatable object form.
Use the compiler command `cblcl` to compile and link to 390 load module form
with statically linked z390 and/or zCOBOL programs included.

Once the programs have been successfully linked, then you can use the
z390 `exec` command to execute a load module.

For more about all the options available for z390 executable programs
see the [z390 User Guide](/user_guide/commands)

## Debugging your program

Once you have successfully compiled a COBOL program into a z390 load module,
you can run it with the command EXEC filename.

If the program aborts or fails to produce the expected results, the next step
is to debug the problem.

The zCOBOL option [TRACE](/user_guide/options/zCOBOL_options#TRACE) can be specified to generate a WTO display of the name
of each COBOL paragraph when it is entered. Along with listing of the program
That sometimes is enough to figure out why the program did not work.

If it is necessary to examine the generated HLASM compatible assembler code,
there are several steps that can be taken:

* First the assembly listing with suffix `PRN` produced by the zCOBOL compiler
  can be examined to see if the generated assembler instructions to
  perform the correct operation specified in the COBOL statement
  which precedes the generated code as a comment statement.
  See `zcobol/demo/HELLO.PRN` as an example.  
* Next an execution trace of every assembler instruction executed along
  with the data values associated with each instruction can be produced
  by adding the option `TRACE(E)` which results in file with TRE suffix.
  If you also specify TRACE option, the WTO for every paragraph will also appear
  in the TRE trace file which can be handy for finding the start of code
  in a particular paragraph. For example, if you run the command
  `cblclg zcobol/demo/POWERS TRACE(E)` then you can view the resulting executing
  trace file `zcobol/demo/POWERS.TRE` as well as the log file `zcobol/demo/POWERS.LOG`.
* If the execution trace fails to pinpoint the problem, another option is
  to include debug test and display statements in the program to further
  isolate where the problem is occurring.

## zCOBOL Compiler Commands

The following is a list of commands available as part of zCOBOL.

These commands are scripts that are included in the z390 source and distribution
and can be enabled by including the script directory in your system path.

Alternatively you can reference the scripts directly from the command line by
specifying the full path.

=== "Windows"

    `(z390 install dir)\bat`

=== "MacOS/Unix"

    `(z390 install dir)/bash`

### Command reference

#### zc390

Convert CBL source file to macro assembler MLC source file

#### cblc

Compile CBL to HLASM BAL and assemble to relocatable object code

#### cblcl

Compile CBL to HLASM BAL, assemble, and link to z390 load module

#### zc390clg

Compile CBL to HLASM BAL, assemble, link, and execute z390 load module


## zCOBOL demos and regression tests

To run all the zCOBOL demo programs, you can execute the script `runcbldemos`
(located in bash and bat folders) which will compile and execute them. You can 
then view the log file for each demo program to see the output produced. 

You can also run all the zCOBOL regression tests using the command `runcbltests`
(located in bash and bat folders) and look at the source code and generated output.

### Demo Programs

The following zCOBOL demo programs can be found in `zcobol/demo`

| Program  | Notes                                                                          |
|----------|--------------------------------------------------------------------------------|
| HELLO    | Display "Hello World" and STOP RUN                                             |
| DATETIME | ACCEPT current date and time and display month, day of week, and year          |
| COPYFILE | Read line sequential ASCII file and copy it to new output line sequential file |
| POWERS   | Calculate and display powers of 2 up to 2^31^                                  |

### Regression test programs

The following zCOBOL regression test programs can be found in `zcobol/test`

| Program  | Notes                                                                                      |
|----------|--------------------------------------------------------------------------------------------|
| TESTADD1 | Test 225 combinations of ADD                                                               |
| TESTADD2 | Test 225 combinations of ADD with different implied decimals                               |
| TESTASM4 | Assembler module statically linked with TESTCAL3.CBL                                       |
| TESTBFP1 | Test Binary Floating Point support                                                         |
| TESTCAL1 | CALL TESTCAL2 statically linked                                                            |
| TESTCAL2 | CALL TESTCAL3 dynamically                                                                  |
| TESTCAL3 | Dynamically loaded zcobol module which calls statically linked TESTASM4 assembler routine  |
| TESTCMP1 | ADD, SUBTRACT, MULTIPLY, and DIVIDE all formats                                            |
| TESTCMP2 | Test ADD, SUBTRACT, MULTIPLY, and DIVIDE                                                   |
| TESTCMP3 | Test COMPUTE with implied decimal points for data type F, G, H, P, Q, and Z                |
| TESTCMP4 | Test COMPUTE with literals and different numberic values and implied decimals              |
| TESTCMP5 | Test 225 combinations of COMPUTE                                                           |
| TESTCMP6 | Test 225 combinations of COMPUTE with different implied decimals                           |
| TESTCPY1 | COPY                                                                                       |
| TESTCPY2 | nested COPY                                                                                |
| TESTDFP1 | Test Decimal Floating Point )DFP) support                                                  |
| TESTDIV1 | Test 225 combinations of DIVIDE                                                            |
| TESTDIV2 | Test 225 combinations of DIVIDE with different implied decimals                            |
| TESTDSP1 | DISPLAY all formats                                                                        |
| TESTFIL1 | Test file access                                                                           |
| TESTFIL2 | Test file access                                                                           |
| TESTFUN1 | ACCEPT, TRANSFORM, NUMERIC, etc.                                                           |
| TESTGO1  | GO TO DEPENDING ON                                                                         |
| TESTHFP1 | Test Hexidecimal Floating Point (HFP) support                                              |
| TESTIF1  | Compound IF requiring use of intermediate T/F flags                                        |
| TESTIF2  | Test IF with omitted operands such as IF A = B OR C                                        |
| TESTIF3  | Test IF with parenthesis                                                                   |
| TESTINT1 | Test integer data types H, F, G, Q, P, and Z                                               |
| TESTISP1 | INSPECT TALLYING, REPLACING, TRANSFORMING                                                  |
| TESTMOV1 | MOVE all formats                                                                           |
| TESTMOV2 | Test alignment for non-floating point moves                                                |
| TESTMOV3 | Test scaling for implied decimal for non-floating point moves                              |
| TESTMPY1 | Test 225 combinations of MULTIPLY                                                          |
| TESTMPY2 | Test 225 combinations of MULTIPLY with different implied decimals                          |
| TESTPM1  | PERFORM VARYING and PERFORM TIMES                                                          |
| TESTPM2  | Test PERFORM with duplicate paragraph names in different sections                          |
| TESTPM3  | Test reading file using nested PERFORM VARYING                                             |
| TESTRMD1 | Test move reference modification of the form MOVE F1(var1+lit1:len1) TO F2(var2+lit2:len2) |
| TESTSIX1 | Test multiple subscripts                                                                   |
| TESTSIX2 | Test SET and INDEXED form of subscripting                                                  |
| TESTSUB1 | Test 225 combinations of SUBTRACT                                                          |
| TESTSUB2 | Test 225 combinations of SUBTRACT with different implied decimals                          |
| TESTTRC1 | TRUNC                                                                                      |
| TESTTRC2 | NOTRUNC with ONSIZE                                                                        |
| TESTTRC3 | TRUNC and NOR64 to test use of DXR versus DSG                                              |
| TESTWS1  | Working storage REDEFINE and OCCURS with padding                                           |


## z390 IDE

Now you should have a z390 desktop icon which you can double click to start
the z390 GUI Interface. And now you can compile, link, and execute your first
zCOBOL program by entering the following zCOBOL command:
    ``` dos
    zc390clg zcobol\demo\HELLO
    ```
