z390 v1.3.00a PTF 01/20/07
Copyright 2007 Automated Software Tools Corporation

The enclosed z390.jar contains following cumulative fixes for v1.3.00:

1.  RPI 535 - correct mz390 to issue error and prevent trap on 
    computed AGO with invalid syntax or continuation.

2.  RPI 536 - issue system abend SFFF on any internal ez390 trap
    and generate memory dump along with PSW, registers, TIOT, etc.

3.  RPI 537 - add new Linux mods zip file download with the following
    files contributed by Martin Ward:
    1) cmd.pl  - Emulation of cmd.exe for handling simple batch files
    2) z390     -  perl script for running the z390 GUI and command line
    3) src/mainClass.txt - main z390 class for compiling under Linux
    4) DOIT: shell script for compiling z390.java under Linux
    http://www.automatedsoftwaretools.com/z390/Linux/z390_Linux_Support.htm

4.  RPI 538 - Correct the following:
    1) Correct TEST to correctly handle single step through EX target  
    2) Correct assembly of SS PKA instruction to handle explicit d1(b1) with no length specified (was treating b1 as length versus base)
    3)  Add option PROTECT to prevent user modification of PSA.  
        See RT\TEST\TESTSVC1.MLC which requires NOPROTECT option now in order
        to modify SVC new PSW in PSA.
    4) Correct assembly of DC with duplication factor for P type data

5.  RPI 539 - Update BMK\INSTTIME.MLC benchmark timing report generated contributed
    by Melvyn Maltz which now includes DFP instructions.  See updated report here:
    http://www.automatedsoftwaretools.com/z390/z390_Benchmark_Instruction_Timing.htm

6.  RPI 540 - correct the following bugs:
    1) Correct pz390 TRACE to show 64 register for SRLG etc.
    2) Correct pz390 TRACE to show 128 bit DLG and MLG regs
    3) Correct pz390 DLG to prevent internal trap on divide by zero
    4) Optimize DLG and DLGR by removing unnecessary move to work area before conversion to BigInteger.
    5) Issue TEST error if address entered exceeds allocated memory

7.  RPI 541 - correct z390 GUI interface to reset file selection dialog
    invoked from file menu to correct file type starting at current directory 
    each time.  Use CD menu to reset current directory.  

See updated RT\TEST\TESTINS2.MLC
See updated RT\TEST\TESTDC1.MLC
To install PTF follow these steps.

1.  Copy the z390.jar file into install directory for z390
    replacing existing copy of z390.jar.

2.  If you need to change Java source for this PTF version, all
    the Java source is in src directory and can be copied to 
    z390\src directory.

Visit www.z390.org for more information on latest updates.
