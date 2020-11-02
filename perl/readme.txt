================================================================================
Begin readme.txt
================================================================================
Initial:     2020-10-12
Last update: 2020-10-14

To:   Don Higgins
From: John Ganci

********************************************************************************

Contents

The v17b02-pk01.zip file contains the following RPIs:

    1. RPI 2011 Linux-related changes for z390 version 1.7.

********************************************************************************
1. Installation instructions

--------------------------------------------------------------------------------
1.1. Unzip the v1-7_b02-pk01.zip file. The resulting structure is a directory
     named v1-7_b02-pk01 that contains the readme.txt file you are now reading
     and three subdirectories:

         bat
         perl
         src

     ***************************************************************************
     *  WARNING: All text files have unix endline format (lines end with new
     *           line).
     *
     *           Using them on a Windows machine might cause problems.
     *           If so, use an editor like Notepad++ to edit each one
     *           and save it in Windows format.
     ***************************************************************************


     ***************************************************************************
     *  WARNING: We are assuming that the "Linux Perl" subdirectory is being
     *           renamed to "perl".
     *
     *           See 1.3.1 below.
     ***************************************************************************

--------------------------------------------------------------------------------
1.1.1. The bat subdirectory contains the BAT files that have changes. There are
       eight that require change.

       ASMLG.BAT
       AZ390.BAT
       CBLC.BAT
       CBLCL.BAT
       CBLCLG.BAT
       EZ390.BAT
       LZ390.BAT
       ZC390.BAT

       1, ASMLG.BAT

          The beta 1 version has two lines that should be removed:

          a. Line 3: if exist /usr/local/lib/z390/z390.jar goto linux
          b. Line 8: :linux          

          RPI 2011 changes in the Perl script cmd.pl fix a problem that arises
          if "asmlg tron <source file to process>" is issued. The older cmd.pl
          did not handle a "shift" operation. Now it does.


       2. AZ390.BAT

          Last line: Add "..\" between "dps0" and "z390.jar. That is, replace
              java -classpath %~dps0z390.jar -Xrs %J2SEOPTIONS% az390 %1 %2 %3 %4 %5 %6 %7 %8 %9
          with
              java -classpath %~dps0..\z390.jar -Xrs %J2SEOPTIONS% az390 %1 %2 %3 %4 %5 %6 %7 %8 %9

       3. CBLC.BAT

          Change "cblopt" to "CBLOPT" on the "call %~dps0MZ390 ..." line. (Line 26.)

       4. CBLCL.BAT

          Change "cblopt" to "CBLOPT" on the "call %~dps0MZ390 ..." line. (Line 26.)

       5. CBLCLG.BAT

          Change "cblopt" to "CBLOPT" on the "call %~dps0MZ390 ..." line. (Line 26.)

       6. EZ390.BAT

          Remove temporary test lines and replace "java -classpath ..." line as was done for AZ390.BAT.
          That is, replace

              if exist z390.jar goto jar
              java -classpath W:\dsh6w\work\eclipse\workspace\z390\bin -Xrs %J2SEOPTIONS% ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
              goto end
              :jar
              java -classpath z390.jar -Xrs %J2SEOPTIONS% ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9
              :end
          with
              java -classpath %~dps0..\z390.jar -Xrs %J2SEOPTIONS% ez390 %1 %2 %3 %4 %5 %6 %7 %8 %9

          The new "java -classpath ..." line is the line in EZ390.BAT with "..\" added.

       7. LZ390.BAT

          Remove tem[prary test lines and replace "java -classpath ..." line as was done for AZ390.BAT.
          That is, replace

              if exist z390.jar goto jar
              java -classpath W:\dsh6w\work\eclipse\workspace\z390\bin -Xrs %J2SEOPTIONS% lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
              goto end
              :jar
              java -classpath z390.jar -Xrs %J2SEOPTIONS% lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
              :end
          with
              java -classpath %~dps0..\z390.jar -Xrs %J2SEOPTIONS% lz390 %1 %2 %3 %4 %5 %6 %7 %8 %9

          The new "java -classpath ..." line is the line in LZ390.BAT with "..\" added.

       8. ZC390.BAT

          Last line: Add "%~dps0..\" in front of "z390.jar. That is, replace
              java -classpath z390.jar -Xrs zc390 %1 %2 %3 %4 %5 %6 %7 %8 %9
          with
              java -classpath %~dps0..\z390.jar -Xrs zc390 %1 %2 %3 %4 %5 %6 %7 %8 %9

          Note: The original line seems to work, as does the new line. Why?
                1. Added "dir" line just above the "java ..." line.
                2. When ZC390.BAT runs, the working directory is the z390 main
                   directory (/home/john/z390 for me) and z390.jar is there.
                   Therefore, the original line classpath setting is okay.
                3. The cmd.pl script replaces the "%~dps0" reference with the
                   $bat variable value, which is (for me)
                   /home/john/lib/z390/bat. Therefore, one directory up is
                   /home/john/lib/z390, the same as the current directory.
                4, So, we could replace all the "%~dps0..\z390.jar" occurrences
                   with "z390.jar" since the current directory at the time
                   seems to always be /home/john/lib/z390.
                5. I feel better leaving in the "%~dps0..\z390.jar" occurrences.
                6. This same comment applies to ALL bat files that were changed
                   to use "%~dps0..\z390.jar". It is my opinion that the changes
                   should remain.
               
--------------------------------------------------------------------------------
1.2.1. The perl subdirectory contains the Perl scripts that require change.
       There are two.

       DOS.PL
       CMD.PL

       1, DOS.PL

          a. Look for the z390 directory using the same logic as in cmd.pl.
          b. Change the invocation of cmd.pl to use the new perl subdirectory.

       2. CMD.PL

          a. Change the directory for bat files to use the new bat subdirectory.
          b. Unreleated, but add case for "shift" in subroutine batch_file().
      
--------------------------------------------------------------------------------   
1.3.1. The src subdirectory contains the java source that require change. There
       is one module.

       z390.java

       1. z390.java

          a. Add RPI 2011 line at end of list of modifications
          b. Modify the two lines where cmd.pl is invoked to use the new perl
             subdirectory.

          **********************************************************************
          * WARNING: The supplied change depends on the "Linux Perl"
          *          subdirectory being renamed to "perl".
          **********************************************************************
          
--------------------------------------------------------------------------------
1.4. Back up your existing beta 01 z390 directory and subdirectories.

--------------------------------------------------------------------------------
1.5. Follow your process to incorporate the changes in the source file(s)
     into your source file(s).

--------------------------------------------------------------------------------
1.6. Rebuild the z390.jar file for both Oracle and OpenJDK.

--------------------------------------------------------------------------------
1.7. Copy the appropriate new z390.jar file to wherever you have installed z390.
     This appears to be in the z390 directory.

********************************************************************************
2. Testing instructions for Windows.

   Two steps.

   1. Do whatever you do for testing.

   2. Add manual step or update regression test bat file(s) to add the
      "tron" tests for assembly and COBOL compiles. See step 3.3.2 for
      assembler and step 3.4.2 for COBOL.

********************************************************************************
3. Testing instructions for Linux or macOS. Use terminal window.

   The suggested install locations are one of

       $HOME/lib/z390
       /usr/lib/z390
       /usr/local/lib/z390

   The beta 1 file has the Perl scripts in a subdirectory named "Linux Perl".
   Note the space in the name. We assume here that the beta 2 distribution
   renames this subdirectory to perl.

   All testing is done from a terminal window.

--------------------------------------------------------------------------------
3.1. Testing is done with the supplied z390 test programs. In what follows,
     we will use <z390 path> to represent the full path to the z390 directory.
     For example, if we are logged in as fred and we installed z390 in the
     home directory, then <z390 path>=/home/fred/lib/z390.

     The assembler test programs are in the tests subdirectory.

     The COBOL test programs are in the zcobol/tests subdirectory.

--------------------------------------------------------------------------------
3.2. You have three ways to invoke a Perl script:

         perl <path to directory containing script>/<Perl script name>

         ./<Perl script name> (if the current directory contains the script)

         <path to directory containing script>/<Perl script name>

     The last two require that the Perl script be "executable". When you
     initially create the z390 directory, the Perl scripts in the perl
     subdirectory are not "executable". If you want to invoke a Perl script
     using one of these two ways, then first

         change to the directory that contains the Perl script(s)
         issue
             chmod 755 <Perl script name>
         for each such script.

     We will use the second and third method, so perform the following steps:

         cd <z390 path>
         chmod 755 cmd.pl
         chmod 755 dos.pl
         chmod 755 z390.pl

     This only needs to be done once when doing a fresh install.

--------------------------------------------------------------------------------
3.3. Assembler tests. Issue

         <z390 path>/perl/dos.pl

     You will see a command processor started message followed by a new line
     where you will enter commands.

--------------------------------------------------------------------------------
3.3.1. Issue command and note output. All should end successfully.

3.3.1.1. asm <z390 path>/tests/TESTINS2.MLC

3.3.1.2. asml <z390 path>/tests/TESTINS2.MLC

3.3.1.3. asmlg <z390 path>/tests/TESTINS2.MLC

--------------------------------------------------------------------------------
3.3.2. Issue command and note output. All should end successfully. The "tron"
       parameter signals that you want "ECHO ON" for the bat file. You will see
       the same output as in 3.3.1 but additionally you will see the lines from
       the corresponding bat files, just as you would when setting ECHO ON in
       Windows.

3.3.2.1. asm tron <z390 path>/tests/TESTINS2.MLC

3.3.2.2. asml tron <z390 path>/tests/TESTINS2.MLC

3.3.2.3. asmlg tron <z390 path>/tests/TESTINS2.MLC

--------------------------------------------------------------------------------
3.3.2. End assembler test. Press Ctrl-C. The dos.pl script will end.

--------------------------------------------------------------------------------
3.4. COBOL tests. Issue

         <z390 path>/dos.pl

     You will see a command processor started message followed by a new line
     where you will enter commands.

--------------------------------------------------------------------------------
3.4.1. Issue command and note output. All should end successfully.

3.4.1.1. cblc <z390 path>/zcobol/tests/TESTADD1

3.4.1.2. cblcl <z390 path>/zcobol/tests/TESTADD1

3.4.1.3. cblclg <z390 path>/zcobol/tests/TESTADD1

--------------------------------------------------------------------------------
3.4.2. Issue command and note output. All should end successfully. Then "tron"
       parameter signals that you want "ECHO ON" for the bat file. You will see
       the same output as in 3.4.1 but additionally you will see the lines from
       the corresponding bat files, just as you would when setting ECHO ON in
       Windows.

3.4.2.1. cblc tron <z390 path>/zcobol/tests/TESTADD1

3.4.2.2. cblcl tron <z390 path>/zcobol/tests/TESTADD1

3.4.2.3. cblclg tron <z390 path>/zcobol/tests/TESTADD1

--------------------------------------------------------------------------------
3.4.2. End COBOL test. Press Ctrl-C. The dos.pl script will end.

********************************************************************************
4. Testing instructions for Linux or macOS (or Windows). Use GUI.

   See the introduction to 3 for a description of the environment.

   All testing is done from the GUI.

   We will repeat ourselves in case you did not do step 3.

--------------------------------------------------------------------------------
4.1. Testing is done with the supplied z390 test programs. In what follows,
     we will use <z390 path> to represent the full path to the z390 directory.
     For example, if we are logged in as fred and we installed z390 in the
     home directory, then <z390 path>=/home/fred/lib/z390.

     The assembler test programs are in the tests subdirectory.

     The COBOL test programs are in the zcobol/tests subdirectory.

--------------------------------------------------------------------------------
4.2. Read 3.2 to see how to invoke a Perl script.

--------------------------------------------------------------------------------
4.3. Assembler tests. Issue

         <z390 path>z390.pl

     The GUI will start.

--------------------------------------------------------------------------------
4.3.1. Each test is initiated by
         1. click File
         2. click one of ASM, ASML, ASMLG
         3. navigate to <z390 path>/tests directory
         4. double-click TESTINS2.MLC
         5. Note output in text area box; all should end successfully.

4.3.1.1. File->ASM.. navigate to <z390 path>/tests/TESTINS2.MLC

4.3.1.2. File->ASML.. navigate to <z390 path>/tests/TESTINS2.MLC

4.3.1.3. File->ASMLG.. navigate to <z390 path>/tests/TESTINS2.MLC

--------------------------------------------------------------------------------
4.3.2. Enter commands manually in the Command text box and note the output in
       the text area. All should end successfully.

4.3.2.1. asm <z390 path>/tests/TESTINS2.MLC

4.3.2.2. asml <z390 path>/tests/TESTINS2.MLC

4.3.2.3. asmlg <z390 path>/tests/TESTINS2.MLC

--------------------------------------------------------------------------------
4.3.3. Enter commands manually in the Command text box and note the output in
       the text area. All should end successfully. In addition to the output
       seen in 4.3.2, you will see ECHO ON output produced by the bat files.

4.3.3.1. asm tron <z390 path>/tests/TESTINS2.MLC

4.3.3.2. asml tron <z390 path>/tests/TESTINS2.MLC

4.3.3.3. asmlg tron <z390 path>/tests/TESTINS2.MLC

--------------------------------------------------------------------------------
4.3.4. End assembler test. Exit the GUI.

--------------------------------------------------------------------------------
4.4. COBOL tests. Issue

         <z390 path>z390.pl

     The GUI will start.

--------------------------------------------------------------------------------
4.4.1. There are no COBOL compile, compile-link, and compile-link-go shortcuts
       in the File menu. Commands are entered manually in the Command text box.
       Enter command and note output in the text area. All should end
       successfully.

4.4.1.1. cblc <z390 path>/zcobol/tests/TESTADD1

4.4.1.2. cblcl <z390 path>/zcobol/tests/TESTADD1

4.4.1.3. cblclg <z390 path>/zcobol/tests/TESTADD1

--------------------------------------------------------------------------------
4.4.2. Enter command and note output in the text area. All should end
       successfully. In addition to the output seen in 4.4.1, you will see
       ECHO ON output produced by the bat files.

4.4.2.1. cblc tron <z390 path>/zcobol/tests/TESTADD1

4.4.2.2. cblcl tron <z390 path>/zcobol/tests/TESTADD1

4.4.2.3. cblclg tron <z390 path>/zcobol/tests/TESTADD1

--------------------------------------------------------------------------------
4.4.3. End COBOL test. Exit the GUI.

********************************************************************************
5. Cleanup.

When testing is complete, restore your z390 directory.

********************************************************************************

================================================================================
End readme.txt
================================================================================
