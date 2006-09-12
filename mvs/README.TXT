This is the z390 MVS 3.8J pubic domain macro libary directory 
distributed as an optional download from www.z390.org.

1.  Go to www.z390.org and download z390_MVS.ZIP.
2.  Unzip to the z390 install directory to create subdiretories:
    MVS - MVS 3.8J public domain sys1.maclib macros plus demo and tests
      MACLIB - source macros in ascii form with .mac suffix
      TEST   - source mlc programs and generated test files
      SAVE   - last verified save of all TEST files
      DIF    - comparison file for each TEST versus SAVE file
3.  Start z390 GUI interface or Windows command line at z390 install dir
4.  Issue the command RTMVS.BAT to run the MVS regression tests:
    DEMO.MLC - assemble, link, and execute demo with WTO 'Hello World'
    TESTCVT1.MLC - test assembly and link of MVS CVT.MAC
    TESTMVS1.MLC - test assembly only of common MVS macros

For more information visit www.z390.org

Don Higgins
don@higgins.net
