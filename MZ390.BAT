rem execute mz390 runtime with java runtime options:
rem 01/15/09 DSH added 150 MB init and max mem for zcobol
rem -classpath  path to jar file
rem -verbose:gc trace garbage collection to detect memory leaks
rem -XmsnK      set initial memory allocation to nK
rem -XmxnK      set max     memory allocation to nK
rem -Xrs        allow control break interruption of java tasks
java -classpath %~dps0z390.jar -Xrs -Xms150000K -Xmx150000K %J2SEOPTIONS% mz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
