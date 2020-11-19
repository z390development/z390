rem execute zc390 to translate source CBL file to MLC file with zcobol macro calls
rem -classpath  path to jar file
rem -verbose:gc trace garbage collection to detect memory leaks
rem -XmsnK      set initial memory allocation to nK
rem -XmxnK      set max     memory allocation to nK
rem -Xrs        allow control break interruption of java tasks
java -classpath %~dps0..\z390.jar -Xrs zc390 %1 %2 %3 %4 %5 %6 %7 %8 %9
