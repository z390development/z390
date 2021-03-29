rem execute zc390 with java runtime options:
rem -classpath  path to jar file
rem -verbose:gc trace garbage collection to detect memory leaks
rem -XmsnK      set initial memory allocation to nK
rem -XmxnK      set max     memory allocation to nK
rem -Xrs        allow control break interruption of java tasks
if exist z390.jar goto jar
java -classpath W:\work\eclipse\workspace\z390\bin -Xrs zc390 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto end
:jar
java -classpath z390.jar -Xrs zc390 %1 %2 %3 %4 %5 %6 %7 %8 %9
:end
