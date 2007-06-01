rem run z390 installation  verification program iz390.class
if exist z390.jar goto jar
rem include -verbose:gc to trace Java garbage collection
java -classpath d:\work\eclipse\workspace\z390\lib -Xrs iz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto end
:jar
java -classpath z390.jar -Xrs iz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
:end
