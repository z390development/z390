rem run z390 installation  verification program iz390.class
ECHO OFF
if exist /usr/local/lib/z390/z390.jar goto linux
if %1. == tron. ECHO ON
if %1. == tron. shift
if %1. == TRON. ECHO ON
if %1. == TRON. shift
:linux
java -classpath z390.jar -Xrs iz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
:end
