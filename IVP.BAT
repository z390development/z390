rem run z390 installation  verification program iz390.class
rem 2016-04-16 RPI 1581 Abe Kornelis - Unix-specific code no longer needed - removed
rem 2019-07-22 RPI 1636 Abe Kornelis - Shift in .bat files not handled correctly - option /1 added
ECHO OFF
if %1. == tron. ECHO ON
if %1. == tron. shift /1
if %1. == TRON. ECHO ON
if %1. == TRON. shift /1
java -classpath z390.jar -Xrs iz390 %1 %2 %3 %4 %5 %6 %7 %8 %9
:end
