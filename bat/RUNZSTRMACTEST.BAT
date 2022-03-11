@if /I %1. == tron. (echo on) else (echo off)
rem ========================================================================
rem  This script builds HLASM compatible structured programming macros
rem  Usage: bldzstrmac <output dir>
rem         If no output dir is provided, defaults to build\zstrmac
rem ========================================================================

setlocal
if /I %1. == tron. (set z_TraceMode=tron
                    shift /1
            ) else (if /I %1. == troff. (set z_TraceMode=troff
                                         shift /1
                                 ) else (set z_TraceMode=)
                    )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dp0\..
echo Test zstrmac basic structures using bootstrap version 1
set SYSUT1=rt\test\TESTSPE1.ZSM 
set SYSUT2=rt\test\TESTSPE1.MLC 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC1     NOASM NOTIMING STATS || goto error
call bat\ASMLG      %z_TraceMode% rt\test\TESTSPE1       BAL NOTIMING STATS || goto error

set SYSUT1=rt\test\TESTSPE2.ZSM 
set SYSUT2=rt\test\TESTSPE2.MLC 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC1     NOASM NOTIMING STATS || goto error
call bat\ASMLG      %z_TraceMode% rt\test\TESTSPE2       BAL NOTIMING STATS || goto error

echo Translate structured version 2 using bootstrap version 1
set SYSUT1=rt\test\ZSTRMAC2.ZSM 
set SYSUT2=rt\test\ZSTRMAC2.MLC 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC1     NOASM NOTIMING STATS || goto error

echo Verify TESTSPE1.ZSM translation using zstrmac2 matches zstrmac1
set SYSUT1=rt\test\TESTSPE1.ZSM 
set SYSUT2=rt\test\TESTSPE1.TXT 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC2     NOASM NOTIMING STATS || goto error
comp /M rt\test\TESTSPE1.MLC rt\test\TESTSPE1.TXT
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% GTR 0 (set z_ReturnCode=8
                         echo %0 ERROR: TESTSPE1 code was not generated correctly
                         goto return
                         )

echo Regen ZSTRMAC2 using mz390 support to ver zstrmac2.txt = mlc
set SYSUT1=rt\test\ZSTRMAC2.ZSM 
set SYSUT2=rt\test\ZSTRMAC2.MLC 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC2.ZSM NOASM NOTIMING STATS || goto error

set SYSUT1=rt\test\TESTSPE1.ZSM 
set SYSUT2=rt\test\TESTSPE1.TXT 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC2     NOASM NOTIMING STATS || goto error
comp /M rt\test\TESTSPE1.MLC rt\test\TESTSPE1.TXT
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% GTR 0 (set z_ReturnCode=8
                         echo %0 ERROR: TESTSPE1 code was not generated correctly
                         goto return
                         )

echo Test extensions to ACASE added in zstrmac2 C,X,(v1,v2)
set SYSUT1=rt\test\TESTSPE3.ZSM 
set SYSUT2=rt\test\TESTSPE3.MLC 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC2.ZSM NOASM NOTIMING STATS || goto error
call bat\ASMLG      %z_TraceMode% rt\test\TESTSPE3       BAL NOTIMING STATS || goto error

echo Test zstrmac error messages - expected to fail with RC=8
set SYSUT1=rt\test\TESTSPE4.ZSM 
set SYSUT2=rt\test\TESTSPE4.MLC 
call bat\MZ390      %z_TraceMode% rt\test\ZSTRMAC2.ZSM NOASM NOTIMING STATS
set z_ReturnCode=%ERRORLEVEL%
if %z_ReturnCode% EQU 8 (set z_ReturnCode=0
                 ) else (set z_ReturnCode=8
                         echo %0 ERROR: TESTSPE4 did not generate expected RC=8
                         goto return
                         )

echo Test ZSTRMAC SPM's using z390 SPE
call bat\asmlg      %z_TraceMode% rt\test\TESTSPM1       BAL NOTIMING STATS || goto error

echo Test ZSTRMAC SPM's macros using generated HLASM compatible macros
call bat\BLDZSTRMAC %z_TraceMode% build\zstrmac                             || goto error
call bat\ASMLG      %z_TraceMode% rt\test\TESTSPM1 SYSMAC(build\zstrmac+mac) SYSCPY(build\zstrmac+mac) BAL NOTIMING STATS || goto error
goto return

:error
set z_ReturnCode=%ERRORLEVEL%
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
