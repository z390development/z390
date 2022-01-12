@ECHO OFF
::========================================================================
:: z390 - Mainframe assembler emulator and run-time engine
:: Copyright (C) 2022 z390 Assembler LLC
::
:: This file is part of z390.
:: z390 is free software; you can redistribute it and/or modify
:: it under the terms of the GNU General Public License as published by
:: the Free Software Foundation; either version 2 of the License, or
:: (at your option) any later version.
::
:: z390 is distributed in the hope that it will be useful,
:: but WITHOUT ANY WARRANTY; without even the implied warranty of
:: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
:: GNU General Public License for more details.
::
:: You should have received a copy of the GNU General Public License along
:: with this program; if not, see <https://www.gnu.org/licenses/>.
::========================================================================
:: This script builds HLASM compatible structured programming macros
:: Usage: bldzstrmac <output dir>
::        If no output dir is provided, defaults to build/zstrmac
::========================================================================
setlocal
pushd %~dp0
cd ..

ECHO Test zstrmac basic structures using bootstrap version 1
SET SYSUT1=rt\test\TESTSPE1.ZSM 
SET SYSUT2=rt\test\TESTSPE1.MLC 
CALL bat\MZ390 rt\test\ZSTRMAC1 NOASM STATS NOTIMING || goto error
CALL bat\ASMLG rt\test\TESTSPE1 BAL NOTIMING STATS || goto error

SET SYSUT1=rt\test\TESTSPE2.ZSM 
SET SYSUT2=rt\test\TESTSPE2.MLC 
CALL bat\MZ390 rt\test\ZSTRMAC1 NOASM STATS NOTIMING || goto error
CALL bat\ASMLG rt\test\TESTSPE2 BAL NOTIMING STATS || goto error

ECHO Translate structured version 2 using bootstrap version 1
SET SYSUT1=rt\test\ZSTRMAC2.ZSM 
SET SYSUT2=rt\test\ZSTRMAC2.MLC 
CALL bat\MZ390 rt\test\ZSTRMAC1 NOASM STATS NOTIMING || goto error

ECHO Verify TESTSPE1.ZSM translation using zstrmac2 matches zstrmac1
SET SYSUT1=rt\test\TESTSPE1.ZSM 
SET SYSUT2=rt\test\TESTSPE1.TXT 
CALL bat\MZ390 rt\test\ZSTRMAC2 NOASM STATS NOTIMING || goto error

@REM # use cmp to compare two files. If variations, will return non zero
@REM cmp rt/test/TESTSPE1.MLC rt/test/TESTSPE1.TXT

ECHO Regen ZSTRMAC2 using mz390 support to ver zstrmac2.txt = mlc
SET SYSUT1=rt\test\ZSTRMAC2.ZSM 
SET SYSUT2=rt\test\ZSTRMAC2.MLC 
CALL bat\MZ390 rt\test\ZSTRMAC2.ZSM NOASM STATS NOTIMING || goto error

SET SYSUT1=rt\test\TESTSPE1.ZSM 
SET SYSUT2=rt\test\TESTSPE1.TXT 
CALL bat\MZ390 rt\test\ZSTRMAC2 NOASM STATS NOTIMING || goto error

@REM cmp rt/test/TESTSPE1.MLC rt/test/TESTSPE1.TXT
FC rt\test\TESTSPE1.MLC rt\test\TESTSPE1.TXT > nul
IF %ERRORLEVEL% NEQ 0 GOTO error

ECHO Test extensions to ACASE added in zstrmac2 C,X,(v1,v2)
SET SYSUT1=rt\test\TESTSPE3.ZSM 
SET SYSUT2=rt\test\TESTSPE3.MLC 
CALL bat\MZ390 rt\test\ZSTRMAC2.ZSM NOASM STATS NOTIMING || goto error
CALL bat\ASMLG rt\test\TESTSPE3 BAL NOTIMING STATS || goto error

ECHO Test zstrmac error messages
SET SYSUT1=rt\test\TESTSPE4.ZSM 
SET SYSUT2=rt\test\TESTSPE4.MLC 
CALL bat\MZ390 rt\test\ZSTRMAC2.ZSM NOASM STATS NOTIMING
@REM ERROR if not RC=8
IF %ERRORLEVEL% NEQ 8 GOTO error

ECHO Test ZSTRMAC SPM's using z390 SPE
CALL bat\asmlg rt\test\TESTSPM1 BAL NOTIMING STATS || goto error

ECHO Test ZSTRMAC SPM's macros using generated HLASM compatabile macros
CALL bat\BLDZSTRMAC build\zstrmac || goto error
CALL bat\ASMLG rt\test\TESTSPM1 SYSMAC(build\zstrmac+mac) SYSCPY(build\zstrmac+mac) BAL NOTIMING STATS || goto error

goto:eof

:error
echo BLDZSTRM ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%
