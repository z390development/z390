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

SET DEST=%1
IF "%DEST%"=="" (
    SET DEST=build\zstrmac
)
ECHO %DEST%

if NOT EXIST "%DEST%" mkdir %DEST%
ECHO Deleting %DEST%\*.*
ERASE /Q %DEST%\*.*

SET ZSTR=rt\test\ZSTRMAC2.ZSM

@REM Generate the HLASM versions of the macros
SET MACS=( CASE CASENTRY DO ELSE ELSEIF ENDCASE ENDDO ENDIF ENDSEL IF OTHRWISE SELECT WHEN ZSTREQU )
FOR %%M IN %MACS% DO (
    ECHO SYSUT1=mac\%%M.MAC SYSUT2=%DEST%\%%M.MAC bat\MZ390 %ZSTR% NOASM STATS
    SET SYSUT1=mac\%%M.MAC 
    SET SYSUT2=%DEST%\%%M.MAC 
    CALL bat\MZ390 %ZSTR% NOASM STATS
)

@REM Copy the static copy files
SET CPYS=(mac\ASMMSP.CPY mac\ZSTRGBL.CPY mac\ZSTRMAC.CPY)
FOR %%C IN %CPYS% DO (
    ECHO Adding %%C to %DEST%
    COPY %%C %DEST% >nul 2>&1
)
