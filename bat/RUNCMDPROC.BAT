::========================================================================
:: z390 - Mainframe assembler emulator and run-time engine
:: Copyright (C) 2021 z390 Assembler LLC
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
rem run programs that test CMDPROC (SVC 34)
@setlocal
@pushd %~dp0
cd..

:: Test one Command Processor
call bat\asmlg rt\test\TESTCMD3
IF %ERRORLEVEL% NEQ 0 (
    echo RUNCMDPR.ERROR: ERROR IN TESTCMD3 RC=%ERRORLEVEL% - exiting
    EXIT /B %ERRORLEVEL%
)

:: Test two Command Processors (3 tests)
:: 1. Both have same number of lines of output
:: 2. Second CP has more lines of output
:: 3. First CP has more lines of output

call bat\asmlg rt\test\TESTCMD4 PARM(1)
IF %ERRORLEVEL% NEQ 0 (
    echo RUNCMDPR.ERROR: ERROR IN TESTCMD4 PARM(1) RC=%ERRORLEVEL% - exiting
    EXIT /B %ERRORLEVEL%
)

call bat\asmlg rt\test\TESTCMD4 PARM(2)
IF %ERRORLEVEL% NEQ 0 (
    echo RUNCMDPR.ERROR: ERROR IN TESTCMD4 PARM(2) RC=%ERRORLEVEL% - exiting
    EXIT /B %ERRORLEVEL%
)

call bat\asmlg rt\test\TESTCMD4 PARM(3)
IF %ERRORLEVEL% NEQ 0 (
    echo RUNCMDPR.ERROR: ERROR IN TESTCMD4 PARM(3) RC=%ERRORLEVEL% - exiting
    EXIT /B %ERRORLEVEL%
)
