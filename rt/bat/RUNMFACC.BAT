@if /I "%1" == "tron" (echo on) else (echo off)
rem run MainFrame Assembler Coding Contest solutions

setlocal
if /I "%1" == "tron" (set z_TraceMode=tron
                      shift /1
              ) else (if /I "%1" == "troff" (set z_TraceMode=troff
                                             shift /1
                                     ) else (set z_TraceMode=)
                      )
set /A z_NestLevel=%z_NestLevel%+1
rem ----- Lvl(%z_NestLevel%) Start %0 %1 %2 %3 %4 %5 %6 %7 %8 %9

pushd %~dps0..\..
rem -- For now: assembling and running; not validating output SNAP content
call bat\ASMLG %z_TraceMode% mfacc\P1C1     notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P1DSH1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P1DSH1A  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P1RAFA1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P2MD1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P3DW1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P3LKM1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P3MM1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4AN1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4APN2   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4DSH1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4DW1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4RAFA1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4RAFA2  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4RJ1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P5DW1    notiming stats bal   loadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P5MM1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6BR1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6MM1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6PJF1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6PL1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6RW1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P7DSH1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P7EH1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P7RV1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8DSH1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8DSH2   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8LM1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8MM1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P9DSH1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P9MM1    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P9MM2    notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P10DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P10DSH2  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P10MB1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11DSH2  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11DW1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11FIND1 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11FIND2 notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11MODEL notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11WR1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P12DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P12DSH2  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P12DSH3  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P13DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P13SC1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P14DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P14MW1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P15DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P15WR1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P16DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P16WR1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P17DW1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P17WR1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P18DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P19WR1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASML  %z_TraceMode% mfacc\P20WR1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error rem runs too long to include
call bat\ASMLG %z_TraceMode% mfacc\P21DW1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22DSH1  notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22FS1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22GH1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22MM1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22RL1   notiming stats bal noloadhigh %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set SYSOUT=mfacc\P16AFK1.OUT
call bat\ASMLG %z_TraceMode% mfacc\P16AFK1  notiming stats bal noloadhigh PARM(ASCII) %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
fc mfacc\P16AFK1.OUT mfacc\P16AFK1.TF1 > nul || goto error
set z_ReturnCode=0
goto return

:error
if %ERRORLEVEL% EQU 0 (set z_ReturnCode=6
               ) else (set z_ReturnCode=%ERRORLEVEL%)
echo %0 ERROR: Encountered RC %z_ReturnCode% - exiting
:return
popd
rem ----- Lvl(%z_NestLevel%)  End %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
exit /b %z_ReturnCode%
