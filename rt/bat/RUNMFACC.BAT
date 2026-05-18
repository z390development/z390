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
call bat\ASML  %z_TraceMode% mfacc\EXSAMPL2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error rem abends when run
call bat\ASMLG %z_TraceMode% mfacc\P1C1     %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P1DSH1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P1DSH1A  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P1RAFA1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P2MD1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P3DW1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P3LKM1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P3MM1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4AN1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4APN2   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4DSH1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4DW1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4RAFA1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4RAFA2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P4RJ1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P5DW1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P5MM1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6BR1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6MM1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6PJF1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6PL1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P6RW1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P7DSH1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P7EH1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P7RV1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8DSH1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8DSH2   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8LM1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P8MM1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P9DSH1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P9MM1    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P9MM2    %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P10DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P10DSH2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P10MB1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11DSH2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11DW1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11FIND1 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11FIND2 %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11MODEL %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P11WR1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P12DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P12DSH2  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P12DSH3  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P13DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P13SC1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P14DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P14MW1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P15DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P15WR1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
set SYSOUT=mfacc\P16AFK1.OUT
call bat\ASMLG %z_TraceMode% mfacc\P16AFK1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P16DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P16WR1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P17DW1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P17WR1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P18DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P19WR1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASML  %z_TraceMode% mfacc\P20WR1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error rem abends when run
call bat\ASMLG %z_TraceMode% mfacc\P21DW1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22DSH1  %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22FS1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22GH1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22MM1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
call bat\ASMLG %z_TraceMode% mfacc\P22RL1   %1 %2 %3 %4 %5 %6 %7 %8 %9 || goto error
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
