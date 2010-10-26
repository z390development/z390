rem assemble, link, and exec ASSIST prgram %1.MLC
echo off
set XREAD=%1.XRD
set XPRNT=%1.XPR
set XPNCH=%1.XPH
set XGET=%1.XGT
set XPUT=%1.XPT
call ASMLG %1 assist %2 %3 %4 %5 %6 %7 %8 %9
