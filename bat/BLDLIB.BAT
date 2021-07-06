@echo off
setlocal
pushd %~dp0
cd ..

CALL bat\asml linklib\REPRO bal notiming stats || goto error
CALL bat\asml linklib\SUPERZAP bal notiming stats || goto error
CALL bat\asml linklib\UNREF ascii bal notiming stats || goto error
CALL bat\asm linklib\FPCONVRT bal notiming stats || goto error
CALL bat\asm linklib\FPCONMFC bal notiming stats || goto error
CALL bat\asm linklib\CVTTOHEX bal notiming stats || goto error
CALL bat\asm linklib\SQXTR bal notiming stats || goto error
CALL bat\asm linklib\DAT bal notiming stats || goto error

CALL bat\asmlg linklib\IEFBR14 sysobj(+linklib) || goto error
CALL bat\asmlg linklib\TESTFPC1 sysobj(+linklib) || goto error
CALL bat\asmlg linklib\TESTFPC2 sysobj(+linklib) || goto error

CALL bat\asm  linklib\RTGENDIR noasm bal notiming stats sysparm(RTGENDIR) || goto error
CALL bat\asm  linklib\RTGENCMP noasm bal notiming stats sysparm(RTGENCMP) || goto error

goto eof

:error
echo BLDLIB   ERROR: Encountered RC %ERRORLEVEL% - exiting
exit /b %ERRORLEVEL%

@REM # SYSUT1=zstrmac.zsm SYSUT2=zstrmac.txt bat/asm linklib/ZSTRMAC.zsm noasm stats

@REM # ZAPDEMO1
@REM cp linklib/ZAPDEMO1.SAV linklib/ZAPDEMO1.DEMO
@REM temp_file=$(mktemp)
@REM cat >${temp_file} <<EOL
@REM * DEMO FOR SUPERZAP TO CHANGE '111' TO '222' IN 'AAA111BBB' TWICE    
@REM  NAME linklib/ZAPDEMO1.DEMO
@REM  DUMP
@REM  ASCII
@REM  DUMP
@REM  FIND 'AAA111BBB'
@REM  VER  *+3 '111'
@REM  REP  *+3 '222'
@REM  DUMP * 9
@REM  LOOP
@REM  HELP
@REM EOL
@REM set +e
@REM SYSIN=${temp_file} SYSPRINT=linklib/ZAPDEMO1.RPT bat/exec linklib/SUPERZAP stats notiming
@REM res=$?
@REM set -e
@REM echo "SUPERZAP rc=${res}"
@REM if [ ${res} -ne 4 ]; then
@REM     rm ${temp_file}
@REM     echo "Unexpected rc - exiting"
@REM     exit 4
@REM fi
@REM rm ${temp_file}
@REM echo "***** ZAPDEMO1"
@REM # cat linklib/ZAPDEMO1.DEMO
@REM rm linklib/ZAPDEMO1.DEMO
@REM echo "*****"
@REM # cat linklib/ZAPDEMO1.RPT
@REM rm linklib/ZAPDEMO1.RPT

@REM # ZAPDEMO2
@REM temp_file=$(mktemp)
@REM zap="* ZAPDEMO2 DUMP SUPERZAP.390 IN EBCDIC\n NAME linklib/SUPERZAP.390\n DUMP"
@REM printf "${zap}" > ${temp_file}
@REM SYSIN=${temp_file} SYSPRINT=linklib/ZAPDEMO2.RPT bat/exec linklib/SUPERZAP stats notiming
@REM rm ${temp_file}
@REM echo "***** ZAPDEMO2"
@REM rm linklib/ZAPDEMO2.RPT