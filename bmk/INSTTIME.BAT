rem run z390 instruction timing benchmark and view output
rem note bmk\insttime.mlc has hardcoded path to files 
call asmlg bmk\insttime ascii mem(5) notiming
if errorlevel 1 pause error during insttime execution see log
notepad bmk\insttime.out
