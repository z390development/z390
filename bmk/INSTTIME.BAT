rem run z390 instruction timing benchmark and view output
call asmlg bmk\insttime ascii mem(5) notiming sysparm(bmk)
if errorlevel 1 pause error during insttime execution see log
notepad bmk\insttime.out
