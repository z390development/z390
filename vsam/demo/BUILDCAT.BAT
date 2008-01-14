REM build DEMOCAT.390 with all VSAM catalog entries for vsam\demo pgms
CALL ASML vsam\demo\DEMOCAT notiming stats
IF ERRORLEVEL 1 PAUSE ERROR BUILDING DEMOCAT.390 VSAM CATALOG
