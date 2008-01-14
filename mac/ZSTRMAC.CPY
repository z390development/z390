.********************************************************************
.* ZSTRMAC - Z390 PUBLIC DOMAIN STRUCTRUED PROGRAMMING MACROS
.********************************************************************
.* 05/16/06 DSH RPI 316 UPDATED FOR COMPATIBILITY
.********************************************************************
.* ORIGINAL MACROS WRITTEN BY DON HIGGINS IN 1974 AT FLORIDA POWER
.* SEE ACM SIGPLAN ARTICLE PUBLISHED IN FEBRUARY 1975
.* ORIGINAL MACROS CONTRIBUTED TO SHARE SPLA LIBRARY IN LATE 1970'S
.* ORIGINAL MACROS RETRIEVED FROM CBT OVERFLOW TAPE 178:
.*   http://www.cbttape.org/covdowns.htm
.* THE ZSTRMAC MACROS HAVE BEEN UPDATED TO BE A COMPATIBLE
.* SUBSET OF IBM HLASM TOOLKIT MACROS BASED ON SHARE PRESENTATION
.* BY ED JAFFEE:
.*   ftp://ftp.phoenixsoftware.com/pub/demo/Structured_Assembler.pdf
.********************************************************************
.* THE ZSTRMAC SUPPORTED MACROS ARE AS FOLLOWS
.*
.*   1. DO     - ITERATION START WITH OPTIONS WHILE= OR UNTIL=
.*   2. ENDDO  - ITERATION END
.*   3. IF     - ALTERNATE SELECTION FIRST TEST
.*   4. ELSEIF - ALTERNATE SELECTION ALTERNATIVE WITH TEST
.*   5. ELSE   - ALTERNATE SELECTION DEFAULT ALTERNATVE
.*   6. ENDIF  - ALTERNATE SELECTION END
.*
.* See the following demo programs:
.*   1.  DEMO\DEMOSTR1 - example of DO WHILE, DO UNTIL, and IF
.*       structured code with no branches and no register usage.
.*       Use the following command to assemble, link, and execute
.*       this demo program from z390 command line:
.*       ASMLG DEMO\DEMOSTR1 SYSMAC(MAC+ZSTRMAC) SYSCPY(MAC+ZSTRMAC)
.*       See DEMO\DEMOSTR1.PRN for listing with PRINT NOGEN, and
.*       see DEMO\DEMOSTR1.BAL for generated code, and
.*       see DEMO\DEMOSTR1.LOG for WTO's executed in selected blocks.
.********************************************************************
.* TO USE THE ZSTRMAC MACROS, Z390 USER PROGRAMS MUST CONTAIN 
.* THE FOLLOWING STATEMENT PRIOR TO FIRST USE:
.*   COPY ASMMSP
.* THE COPYBOOK MAC\ASMMSP.CPY COPIES MAC\ZSTRMAC.CPY 
.* THE COPYBOOK ZSTRMAC\ZSTRMAC.CPY COPIES THE ABOVE MACROS 
.* PLUS THE COPYBOOK ZSTRMAC\ZSTREQU.CPY WHICH DEFINES EQU SYMBOLS
.* FOR BRANCH ON CONDITION MASKS SUCH AS E, NE, LT, GT, P, M, O, Z ETC.
.********************************************************************
         PUSH PRINT
         PRINT OFF
         COPY DO.MAC       
         COPY ENDDO.MAC     
         COPY IF.MAC 
         COPY ELSEIF.MAC   
         COPY ELSE.MAC
         COPY ENDIF.MAC
         COPY ZSTREQU.CPY
         POP  PRINT
