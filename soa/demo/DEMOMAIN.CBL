      *********************************************************************
      * z390 - Mainframe assembler emulator and run-time engine
      * Copyright (C) 2021 z390 Assembler LLC
      *
      * This file is part of z390.
      *
      * z390 is free software; you can redistribute it and/or modify
      * it under the terms of the GNU General Public License as 
      * published by the Free Software Foundation; either version 2 of 
      * the License, or (at your option) any later version.
      * z390 is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the GNU General Public 
      * License along with this program; if not, see 
      * https://www.gnu.org/licenses.
      * Author - Don Higgins                                              *
      * Date   - 12/26/06                                                 *
      *********************************************************************
      * 12/31/06 RPI 523 CODE DEMOMAIN CALLING DEMOSUB1/DEMOSUB2  
      * 01/08/07 RPI 523 ADD TIMING IN MICRO-SEC AND PERFORM TWICE
      * 05/23/08 RPI 853 COBOL VERSION FOR TEST OF EZASOKET SUPPORT  
      *********************************************************************
      * 1.  DEMOMAIN CALLS DEMOSUB1 TO CALCULATE AND DISPLAY SUM OF 2     
      *     DISPLAY SCIENTIFIC NOTATION VALUES USING CFD AND CTD MACROS.
      *     FOR CONVERSION TO/FROM LD EXTENDED DFP FORMAT FOR ADDITION
      * 2.  DEMOMAIN CALLS DEMOSUB2 TO CALCULATE AND DISPLAY SUM OF 2 
      *     INTEGER VALUES.
      * 3.  RUN SOA\DEMO\DEMOSTD.BAT TO ASSEMBLE, STATICALLY LINK, AND
      *     EXECUTE DEMO APPLICATION AS STANDARD LOCAL SINGLE PROCESS.
      * 4.  RUN SOA\DEMO\DEMOSOA.BAT TO GENERATED, ASSEMBLE, LINK, AND 
      *     EXECUTE DEMO APPLICAITON USING SOA CLIENT SERVER TO ALLOW
      *     RUNNING THE TWO CALLED SUBROUTINES AS SERVICES RUNNING ON
      *     SEPARATE PROCESS ON SAME OR ANY TCP/IP CONNECTED PLATFORM.
      * 5.  RUN SOA\DEMO\DEMOSOAC.BAT TO GENERATE COBOL STUBS AND DEMOCMGR
      * 6.  START SERVER SOA\DEMO\DEMOSMGR NOTIME
      * 7.  RUN DEMOMAIN.CBL USING MAINFRAME EXPRESS WITH EXASOKET SUPPORT
      * 8.  SEE Z390 SOA SUPPORT WEBPAGE FOR SCREEN VIEW OF OUTPUT
      *********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DEMOMAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFP1  PIC X(45) VALUE '1.1'.
       01  DFP2  PIC X(45) VALUE '2.2'.
       01  DFP3  PIC X(45) VALUE SPACES.
       01  DFP3V PIC X(45) VALUE '3.3'.
       01  INT1  PIC S9(8) COMP VALUE 1.
       01  INT2  PIC S9(8) COMP VALUE 2.
       01  INT3  PIC S9(8) COMP VALUE 0.
       01  INT3V PIC S9(8) COMP VALUE 3.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY 'DEMOMAIN.CBL SOA DEMO APPLICATION'
           PERFORM CALL-SUB1 5 TIMES
           PERFORM CALL-SUB2 5 TIMES
           DISPLAY 'DEMOMAIN.CBL ENDED OK'
           GOBACK.
       CALL-SUB1.
           CALL "DEMOSUB1" USING DFP1, DFP2, DFP3
           IF DFP3 NOT = DFP3V GO TO ABORT.
       CALL-SUB2.
           CALL "DEMOSUB2" USING INT1, INT2, INT3
           IF  INT3 NOT = INT3V GO TO ABORT.
       ABORT.
           DISPLAY 'DEMOMAIN.CBL SOA ERROR' 
           MOVE 16 TO RETURN-CODE
           STOP RUN.
