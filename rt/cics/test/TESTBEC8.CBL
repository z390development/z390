      *****************************************************************

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
      *****************************************************************
      * Author - Don Higgins                                          *
      *****************************************************************
        IDENTIFICATION DIVISION.
      *
      * HANDLE ABEND BY PROGRAM (PART OF TRANSID BEC5)
      *
        PROGRAM-ID. TESTBEC8.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  TEST1.
            02  FILLER   PIC X(4)  VALUE X'C311C8F1'.
            02  FILLER   PIC X(42)
                VALUE 'TESTBEC8 SAYS HELLO AND THE COMMAREA SAYS '.
            02  TESTAEI0 PIC X(4).
        77  TEST1L   PIC 9(4) COMP.

        LINKAGE SECTION.
        01  DFHCOMMAREA.
            02  COMMMSG  PIC X(4).

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF TEST1   TO TEST1L.

      * BEC5 ABEND HANDLER
      *  L     R6,DFHEICAP        R6=COMMAREA ADDRESS
      *  MVC   TESTAEI0,0(R6)     MOVE TO MESSAGE
            MOVE COMMMSG TO TESTAEI0.
            EXEC CICS SEND FROM(TEST1) LENGTH(TEST1L) END-EXEC.
            EXEC CICS RETURN END-EXEC.
