      ************************************************************
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMOSUB2.
       COPY SOASTUB1.
       01  SOA-STUB-SVC-NAME     PIC X(8) VALUE 'DEMOSUB2'.
       01  SOA-STUB-SVC-MGR      PIC X(8) VALUE 'DEMOCMGR'.
       01  SOA-STUB-MSG-SEND-LEN PIC S9(8) COMP VALUE 36.
       01  SOA-STUB-MSG-RECV-LEN PIC S9(8) COMP VALUE 28.
       01  SOA-MSG.
           05  SOA-MSG-PFX.
               10  SOA-MSG-LEN      PIC S9(8) COMP.
               10  SOA-MSG-TIME     PIC S9(18) COMP.
               10  SOA-MSG-SVC-NAME PIC X(8).
               10  SOA-MSG-SVC-RC   PIC S9(8) COMP.
           05  SOA-MSG-PARMS    PIC X(12).
       LINKAGE SECTION.
       01 P1 PIC X(4).
       01 P2 PIC X(4).
       01 P3 PIC X(4).
       PROCEDURE DIVISION USING
           P1
           P2
           P3
           .
           MOVE P1 TO SOA-MSG-PARMS(1:4)
           MOVE P2 TO SOA-MSG-PARMS(5:4)
           MOVE P3 TO SOA-MSG-PARMS(9:4)
           .
       COPY SOASTUB2.
           MOVE SOA-MSG-PARMS(1:4) TO P3
           .
       COPY SOASTUB3.
