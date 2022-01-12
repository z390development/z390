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
       PROGRAM-ID. DEMOCMGR.
       COPY SOACMGR1.
       77  SOA-HOST-NAME PIC X(24) VALUE z'DSH3'.
       77  SOA-HOST-PORT PIC S9(9) COMP VALUE 3900.
       COPY SOACMGR2.
       LINKAGE SECTION.
       01  SOA-MSG.
           05  SOA-MSG-PFX.
               10  SOA-MSG-LEN  PIC S9(8) COMP.
               10  SOA-MSG-TIME PIC S9(18) COMP.
               10  SOA-MSG-SVC-NAME PIC X(8).
               10  SOA-MSG-SVC-RC   PIC S9(8) COMP.
           05  SOA-MSG-PARMS        PIC X(135).
       COPY SOACMGR3.
