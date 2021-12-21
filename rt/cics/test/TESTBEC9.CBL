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
      * TEMPORARY STORAGE TESTER
      *
      *    MYQUEUEA      WRITE ITEM 1
      *    MYQUEUEA      WRITE ITEM 2
      *    MYLONGQUEUEA  WRITE ITEM 1
      *    MYQUEUEA      REWRITE ITEM 1
      *    RAISE QIDERR
      *    RAISE ITEMERR
      *    RAISE LENGERR
      *    MYLONGQUEUEB  WRITE ITEM 1
      *    MYLONGQUEUEA  DELETED
      *    RAISE QIDERR
      *    RAISE INVREQ
      *    MYQUEUEA      READ ITEM 1
      *    MYQUEUEA      READ ITEM NEXT(2)
      *    RAISE ITEMERR
      *    RAISE QIDERR
      *    RAISE LENGERR
      *    MYQUEUEA      DELETED
      *    MYLONGQUEUEB  DELETED
      *
      * CEBR TESTS
      *    MYQUEUEA      BUILD 16 ITEMS
      *    MYQUEUEB      BUILD 50 ITEMS
      *    X'02CB458F'   BUILD 80 ITEMS
      *
        PROGRAM-ID. TESTBEC9.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        77  TSREC    PIC X(200).
        77  TSNUMIT  PIC 9(4) COMP.
        01  QWT.
            02   QWTSBA  PIC X(4).
            02   QWTMSG  PIC X(39).
        77  QOP1     PIC X(4)    VALUE X'C311C260'.
        77  QOP2     PIC X(4)    VALUE X'C311C3F0'.
        77  QOP3     PIC X(4)    VALUE X'C311C540'.
        77  QOP4     PIC X(4)    VALUE X'C311C650'.
        77  QOP5     PIC X(4)    VALUE X'C311C760'.
        77  QOP6     PIC X(4)    VALUE X'C311C8F0'.
        77  QOP7     PIC X(4)    VALUE X'C3114A40'.
        77  QOP8     PIC X(4)    VALUE X'C3114B50'.
        77  QOP9     PIC X(4)    VALUE X'C3114C60'.
        77  QOP10    PIC X(4)    VALUE X'C3114DF0'.
        77  QOP11    PIC X(4)    VALUE X'C3114F40'.
        77  QOP12    PIC X(4)    VALUE X'C3115050'.
        77  QOP13    PIC X(4)    VALUE X'C311D160'.
        77  QOP14    PIC X(4)    VALUE X'C311D2F0'.
        77  QOP15    PIC X(4)    VALUE X'C311D440'.
        77  QOP16    PIC X(4)    VALUE X'C311D550'.
        77  QOP17    PIC X(4)    VALUE X'C311D660'.
        77  QOP18    PIC X(4)    VALUE X'C311D7F0'.
        77  QOP19    PIC X(4)    VALUE X'C311D940'.
        77  QOP20    PIC X(4)    VALUE X'C3115A50'.
        77  QOP1C2   PIC X(4)    VALUE X'C311C3C8'.
        77  QOP2C2   PIC X(4)    VALUE X'C311C4D8'.
        77  QOP3C2   PIC X(4)    VALUE X'C311C5E8'.
        77  QOP4C2   PIC X(4)    VALUE X'C311C6F8'.

        01  QOP.
            02  FILLER   PIC X(6)  VALUE 'QUEUE '.
            02  QOPNAME  PIC X(16).
            02  FILLER   PIC X(6)  VALUE ' ITEM '.
            02  QOPITEM  PIC X.
            02  FILLER   PIC X     VALUE SPACE.
            02  QOPTYPE  PIC X(7)  VALUE 'WRITE  '.
            02  FILLER   PIC X     VALUE SPACE.
            02  QOPSEP   PIC X     VALUE '|'.

        01  QHAND.
            02  QHANDCON PIC X(7)  VALUE SPACES.
            02  FILLER   PIC X(31)
                         VALUE ' CONDITION RAISED              '.
            02  QHSEP    PIC X     VALUE '|'.
        77  QHANDL   PIC 9(4) COMP.

        01  CEBRMSG1.
            02  FILLER   PIC X(4)  VALUE X'C311C8C8'.
            02  FILLER   PIC X(27) VALUE 'CEBR QUEUE BUILDING STARTED'.
        77  CEBR1LEN PIC 9(4) COMP.

        01  CEBRMSG2.
            02  FILLER   PIC X(4)  VALUE X'C311C9D8'.
            02  FILLER   PIC X(25) VALUE 'CEBR QUEUE BUILDING ENDED'.
        77  CEBR2LEN PIC 9(4) COMP.

        01  ENDMSG.
            02  FILLER   PIC X(4)  VALUE X'C3115CF0'.
            02  FILLER   PIC X(31)
                         VALUE 'TESTBEC9 COMPLETED, PRESS CLEAR'.
        77  ENDMSGL  PIC 9(4) COMP.

        77  LEN      PIC 9(4) COMP  VALUE 0.
        77  HEXQNAME PIC X(8)       VALUE X'02CB458F40404040'.
        01  HWORLD.
            02  HWORLDEX PIC 9(4)  COMP VALUE 0.
            02  FILLER   PIC X(11) VALUE 'HELLO WORLD'.
        01  HWORLDAS.
            02  HWORLDAX PIC 9(4)  COMP VALUE 0.
            02  FILLER   PIC X(11) VALUE X'48454C4C4F20574F524C44'.

      * GO TO DEPENDING CONTROL
      *   1=ABEND
      *   2=BEC9RTN1
      *   3=BEC9RTN2
      *   4=BEC9RTN3
      *   5=BEC9RTN4
      *   6=BEC9RTN5
      *   7=BEC9RTN6
      *   8=BEC9RTN7
      *   9=BEC9RTN8
        77  DEPCTRL   PIC 9(4) COMP.

        77  HEXCOUNT  PIC 9(4) COMP.
        01  HEXTAB  PIC X(256)  VALUE X'000102030405060708090A0B0C0D0E0F
      -                                '101112131415161718191A1B1C1D1E1F
      -                                '202122232425262728292A2B2C2D2E2F
      -                                '303132333435363738393A3B3C3D3E3F
      -                                '404142434445464748494A4B4C4D4E4F
      -                                '505152535455565758595A5B5C5D5E5F
      -                                '606162636465666768696A6B6C6D6E6F
      -                                '707172737475767778797A7B7C7D7E7F
      -                                '808182838485868788898A8B8C8D8E8F
      -                                '909192939495969798999A9B9C9D9E9F
      -                                'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF
      -                                'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF
      -                                'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF
      -                                'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF
      -                                'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF
      -                             'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'.
        01  HEXINTAB REDEFINES HEXTAB OCCURS 16 TIMES PIC X(16).
        01  HEXTAB16  PIC X(16).

        77  QIDNUM    PIC 9(4) COMP.
        77  ITMNUM    PIC 9(4) COMP.
        77  LNGNUM    PIC 9(4) COMP.

        LINKAGE SECTION.
        01  TSRECLK PIC X(200).

        PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

            MOVE LENGTH OF QWT      TO QHANDL.
            MOVE LENGTH OF CEBRMSG1 TO CEBR1LEN.
            MOVE LENGTH OF CEBRMSG2 TO CEBR2LEN.
            MOVE LENGTH OF ENDMSG   TO ENDMSGL.

            EXEC CICS GETMAIN SET(ADDRESS OF TSRECLK)
                      LENGTH(LENGTH OF TSRECLK) END-EXEC.

      *  MVI   QOPSEP+1,C'|'      SET COL MARKER
      *  MVI   QHSEP,C'|'         SET COL MARKER
            MOVE '|' TO QOPSEP.
            MOVE '|' TO QHSEP.
      *  MVI   QIDNUM,C'1'        RESET QIDERR NUMBER
      *  MVI   ITMNUM,C'1'        RESET ITEMERR NUMBER
      *  MVI   LNGNUM,C'1'        RESET LENGERR NUMBER
            MOVE 1 TO QIDNUM.
            MOVE 1 TO ITMNUM.
            MOVE 1 TO LNGNUM.
      *
      * 1ST Q NAME, 1ST ITEM, SHORT DATA
      *
      *  MVC   QWTSBA,QOP1        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP1 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  MVI   TSREC,X'AB'        BYTE TO PROPAGATE
      *  MVC   TSREC+1(49),TSREC  MAKE 50
            MOVE LOW-VALUES TO TSREC.
            INSPECT TSREC CONVERTING LOW-VALUES TO X'AB'.
            MOVE 1 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEA') FROM(TSREC)
                      LENGTH(50) NUMITEMS(TSNUMIT) END-EXEC.
      *
      * 1ST Q NAME, 2ND ITEM, LONG DATA
      *
      *  MVC   QWTSBA,QOP2        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'2'       SET ITEM IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP2 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '2' TO QOPITEM.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  MVI   TSREC,X'CD'        BYTE TO PROPAGATE
      *  MVC   TSREC+1(199),TSREC MAKE 200
            MOVE LOW-VALUES TO TSREC.
            INSPECT TSREC CONVERTING LOW-VALUES TO X'CD'.
            MOVE 1 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEA') FROM(TSREC)            X
                      NUMITEMS(TSNUMIT) END-EXEC.
      *
      * 2ND Q NAME, 1ST ITEM, USING QNAME, LONG DATA
      *
      *  MVC   QWTSBA,QOP3        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYLONGQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP3 TO QWTSBA.
            MOVE 'MYLONGQUEUEA    ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  MVI   TSREC,X'EF'        BYTE TO PROPAGATE
      *  MVC   TSREC+1(149),TSREC MAKE 150
            MOVE LOW-VALUES TO TSREC.
            INSPECT TSREC CONVERTING LOW-VALUES TO X'EF'.
            MOVE 1 TO DEPCTRL.
            EXEC CICS WRITEQ TS QNAME('MYLONGQUEUEA') FROM(TSREC)
                      NUMITEMS(TSNUMIT) LENGTH(150) END-EXEC.
      *
      * 1ST Q NAME, REWRITE 1ST ITEM
      *
      *  MVC   QWTSBA,QOP4        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'REWRITE' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP4 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE 'REWRITE' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  MVC   TSREC(10),=10X'01' MOVE NEW ITEM
            MOVE LOW-VALUES TO TSREC.
            INSPECT TSREC CONVERTING LOW-VALUES TO X'01'.
            MOVE 1 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEA') FROM(TSREC)
                      LENGTH(10) ITEM(1) REWRITE END-EXEC.
      *
      * HANDLE ALL CONDITIONS, ALWAYS RETURN TO MAIN LINE
      *
            EXEC CICS HANDLE CONDITION                                   X
                 QIDERR(BEC9QID)                                         X
                 ITEMERR(BEC9ITM)                                        X
                 LENGERR(BEC9LNG)                                        X
                 INVREQ(BEC9INV)
                 END-EXEC.
      *
      * BAD REWRITE, BAD Q NAME, RAISE QIDERR 1
      *
      *  MVC   QWTSBA,QOP5        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'DUMMY' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'REWRITE' SET TYPE IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP5 TO QWTSBA.
            MOVE 'DUMMY           ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE 'REWRITE' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN1        R7=HANDLE RETURN
            MOVE 2 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('DUMMY') FROM(TSREC)
                      LENGTH(10) ITEM(1) REWRITE END-EXEC.

      * BED9RTN1 EQU   *
        BEC9RTN1.
      *
      * BAD REWRITE, BAD ITEM, RAISE ITEMERR 1
      *
      *  MVC   QWTSBA,QOP7        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'5'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'REWRITE' SET TYPE IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP7 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '5' TO QOPITEM.
            MOVE 'REWRITE' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN2        R7=HANDLE RETURN
            MOVE 3 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEA') FROM(TSREC)
                      LENGTH(10) ITEM(5) REWRITE END-EXEC.

      * BED9RTN2 EQU   *
        BEC9RTN2.
      *
      * BAD WRITE, RAISE LENGERR
      *
      *  MVC   QWTSBA,QOP9        MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'3'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'WRITE  ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP9 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '3' TO QOPITEM.
            MOVE 'WRITE  ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN3        R7=HANDLE RETURN
            MOVE 4 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEA') FROM(TSREC)
                      LENGTH(LEN) END-EXEC

      * BED9RTN3 EQU   *
        BEC9RTN3.
      *
      * 3RD Q NAME, 1ST ITEM, SHORT DATA
      *
      *  MVC   QWTSBA,QOP11       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYLONGQUEUEB' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'WRITE  ' SET TYPE IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP11 TO QWTSBA.
            MOVE 'MYLONGQUEUEB    ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE 'WRITE  ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  MVI   TSREC,X'45'        BYTE TO PROPAGATE
      *  MVC   TSREC+1(49),TSREC  MAKE 50
            MOVE LOW-VALUES TO TSREC.
            INSPECT TSREC CONVERTING LOW-VALUES TO X'45'.
      *  SR    R7,R7              DUMP IF CONDITION RAISED
            MOVE 1 TO DEPCTRL.
            EXEC CICS WRITEQ TS QNAME('MYLONGQUEUEB') FROM(TSREC)
                      NUMITEMS(TSNUMIT) LENGTH(50) END-EXEC.
      *
      * DELETE 2ND Q NAME
      *
      *  MVC   QWTSBA,QOP12       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYLONGQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C' '       NO ITEM
      *  MVC   QOPTYPE,=C'DELETE ' SET TYPE IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP12 TO QWTSBA.
            MOVE 'MYLONGQUEUEA    ' TO QOPNAME.
            MOVE SPACE TO QOPITEM.
            MOVE 'DELETE ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  SR    R7,R7              DUMP IF CONDITION RAISED
            MOVE 1 TO DEPCTRL.
            EXEC CICS DELETEQ TS QNAME('MYLONGQUEUEA') END-EXEC.
      *
      * BAD DELETE, RAISE QIDERR 2
      *
      *  MVC   QWTSBA,QOP13       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'FRED' SET Q NAME IN MSG
      *  MVI   QOPITEM,C' '       NO ITEM
      *  MVC   QOPTYPE,=C'DELETE ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP13 TO QWTSBA.
            MOVE 'FRED            ' TO QOPNAME.
            MOVE SPACE TO QOPITEM.
            MOVE 'DELETE ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN4        R7=HANDLE RETURN
            MOVE 5 TO DEPCTRL.
      *  MVI   QIDNUM,C'2'        SET QIDERR 2
            MOVE 2 TO QIDNUM.
            EXEC CICS DELETEQ TS QUEUE('FRED') END-EXEC.

      * BED9RTN4 EQU   *
        BEC9RTN4.
      *
      * BAD WRITE, RAISE INVREQ
      *
      *  MVC   QWTSBA,QOP15       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'DFNAME' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'WRITE  ' SET TYPE IN MESSAGE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP15 TO QWTSBA.
            MOVE 'DFNAME          ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE 'WRITE  ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN5        R7=HANDLE RETURN
            MOVE 6 TO DEPCTRL.
            EXEC CICS WRITEQ TS QUEUE('DFNAME') FROM(TSREC)
                      LENGTH(10) END-EXEC.

      * BED9RTN5 EQU   *
        BEC9RTN5.
      *
      * 1ST Q NAME, READ 1ST ITEM
      *
      *  MVC   QWTSBA,QOP17       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'READ   ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP17 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE 'READ   ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  SR    R7,R7              DUMP IF CONDITION RAISED
            MOVE 1 TO DEPCTRL.
            EXEC CICS READQ TS QUEUE('MYQUEUEA') INTO(TSREC)
                      LENGTH(10) ITEM(1) END-EXEC.
      *
      * 1ST Q NAME, READ NEXT (2ND) ITEM
      *
      *  MVC   QWTSBA,QOP18       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'2'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'READ   ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP18 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '2' TO QOPITEM.
            MOVE 'READ   ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  SR    R7,R7              DUMP IF CONDITION RAISED
            MOVE 1 TO DEPCTRL.
            EXEC CICS READQ TS QUEUE('MYQUEUEA')
                      SET(ADDRESS OF TSRECLK)
                      LENGTH(200) NEXT END-EXEC.
      *
      * 1ST Q NAME, READ NEXT (3RD) ITEM, RAISE ITEMERR 2
      *
      *  MVC   QWTSBA,QOP19       MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'3'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'READ   ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP19 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '3' TO QOPITEM.
            MOVE 'READ   ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN6        R7=HANDLE RETURN
            MOVE 7 TO DEPCTRL.
      *  MVI   ITMNUM,C'2'        SET ITEMERR 2
            MOVE 2 TO ITMNUM.
            EXEC CICS READQ TS QUEUE('MYQUEUEA')
                      SET(ADDRESS OF TSRECLK)
                      LENGTH(200) NEXT END-EXEC.

      * BED9RTN6 EQU   *
        BEC9RTN6.
      *  MVI   QOPSEP+1,C' '      NO COL MARKER NOW
      *  MVI   QHSEP,C' '         NO COL MARKER NOW
            MOVE SPACE TO QOPSEP.
            MOVE SPACE TO QHSEP.
      *
      * BAD Q NAME, READ ITEM, RAISE QIDERR 3
      *
      *  MVC   QWTSBA,QOP1C2      MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'BERT' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'1'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'READ   ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP1C2 TO QWTSBA.
            MOVE 'BERT            ' TO QOPNAME.
            MOVE '1' TO QOPITEM.
            MOVE 'READ   ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN7        R7=HANDLE RETURN
            MOVE 8 TO DEPCTRL.
      *  MVI   QIDNUM,C'3'        SET QIDERR 3
            MOVE 3 TO QIDNUM.
            EXEC CICS READQ TS QUEUE('BERT') SET(ADDRESS OF TSRECLK)
                      LENGTH(200) ITEM(1) END-EXEC.

      * BED9RTN7 EQU   *
        BEC9RTN7.
      *
      * 1ST Q NAME, READ 2ND ITEM, LENGTH TRUNCATED, RAISE LENGERR
      *
      *  MVC   QWTSBA,QOP3C2      MOVE WCC,SBA
      *  MVC   QOPNAME,=CL16'MYQUEUEA' SET Q NAME IN MSG
      *  MVI   QOPITEM,C'2'       SET ITEM IN MESSAGE
      *  MVC   QOPTYPE,=C'READ   ' SET TYPE
      *  MVC   QWTMSG,QOP         MOVE MESSAGE TEXT
            MOVE QOP3C2 TO QWTSBA.
            MOVE 'MYQUEUEA        ' TO QOPNAME.
            MOVE '2' TO QOPITEM.
            MOVE 'READ   ' TO QOPTYPE.
            MOVE QOP TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
      *  LA    R7,BED9RTN8        R7=HANDLE RETURN
            MOVE 9 TO DEPCTRL.
      *  MVI   LNGNUM,C'2'        SET LENGERR 2
            MOVE 2 TO LNGNUM.
            EXEC CICS READQ TS QUEUE('MYQUEUEA')
                      SET(ADDRESS OF TSRECLK)
                      LENGTH(20) ITEM(2) END-EXEC.

      * BED9RTN8 EQU   *
        BEC9RTN8.
      *
      * CLEAN UP AND BUILD QUEUES TO TEST CEBR
      *
            EXEC CICS SEND FROM(CEBRMSG1) LENGTH(CEBR1LEN) END-EXEC.
      *  SR    R7,R7              DUMP IF CONDITION RAISED
            MOVE 1 TO DEPCTRL.
            EXEC CICS DELETEQ TS QUEUE('MYQUEUEA') END-EXEC.
            EXEC CICS DELETEQ TS QNAME('MYLONGQUEUEB') END-EXEC.
      * MYQUEUEA IS HEX TABLE IN 16 ROWS OF 16
      *  LA    R6,HEXTAB          R6=HEX TABLE
      *  LA    R8,256             ALL THE HEX CHARS
      *  HEXLOOP  EQU   *
      *  STC   R7,0(R6)           SAVE IT
      *  AHI   R6,1               BUMP HEX TABLE POINTER
      *  AHI   R7,1               NEXT HEX
      *  BCT   R8,HEXLOOP         LOOP
      *
      * WRITEQ ALL THE HEX CHARS FOR LATER TRANSLATION
      *
      *  LA    R6,HEXTAB          R6=HEX TABLE
      *  LA    R8,16              R8=ITEMS TO WRITE
      *  SR    R7,R7              DUMP IF CONDITION RAISED
      *  HEXQ     EQU   *
      *  EXEC  CICS WRITEQ TS QUEUE('MYQUEUEA') FROM(0(R6)) LENGTH(16)
      *  AHI   R6,16              BUMP POINTER
      *  BCT   R8,HEXQ            LOOP
            MOVE 1 TO DEPCTRL.
            MOVE 1 TO HEXCOUNT.
        HEXQ.
            MOVE HEXINTAB (HEXCOUNT) TO HEXTAB16.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEA')
                      FROM(HEXTAB16) LENGTH(16) END-EXEC.
            ADD 1 TO HEXCOUNT.
            IF HEXCOUNT NOT = 17 THEN GO TO HEXQ.

      * MYQUEUEB IS 50 ROWS OF THE WHOLE HEX TABLE
      *  LA    R8,50              R8=ITEMS TO WRITE
      *  SR    R7,R7              DUMP IF CONDITION RAISED
      *  HEXBIGQ  EQU   *
      *  EXEC  CICS WRITEQ TS QUEUE('MYQUEUEB') FROM(HEXTAB)
      *        LENGTH(256)
      *  BCT   R8,HEXBIGQ         LOOP
            MOVE 1 TO DEPCTRL.
            MOVE 1 TO HEXCOUNT.
        HEXBIGQ.
            EXEC CICS WRITEQ TS QUEUE('MYQUEUEB') FROM(HEXTAB)
                      LENGTH(256) END-EXEC.
            ADD 1 TO HEXCOUNT.
            IF HEXCOUNT NOT = 51 THEN GO TO HEXBIGQ.

      * X'02CB458F' IS HEX QUEUE NAME
      * 20 LINES OF HELLO WORLD
      * 20 LINES OF ASCII HELLO WORLD
      * 20 LINES OF 72 BYTES OF HEXTAB
      * 20 LINES OF 36 BYTES OF HEXTAB
      *  SR    R7,R7              DUMP IF CONDITION RAISED
            MOVE 1 TO DEPCTRL.
      *  LA    R6,1               CROSS CHECK ITEM NUMBER
      *  LA    R8,20              R8=ITEMS TO WRITE
      *  HEXQ1    EQU   *
      *  STH   R6,HWORLD          SAVE ITEM NUMBER
      *  EXEC  CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HWORLD) LENGTH(13)
      *  AHI   R6,1               ITEM COUNT
      *  BCT   R8,HEXQ1           LOOP
            MOVE 1 TO HEXCOUNT.
        HEXQ1.
            MOVE HEXCOUNT TO HWORLDEX.
            EXEC CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HWORLD)
                      LENGTH(13) END-EXEC.
            ADD 1 TO HEXCOUNT.
            IF HEXCOUNT NOT = 21 THEN GO TO HEXQ1.

      *  LA    R8,20              R8=ITEMS TO WRITE
      * HEXQ2    EQU   *
      *  STH   R6,HWORLDAS        SAVE ITEM NUMBER
      *  EXEC  CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HWORLDAS) LENGTH(13)
      *  AHI   R6,1               ITEM COUNT
      *  BCT   R8,HEXQ2           LOOP
            MOVE 1 TO HEXCOUNT.
        HEXQ2.
            MOVE HEXCOUNT TO HWORLDAX.
            EXEC CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HWORLDAS)
                      LENGTH(13) END-EXEC.
            ADD 1 TO HEXCOUNT.
            IF HEXCOUNT NOT = 21 THEN GO TO HEXQ2.

      *  LA    R8,20              R8=ITEMS TO WRITE
      *  HEXQ3    EQU   *
      *  STH   R6,HEXCOUNT        SAVE ITEM NUMBER
      *  EXEC  CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HEXCOUNT) LENGTH(72)
      *  AHI   R6,1               ITEM COUNT
      *  BCT   R8,HEXQ3           LOOP
            MOVE 1 TO HEXCOUNT.
        HEXQ3.
            EXEC CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HEXCOUNT)
                      LENGTH(72) END-EXEC.
            ADD 1 TO HEXCOUNT.
            IF HEXCOUNT NOT = 21 THEN GO TO HEXQ3.

      *  LA    R8,20              R8=ITEMS TO WRITE
      *  HEXQ4    EQU   *
      *  STH   R6,HEXCOUNT        SAVE ITEM NUMBER
      *  EXEC  CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HEXCOUNT) LENGTH(36)
      *  AHI   R6,1               ITEM COUNT
      *  BCT   R8,HEXQ4           LOOP
            MOVE 1 TO HEXCOUNT.
        HEXQ4.
            EXEC CICS WRITEQ TS QUEUE(HEXQNAME) FROM(HEXCOUNT)
                      LENGTH(36) END-EXEC.
            ADD 1 TO HEXCOUNT.
            IF HEXCOUNT NOT = 21 THEN GO TO HEXQ4.

            EXEC CICS SEND FROM(CEBRMSG2) LENGTH(CEBR2LEN) END-EXEC.
            EXEC CICS SEND FROM(ENDMSG) LENGTH(ENDMSGL) END-EXEC.
            EXEC CICS RETURN END-EXEC.
      *
      * HANDLE ROUTINES
      *
      * BED9QID  DS    0H
      *  MVC   QWTSBA,QOP6        MOVE WCC,SBA FOR QIDERR 1
      *  CLI   QIDNUM,C'1'        1ST QIDERR ?
      *  BE    BED9QDOK           EXIT IF IT IS
      *  MVC   QWTSBA,QOP14       MOVE WCC,SBA FOR QIDERR 2
      *  CLI   QIDNUM,C'2'        2ND QIDERR ?
      *  BE    BED9QDOK           EXIT IF IT IS
      *  MVC   QWTSBA,QOP2C2      MOVE WCC,SBA FOR QIDERR 3
      *  BED9QDOK EQU   *
      *  MVC   QHANDCON,=C'QIDERR ' MOVE CONDITION
      *  B     QWTHSEND           EXIT
        BEC9QID.
            GO TO QID1 QID2 QID3 DEPENDING ON QIDNUM.
            GO TO ABEND.
        QID1.
            MOVE QOP6 TO QWTSBA.
            GO TO QIDEND.
        QID2.
            MOVE QOP14 TO QWTSBA.
            GO TO QIDEND.
        QID3.
            MOVE QOP2C2 TO QWTSBA.
        QIDEND.
            MOVE 'QIDERR ' TO QHANDCON.
            GO TO QWTHSEND.

      *  BED9ITM  DS    0H
      *  MVC   QWTSBA,QOP8        MOVE WCC,SBA FOR ITEMERR 1
      *  CLI   ITMNUM,C'2'        2ND ITMERR ?
      *  BNE   BED9ITOK           EXIT IF NOT
      *  MVC   QWTSBA,QOP20       MOVE WCC,SBA FOR ITEMERR 2
      *  BED9ITOK EQU   *
      *  MVC   QHANDCON,=C'ITEMERR' MOVE CONDITION
      *  B     QWTHSEND           EXIT
        BEC9ITM.
            GO TO ITM1 ITM2 DEPENDING ON ITMNUM.
            GO TO ABEND.
        ITM1.
            MOVE QOP8 TO QWTSBA.
            GO TO ITMEND.
        ITM2.
            MOVE QOP20 TO QWTSBA.
        ITMEND.
            MOVE 'ITEMERR' TO QHANDCON.
            GO TO QWTHSEND.

      *  BED9LNG  DS    0H
      *  MVC   QWTSBA,QOP10       MOVE WCC,SBA FOR LENGERR 1
      *  CLI   LNGNUM,C'2'        2ND LENGERR ?
      *  BNE   BED9LNOK           EXIT IF NOT
      *  MVC   QWTSBA,QOP4C2      MOVE WCC,SBA FOR ITEMERR 2
      *  BED9LNOK EQU   *
      *  MVC   QHANDCON,=C'LENGERR' MOVE CONDITION
      *  B     QWTHSEND           EXIT
        BEC9LNG.
            GO TO LNG1 LNG2 DEPENDING ON LNGNUM.
            GO TO ABEND.
        LNG1.
            MOVE QOP10 TO QWTSBA.
            GO TO LNGEND.
        LNG2.
            MOVE QOP4C2 TO QWTSBA.
        LNGEND.
            MOVE 'LENGERR' TO QHANDCON.
            GO TO QWTHSEND.

      *  BED9INV  DS    0H
      *  MVC   QWTSBA,QOP16       MOVE WCC,SBA
      *  MVC   QHANDCON,=C'INVREQ ' MOVE CONDITION
        BEC9INV.
            MOVE QOP16 TO QWTSBA.
            MOVE 'INVREQ ' TO QHANDCON.

      *  QWTHSEND EQU   *
      *  MVC   QWTMSG,QHANDCON    MOVE MESSAGE TEXT
      *  EXEC  CICS SEND FROM(QWT) LENGTH(QHANDL)
      *  BR    R7                 RETURN TO MAIN LINE
        QWTHSEND.
            MOVE QHAND TO QWTMSG.
            EXEC CICS SEND FROM(QWT) LENGTH(QHANDL) END-EXEC.
            GO TO ABEND BEC9RTN1 BEC9RTN2 BEC9RTN3 BEC9RTN4
                        BEC9RTN5 BEC9RTN6 BEC9RTN7 BEC9RTN8
                  DEPENDING ON DEPCTRL.

        ABEND.
            EXEC CICS ABEND ABCODE('BEC9') END-EXEC.
