       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPTYPE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
***************************************************************
      
***************************************************************
       01  Z-cobol-data-Types-comp-unsigned.
         03 comp-9-00 
            pic  9(00) comp 
                       value x'FF00'.
         03 comp-9-01 
            pic  9(01) comp 
                       value x'FF01'.
         03 comp-9-02 
            pic  9(02) comp 
                       value x'FF02'.
         03 comp-9-03 
            pic  9(03) comp 
                       value x'FF03'.
         03 comp-9-04 
            pic  9(04) comp 
                       value x'FF04'.
         03 comp-9-05 
            pic  9(05) comp 
                       value x'FF05'.
         03 comp-9-06 
            pic  9(06) comp 
                       value x'FF06'.
         03 comp-9-07 
            pic  9(07) comp 
                       value x'FF07'.
         03 comp-9-08 
            pic  9(08) comp 
                       value x'FF08'.
         03 comp-9-09 
            pic  9(09) comp 
                       value x'FF000009'.
         03 comp-9-10 
            pic  9(10) comp 
                       value x'FF0000000000000A'.
         03 comp-9-11 
            pic  9(11) comp 
                       value x'FF0000000000000B'.
         03 comp-9-12 
            pic  9(12) comp 
                       value x'FF0000000000000C'.
         03 comp-9-13 
            pic  9(13) comp 
                       value x'FF0000000000000D'.
         03 comp-9-14 
            pic  9(14) comp 
                       value x'FF0000000000000E'.
         03 comp-9-15 
            pic  9(15) comp 
                       value x'FF0000000000000F'.
         03 comp-9-16 
            pic  9(16) comp 
                       value x'FF00000000000010'.
         03 comp-9-17 
            pic  9(17) comp 
                       value x'FF00000000000011'.
         03 comp-9-18 
            pic  9(18) comp 
                       value x'FF00000000000012'.
         03 comp-9-19 
            pic  9(19) comp 
                       value x'FF000000000000000000000000000013'.
         03 comp-9-20 
            pic  9(20) comp 
                       value x'FF000000000000000000000000000014'.
         03 comp-9-21 
            pic  9(21) comp 
                       value x'FF000000000000000000000000000015'.
         03 comp-9-22 
            pic  9(22) comp 
                       value x'FF000000000000000000000000000016'.
         03 comp-9-23 
            pic  9(23) comp 
                       value x'FF000000000000000000000000000017'.
         03 comp-9-24 
            pic  9(24) comp 
                       value x'FF000000000000000000000000000018'.
         03 comp-9-25 
            pic  9(25) comp 
                       value x'FF000000000000000000000000000019'.
         03 comp-9-26 
            pic  9(26) comp 
                       value x'FF00000000000000000000000000001A'.
         03 comp-9-27 
            pic  9(27) comp 
                       value x'FF00000000000000000000000000001B'.
         03 comp-9-28 
            pic  9(28) comp 
                       value x'FF00000000000000000000000000001C'.
         03 comp-9-29 
            pic  9(29) comp 
                       value x'FF00000000000000000000000000001D'.
         03 comp-9-30 
            pic  9(30) comp 
                       value x'FF00000000000000000000000000001E'.
         03 comp-9-31 
            pic  9(31) comp 
                       value x'FF00000000000000000000000000001F'.
         03 comp-9-32 
            pic  9(32) comp 
                       value x'FF000000000000000000000000000020'.
         03 comp-9-33 
            pic  9(33) comp 
                       value x'FF000000000000000000000000000021'.
         03 comp-9-34 
            pic  9(34) comp 
                       value x'FF000000000000000000000000000022'.
         03 comp-9-35 
            pic  9(35) comp 
                       value x'FF000000000000000000000000000023'.
         03 comp-9-36 
            pic  9(36) comp 
                       value x'FF000000000000000000000000000024'.
         03 comp-9-37 
            pic  9(37) comp 
                       value x'FF000000000000000000000000000025'.
         03 comp-9-38 
            pic  9(38) comp 
                       value x'FF000000000000000000000000000026'.
         03 comp-9-39 
            pic  9(39) comp 
                       value x'FF000000000000000000000000000027'.
                                       
       01  Z-cobol-data-Types-comp-Signed.
      ***************************************************************
         03 comp-S9-00 
             pic S9(00) comp 
                        value x'8000'.
         03 comp-S9-01 
             pic S9(01) comp 
                        value x'8001'.
         03 comp-S9-02 
             pic S9(02) comp 
                        value x'8002'.
         03 comp-S9-03 
             pic S9(03) comp 
                        value x'8003'.
         03 comp-S9-04 
             pic S9(04) comp 
                        value x'8004'.
         03 comp-S9-05 
             pic S9(05) comp 
                        value x'8005'.
         03 comp-S9-06 
             pic S9(06) comp 
                        value x'8006'.
         03 comp-S9-07 
             pic S9(07) comp 
                        value x'8007'.
         03 comp-S9-08 
             pic S9(08) comp 
                        value x'8008'.
         03 comp-S9-09 
             pic S9(09) comp 
                        value x'80000009'.
         03 comp-S9-10 
             pic S9(10) comp 
                        value x'800000000000000A'.
         03 comp-S9-11 
             pic S9(11) comp 
                        value x'800000000000000B'.
         03 comp-S9-12 
             pic S9(12) comp 
                        value x'800000000000000C'.
         03 comp-S9-13 
             pic S9(13) comp 
                        value x'800000000000000D'.
         03 comp-S9-14 
             pic S9(14) comp 
                        value x'800000000000000E'.
         03 comp-S9-15 
             pic S9(15) comp 
                        value x'800000000000000F'.
         03 comp-S9-16 
             pic S9(16) comp 
                        value x'8000000000000010'.
         03 comp-S9-17 
             pic S9(17) comp 
                        value x'8000000000000011'.
         03 comp-S9-18 
             pic S9(18) comp 
                        value x'8000000000000012'.
         03 comp-S9-19 
             pic S9(19) comp 
                        value x'80000000000000000000000000000013'.
         03 comp-S9-20 
             pic S9(20) comp 
                        value x'80000000000000000000000000000014'.
         03 comp-S9-21 
             pic S9(21) comp 
                        value x'80000000000000000000000000000015'.
         03 comp-S9-22 
             pic S9(22) comp 
                        value x'80000000000000000000000000000016'.
         03 comp-S9-23 
             pic S9(23) comp 
                        value x'80000000000000000000000000000017'.
         03 comp-S9-24 
             pic S9(24) comp 
                        value x'80000000000000000000000000000018'.
         03 comp-S9-25 
             pic S9(25) comp 
                        value x'80000000000000000000000000000019'.
         03 comp-S9-26 
             pic S9(26) comp 
                        value x'8000000000000000000000000000001A'.
         03 comp-S9-27 
             pic S9(27) comp 
                        value x'8000000000000000000000000000001B'.
         03 comp-S9-28 
             pic S9(28) comp 
                        value x'8000000000000000000000000000001C'.
         03 comp-S9-29 
             pic S9(29) comp 
                        value x'8000000000000000000000000000001D'.
         03 comp-S9-30 
             pic S9(30) comp 
                        value x'8000000000000000000000000000001E'.
         03 comp-S9-31 
             pic S9(31) comp 
                        value x'8000000000000000000000000000001F'.
         03 comp-S9-32 
             pic S9(32) comp 
                        value x'80000000000000000000000000000020'.
         03 comp-S9-33 
             pic S9(33) comp 
                        value x'80000000000000000000000000000021'.
         03 comp-S9-34 
             pic S9(34) comp 
                        value x'80000000000000000000000000000022'.
         03 comp-S9-35 
             pic S9(35) comp 
                        value x'80000000000000000000000000000023'.
         03 comp-S9-36 
             pic S9(36) comp 
                        value x'80000000000000000000000000000024'.
         03 comp-S9-37 
             pic S9(37) comp 
                        value x'80000000000000000000000000000025'.
         03 comp-S9-38 
             pic S9(38) comp 
                        value x'80000000000000000000000000000026'.
         03 comp-S9-39 
             pic S9(39) comp 
                        value x'80000000000000000000000000000027'.
      ***************************************************************
       PROCEDURE DIVISION.
        Display 'Computational Data Types'.
        Stop Run.
