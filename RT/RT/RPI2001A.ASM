         TITLE 'Test BSM'
***********************************************************************
* Start create : 02-22-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test BSM
***********************************************************************
*
* Define constants DSECTs and environment
*
***********************************************************************
         EQUREGS ,
***********************************************************************
*
* Define workareas and fixed constants
*
***********************************************************************
WORKAREA DSECT
WASAVE   DS    18F                 Save area
WAEYE    DS    CL8                 Eyecatcher
WAPARM@  DS    A                   Address of parameter string/list
WAR12H   DS    A                   Original GG12 high half
WALEN    EQU   *-WORKAREA          Length of work area
*
***********************************************************************
*
RPI2001A CSECT ,
RPI2001A AMODE 24
RPI2001A RMODE 24
         B     RPI20010-RPI2001A(,R15) Branch around header
         DC    AL1(RPI20010-*),C'RPI2001A:&SYSDATC:&SYSTIME'
RPI20010 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2001A,R12            Establish addressability
         LA    R0,WALEN                Length of work area
         GETMAIN R,LV=(R0),LOC=BELOW   Obtain work area storage
         ST    R1,8(,R13)              Set forward chain in caller
         ST    R13,4(,R1)              Set back chain
         LR    R13,R1                  Current save area
         USING WORKAREA,R13            Overlay work area
         XR    R0,R0                   Zero
         ST    R0,WASAVE                    first word in save area
         L     R1,4(,R13)              Caller's save area
         LM    R14,R1,12(R1)           Restore altered registers
         ST    R1,WAPARM@              Save pointer to parameter list
         STMH  R12,R12,WAR12H          Save high half of R12
         LLGTR R12,R12                 Ensure high half of R12 is zero
         MVC   WAEYE,=CL8'RPI2001A'    Set eyecatcher
***********************************************************************
*
* 0B = BSM - Branch and Set Mode
*
* operand1 is 32-bit or 64-bit link information
* operand2 is 32-bit or 64-bit branch address
*
***********************************************************************
*
* Note: initial amode = 31
*
         SAM31 ,                   Amode 31

***********************************************************************
*  1. Do nothing BSM; note: initial amode = 31
***********************************************************************
T01      DS    0H
*
         SAM24 ,                   Amode 24
*
         LGHI  R0,-1               Set GG0 to known value
         LGR   R3,R0               Copy value
*
         BSM   R0,R0               Do-nothing BSM
*        v psw.amode = x'0'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T0124               Unchanged
         DC    H'0'                Stop if changed
T0124    DS    0H
*
         SAM31 ,                   Amode 31   
         BSM   R0,R0               Do-nothing BSM
*        v psw.amode = x'1'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T0131               Unchanged
         DC    H'0'                Stop if changed
T0131    DS    0H
*
         SAM64 ,                   AMODE 64
         BSM   R0,R0               Do-nothing BSM
*        v psw.amode = x'3'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T0164               Unchanged
         DC    H'0'                Stop if changed
T0164    DS    0H
*
T01END   DS    0H                  End test 1
         SAM31 ,                   Back to amode 31
***********************************************************************
*  2. branch; no return; change amode
***********************************************************************
T02      DS    0H
*
* part 1: switch from amode 24 to amode 24, 31, 64
*
         SAM24 ,                   Amode 24
*
         LGHI  R8,-1               Set GG8 to known value
         LGR   R0,R8               Copy value
         LGR   R3,R8               Copy value
*
         LA    R8,B022424          Branch address
         BSM   R0,R8               Branch and set amode 24
         DC    H'0'                Never get here
B022424  DS    0H
*        v psw.amode = x'0'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T022424             Unchanged
         DC    H'0'                Stop if changed
T022424  DS    0H
*
         SAM24 ,                   Amode 24
         LA    R8,B022431          Branch address
         OILH  R8,X'8000'          Amode 31 bit
         BSM   R0,R8               Branch and set amode 31
         DC    H'0'                Never get here
B022431  DS    0H
*        v psw.amode = x'1'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T022431             Unchanged
         DC    H'0'                Stop if changed
T022431  DS    0H
*
         SAM24 ,                   Amode 24
         LA    R8,B022464          Branch address
         LLGTR R8,R8               It's a 64-bit address
         OILL  R8,X'0001'          Amode 64 bit
         BSM   R0,R8               Branch and set amode 64
         DC    H'0'                Never get here
B022464  DS    0H
*        v psw.amode = x'3'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T022464             Unchanged
         DC    H'0'                Stop if changed
T022464  DS    0H
*
* end part 1
*
* part 2: switch from amode 31 to amode 24, 31, 64
*
         SAM31 ,                   Amode 31
*
         LGHI  R8,-1               Set GG8 to known value
*
         LA    R8,B023124          Branch address
         BSM   R0,R8               Branch and set amode 24
         DC    H'0'                Never get here
B023124  DS    0H
*        v psw.amode = x'0'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T023124             Unchanged
         DC    H'0'                Stop if changed
T023124  DS    0H
*
         SAM31 ,                   Amode 31
         LA    R8,B023131          Branch address
         OILH  R8,X'8000'          Amode 31 bit
         BSM   R0,R8               Branch and set amode 31
         DC    H'0'                Never get here
B023131  DS    0H
*        v psw.amode = x'1'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T023131             Unchanged
         DC    H'0'                Stop if changed
T023131  DS    0H
*
         SAM31 ,                   Amode 31
         LA    R8,B023164          Branch address
         LLGTR R8,R8               It's a 64-bit address
         OILL  R8,X'0001'          Amode 64 bit
         BSM   R0,R8               Branch and set amode 64
         DC    H'0'                Never get here
B023164  DS    0H
*        v psw.amode = x'3'
         CGR   R3,R0               Verify unchanged (CGR intentional)
         BE    T023164             Unchanged
         DC    H'0'                Stop if changed
T023164  DS    0H
*
         SAM31 ,
*
* end part 2
*
* part 3: switch from amode 64 to amode 24, 31, 64
*
         SAM64 ,                   Amode 64
*
         SGR   R8,R8               Set GG8 high half to 0; LA bug
*
         LA    R8,B026424          Branch address
         BSM   R0,R8               Branch and set amode 24
         DC    H'0'                Never get here
B026424  DS    0H
*        v psw.amode = x'0'
         CGR   R3,R0               Verify unchanged
         BE    T026424             Unchanged
         DC    H'0'                Stop if changed
T026424  DS    0H
*
         SAM64 ,                   Amode 64
         LA    R8,B026431          Branch address
         OILH  R8,X'8000'          Amode 31 bit
         BSM   R0,R8               Branch and set amode 31
         DC    H'0'                Never get here
B026431  DS    0H
*        v psw.amode = x'1'
         CGR   R3,R0               Verify unchanged
         BE    T026431             Unchanged
         DC    H'0'                Stop if changed
T026431  DS    0H
*
         SAM64 ,                   Amode 64
         LA    R8,B026464          Branch address
         OILL  R8,X'0001'          Amode 64 bit
         BSM   R0,R8               Branch and set amode 64
         DC    H'0'                Never get here
B026464  DS    0H
*        v psw.amode = x'3'
         CGR   R3,R0               Verify unchanged
         BE    T026464             Unchanged
         DC    H'0'                Stop if changed
T026464  DS    0H
*
         SAM31 ,
*
* end part 3
*
T02END   DS    0H                  End test 2
         SAM31 ,                   Back to amode 31
***********************************************************************
*  3. branch; change amode; return; amode restored
***********************************************************************
T03      DS    0H
*
* part 1: start amode 24
*
         SAM24 ,
*
         LGHI  R8,-1               Set GG8 to known value
         LGR   R7,R8               Also GG7
         LGR   R3,R8               Also GG3
*
         LA    R7,B032424          Return point
         LA    R8,B03SUB           Branch address
         BSM   R7,R8               Branch; returns; amode restored
B032424  DS    0H
*        v psw.amode = x'0'
         LA    R3,*                After BSM
         CGR   R3,R7               Should be the same (CGR intentional)
         BE    T032424             Continue if equal
         DC    H'0'                Stop if not equal
T032424  DS    0H
*
         LA    R7,B032431          Return point
         LA    R8,B03SUB           Branch address
         OILH  R8,X'8000'          Amode 31
         BSM   R7,R8               Branch; returns; amode restored
B032431  DS    0H
*        v psw.amode = x'0'
         LA    R3,*                After BSM
         CGR   R3,R7               Should be the same (CGR intentional)
         BE    T032431             Continue if equal
         DC    H'0'                Stop if not equal
T032431  DS    0H
*
         LA    R7,B032464          Return point
         LA    R8,B03SUB           Branch address
         LLGTR R8,R8               It's a 64-bit address
         OILL  R8,X'0001'          Amode 64
         BSM   R7,R8               Branch; returns; amode restored
B032464  DS    0H
*        v psw.amode = x'0'
         LA    R3,*                After BSM
         CGR   R3,R7               Should be the same (CGR intentional)
         BE    T032464             Continue if equal
         DC    H'0'                Stop if not equal
T032464  DS    0H
*
* end part 1
*
* part 2: start amode 31
*
         SAM31 ,
*
         LGHI  R8,-1               Set GG8 to known value
         LGR   R7,R8               Also GG7
         LGR   R3,R8               Also GG3
*
         LA    R7,B033124          Return point
         LR    R6,R7               Copy for compare
         LA    R8,B03SUB           Branch address
         BSM   R7,R8               Branch; returns; amode restored
B033124  DS    0H
*        v psw.amode = x'1'
         LA    R3,*                After BSM
         OILH  R3,X'8000'          Set amode 31 bit
         CGR   R3,R7               Should be the same (CGR intentional)
         BE    T033124             Continue if equal
         DC    H'0'                Stop if not equal
T033124  DS    0H
*
         LA    R7,B033131          Return point
         LA    R8,B03SUB           Branch address
         OILH  R8,X'8000'          Amode 31
         BSM   R7,R8               Branch; returns; amode restored
B033131  DS    0H
*        v psw.amode = x'1'
         LA    R3,*                After BSM
         OILH  R3,X'8000'          Set amode 31 bit
         CGR   R3,R7               Should be the same (CGR intentional)
         BE    T033131             Continue if equal
         DC    H'0'                Stop if not equal
T033131  DS    0H
*
         LA    R7,B033164          Return point
         LA    R8,B03SUB           Branch address
         LLGTR R8,R8               It's a 64-bit address
         OILL  R8,X'0001'          Amode 64
         BSM   R7,R8               Branch; returns; amode restored
B033164  DS    0H
*        v psw.amode = x'1'
         LA    R3,*                After BSM
         OILH  R3,X'8000'          Set amode 31 bit
         CGR   R3,R7               Should be the same (CGR intentional)
         BE    T033164             Continue if equal
         DC    H'0'                Stop if not equal
T033164  DS    0H
*
* end part 2
*
* part 3: start amode 64
*
T03P3    DS    0H
*
         SAM64 ,
*
         SGR   R8,R8               Set GG8 high half to 0; LA bug
         LGR   R7,R8               Also GG7
         LGR   R3,R8               Also GG3
*
         LA    R7,B036424          Return point
         LA    R8,B03SUB           Branch address
         BSM   R7,R8               Branch; returns; amode restored
B036424  DS    0H
*        v psw.amode = x'3'
         LA    R3,*                After BSM
         OILL  R3,X'0001'          Amode is 64
         CGR   R3,R7               Should be the same
         BE    T036424             Continue if equal
         DC    H'0'                Stop if not equal
T036424  DS    0H
*
         LA    R7,B036431          Return point
         LA    R8,B03SUB           Branch address
         OILH  R8,X'8000'          Amode 31
         BSM   R7,R8               Branch; returns; amode restored
B036431  DS    0H
*        v psw.amode = x'3'
         LA    R3,*                After BSM
         OILL  R3,X'0001'          Amode is 64
         CGR   R3,R7               Should be the same
         BE    T036431             Continue if equal
         DC    H'0'                Stop if not equal
T036431  DS    0H
*
         LA    R7,B036464          Return point
         LA    R8,B03SUB           Branch address
         OILL  R8,X'0001'          Amode 64
         BSM   R7,R8               Branch; returns; amode restored
B036464  DS    0H
*        v psw.amode = x'3'
         LA    R3,*                After BSM
         OILL  R3,X'0001'          Amode is 64
         CGR   R3,R7               Should be the same
         BE    T036464             Continue if equal
         DC    H'0'                Stop if not equal
T036464  DS    0H
*
* end part 3
*
         B     T03END              Jump around internal subroutine
*
***********************************************************************
*        Subroutine; entered via BSM   R7,R8
***********************************************************************
B03SUB   DS    0H
         BSM   R0,R7               Return; restore amode
*
T03END   DS    0H                  End test 3
         SAM31 ,                   Back to amode 31
***********************************************************************
*  4. repeat test 3; use same register for both operands
***********************************************************************
T04      DS    0H
*
* part 1: start amode 24
*
         SAM24 ,
*
         LGHI  R8,-1               Set GG8 tp known value
         LGR   R3,R8               Also GG3
*
         LA    R8,B042424          Branch address
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B042424  DS    0H
*        v psw.amode = '0'
         LA    R3,*                BSM destination
         CGR   R3,R8               Should be equal (CGR intentional)
         BE    T042424             Continue if equal
         DC    H'0'                Stop if not equal
T042424  DS    0H
*
         SAM24 ,
         LA    R8,B042431          Branch address
         OILH  R8,X'8000'          Amode 31
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B042431  DS    0H
*        v psw.amode = '1'
         LA    R3,*                BSM destination
         CGR   R3,R8               Should be equal (CGR intentional)
         BE    T042431             Continue if equal
         DC    H'0'                Stop if not equal
T042431  DS    0H
*
         SAM24 ,
         LA    R8,B042464          Branch address
         LLGTR R8,R8               It's a 64-bit address
         OILL  R8,X'0001'          Amode 64
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B042464  DS    0H
*        v psw.amode = '3'
         LA    R3,*                BSM destination
         LLGTR R3,R3               Extend to 64-bit address
         OILL  R3,X'0001'          Set amode 64 bit
         CGR   R3,R8               Should be equal
         BE    T042464             Continue if equal
         DC    H'0'                Stop if not equal
T042464  DS    0H
*
* end part 1
*
* part 2: start amode 31
*
         SAM31 ,
*
         LGHI  R8,-1               Set GG8 to known value
         LGR   R3,R8               Also GG3
*
         LA    R8,B043124          Branch address
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B043124  DS    0H
*        v psw.amode = '0'
         LA    R3,*                BSM destination
         OILH  R3,X'8000'          Set amode 31 bit
         CGR   R3,R8               Should be equal (CGR intentional)
         BE    T043124             Continue if equal
         DC    H'0'                Stop if not equal
T043124  DS    0H
*
         SAM31 ,
         LA    R8,B043131          Branch address
         OILH  R8,X'8000'          Amode 31
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B043131  DS    0H
*        v psw.amode = '1'
         LA    R3,*                BSM destination
         OILH  R3,X'8000'          Set amode 31 bit
         CGR   R3,R8               Should be equal (CGR intentional)
         BE    T043131             Continue if equal
         DC    H'0'                Stop if not equal
T043131  DS    0H
*
         SAM31 ,
         LA    R8,B043164          Branch address
         LLGTR R8,R8               It's a 64-bit address
         OILL  R8,X'0001'          Amode 64
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B043164  DS    0H
*        v psw.amode = '3'
         LA    R3,*                BSM destination
         LLGTR R3,R3               Extend to 64-bit address
         OILH  R3,X'8000'          Set amode 31 bit
         OILL  R3,X'0001'          Also set amode 64 bit
         CGR   R3,R8               Should be equal
         BE    T043164             Continue if equal
         DC    H'0'                Stop if not equal
T043164  DS    0H
*
* end part 2
*
* part 3: start amode 64
*
         SAM64 ,
*
         SGR   R8,R8               Set GG8 high half to 0; LA bug
         LGR   R3,R8               Also GG3
*
         LA    R8,B046424          Branch address
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B046424  DS    0H
*        v psw.amode = '0'
         LA    R3,*                BSM destination
         OILL  R3,X'0001'          Set amode 64 bit
         CGR   R3,R8               Should be equal
         BE    T046424             Continue if equal
         DC    H'0'                Stop if not equal
T046424  DS    0H
*
         SAM64 ,
         LA    R8,B046431          Branch address
         OILH  R8,X'8000'          Amode 31
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B046431  DS    0H
*        v psw.amode = '1'
         LA    R3,*                BSM destination
         OILL  R3,X'0001'          Set amode 64 bit
         OILH  R3,X'8000'          Also set amode 31 bit
         CGR   R3,R8               Should be equal
         BE    T046431             Continue if equal
         DC    H'0'                Stop if not equal
T046431  DS    0H
*
         SAM64 ,
         LA    R8,B046464          Branch address
         OILL  R8,X'0001'          Amode 64
         BSM   R8,R8               Branch; no return; amode changed
         DC    H'0'
B046464  DS    0H
*        v psw.amode = '3'
         LA    R3,*                BSM destination
         OILL  R3,X'0001'          Set amode 64 bit
         CGR   R3,R8               Should be equal
         BE    T046464             Continue if equal
         DC    H'0'                Stop if not equal
T046464  DS    0H
*
* end part 3
*
T04END   DS    0H                  End test 4
         SAM31 ,                   Back to amode 31
***********************************************************************
* All done
***********************************************************************
*NSI     B     EXIT                Done
***********************************************************************
EXIT     DS    0H
         LMH   R12,R12,WAR12H      Restore high half of R12
         LR    R1,R13              R1 --> work area
         L     R13,4(,R13)         R13 --> Caller's save area
         LA    R0,WALEN            Length of work area
         FREEMAIN R,LV=(R0),A=(R1) Release work area storage
         LM    R14,R12,12(R13)     Restore caller's registers
         SR    R15,R15             Set return code to zero
         BR    R14                 Return to caller
*
         LTORG
*
         END
