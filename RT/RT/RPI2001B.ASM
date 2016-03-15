         TITLE 'Test BASSM'
***********************************************************************
* Start create : 02-22-2016
* 1st delivery :
* Designer     : J Ganci
* Programmer   : J Ganci
* Purpose      : Test BASSM
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
RPI2001B CSECT ,
RPI2001B AMODE 24
RPI2001B RMODE 24
         B     RPI20010-RPI2001B(,R15) Branch around header
         DC    AL1(RPI20010-*),C'RPI2001B:&SYSDATC:&SYSTIME'
RPI20010 DS    0H
         STM   R14,R12,12(R13)         Save caller's registers
         LR    R12,R15                 R12 = base register
         USING RPI2001B,R12            Establish addressability
         LA    R0,WALEN                Length of work area
         GETMAIN R,LV=(R0),LOC=BELOW   Obtain work area storage
         ST    R1,8(,R13)              Set forward chain in caller
         ST    R13,4(,R1)              Set back chain
         LR    R13,R1                  Current save area
         USING WORKAREA,R13            Overlay work area
         XR    R0,R0                   Zero
         ST    R0,WASAVE                    first word in save area
         L     R1,8(,R13)              Caller's save area
         LM    R14,R1,12(R13)          Restore altered registers
         ST    R1,WAPARM@              Save pointer to parameter list
         STMH  R12,R12,WAR12H          Save high half of R12
         LLGTR R12,R12                 Ensure high half of R12 is zero
         MVC   WAEYE,=CL8'RPI2001B'    Set eyecatcher
***********************************************************************
*
* 0C = BASSM - Branch and Save and Set Mode
*
* operand1 is 32-bit or 64-bit link information
* operand2 is 32-bit or 64-bit branch address
*
***********************************************************************
*
* Note: initial amode = 31
*
         SAM31 ,                   Amode 31
*
***********************************************************************
*  1. No branch; just set link register; link register = 0
***********************************************************************
T01      DS    0H
         SAM24 ,                   Amode 24
*
         LGHI  R0,-1               Set high half of GG0 to known value
         LGR   R3,R0               Also GG3
*
         LHI   R0,-1               Link register for BASSM
         BASSM R0,R0               Do-nothing BASSM; sets link register
         LA    R3,*                Address after BASSM
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T012424             Continue if equal
         DC    H'0'                Stop if not
T012424  DS    0H
*
         SAM31 ,                   Amode 31
         LHI   R0,-1               Link register for BASSM
         BASSM R0,R0               Do-nothing BASSM; sets link register
         LA    R3,*                Address after BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T012431             Continue if equal
         DC    H'0'                Stop if not
T012431  DS    0H
*
         SAM64 ,                   Amode 64
*
         SGR   R3,R3               Ensure GG3 high half is zero; LA bug
*
         LGHI  R0,-1               Link register for BASSM
         BASSM R0,R0               Do-nothing BASSM; sets link register
         LA    R3,*                Address after BASSM
         OILL  3,X'0001'           Low bit should be on
         CGR   R3,R0               Should be equal
         BE    T012464             Continue if equal
         DC    H'0'                Stop if not
T012464  DS    0H
*
         SAM31 ,                   End test 1
***********************************************************************
*  2. No branch; just set link register; link register = 15
***********************************************************************
T02      DS    0H
*
         SAM24 ,                   Amode 24
*
         LGHI  R15,-1              Set high half of GG15 to known value
         LGR   R3,R15              Also GG3
*
         LHI   R15,-1              Link register for BASSM
         BASSM R15,R0              Do-nothing BASSM; sets link register
         LA    R3,*                Address after BASSM
         CGR   R3,R15              Should be equal (CGR intentional)
         BE    T022424             Continue if equal
         DC    H'0'                Stop if not
T022424  DS    0H
*
         SAM31 ,                   Amode 31
         LHI   R15,-1              Link register for BASSM
         BASSM R15,R0              Do-nothing BASSM; sets link register
         LA    R3,*                Address after BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R15              Should be equal (CGR intentional)
         BE    T022431             Continue if equal
         DC    H'0'                Stop if not
T022431  DS    0H
*
         SAM64 ,                   Amode 64
*
         SGR   R3,R3               Ensure GG3 high half is zero; LA bug
*
         LGHI  R15,-1              Link register for BASSM
         BASSM R15,R0              Do-nothing BASSM; sets link register
         LA    R3,*                Address after BASSM
         OILL  R3,X'0001'          Low bit should be on
         CGR   R3,R15              Should be equal
         BE    T022464             Continue if equal
         DC    H'0'                Stop if not
T022464  DS    0H
*
         SAM31 ,                   End test 2
***********************************************************************
*  3. Branch; no link register
***********************************************************************
T03      DS    0H
*
* part 1: start amode 24
*
         SAM24 ,                   Amode 24
*
         LGHI  R15,-1              Set high half of GG15 to known value
         LGR   R0,R15              Also GG0
         LGR   R3,R15              Also GG3
*
         LA    R15,B032424         Branch address
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; amode same
         DC    H'0'
B032424  DS    0H
*        v psw.amode = x'0'
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T032424             Continue if equal
         DC    H'0'                Stop if not
T032424  DS    0H
*
******** SAM24 ,                   Not needed; amode 24
         LA    R15,B032431         Branch address
         OILH  R15,X'8000'         Amode 31
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 31
         DC    H'0'
B032431  DS    0H
*        v psw.amode = x'1'
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T032431             Continue if equal
         DC    H'0'                Stop if not
T032431  DS    0H
*
         SAM24 ,                   Back to amode 24
         LA    R15,B032464         Branch address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Amode 64
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 64
         DC    H'0'
B032464  DS    0H
*        v psw.amode = x'3'
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T032464             Continue if equal
         DC    H'0'                Stop if not
T032464  DS    0H
*
         SAM24 ,                   Back to amode 24
*
* end part 1
*
* part 2: start amode 31
*
         SAM31 ,                   Amode 31
*
         LGHI  R15,-1              Set high half of GG15 to known value
         LGR   R3,R15              Also GG3
*
         LA    R15,B033124         Branch address
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; amode 24
         DC    H'0'
B033124  DS    0H
*        v psw.amode = x'0'
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T033124             Continue if equal
         DC    H'0'                Stop if not
T033124  DS    0H
*
         SAM31 ,                   Back to amode 31
         LA    R15,B033131         Branch address
         OILH  R15,X'8000'         Amode 31
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 31
         DC    H'0'
B033131  DS    0H
*        v psw.amode = x'1'
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T033131             Continue if equal
         DC    H'0'                Stop if not
T033131  DS    0H
*
         SAM31 ,                   Back to amode 31
         LA    R15,B033164         Branch address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Amode 64
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 64
         DC    H'0'
B033164  DS    0H
*        v psw.amode = x'3'
         OILH  R3,X'8000'          Set amode 31
         CGR   R3,R0               Should be equal (CGR intentional)
         BE    T033164             Continue if equal
         DC    H'0'                Stop if not
T033164  DS    0H
*
         SAM31 ,                   Back to amode 31
*
* end part 2
*
* part 3: start amode 64
*
         SAM64 ,                   Amode 64
*
         SGR   R15,R15             Set GG15 high half to zero; LA bug
         LGR   R3,R15              Also GG3
*
         LA    R15,B036424         Branch address
         LGHI  R0,-1               Preset link register
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 24
         DC    H'0'
B036424  DS    0H
*        v psw.amode = x'0'
         OILL  R3,X'0001'          Previous amode 64
         CGR   R3,R0               Should be equal
         BE    T036424             Continue if equal
         DC    H'0'                Stop if not
T036424  DS    0H
*
         SAM64 ,                   Back to amode 64
         LA    R15,B036431         Branch address
         OILH  R15,X'8000'         Amode 31
         LGHI  R0,-1               Preset link register
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 31
         DC    H'0'
B036431  DS    0H
*        v psw.amode = x'1'
         OILL  R3,X'0001'          Previous amode 64
         CGR   R3,R0               Should be equal
         BE    T036431             Continue if equal
         DC    H'0'                Stop if not
T036431  DS    0H
*
         SAM64 ,                   Back to amode 64
         LA    R15,B036464         Branch address
         OILL  R15,X'0001'         Amode 64
         LGHI  R0,-1               Preset link register
         LA    R3,*+6              Return address for check
         BASSM R0,R15              Branch; no return; new amode 64
         DC    H'0'
B036464  DS    0H
*        v psw.amode = x'3'
         OILL  R3,X'0001'          Previous amode 64
         CGR   R3,R0               Should be equal
         BE    T036464             Continue if equal
         DC    H'0'                Stop if not
T036464  DS    0H
*
* end part 3
*
* end test 3
*
T03END   DS    0H                  End test 3
         SAM31 ,                   Back to amode 31
*
***********************************************************************
*  4. Branch register and link register
***********************************************************************
T04      DS    0H
*
* part 1: start amode 24
*
         SAM24 ,                   Amode 24
*
         LGHI  R15,-1              Set high half of GG15 to known value
         LGR   R3,R15              Also GG3
         LGR   R14,R15             Also GG14
*
         SR    R14,R14             Link register
         LA    R15,B04SUB24        Subroutine address
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                After BASSM
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T042424             Continue if equal
         DC    H'0'                Stop if not
T042424  DS    0H
*
         SR    R14,R14             Link register
         LA    R15,B04SUB31        Subroutine address
         OILH  R15,X'8000'         Amode 31
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                After BASSM
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T042431             Continue if equal
         DC    H'0'                Stop if not
T042431  DS    0H
*
         SR    R14,R14             Link register
         LA    R15,B04SUB64        Subroutine address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Amode 64
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                After BASSM
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T042464             Continue if equal
         DC    H'0'                Stop if not
T042464  DS    0H
*
* end part 1
*
* part 2: start amode 31
*
         SAM31 ,                   Amode 31
*
         LGHI  R15,-1              Set high half of GG15 to known value
*
         SR    R14,R14             Link register
         LA    R15,B04SUB24        Subroutine address
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                After BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T043124             Continue if equal
         DC    H'0'                Stop if not
T043124  DS    0H
*
         SR    R14,R14             Link register
         LA    R15,B04SUB31        Subroutine address
         OILH  R15,X'8000'         Amode 31
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                After BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T043131             Continue if equal
         DC    H'0'                Stop if not
T043131  DS    0H
*
         SR    R14,R14             Link register
         LA    R15,B04SUB64        Subroutine address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Amode 64
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                After BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T043164             Continue if equal
         DC    H'0'                Stop if not
T043164  DS    0H
*
* end part 2
*
* part 3: start amode 64
*
         SAM64 ,                   Amode 64
*
         SGR   R15,R15             Set GG15 high half to zero; LA bug
         LGR   R3,R15              Also GG3
*
         LGHI  R14,-1              Link register
         LA    R15,B04SUB24        Subroutine address
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                After BASSM
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T046424             Continue if equal
         DC    H'0'                Stop if not
T046424  DS    0H
*
         LGHI  R14,-1              Link register
         LA    R15,B04SUB31        Subroutine address
         OILH  R15,X'8000'         Amode 31
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                After BASSM
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T046431             Continue if equal
         DC    H'0'                Stop if not
T046431  DS    0H
*
         LGHI  R14,-1              Link register
         LA    R15,B04SUB64        Subroutine address
         OILL  R15,X'0001'         Amode 64
         BASSM R14,R15             Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                After BASSM
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T046464             Continue if equal
         DC    H'0'                Stop if not
T046464  DS    0H
*
* end part 3
*
         B     T04END              Branch around subroutines
***********************************************************************
B04SUB24 DS    0H                  Subroutine entered amode 24
*        v psw.amode = x'0'
         BSM   R0,R14              Return; restore amode
*
B04SUB31 DS    0H                  Subroutine entered amode 31
*        v psw.amode = x'1'
         BSM   R0,R14              Return; restore amode
*
B04SUB64 DS    0H                  Subroutine entered amode 64
*        v psw.amode = x'3'
         BSM   R0,R14              Return; restore amode
*
* end test 4
*
T04END   DS    0H                  End test 4
         SAM31 ,                   Back to amode 31
*
***********************************************************************
*  5. Executed BASSMs
***********************************************************************
T05      DS    0H
*
         LA    R1,14*16            Execute mask
*
* part 1: start amode 24
*
         SAM24 ,                   Amode 24
*
         LGHI  R14,-1              Set known value in GG14 high half
         LGR   R3,R14              Same for GG3
*
         LA    R15,B05SUB24        Subroutine address
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                Address after EX
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05241              Continue if equal
         DC    H'0'                Stop if not
T05241   DS    0H
*
         LA    R15,B05SUB31        Subroutine address
         OILH  R15,X'8000'         Switch to amode 31
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                Address after EX
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05242              Continue if equal
         DC    H'0'                Stop if not
T05242   DS    0H
*
         LA    R15,B05SUB64        Subroutine address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Switch to amode 64
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                Address after EX
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05243              Continue if equal
         DC    H'0'                Stop if not
T05243   DS    0H
*
         EX    R1,EBASSM0          No branch
*        v psw.amode = x'0'
         LA    R3,*                Address after EX
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05244              Continue if equal
         DC    H'0'                Stop if not
T05244   DS    0H
*
         LA    R15,B05SUB24        Subroutine address
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                Address after EXRL
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05245              Continue if equal
         DC    H'0'                Stop if not
T05245   DS    0H
*
         LA    R15,B05SUB31        Subroutine address
         OILH  R15,X'8000'         Switch to amode 31
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                Address after EXRL
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05246              Continue if equal
         DC    H'0'                Stop if not
T05246   DS    0H
*
         LA    R15,B05SUB64        Subroutine address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Switch to amode 64
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                Address after EXRL
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05247              Continue if equal
         DC    H'0'                Stop if not
T05247   DS    0H
*
         EXRL  R1,EBASSM0          No branch
*        v psw.amode = x'0'
         LA    R3,*                Address after EXRL
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05248              Continue if equal
         DC    H'0'                Stop if not
T05248   DS    0H
*
* end part 1
*
* part 2: start amode 31
*
         SAM31 ,                   Amode 31
*
         LGHI  R14,-1              Set known value in GG14 high half
         LGR   R3,R14              Same for GG3
*
         LA    R15,B05SUB24        Subroutine address
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                Address after EX
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05311              Continue if equal
         DC    H'0'                Stop if not
T05311   DS    0H
*
         LA    R15,B05SUB31        Subroutine address
         OILH  R15,X'8000'         Switch to amode 31
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                Address after EX
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05312              Continue if equal
         DC    H'0'                Stop if not
T05312   DS    0H
*
         LA    R15,B05SUB64        Subroutine address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Switch to amode 64
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                Address after EX
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05313              Continue if equal
         DC    H'0'                Stop if not
T05313   DS    0H
*
         EX    R1,EBASSM0          No branch
*        v psw.amode = x'1'
         LA    R3,*                Address after EX
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05314              Continue if equal
         DC    H'0'                Stop if not
T05314   DS    0H
*
         LA    R15,B05SUB24        Subroutine address
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                Address after EXRL
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05315              Continue if equal
         DC    H'0'                Stop if not
T05315   DS    0H
*
         LA    R15,B05SUB31        Subroutine address
         OILH  R15,X'8000'         Switch to amode 31
         EXRL  1,EBASSM            Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                Address after EXRL
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05316              Continue if equal
         DC    H'0'                Stop if not
T05316   DS    0H
*
         LA    R15,B05SUB64        Subroutine address
         LLGTR R15,R15             It's a 64-bit address
         OILL  R15,X'0001'         Switch to amode 64
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                Address after EXRL
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05317              Continue if equal
         DC    H'0'                Stop if not
T05317   DS    0H
*
         EXRL  R1,EBASSM0          No branch
*        v psw.amode = x'1'
         LA    R3,*                Address after EXRL
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T05318              Continue if equal
         DC    H'0'                Stop if not
T05318   DS    0H
*
* end part 2
*
* part 3: start amode 64
*
         SAM64 ,                   Amode 64
*
         SGR   R15,R15             Set GG15 high half to zero; LA bug
         SGR   R3,R3               Also GG3
*
         LGHI  R14,-1              Initialize link register
         LA    R15,B05SUB24        Subroutine address
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                Address after EX
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05641              Continue if equal
         DC    H'0'                Stop if not
T05641   DS    0H
*
         LGHI  R14,-1              Initialize link register
         LA    R15,B05SUB31        Subroutine address
         OILH  R15,X'8000'         Switch to amode 31
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                Address after EX
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05642              Continue if equal
         DC    H'0'                Stop if not
T05642   DS    0H
*
         LGHI  R14,-1              Initialize link register
         LA    R15,B05SUB64        Subroutine address
         OILL  15,X'0001'          Switch to amode 64
         EX    R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                Address after EX
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05643              Continue if equal
         DC    H'0'                Stop if not
T05643   DS    0H
*
         LGHI  R14,-1              Initialize link register
         EX    R1,EBASSM0          No branch
*        v psw.amode = x'3'
         LA    R3,*                Address after EX
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05644              Continue if equal
         DC    H'0'                Stop if not
T05644   DS    0H
*
         LGHI  R14,-1              Initialize link register
         LA    R15,B05SUB24        Subroutine address
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                Address after EXRL
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05645              Continue if equal
         DC    H'0'                Stop if not
T05645   DS    0H
*
         LGHI  R14,-1              Initialize link register
         LA    R15,B05SUB31        Subroutine address
         OILH  R15,X'8000'         Switch to amode 31
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                Address after EXRL
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05646              Continue if equal
         DC    H'0'                Stop if not
T05646   DS    0H
*
         LGHI  R14,-1              Initialize link register
         LA    R15,B05SUB64        Subroutine address
         OILL  R15,X'0001'         Switch to amode 64
         EXRL  R1,EBASSM           Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                Address after EXRL
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05647              Continue if equal
         DC    H'0'                Stop if not
T05647   DS    0H
*
         LGHI  R14,-1              Initialize link register
         EXRL  R1,EBASSM0          No branch
*        v psw.amode = x'3'
         LA    R3,*                Address after EXRL
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T05648              Continue if equal
         DC    H'0'                Stop if not
T05648   DS    0H
*
* end part 3
*
         B     T05END              Branch around subroutines and data
***********************************************************************
B05SUB24 DS    0H                  Subroutine entered amode 24
*        v psw.amode = x'0'
         BSM   R0,R14              Return; restore amode
*
B05SUB31 DS    0H                  Subroutine entered amode 31
*        v psw.amode = x'1'
         BSM   R0,R14              Return; restore amode
*
B05SUB64 DS    0H                  Subroutine entered amode 64
*        v psw.amode = x'3'
         BSM   R0,R14              Return; restore amode
***********************************************************************
* Executed BASSMs
***********************************************************************
EBASSM   BASSM *-*,R15             Executed
EBASSM0  BASSM *-*,R0              Executed
*
* end test 5
*
T05END   DS    0H                  End test 5
         SAM31 ,                   Back to amode 31
*
***********************************************************************
*  6. Same as test 4 but use same register for link and branch
***********************************************************************
T06      DS    0H
*
* part 1: start amode 24
*
         SAM24 ,                   Amode 24
*
         LGHI  R14,-1              Set high half of GG14 to known value
         LGR   R3,R14              Also GG3
*
         LA    R14,B06SUB24        Subroutine address
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                After BASSM
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T062424             Continue if equal
         DC    H'0'                Stop if not
T062424  DS    0H
*
         LA    R14,B06SUB31        Subroutine address
         OILH  R14,X'8000'         Amode 31
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                After BASSM
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T062431             Continue if equal
         DC    H'0'                Stop if not
T062431  DS    0H
*
         LA    R14,B06SUB64        Subroutine address
         LLGTR R14,R14             It's a 64-bit address
         OILL  R14,X'0001'         Amode 64
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'0'
         LA    R3,*                After BASSM
         LLGTR R3,R3               It's a 64-bit address
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T062464             Continue if equal
         DC    H'0'                Stop if not
T062464  DS    0H
*
* end part 1
*
* part 2: start amode 31
*
         SAM31 ,                   Amode 31
*
         LGHI  R14,-1              Set high half of GG14 to known value
         LGR   R3,R14              Also GG3
*
         LA    R14,B06SUB24        Subroutine address
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                After BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T063124             Continue if equal
         DC    H'0'                Stop if not
T063124  DS    0H
*
         LA    R14,B06SUB31        Subroutine address
         OILH  R14,X'8000'         Amode 31
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                After BASSM
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T063131             Continue if equal
         DC    H'0'                Stop if not
T063131  DS    0H
*
         LA    R14,B06SUB64        Subroutine address
         LLGTR R14,R14             It's a 64-bit address
         OILL  R14,X'0001'         Amode 64
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'1'
         LA    R3,*                After BASSM
         LLGTR R3,R3               It's a 64-bit address
         OILH  R3,X'8000'          Amode 31
         CGR   R3,R14              Should be equal (CGR intentional)
         BE    T063164             Continue if equal
         DC    H'0'                Stop if not
T063164  DS    0H
*
* end part 2
*
* part 3: start amode 64
*
         SAM64 ,                   Amode 64
*
         SGR   R14,R14             Set GG14 high half to zero; LA bug
         LGR   R3,R14              Also GG3
*
         LA    R14,B06SUB24        Subroutine address
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                After BASSM
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T066424             Continue if equal
         DC    H'0'                Stop if not
T066424  DS    0H
*
         LA    R14,B06SUB31        Subroutine address
         OILH  R14,X'8000'         Amode 31
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                After BASSM
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T066431             Continue if equal
         DC    H'0'                Stop if not
T066431  DS    0H
*
         LA    R14,B06SUB64        Subroutine address
         OILL  R14,X'0001'         Amode 64
         BASSM R14,R14             Call; return; amode restored
*        v psw.amode = x'3'
         LA    R3,*                After BASSM
         OILL  R3,X'0001'          Amode 64
         CGR   R3,R14              Should be equal
         BE    T066464             Continue if equal
         DC    H'0'                Stop if not
T066464  DS    0H
*
* end part 3
*
         B     T06END              Branch around subroutines
***********************************************************************
B06SUB24 DS    0H                  Subroutine entered amode 24
*        v psw.amode = x'0'
         BSM   R0,R14              Return; restore amode
*
B06SUB31 DS    0H                  Subroutine entered amode 31
*        v psw.amode = x'1'
         BSM   R0,R14              Return; restore amode
*
B06SUB64 DS    0H                  Subroutine entered amode 64
*        v psw.amode = x'3'
         BSM   R0,R14              Return; restore amode
*
* end test 6
*
T06END   DS    0H                  End test 6
         SAM31 ,                   Back to amode 31
*
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
