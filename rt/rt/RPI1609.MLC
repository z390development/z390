SAMPLE2  CSECT
         BASR  R3,0
         USING *,R3
         LLIHF R1,C'9999'
         LLIHF R1,C'99'
         LLIHF R1,4C'9'
         LLIHF R1,CL4'99'
         CFI   R1,4X'FF'
         CFI   R1,X'FFFF'
         CFI   R1,-1
         AHI   R1,XL2'00FFFF'
         AHI   R1,X'00FFFF'
         AHI   R1,B'1111'
         AHI   R1,-1
         CFI   R1,C'9'
         CFI   R1,CL4'9'
         CFI   R1,C'   9'
         CLFI  R1,-1
         AHI   R1,65535
         CLI   HERE,-1
         CLI   HERE,C'9'
         BALR  2FL.41'7',3BL.4'101'
         DC    2FL.41'7'
         DC    3BL.4'101'
         BALR  FL.4'7',3BL.4'101'
         BALR  FL.4'7',BL.4'101'
         DC    FL.4'7'
         DC    BL.4'101'
HERE     DS    C
         YREGS
         END
