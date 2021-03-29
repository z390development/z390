.********************************************************************
.* RESET LAB LEN - REMOVE SS1 LENGTH IF ANY FROM LAB 
.* USED BY ZC_CALC, GEN_ADD
.********************************************************************
.* 05/26/09 RPI 1019 INITIAL CODING SEE TESTSIX1 CALC TEST
,* 04/06/11 ROU 1139 FIX SETA STRING FOR HLASM COMP.
.********************************************************************
         AENTRY RESET_LAB_LEN
         AIF   ('&LAB'(1,1) GE '0')  IS THIS O(L,R) 
               :&ILAB SETA ('&LAB' INDEX '(')
               AIF (&ILAB GT 1)
                   :&FIELD_OFF SETA D2A('&LAB'(1,&ILAB-1)) RPI 1139
                   :&JLAB SETA ('&LAB' INDEX ',')
                   AIF (&JLAB GT &ILAB+1)
                     :&FIELD_LEN SETA D2A('&LAB'(&ILAB+1,&JLAB-1)) RPI 1139
                     :&FIELD_REG SETC '&LAB'(&JLAB+1,K'&LAB-&JLAB-1)
                     :&LAB SETC '&LAB'(1,&ILAB).'&LAB'(&JLAB+1,*) O(R)
                   AEND
               AEND
         AELSEIF ('&LAB'(1,5) EQ 'ZCVT_') STRIP ZCVT_WORKAREA+N(LEN)
               :&ILAB SETA ('&LAB' INDEX '(')
               AIF (&ILAB GT 1)
                   :&LAB SETC '&LAB'(1,&ILAB-1)
               AEND
         AEND
         AEND
