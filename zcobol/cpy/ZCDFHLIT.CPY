.*
.* GET DFHRESP - SET &DFHLIT FOR &DFHRESP
.*
.********************************************************************
.* 01/25/12 DSH RPI 1181 PREVENT DUP ALLOC ERROR
.********************************************************************
         AENTRY GET_DFHRESP
         :&ZC_DFHRESP SETB &ZC_DFHRESP RPI 1181 PREVENT DUP ALLOC 
         AIF   (NOT &ZC_DFHRESP)
               :&ZC_DFHRESP SETB 1
               COPY ZC_DFHRESP                            RPI 1057
         AEND
         :&DFHSYM SETC 'DFHRESP_&DFHRESP'
         :&(&DFHSYM) SETC '&(&DFHSYM)' RPI 1181 PREVENT DUP ALLOC
         :&DFHLIT SETC '&(&DFHSYM)'
         AIF   (K'&DFHLIT EQ 0)
               MNOTE 8,'ZCDFHLIT DFHRESP VALUE NOT FOUND - &DFHRESP'
               MEXIT
         AEND
         AEND
.*
.* GET DFHVALUE - SET &DFHLIT FOR &DFHVALUE
.*
         AENTRY GET_DFHVALUE
         &ZC_DFHVALUE SETB &ZC_DFHVALUE RPI 1181 PREVENT DUP ALLOC
         AIF   (NOT &ZC_DFHVALUE)
               :&ZC_DFHVALUE SETB 1
               COPY ZC_DFHVALUE                           RPI 1057
         AEND
         :&DFHSYM SETC 'DFHVALUE_&DFHVALUE'
         :&(&DFHSYM) SETC &(&DFHSYM) RPI 1181 PREVENT DUP ALLOC
         :&DFHLIT SETC '&(&DFHSYM)'
         AIF   (K'&DFHLIT EQ 0)
               MNOTE 8,'ZCDFHLIT DFHVALUE NOT FOUND - &DFHVALUE'
               MEXIT
         AEND
         AEND
