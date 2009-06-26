.*
.* GET DFHRESP - SET &DFHLIT FOR &DFHRESP
.*
         AENTRY GET_DFHRESP
         GBLB  &ZC_DFHRESP
         AIF   (NOT &ZC_DFHRESP)
               :&ZC_DFHRESP SETB 1
               COPY ZC_DFHRESP                            RPI 1057
         AEND
         :&DFHSYM SETC 'DFHRESP_&DFHRESP'
         GBLC  &(&DFHSYM)
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
         GBLB  &ZC_DFHVALUE
         AIF   (NOT &ZC_DFHVALUE)
               :&ZC_DFHVALUE SETB 1
               COPY ZC_DFHVALUE                           RPI 1057
         AEND
         :&DFHSYM SETC 'DFHVALUE_&DFHVALUE'
         GBLC  &(&DFHSYM)
         :&DFHLIT SETC '&(&DFHSYM)'
         AIF   (K'&DFHLIT EQ 0)
               MNOTE 8,'ZCDFHLIT DFHVALUE NOT FOUND - &DFHVALUE'
               MEXIT
         AEND
         AEND
