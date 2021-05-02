.********************************************************************
.*             AUTHOR.   DON HIGGINS.
.*             DATE.     09/23/08.
.*             REMARKS.  SHARED GLOBAL VARIABLES FOR ZSTRMAC SPMS
.********************************************************************
.* 09/23/08 RPI 909 upgrade to use ZSTRMAC SPE's and extend compat.
.********************************************************************
.*
.*  DEFINE SHARED CONDTION CODES FOR USE IN SPM OPERANDS
.*
         ZSTREQU
.*
.*  SHARED LEVEL CONTROL FOR IF, DO, SELECT, AND CASENTRY
.*
         GBLA &ZSTR_LVL      CURRENT LEVEL OF NESTING
         GBLA &ZSTR_MAX_LVL  MAX LEVEL OF NESTING
&ZSTR_MAX_LVL SETA 100
         GBLC &ZSTR_LVL_TYPE(&ZSTR_MAX_LVL) IF,DO,SELECT,CASENTRY
         GBLA &ZSTR_LVL_TCNT(&ZSTR_MAX_LVL) TYPE  COUNTER
         GBLA &ZSTR_LVL_BCNT(&ZSTR_MAX_LVL) BLOCK COUNTER
         GBLB &ZSTR_LVL_ELSE(&ZSTR_MAX_LVL) ELSE BLOCK DEFINED
         GBLB &ZSTR_LVL_TEND(&ZSTR_MAX_LVL) END LABEL REQUESTED
.*
.*  IF, ELSEIF, ELSE, ENDIF
.*
         GBLA  &ZSTR_AIF_CNT                AIF COUNTER
.*
.*  DO, ENDDO
.*
         GBLA  &ZSTR_DO_CNT
.*
.*  SELECT, WHEN, OTHRWISE, ENDSEL
.*
         GBLA  &ZSTR_SEL_CNT             SELECT COUNTER
         GBLC  &ZSTR_SEL_OP(100)   SELECT OPCODE ('' IF NONE)
         GBLC  &ZSTR_SEL_FLD(100)  SELECT FIELD
         GBLC  &ZSTR_SEL_CMP(100)  SELECT COMPARE TYPE
.*
.*  CASENTRY, CASE, ENDCASE
.*
         GBLA &ZSTR_CASE_CNT                 CASENTRY COUNTER
         GBLC &ZSTR_CASE_REG(&ZSTR_MAX_LVL)  REG WITH INDEX 1-N
         GBLA &ZSTR_CASE_MAX(&ZSTR_MAX_LVL)  MAX CASE INDEX 1-N VALUE
.*
.*  END OF ZSTRGBL.CPY
.*
