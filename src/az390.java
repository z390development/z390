/*
z390 - Mainframe assembler emulator and run-time engine
Copyright (C) 2021 z390 Assembler LLC

This file is part of z390.
z390 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

z390 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <https://www.gnu.org/licenses/>.
*/

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.RandomAccessFile;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.nio.ByteBuffer;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;                                // #568
import java.util.Date;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JTextArea;

public  class  az390 implements Runnable {
   /*
    az390 is the assembler component of z390 which can be called from
    z390 gui interface or from command line to read bal source and
    generate obj relocatable object code file.

    ****************************************************
    * Maintenance
    * ***************************************************
    * 03/30/05 copied from mz390.java and modified
    * 04/03/05 completed basic assembly of demo with
    *          obj and prn file output but missing 
    *          operand parsing.
    * 04/15/05 completed demo support with literal
    *          and RLD support for DC A type fields
    * 05/17/05 add support for '' reduction in dcc
    * 05/29/05 add DCP support 
    * 05/31/05 add packed dec L1 and L2 support
    * 06/25/05 fix r3,r2 code sequence in RRF format
    * 07/05/05 fix shift ins format to skip r3
    * 07/11/05 add DB,DH,EB,EH,LB, and LH fp data
    * 07/19/05 add r1 only exception for SPM and IPM
    * 07/23/05 fix RSL setup for TP d1(l1,b1) packed field type
    * 07/24/05 fix fp constant calc to handle LB/LH
    *          exponent range beyond double by using
    *          equivalent log(X*10**N)=Log(X)+N*log(10)
    * 07/27/05 fix RRE to allow 1 opr (IPM,EFPC,SFPC)
    * 07/30/05 fix sequence of r3,r2 in RRF format for FIEBR
    * 07/31/05 add format RR4 for RRF DIEBR and DIDBR
    * 08/04/05 fix trap error when ins has missing operands
    * 08/17/05 add EXTRN support - unit test TESTEXT1
    * 08/19/05 fix esd and offset in obj for mult sect6s
    * 08/19/05 add dcv_data Vcon and ENTRY support
    * 08/22/05 add SYSBAL, SYSOBJ, SYSPRN dir options
    * 08/28/05 add DS/DC S type support
    * 08/28/05 add dependent and labeled USING support
    * 09/01/05 ADD ORG support and comma delimited continue
    * 09/08/05 fix address errors for mult sect pgms by
    *          forcing pass for any sect change
    * 09/09/05 add CNOP support for use in READ/WRITE
    * 10/03/05 RPI2  fix DC duplication factor error 51
    * 10/03/05 RPI3  fix RS/RX with ddd(,B) syntax
    * 10/03/05 RPI9  fix L' operator parsing
    * 10/03/05 RPI10 fix ORG with no operand error
    * 10/03/05 RPI11 fix DC S(1) error 38 no base
    * 10/03/05 RPI11 fix DC S(X) error 88 xref error
    * 10/04/05 RPI5 - option ASCII use ASCII vs EBCDIC
    *                    DC C'...' ascii char data
    *                    C'..' self def. term value
    * 10/04/05 RPI6 - option ERR(nn) limit errors
    * 10/05/05 RPI5 - add DC and SDT C".." ascii char
    * 10/05/05 RPI12 - reset lit_ref/gen after errors
    * 10/17/05 RPI25 - change TRACE to TRACEA option
    * 10/18/05 RPI29 - use AZ390E and AZ390I prefixes
    * 10/19/05 RPI34 - full ascii / ebcdic translate
    * 11/07/05 RPI73 support C!..! EBCDIC always
    * 11/08/05 RPI73 fix PKA X'E9' rflen from s2
    * 11/11/05 RPI87 fix ORG when preceeded by abs exp. calc
    * 11/12/05 RPI85 issue error if AFHVY value
    *          exceeds size of field.
    * 11/13/05 RPI90 fix regression in RPI73 causing
    *          B, C, and X symbols to trap
    * 11/28/05 RPI113 file path with drive: and no separator
    * 12/03/05 RPI115 fix continuation support for lit comma
    * 12/03/05 RPI116 issue error if no END found
    * 12/07/05 RPI122 ignore following opcodes AMODE, RMODE,
    *          EJECT, SPACE, 
    * 12/07/05 RPI124 remove trailing spaces from source
    * 12/08/05 RPI120 fix SRP explicit d2(b2) format
    * 12/12/05 RPI131 ignore label on TITLE to avoid dup.
    * 12/15/05 RPI135 use tz390 shared tables
    * 12/17/05 RPI57 - symbol cross reference option xref
    * 12/19/05 RPI142 add DS/DC Y and share type tables
    * 12/23/05 RPI127 remove user mlc type from file name
    *          and use shared set_pgm_dir_name_type
    * 12/23/05 RPI131 limit file output to maxfile(mb)
    * 12/31/05 change MNOTE opcode case value for opsyn
    * 01/01/06 RPI150 add OPSYN support 
    * 01/06/06 RPI157 check for extra instruction parms
    * 01/06/06 RPI159 trap = as literal error in expression
    * 01/09/06 RPI161 allow d(,b) in  by eliminating
    *          duplicate code not fixed by RPI3
    * 01/09/06 RPI164 convert EXTRN to CSECT or ENTRY
    * 01/10/06 RPI165 xref USAGE references
    * 01/10/06 RPI167 issue error for contiunation text < 16
    * 01/11/06 RPI166 correct RSY b(d) generation
    * 01/13/05 RPI171 correct unary +- support
    * 01/19/06 RPI181 terminate on any white space char
    * 01/24/06 RPI182 PRINT, PUSH, POP, WXTRN
    * 01/25/06 RPI128 add bin obj with hex obj option
    * 01/26/06 RPI 172 move options to tz390
    * 02/10/06 RPI 199 add BLX branch relative on condition long
    * 02/12/06 RPI 189 sort lits and symbols in XREF
    * 02/18/06 RPI 206 correct RRF 3 formats
    *          a) case 30 - DIEBR, DIDBR     > r1,r3,r2,m4 > 3412 
    *          b) case 15 - MA?R, MS?R, MY?R = r1,r3,r2    > 1032 
    *          c) case 34 - CG?R, CF?R, FI?R, IDTE, TB?R > r1,m3,r2 > 3012
    * 02/21/06 RPI 208 use tz390.z390_abort flag
    * 03/16/06 RPI 230 add COM, RSECT, START limited support
    * 03/16/06 RPI 238 MNOTE error if level > 4
    * 03/17/06 RPI 233 support macro call/exit level of nesting
    * 03/19/06 RPI 232 support integers with exponents in expressions and DC's
    * 03/21/06 RPI 253 allow _ to start symbols
    * 03/21/06 RPI 258 allow , delimiter on ORG
    * 03/22/06 RPI 254 allow blank section reference to private
    * 03/22/06 RPI 260 improve error messages for parsing errors
    * 03/22/06 RPI 237 fix DROP to handle comments and use parm_pat
    * 04/01/06 RPI 265 support alignment within DS/DC
    * 04/02/06 RPI 264 repeat passes at least twice to
    *          try and resolve errors even if at max.
    * 04/04/06 RPI 270 support DS/DC/SDT CA, CB, AD, FD, VD
    * 04/06/06 RPI 274 support dependent USING ref to DSECT base
    * 04/10/06 RPI 276 don't issue error on missing END if PROFILE
    * 04/12/06 RPI 277 support 13 opcodes with no operands (E,S, and RRE)
    * 04/12/06 RPI 278 support NOPRINT on PUSH/POP
    * 04/12/06 RPI 279 use correct version of max_time_seconds
    * 04/12/06 RPI 280 increase tz390.opt_maxsym to 50,000
    * 04/17/06 RPI 284 and opt_max???? for init_arryas()
    * 04/21/06 RPI 288 fix ENTRY for CSECT and supress SPACE, EJECT
    * 04/23/06 RPI 285 force printing MNOTE errors and az390 errors
    * 04/28/06 RPI 301 use esd_base to handle loctr bases
    * 04/28/06 RPI 304 add NOPRINT support for PRINT
    * 04/30/06 RPI 306 update OPSYN support, supress copy stmt
    * 05/09/06 RPI 312 add name to return code message
    * 05/11/06 RPI 313 change MNOTE to set max return code 
    *          but do not issue error and fix exp parser
    *          to handle -(...) unary +- before (.
    * 06/04/06 RPI 327 issue error if dup < 0
    * 06/08/06 RPI 338 ignore unsupported options on PRINT
    * 06/09/06 RPI 330 add MNOTE's with level > 0 to error log
    * 07/05/06 RPI 356 prevent trap in calc_exp with null parsm
    * 07/13/06 RPI 365 allow literals as targets of pfx operator in exp.
    * 07/13/06 RPI 367 support floating point (MIN) and (MAX) constants
    * 07/13/06 RPI 368 add support for Snn scale factor
    * 07/14/06 RPI 369 add suppor to allow EQU 4th and 5th parms
    * 07/14/06 RPI 371 allow spaces in DC X'...' constants
    * 07/15/06 RPI 368 ignore ACONTROL and ALIAS
    * 07/16/06 RPI 373 correct alignment to *8 for L etc.
    * 07/20/06 RPI 378 correct to use first SYSOBJ file dir
    * 07/26/06 RPI 384 fix HFP exact 0 to all zeros
    * 08/03/06 RPI 388 fix to generated duplicate DEF constants
    * 08/14/06 RPI 414 recognize ERR(nnn) limit override 
    * 08/15/06 RPI 415 merge mz390 and az390 for MFC using process_bal()
    * 08/27/06 RPI 411 replace loops with get_dup_string and array fills
    * 09/01/06 RPI 423 add runable thread exception handler
    *          to correctly shut down az390 thread on interal error
    * 09/01/06 RPI 424 catch invalid constant errors
    * 09/01/06 RPI 425 list file xref on PRN
    * 09/06/06 RPI 437 eliminate TEXT option
    * 09/07/06 RPI 431 fix USING and DROP to replace dup reg
    *          and not do drop reg.  Only drop labeled using explicitly.
    * 09/08/06 RPI 440 route all MNOTE's to ERR file
    * 09/08/06 RPI 442 fix loc_ctr reset in DSECT's for EQU * calc
    * 09/13/06 RPI 444 remove MNOTE '..' and *,'..' from ERR file
    * 09/15/06 RPI 448 allow use of EQU/DS/DC processes during lookahead
    * 09/16/06 RPI 450 prevent symbol cross reference truncation
    * 09/17/06 RPI 451 prevent rel symbol update to loc_ctr at end of stmt
    *          for EQU and USING
    * 09/18/06 RPI 457 allow literal references across CSECT's
    * 09/18/06 RPI 459 generate CSECT ESD's first in OBJ.
    * 09/19/06 RPI 454 support TR?? RRE operands R1,R2,M
    * 09/20/06 RPI 453 only route stats to BAL, copyright+rc to con
    * 09/20/06 RPI 458 support explicit off(base) in DC S fields
    * 09/25/06 RPI 463 correct tz390.trim_continue to support
    *          string quotes over 1 or more lines followed by
    *          parms and remove leading blanks from continuations
    * 09/25/06 RPI 465 allow R0 as base for PSA DSECT etc.
    * 10/14/06 RPI 481 add I', S' symbol table support
    * 10/26/06 RPI 485 force completion of last BAL passed
    * 11/04/06 RPI 484 add TRA trace file support for TRACEA, TRACEALL
    * 11/09/06 RPI 488 issue error for label ref. to 'M' lookahead symbol
    * 11/09/06 RPI 489 fix ENTRY ref. to forward ref. symbol
    * 11/11/06 RPI 481 add S' support for mz390 via sym_scale
    * 11/12/06 RPI 493 fix opsyn's S type with no operands
    * 11/12/06 RPI 494 allow any bit length if DS and last field
    * 11/15/06 RPI 417 add full DS/DC bit length support
    * 11/28/06 RPI 500 use system newline vs \r\n for Win/Linux
    * 11/28/06 RPI 501 support literal minus abs expression in operands
    * 11/28/06 RPI 503 suppress duplicate MNOTE's on SYSTERM
    * 12/02/06 RPI 511 add option mcall to put mcall/mexit on PRN
    * 12/04/06 RPI 407 add ED, DD, LD types
    * 01/19/07 RPI 538 fix SS instruction PKA d1(b1) to correctly gen b1
    *          also fix duplication factor for DC P type fields
    * 02/20/07 RPI 553 flag CSECT and DSECT duplicate symbols
    *          and flag duplicate EQU symbols.  
    * 03/01/07 RPI 555 allow DS/DC LQ type as default L type for compat. 
    * 03/05/07 RPI 563 correct computed AGO branch to last label 
    * 03/09/07 RPI 564 correct RLD generation when esd base does not match currect esd  
    * 03/12/07 RPI 574 list all BAL lines in error regardless of PRINT setting 
    * 03/17/07 RPI 577 TR?? 3rd M field optional
    * 03/17/08 RPI 578 Correct mult. DC S(abs d(b) terms)  
    * 04/01/07 RPI 567 add CCW, CCW0, CCW1 support   
    * 04/04/07 RPI 581 print COPY  and inline source in PRN unless PRINT OFF 
    * 04/07/07 RPI 585 gen ADDR2 target address for relative BR? and J?? instr.
    * 04/11/07 RPI 588 fix PRINT to avoid trap on bad parm
    *          RPI 588 issue error for d(,b) if no length or index
    * 04/17/07 RPI 597 error 184 if missing EQU label  
    * 04/26/07 RPI 602 error 195 if negative DS/DC length 
    * 04/27/07 RPI 605 add loc_ctr to TRA, add additional
    *          checks for label, equ, and end address value changes 
    *          change section length error messages to show hex
    * 05/07/07 RPI 606 Fix SSF case 32 to not use llbddd for MVCOS 
    * 05/07/07 RPI 609 compatibility fixes
    *           1.  Error 189 if DC with no date and dup > 0  
    *           2.  Prevent non-labeled using ref to labeld using
    *           3.  Error 190 if comment * after col 1 
    *           4.  Error 191 missing comma before comments for type E
    * 05/07/07 RPI 612 RX off(reg) use X vs B  
    * 05/07/07 RPI 613 fix SS off(len) for low storage move 
    * 05/07/07 RPI 615 correct ATTRA string length for FPR
    * 05/09/07 RPI 617 prevent loop on bad PD digit 
    * 05/15/07 RPI 624 correct EQU ATTRA operand when followed by comment 
    * 05/16/07 RPI 620 gen 47000700 for CNOP  compatiblity 
    * 05/30/07 RPI 629 correct USING to drop prev unlabeled USING for reg.
    * 05/31/07 RPI 626 literal substitution for CICS DFHRESP(type) codes   
    * 06/02/07 RPI 635 fix bug in DFHRESP continued text offset 
    * 06/05/07 RPI 632 show old and new ORG addresses
    *          align each new CSECt to double word 
    *          if loctr force 3 passes to check sect changes 
    *          show DC A/Y/V data address as rel module to
    *          match PRN location counter but leave obj data
    *          as relative CSECT for use by linker 
    * 06/10/07 RPI 637 issue error if missing ) on off(reg,reg) opnd 
    * 06/21/07 RPI 643 correct multiple value DCF's 
    * 07/06/07 RPI 646 synchronize abort_error to prevent other task abort errors
    * 07/07/07 RPI 651 prevent trap on USING with no parms 
    * 07/20/07 RPI 662 add DFHRESP lits ITEMERR,QIDERR
    * 07/20/07 RPI 659 error 196 for invalid opcode char.
    * 07/30/07 RPI 667 issue error 197 for invalid binary value string
    * 08/22/07 RPI 673 support symbolic register on DROP
    * 08/25/07 RPI 687 add CICS VSAM DFHRESP literals
    * 09/03/07 RPI 690 correct NOTEND to NOTFND for =F'13'
    * 09/11/07 RPI 694 add option ERRSUM to summarize critical errors 
    *           1. List missing COPY and MACRO files.
    *           2. List undefined symbols if #1 = 0
    *           3. Total errror counts all reported on ERR, PRN, CON
    *           4. ERRSUM turned on automatically if #1 != 0
    * 10/15/07 RPI 719 support LOG(file) override of log, trace, err files 
    * 10/24/07 RPI 726 only issue error 187 if trace
    * 10/24/07 RPI 728 ignore ISEQ and ICTL instructions
    *          handled by mz390 and reformated to std 1,71,16
    * 10/30/07 RPI 729 add DFHRESP code ILLOGIC=F'21' 
    * 11/12/07 RPI 737 correct handling of F/H constant Enn exponent 
    *          prevent trap on ASCII char > 127 causing trap on cvt to EBCDIC   
    * 11/12/07 RPI 737 add STATS(file) option    
    * 11/27/07 RPI 743 set CNOP label attribute type to 'I'
    *          allow comments without , on PR as on other 12 ops without operands
        * 12/06/07 RPI 751 add DFHRESP(EXPIRED)=F'31'
        * 12/07/07 RPI 749 error for X EQU X and lit mod forward refs.
        * 12/23/07 RPI 769 change abort to log error for invalid ASCII
        * 12/25/07 RPI 755 cleanup msgs to log, sta, tr* 
        * 01/08/08 RPI 776 fix parsing error on USING comments
        * 01/10/08 RPI 777 add decimal point and scale factor for P/Z type
        *          and correct sign in low digit zone for Z.
        * 01/10/08 RPI 778 save loc_ctr in esd_loc[cur_esd] for use
        *          in continued sections following neq ORG's.
        * 01/13/08 RPI 786 set fp_form for preferred exp DFP constants
        * 01/17/08 RPI 790 set DFP exp to explicit decimal point or zero for const.
        *          set scale factor if no explicit modifier
        *          support I' and S' operators in expression 
        * 02/28/08 RPI 812 assemble ASSIST opcodes if ASSIST option on 
        * 03/03/08 RPI 817 assemble all 226 new z10 opcodes 
        * 04/17/08 RPI 834 correct neg 0 fp value    
        * 04/24/08 RPI 840 ignore spaces in P and Z data fields  
        * 04/28/08 RPI 841 add DFHRESP MAPFAIL, INVMPSZ, OVERFLOW  
        * 05/05/08 rpi 846 sync stats with mz390 
        * 05/10/08 RPI 821 switch DH from double to BigDecimal cache 
        * 05/20/08 RPI 851 prevent recursive abort after failing ORG 
        * 06/06/08 RPI 843 round half-even for FP constants  
        * 06/23/08 RPI 866 use get_file_name to parse PRN and BAL file names   
        * 08/05/08 RPI 891 correct MCALL/MEXIT to correctly handle GEN/NOGEN   
        * 08/08/08 RPI 893 add SY, AL2(*), and F/H'Unnn' for unsigned 
        * 08/12/08 RPI 894 change AL2(*) to support 2 byt RLD fields
        * 08/11/08 RPI 895 always print PRN if ERR(0) regardless of ERRSUM    
        * 09/16/08 RPI 908 prevent trap on SYSPRN file overide etc. 
        * 09/19/08 RPI 905 add DFHRESP(DSIDERR)=F'12' for ompatiblity
        * 09/20/08 RPI 917 issue error if START not first CSECT
        * 10/08/08 RPI 930 prevent trap on invalid Z char, leading Z x'F0'
        * 10/24/08 RPI 935 prevent recursive abort
        * 10/27/08 RPI 926 leave macro labels on inline and open code macro statements
        * 10/27/08 RPI 928 add DFHRESP codes: 1) TERMIDERR=F'11' 2) IOERR=F'17' 
        *             3) TRANSIDERR=F'28' 4) ENDDATA=F'29' 5) ENVDEFERR=F'56'
        * 10/29/08 RPI 939 pad DC Z field on left with X'F0'
        * 11/03/08 RPI 945 include mz and az errors on ERRSUM rpt
        * 11/08/08 RPI 941 align start of CNOP to *2
        * 11/09/08 RPI 949 display relative addr for RIL type LARL etc.
        * 11/10/08 RPI 946 correct DIAGNOSE missing last byte caught by INIT F6's
        * 11/14/08 RPI 954 correct erroneous extra parm error on LARL
        * 11/16/08 RPI 960 only count LOCTR in pass 1 for max passes
        * 11/18/08 RPI 951 add DFHRESP(ENQBUSY)=F'55'
        * 12/05/08 RPI 955 correct error message for over 256 on MVC
        * 12/05/08 RPI 959 suppress ERRSUM report if no erros
        * 12/17/08 RPI 978 prevent trap on DS/DC with no operands
        * 01/14/09 RPI 982 use higher base reg for coinciding ranges
        * 02/01/09 RPI 987 fix labelled USING and DROP to handle mixed case
        * 02/03/09 RPI 988 allow relative addressing across CSECT's in same assembly
        * 02/04/09 RPI 991 correct unary sign followed by pfx operator AHI 1,-L'var etc.
        * 02/10/09 RPI 994 support neg base displacements for LA using dependenting USING offsets
        * 02/10/09 RPI 995 set az390_private_sect for mz390 use 
        * 06/06/09 RPI 1033 error if SS length too long
        * 05/19/09 RPI 1034 improve error msg 61, 71, 193
        * 05/20/09 RPI 1031 show literals with errors and fix length
        * 05/27/09 RPI 1044 do not resolve V(con) to relative symbol
        * 06/08/09 RPI 1051 prefix all ERRSUM msgs with ERRSUM
        * 06/14/09 RPI 1057 add DFHRESP(END)=F'83' and add DFHVALUE support
        * 06/15/09 RPI 1047 fix AZ390 ENDED if run standalone
        * 06/15/09 RPI 1050 remove dup START/ENDED for TRACEA CON
        * 06/15/09 RPI 1052 issue error for DROP of explict reg with no USING
        * 06/16/09 RPI 1056 issue warning for dup ordinary USING range
        *          and remove dup dep unlabeled USING range
        * 06/29/09 RPI 1044 do not resolve Vcon to relative symbol 
        * 07/10/09 RPI 1062 use z390_abort exit for recursive abort
        * 07/11/09 RPI 1062 set RC=12 for errors and RC=16 for abort  
        * 08/24/09 RPI 1069 add CODEPAGE(ascii+ebcdic+LIST) option  
        * 09/01/09 RPI 1073 support option NOALIGN  
        * 09/02/09 RPI 1079 add DFHRESP(NOTALLOC)=F'61'
        * 09/26/09 RPI 1080 replace init tables with init_tz390
        * 01/08/10 RPI 1099 correct error on 32 digit B type SDT and display 4 byte hex value
        * 01/09/10 RPI 1101 trunc vs error for out of range or x/0 on DC AFHY
        * 02/16/10 RPI 1108 align LQ to 16 bytes (see RPI 594 pending)
        * 07/28/10 RPI 1127 add option PRINTALL to suppress PRINT OFF/NOGEN
        * 10/08/10 RPI 1125 add LEDBR?, LDXBR?, LEXBR?
        * 10/10/10 RPI 1125 ADD SRNMB
        * 10/20/10 RPI 1125 add FIEBR?, FIDBR?, FIXBR?
        * 11/23/10 RPI 1125 add B394-B39A
        * 12/01/10 RPI 1125 ADD B3D0-B3DB MDTRA-SXTRA
        * 12/03/10 RPI 1125 ADD B928-B92D PCKMO KMOTR 
        * 12/04/10 RPI 1125 ADD B941-B95B CFDTR - CXLFTR, FIX MDTRA DFP/BFP RND 
        * 12/09/10 RPI 1125 ADD B9E2-B9FB LOCGR-SLRK 
        * 12/09/10 RPI 1125 ADD C84-C85 LPD-LPDG 
        * 12/19/10 RPI 1125 ADD EBDC-EBFA SRAK - LAAL
        * 12/21/10 RPI 1125 ADD EC51-ECDB RISBLG - ALGSIK
        * 01/30/11 RPI 1152 CORRECT ECTG AND CSST C81/C82 OPCODE
        * 04/18/11 RPI 1159 force align for each LTORG in csect
        * 05/03/11 RPI 1146 limit check signed I2 byte for RIE4/RIE5
        * 05/17/11 RPI 1164 1) correct RISBHGZ/RISBLGZ support
        *                   2) correct optional length field
        * 07/26/11 RPI 1168 add DFH RESP codes for CICS  
        * 07/25/11 RPI 1169 change az390 error to mz390 warning for missing END 
        * 07/30/11 use tz390.check_java_version 
        * 02/16/12 RPI 1186 NOTHREAD starts CSECT's at 0  
        * 03/04/12 RPI 1196 support 20 bit sdt for LAY etc. 
        * 03/07/12 RPI 1197 support OBJ optional entry on .END TXT
        *          1) last .END entry overrides default 0
        *          2) ENTRY command overrides any .END entry  
        * 03/24/12 RPI 1198 correct misspelled sym_esd1 (was sym_sid1)
        *          and remove remove sym_sid1 and sym_sid2  
        * 04/05/12 RPI 1201 use lit_dup*lit_len to align lits  
        * 04/13/12 RPI 1205 issue error for SDT C'12345' too long 
        * 04/13/12 RPI 1206 drop unlabeled dependant using for drop reg
        * 04/17/12 RPI 1208 don't generate RLD's in DSECT  
        * 04/20/12 RPI 1210 correct handling of periods in paths       
        * 05/15/12 RPI 1209A Report OPTABLE contents if LIST specified on OPTABLE or MACHINE (AFK)
        * 07/20/14 RPI VF01  add support for vector opcodes
        * 07/24/14 RPI 1209B Extend az390 to produce correct report of vector optypes
        * 10/27/14 RPI 1209N Re-implement RR-type instructions and create full regression test
        * 11/03/14 RPI 1209O MR/DR instructions should issue error when operand1 is odd
        * 03/28/15 RPI 1522  Load Logical Immediate instructions with a relocatable argument should issue error
        * 09/11/15 RPI 1523  START with non-zero origin is starts location counter at 0 anyway
        * 2020/09/02 RPI 2202  add all opcodes and mnemonics in latest POP
		* 2020/09/11 RPI 2212 correct vector to support v1-v4 from 0-31 using RXB for high bits
		* 2020/10/18 RPI 2212 add missing mnemonics BI, CLT, CLGT,LOCHI,LOCGHI,LOCHHI,LCOFHR,SOC,STOCG,STOCFH
		* 2020/11/02 RPI 2212 fix NOTRK and NOTR by setting R3=R2
		* 2020/11/12 RPI 2221 add 20 missing instr to az390 and zopcheck
		* 2020/12/03 RPI 2223 update RIL instr. to support immediate 32 bit RLDS for non branch and non relative long instr.
		*            note this is documented in IBM APAR PH30740 dated 2020-11-03 
		* 2020-12-31 RPI 2225 add STCCTM, use get_hex_relative_offset(bits) and get_hex_int(bits), and fix BPRP, BPPm, fix vcp, etc.
		* 2021-02-07 RPI 2226 correct RSYb EB17 STCCTM R1,M3,D2(B2) and fix VNOT not setting v3=v2
        * 2821-04-26 Issue #239 fix no error on undefined sym for RIL i2 operand
        * 2021-08-20 DSH issue #230 correct vector instruction erros reported by Dan Greiner
        * 2021-09-07 dsh #230 fix E7CC option, fix E7C0-E7C7 OR 8 with operand m4 or m6	
        * 2022-01-16 DSH #343 move abort for exceeding maxline 
        * 2022-03-28 DSH #327 fix az390 to force odd literals to even address for access by relative halfword offset counts	
        * 2022-05-07 DSH #233 allow spaces within DC numberic values for BDEFHLPXZ such as DC F'123 456' same as F'123456'
		* 2022-05-10 AFK #398 fix typo in error message
        * 2022-05-12 DSH #325 issue error 54 invalid DC field terminator if not '..' for BDEFHLPXZ
		* 2022-06-10 DSH z16 #423 correct rotate instruction errors reported by Dan Greiner
		* 2022-10-24 jjg #451 z390 ignores CODEPAGE option for input;
		*                     replace non-printable with '.' in PRN, BAL, PCH
        * 2024-05-29 afk #500 List suboption for options optable/machine not implemented correctly
        * 2024-07-03 jjg #509 generate error in process_dcc_data if "DC  C''"
        * 2024-08-09 AFK #543 Correct OPTABLE(XA,LIST) output to match HLASM
        * 2024-08-12 #545 Extend generated java doco to include private methods
        * 2024-08-15 AFK #554 Correct OPTABLE(ESA,LIST) output to match HLASM
        * 2024-08-23 AFK #561 Correct OPTABLE(ZOP,LIST) output to match HLASM
        * 2024-09-09 AFK #568 Correct OPTABLE(YOP,LIST) output to match HLASM
        * 2024-10-13 AFK #573 Correct OPTABLE(Z9,LIST)  output to match HLASM
        * 2025-03-18 AFK #602 Correct OPTABLE(Z10,LIST) output to match HLASM
        * 2025-04-02 AFK #612 Correct OPTABLE(Z11,LIST) output to match HLASM
        * 2025-04-15 AFK #613 Correct OPTABLE(Z12,LIST) output to match HLASM
        * 2025-04-26 AFK #614 Correct OPTABLE(Z13,LIST) output to match HLASM
        * 2025-05-03 AFK #615 Correct OPTABLE(Z14,LIST) output to match HLASM
        * 2025-05-04 AFK #616 Correct OPTABLE(Z15,LIST) output to match HLASM
        * 2025-05-07 AFK #617 Correct OPTABLE(Z16,LIST) output to match HLASM
	*****************************************************
    * Global variables                        last rpi
    *****************************************************/
	tz390 tz390 = null;
	String msg_id = "AZ390I ";
    int az390_rc = 0;
    int az390_errors = 0;
    int mz390_errors = 0; // RPI 415 passed from mz390 if option asm
    String mz390_started_msg = ""; // RPI 755
    boolean mz390_abort = false;
    boolean az390_recursive_abort = false; // RPI 935
    int mz390_rc     = 0; // RPI 415 passed from mz390 if option asm
    int cur_pass = 1;
    Date cur_date = new Date();
    long tod_start = cur_date.getTime();
    long tod_end   = 0;
    long tot_sec = 0;
    boolean stats_to_obj = false;
    int tot_obj_bytes = 0;
    File bal_file = null;
    BufferedReader bal_file_buff = null;
    RandomAccessFile obj_file = null;
    File prn_file = null;
    BufferedWriter prn_file_buff = null;
    String bal_line = null;
    String mnote_warning_msg = ""; // RPI 1056
    int mcall_bal_index = 22;
    int mcall_lv_index  = 16;
    String bal_xref_file_name = null;
    char   bal_xref_file_type = ' ';
    char   cur_line_type  = ' '; // RPI 581
    int    cur_line_file_num = 0;
    String bal_xref_file_path = null;
    int    bal_xref_file_num  = 0;
    int    bal_xref_file_line = 0;
    int    tot_xref_files = 0;
    String[] xref_file_name = null;
    String[] xref_file_path = null; // RPI 425
    int[]    xref_file_errors = null;
    char[]   xref_file_type   = null; // RPI 540 '+' macro '=' copy
    int    mz390_xref_file = 0;
    int    mz390_xref_line = 0;
    String bal_label   = null;
    String opsyn_label = null;
    String bal_op = null;
    boolean bal_op_ok = false;
    boolean bal_label_ok = false; // RPI 451
    String bal_parms = null;
    boolean list_use      = false;
    int      mac_inline_level = 0;      // rpi 581
    int      mac_inline_op_macro = 220; // rpi 581
    int      mac_inline_op_mend  = 221; // rpi 581
    int      mac_inline_op_other = 226; // rpi 581

	boolean bal_abort = false;
    int bal_op_index = 0;
    int end_loc = 0; // RPI 605
    boolean report_label_changes = true; // RPI 605
    boolean report_equ_changes = true;   // RPI 605
    boolean bal_eof = false;
    boolean end_found = false;
    String end_entry = "";                // RPI 1197
    int    end_entry_sid = 0;            // RPI 1197
    boolean end_entry_found = false; // RPI 1197
	SimpleDateFormat mmddyy = new SimpleDateFormat("MM/dd/yy");
	SimpleDateFormat hhmmss = new SimpleDateFormat("HH:mm:ss");
    boolean log_tod = true; 
    JTextArea z390_log_text = null;
    /*
     * semaphores used to synchronize mz390 and az390
     */
	Thread  az390_thread = null;    // RPI 415
	boolean az390_running = false;  // RPI 415
    boolean mz390_call = false;     // RPI 415
    boolean lookahead_mode  = false;     // RPI 415
    boolean sym_lock = false;
    String  sym_lock_desc = null;
    final Lock      lock            = new ReentrantLock();
    final Condition lock_condition  = lock.newCondition();
    boolean bal_line_full = false; 
    String pass_bal_line = null;
    String pass_xref_file_name = null;
    char   pass_xref_file_type = ' ';
    int    pass_xref_file_num = 0;
    int    pass_xref_file_line = 0;
    String xref_file_line = null;
    int    xref_bal_index = 0;
    boolean pass_bal_eof = false;
    boolean az390_waiting = false;
    boolean az390_private_sect = false; // RPI 995
    /*
     * static limits
     */
    int sort_index_bias = 100000; // must be > tz390.opt_maxsym and tz390.opt_maxsym
    int sort_index_len  = 6;      // digits in key_index_bias
    int max_exp_stk = 500;
    int max_exp_rld = 500;
    int max_hh = 0x7ffff; // RPI 387
    int min_hh = 0xfff80000; // RPI 387
    int max_text_buff_len = 16;
    long[] max_fh = {((long)(-1) >>> 57),
    		         ((long)(-1) >>> 49),
    		         ((long)(-1) >>> 41),
    		         ((long)(-1) >>> 33),
    		         ((long)(-1) >>> 25),
    		         ((long)(-1) >>> 17),
    		         ((long)(-1) >>> 9),
    		         ((long)(-1) >>> 1),
    		         };
    long[] min_fh = {((long)(-1) << 7),
    		         ((long)(-1) << 15),
    		         ((long)(-1) << 23),
    		         ((long)(-1) << 31),
    		         ((long)(-1) << 39),
    		         ((long)(-1) << 47),
    		         ((long)(-1) << 55),
    		         ((long)(-1) << 63),
    		         };
     /*
     * bal file global variables
     */
    long    tod_time_limit = 0;
    int tot_bal_line = 1;
	int tot_mac_copy = 0;
    int tot_mnote_warning = 0;
    int tot_mnote_errors  = 0;
    int max_mnote_level   = 0;
    String[]  bal_line_text = null; //logical bal line from 1 or more physical lines
    int[]     bal_line_num  = null; //starting generated BAL physical line #
    int[]     bal_line_xref_file_num  = null;
    int[]     bal_line_xref_file_line = null;
    boolean bal_line_gen = true;
    String trace_pfx = null;
    String parm_name = null;
    String parm_value = null;
    int bal_line_index = 1; //current mac line index
    Pattern exp_pattern = null;
    Matcher exp_match   = null;
    Pattern label_pattern = null;
    Matcher label_match   = null;
    Pattern extrn_pattern = null;
    Matcher extrn_match   = null;
    Pattern dcc_sq_pattern = null;  // EBCDIC or ASCII
    Pattern dcc_dq_pattern = null;  //RPI5  C".." ASCII
    Pattern dcc_eq_pattern = null;  //RPI73 C!..! EBCDIC
    Matcher dcc_match   = null;
    /*
     * location counter and ESD tables
     */
    int loc_ctr = 0;
    int loc_start = 0;
    int start_loc = 0; // Value specified on START instruction // RPI 1523
    int loc_len = 0;
	int cur_esd_sid = 0;
    int tot_esd = 0;
    int cur_esd = 0;
    int cur_esd_base = 0;   // RPI 301 first section 
    int first_cst_esd = 0;
    int esd_sdt = 0;
    int esd_cpx_rld = -1;
    int[]     esd_sid  = null;
    int[]     esd_base = null; // RPI 301
    int[]     esd_loc  = null; // RPI 778 current loc within section
    int tot_loc_stmt   = 0;    // RPI 920
    /*
     * using global data
     */
    int cur_use_start = 0;
    int cur_use_end   = 0;
    boolean explicit_drop_reg = false; // RPI 1052
    int[] push_cur_use_start = null;
    int[] push_cur_use_end   = null;
    int cur_use = 0;
    boolean cur_use_depend = false;
    boolean use_eof = false;
    int cur_use_base_esd = 0;
    int cur_use_base_loc = 0;
    int cur_use_base_len = 0;
    int cur_use_reg = -1;
    int cur_use_neg_reg = -1;
    int cur_use_off = 0x80000; // RPI 387 max 20 bit+1
    int cur_use_neg_off = 0xfff00000; // RPI 387 min 20 bit-1
    String cur_use_lab = "";
    String[] use_lab      = null;
    int[]    use_base_esd = null;
    int[]    use_base_loc = null;
    int[]    use_base_len = null;
    int[]    use_reg      = null;
    int[]    use_reg_loc  = null;
    int      use_domain_tot = 0; // rpi 776
    /*
     * push, pop, and, print data
     */
    int using_level = 0;
    int print_level = 0;
    int[]     using_start = null;
    int[]     using_end   = null;
    boolean[] print_on    = null;
    boolean[] print_gen   = null;
    boolean[] print_data  = null;
    boolean  force_list_bal = false; // RPI 285 RPI 891 force msg on PRN
    boolean  list_bal_line  = true; // RPI 891 assume all BAL on PRN
    /*
     * set mac_call_gen on after printing
     * *MCALL LV= 1 and set off after
     * print *MEXIT LV= 1
     */
    boolean  mac_call_gen   = false;
    boolean  mac_call_first = false;
    boolean  mac_call_last  = false;
    /*
     * symbol table global variables
     */
    byte sym_sdt   = 0;  // dec, b'', c'', h''
    byte sym_cst   = 1;  // CSECT )alias REL)
    byte sym_dst   = 2;  // DSECT (alias REL)
    byte sym_ent   = 3;  // ENTRY (alias REL)
    byte sym_ext   = 4;  // EXTRN external link
    byte sym_rel   = 5;  // RX (CST.DST,ENT)_
    byte sym_rld   = 6;  // complex rld exp
    byte sym_lct   = 7;  // loctr (changed to cst/dst). 
    byte sym_wxt   = 8;  // WXTRN weak external link RPI182
    byte sym_und   = 9;  // undefined symbol RPI 694
    int tot_sym = 0;
    int tot_sym_find = 0;
    int tot_sym_comp = 0;
    int cur_sid = 0;
    int prev_sect_sid = 0;
    int prev_sect_esd = 0;
    boolean cur_sym_sect = false; // RPI 553 indicate if sym is sect or not
    boolean sect_change = false;
    byte prev_sect_type = sym_cst;
    String private_csect = "$PRIVATE"; // RPI 995
    String[] sym_type_desc = {
    	"ABS","CST","DST","ENT","EXT","REL","RLD","LCT","WXT","UND"}; //RPI182 RPI 694
    String[]  sym_name         = null;
    int[]     sym_def          = null;
    byte[]    sym_type         = null;
    byte[]    sym_dc_type      = null; // RPI 790
    byte[]    sym_dc_type_sfx  = null; // RPI 790
    byte[]    sym_attr         = null; // RPI 340
    byte[]    sym_attr_elt     = null; // RPI 415 explicit length type attribute
	int[]     sym_scale        = null; // scale factor for int or fp exp
	int[]     sym_attrp        = null; // equ 4th program attribute 4 ebcdic char stored as int
	String[]  sym_attra        = null; // equ 5th assember attribute int RPI 415
    int[]     sym_esd          = null;
    int[]     sym_loc          = null;
    int[]     sym_max_loc      = null;
    int[]     sym_len          = null;
    int[]     sym_sect_type    = null;
    int[]     sym_sect_prev    = null;
    int[]     sym_sect_next    = null;
    TreeSet<Integer>[] sym_xref = null;
    int last_xref_index = 0;
    int last_xref_line  = 0;
    int sym_def_ref       = 0;  // symbol referenced but not defined
    int sym_def_lookahead = -1; // symbol defined during lookahead
	/*
	 * vector registers v1-v4 rxb extended high bits
	 *   get_hex_vreg(int vreg) sets corresponding bits
	 *   get_hex_vreg_rxb appends rxb bits to code and resets vreg_rxb
	 */
	 int vreg_rxb = 0;
    /*
     * ERRSUM critical error data
     */
    int max_missing = 1000;  // rpi 2202 raised from 100 for testins6 debugging
    int tot_missing_copy = 0;
    int tot_missing_macro = 0;
    int tot_missing_sym = 0;
    String missing_copy[] = new String[max_missing];
    String missing_macro[] = new String[max_missing];
    /*
     * DS/DC type and attribute tables
     */
    String dc_type_table    = "ABCDEFHLPSVXYZ";
    String dc_type_explicit = "RBCKKGGKPRVXRZ";
    int[] dc_type_len = {
    		4,  // A
			1,  // B
			1,  // C
			8,  // D
			4,  // E
			4,  // F
			2,  // H
			16, // L
			1,  // P
			2,  // S
			4,  // V
			1,  // X
			2,  // Y
			1   // Z
			};
    int[] dc_type_align = {
    		4,  // A
			0,  // B
			0,  // C
			8,  // D
			4,  // E
			4,  // F
			2,  // H
			8,  // L
			0,  // P
			2,  // S
			4,  // V
			0,  // X
			2,  // Y
			0   // Z
			};
    char[] dc_type_delimiter = {
    		'(',  // A
			'\'', // B
			'\'', // C
			'\'', // D
			'\'', // E
			'\'', // F
			'\'', // H
			'\'', // L
			'\'', // P
			'(',  // S
			'(',  // V
			'\'', // X
			'(',  // Y
			'\''  // Z
			};
    String[] sym_attra_type = {
    		"AR",   // Register - Access
    		"CR",   // CR Register - Control
    		"CR32", // Register - Control 32-bit
    		"CR64", // Register - Control 64-bit
    		"FPR",  // Register - Floating-Point
    		"GR",   // Register - General
    		"GR32", // Register - General 32-bit
    		"GR64"  // Register - General 64-bit
            };
    byte    bal_lab_attr = 0;
    byte    sym_attr_elt_def = 0; // null char
    byte    bal_lab_attr_elt = sym_attr_elt_def; // RPI 415 explicit length attr
    /*
     * literal table for next pool at LTORG or END
     */
    int tot_lit = 0;
    int cur_lit = 0;
    boolean lit_loc_ref = false;
    int cur_lit_pool = 1;
    String[]  lit_name         = null;
    int[]     lit_pool         = null;
    int[]     lit_line         = null;
    int[]     lit_line_loc     = null;
    int[]     lit_esd          = null;
    int[]     lit_loc          = null;
    int[]     lit_len          = null;
    int[]     lit_dup          = null;  // RPI 1200 use dup*len for alignment sizing
    int[]     lit_scale        = null;  // RPI 790
    byte[]    lit_dc_type      = null;  // RPI 790
    byte[]    lit_dc_type_sfx  = null;  // RPI 790
    byte[]    lit_gen          = null;
    int[]     lit_def          = null;
    TreeSet<Integer>[] lit_xref = null;
    
    /*
     * bal operation code data and tables
     */
    String hex_tab   = "0123456789ABCDEF";
    String hex_op    = null;
    String hex_ll    = null;
    String hex_len1  = null;
    String hex_len2  = null;
    String hex_bddd  = null;
    String hex_bddd1 = null;
    boolean get_bdddhh = false; // RPI 387
    String hex_bddd2 = null; // returns bdddhh if get_bdddhh true
    String hex_bddd_loc  = null;
    String hex_bddd1_loc = null;
    String hex_bddd2_loc = null;
    /*
     * expression global variables
     * including polish notation var and op stacks
     */
	String  exp_text  = ""; // expression text
    int     exp_index = 0;  // current starting index
	boolean check_prev_op = true;
	int     exp_val   = 0;
	int     exp_esd   = 0;
	byte    exp_type  = 0;
    byte    exp_attr  = 0;
	int     exp_state = 0;
    int     exp_level = 0;
    String  exp_use_lab = null;
    boolean exp_term = false;
    boolean exp_eot  = false;  // end of text terminator
    String  exp_term_op = "~";
    String  exp_start_op = exp_term_op;
    String  exp_token = null;
    String  exp_op    = " ";
    int     sym_esd1  = 0;
    int     sym_esd2  = 0;
    byte    sym_type1 = 0;
    byte    sym_type2 = 0;
    int     sym_val1  = 0;
    int     sym_val2  = 0;
    String  exp_prev_op = exp_start_op;
    int     exp_sym_index = -1;  // symbol index
    boolean exp_sym_pushed = false;
    boolean exp_sym_last = false; 
    boolean exp_first_sym_len = true; // is this first exp sym len
    boolean exp_equ     = false; // RPI 749
    boolean exp_lit_mod = false; // RPI 749
    int exp_len = 1;    
    int tot_exp_stk_sym = 0;
    int tot_exp_stk_op  = 0;
    int[]     exp_stk_sym_esd  = (int[])Array.newInstance(int.class,max_exp_stk);
    int[]     exp_stk_sym_val  = (int[])Array.newInstance(int.class,max_exp_stk);
    String[]  exp_stk_op   = new String[max_exp_stk];
    int[]     exp_op_class = (int[])Array.newInstance(int.class,256);
    /*
     * define exp actions based on last and
     * next operator class
     *    1 2 3 4 5 6
     *   +-* /( ) ?'~             col = next_op
     *                            row = prev_op
     */ 
          int tot_classes = 6;
          int[] exp_action = {  
          1,3,3,1,3,1,   // 1 +-  prev add/sub
          2,2,3,2,3,2,   // 2 * / prev mpy/div
          3,3,3,4,3,0,   // 3 (   prev open
          0,0,0,0,3,0,   // 4 )   prev close
		  5,5,3,5,3,5,   // 5 ?'  prev pfx oper L' U+/= RPI 313 RPI 991 pfx/pfx=3
          3,3,7,6,3,6,   // 6 ~   prev terminator
		  };
     /* action code routines:
      *   0 error
      *   1 add/sub
      *   2 mpy/div
      *   3 push op
      *   4 POP  op
      *   5 length attribute of symbol or * instr.
      *   6 exit with result of expression
      *   7 check ( for terminator if last_val 
      */
     /*
      * expression relocation definitions RLDS
      */
      int     exp_rld_mod_val = 0;     // RPI 632    
      boolean exp_rld_mod_set = false; // RPI 632
      byte exp_rld_len = 0;  // gen rlds if 3 or 4
      int tot_exp_rld_add = 0;
      int tot_exp_rld_sub = 0;
      int[]     exp_rld_add_esd = (int[])Array.newInstance(int.class,max_exp_rld);
      int[]     exp_rld_sub_esd = (int[])Array.newInstance(int.class,max_exp_rld);
      /*
       * global relocation definitions RLDS
       */
       int tot_rld = 0;
       char rld_add = '+';
       char rld_sub = '-';
       int[]     rld_fld_esd = null;
       int[]     rld_fld_loc = null;
       byte[]    rld_fld_len = null;
       char[]    rld_fld_sgn = null;
       int[]     rld_xrf_esd = null;
  /*
   * object code text buffer variables
   */  
      boolean gen_obj_code = false;
  	  String obj_code = "";
  	  int    list_obj_loc  = 0;
  	  String list_obj_code = "";
      String cur_text_buff = null;
      int cur_text_loc = 0;
      int cur_text_len = 0;
      int cur_text_esd = 0;
  /*
   * binary obj file buffer and layouts
   */
     byte[] bin_byte = new byte[80];
     byte obj_bin_id = 0x02; // first bin obj byte
     byte[] bin_esd_type = {'E','S','D'};
     byte[] bin_txt_type = {'T','X','T'};
     byte[] bin_rld_type = {'R','L','D'};
     byte[] bin_end_type = {'E','N','D'};
  /*
   * DS/DC global variables
   */
      boolean dc_op   = false;  // ds vs dc bal op
      boolean dc_eod  = false;  // ds/dc end of fields
      boolean dc_len_explicit = false;
      boolean dc_scale_explicit = false; // RPI 777
      boolean dc_exp_explicit = false;   // RPI 777
      boolean dc_bit_len  = false;  // RPI 417
      boolean dc_unsigned = false;  // RPI 893
      BigInteger dc_bit_buff  = null;
      byte[]     dc_bit_bytes = null;
      long       dc_bit_value = 0;
      String     dc_bit_hex   = null;
      int     dc_bit_tot      = 0;
      int     dc_bit_byte_len = 0;
      int     dc_bit_fill     = 0;
      int     dcb_len = 0;
      int     dcb_pad = 0;
      String  dcb_bin = null;
      String  dcc_text = null;
      char    dcc_quote = ' ';
      int     dcc_len  = 0;
      int     dcp_len  = 0;
  	  char    dcp_sign;
      int     dcx_len  = 0;
      boolean dc_first_field = true;  // is this first dc field
      boolean dc_lit_ref = false;
      boolean dc_lit_gen = false;
      int     dc_lit_index_start = 0;
      String dc_field = null;
      char   dc_type  = ' '; // ds/ds field type char
      byte    dc_attr_elt = sym_attr_elt_def; // ds/dc explicit length field type char
      boolean dcv_type = false;
      boolean dca_ignore_refs = false;
      char   dc_type_sfx = ' ';
      String fp_text  = null; // RPI 790 fp text for use by DFP
      double fp_log2  = Math.log(2);
	  double fp_log10 = Math.log(10);
      MathContext fp_context = null;
      BigDecimal fp_big_dec1 = BigDecimal.ZERO;
      BigDecimal fp_big_dec2 = BigDecimal.ZERO;
      BigDecimal fp_big_dec3 = BigDecimal.ZERO;
      BigDecimal fp_bd_two  = BigDecimal.valueOf(2);
      byte[] fp_big_byte = null;
      byte[] fp_data_byte = new byte[16];
      ByteBuffer fp_data_buff = ByteBuffer.wrap(fp_data_byte,0,16);
      BigInteger fp_big_int1 = BigInteger.ZERO;
      BigInteger fp_big_int2 = BigInteger.ZERO;
	  BigInteger fp_big_int_one_bits = BigInteger.ONE.shiftLeft(113).subtract(BigInteger.ONE);
	  BigInteger fp_big_int_lx_man_bits = BigInteger.ONE.shiftLeft(112).subtract(BigInteger.ONE); // RPI 821
	  BigInteger fp_big_int_dh_man_bits = BigInteger.ONE.shiftLeft(56).subtract(BigInteger.ONE);  // RPI 821
	  int    fp_int1 = 0;
	  int    fp_round_bit = 0;
      int fp_int_eb_one_bits  = 0xffffff;
      int fp_int_eb_man_bits  = 0x7fffff;
      int fp_int_eh_man_bits  = 0xffffff;
      long   fp_long1 = 0;
      long fp_long_db_one_bits = ((long)(1) << 53) - 1;
      long fp_long_db_man_bits = ((long)(1) << 52) - 1;
      long fp_long_dh_man_bits = ((long)(1) << 56) - 1;
      int    dc_index = 0;
      int    dc_data_start = 0;
      int    dc_dup   = 0;
      int    dc_dup_loc = 0; // rel offset for dup of a/v/s data with loc_ctr
      int    dc_len   = 0;
      int    dc_scale = 0;
      String  dc_digits = "";       // rpi 777 digits in P or Z value
      boolean dc_dec_point = false; // rpi 777 decimal point found in P/Z
      int     dc_dec_scale = 0;     // rpi 777 decimals to right of poind
      int    dc_exp   = 0;
      int    dc_first_len = 0;
      int    dc_first_dup = 9; // RPI 1200
      int    dc_first_loc = 0;
      char   dc_first_type = ' ';  // dc first field type char
      char   dc_first_type_sfx = ' '; // dc first type suffix  RPI 790
      int    dc_first_scale = 0;   // RPI 481
      byte   dc_first_attr_elt = ' '; // dc first explicit length field type char 
      String dc_hex = null;
      byte[]     dc_data_byte = (byte[])Array.newInstance(byte.class,256);
      ByteBuffer dc_data = ByteBuffer.wrap(dc_data_byte,0,256);
      int dc_type_index = 0;
      BigDecimal  dc_bd_val = null;
      BigInteger  dc_bi_val = null;
      byte[]      dc_byte_val = null;
      boolean     dcc_ascii_req = false;
      byte ascii_lf = 0x0a;
      byte ascii_cr = 0x0d;
      byte ascii_period =  (int)'.';
      byte ascii_space = (int) ' ';
      byte ebcdic_period = 0x4B;
      byte ebcdic_space = 0x40;
  /*
   * EXEC CICS DFHRESP(type) literal data substitution per RPI 626
   */
      String[] dfhresp_type = {
    		  "NORMAL)",          // 1 - =F'0'
    		  "ERROR)",           // 2 - =F'1'
    		  "TERMIDERR)",       // 3 - =F'11' RPI 928
    		  "FILENOTFOUND)",    // 4 - =F'12' RPI 687
    		  "DSIDERR)",         // 5 - =F'12' RPI 905
    		  "NOTFND)",          // 6 - =F'13' RPI 687, RPI 690
    		  "DUPREC)",          // 7 - =F'14' RPI 687
    		  "DUPKEY)",          // 8 - =F'15' RPI 687
    		  "INVREQ)",          // 9 - =F'16'
    		  "IOERR)",           //10 - =F'17' RPI 928
    		  "NOSPACE)",         //11 - =F'18' RPI 687
    		  "NOTOPEN)",         //12 - =F'19' RPI 687
    		  "ENDFILE)",         //13 - =F'20' RPI 687
    		  "ILLOGIC)",         //14 - =F'21' RPI 729
    		  "LENGERR)",         //15 - =F'22'   		  
    		  "ITEMERR)",         //16 - =F'26' RPI 662
    		  "PGMIDERR)",        //17 - =F'27'
    		  "TRANSIDERR)",      //18 - =F'28' RPI 928
    		  "ENDDATA)",         //19 - =F'29' RPI 928
    		  "EXPIRED)",         //20 - =F'31' RPI 751
    		  "MAPFAIL)",         //21 - =F'36' RPI 841
    		  "INVMPSZ)",         //22 - =F'38' RPI 841
    		  "OVERFLOW)",        //23 - =F'40' RPI 841
    		  "QIDERR)",          //24 - =F'44' RPI 662
    		  "ENQBUSY)",         //25 - =F'55' RPI 951
    		  "ENVDEFERR)",       //26 - =F'56' RPI 928
    		  "NOTALLOC)",        //27 - =F'61' RPI 1079
    		  "SUPPRESSED)",      //28 - =F'72' RPI 1057
    		  "END)",             //29 - =F'83' RPI 1057
    		  "DISABLED)",        //30 - =F'84' RPI 687
    		  "ACTIVITYERR)",     //31 - =F'109' RPI 1168
    		  "CONTAINERERR)",    //32 - =F'110' RPI 1168
    		  "TOKENERR)",        //33 - =F'112' RPI 1168
    		  "CHANNELERR)",      //34 - =F'122' RPI 1168
    		  };
      String[] dfhresp_lit = {
    		  "=F'0'",           // 1 "NORMAL)" 
    		  "=F'1'",           // 2 "ERROR)" 
    		  "=F'11'",          // 3 "TERMIDERR"     RPI 928
    		  "=F'12'",          // 4 "FILENOTFOUND)" RPI 687
    		  "=F'12'",          // 5 "DSIDERR"       RPI 905
    		  "=F'13'",          // 6 "NOTFND)" RPI 687, RPI 690 
    		  "=F'14'",          // 7 "DUPREC)" RPI 687
    		  "=F'15'",          // 8 "DUPKEY)" RPI 687 
    		  "=F'16'",          // 9 "INVREQ)" 
    		  "=F'17'",          //10 "IOERR"    RPI 928
    		  "=F'18'",          //11 "NOSPACE)" RPI 687 
    		  "=F'19'",          //12 "NOTOPEN)" RPI 687 
    		  "=F'20'",          //13 "ENDFILE)" RPI 687 
    		  "=F'21'",          //14 "ILLOGIC)" RPI 729
    		  "=F'22'",          //15 "LENGERR)" 
    		  "=F'26'",          //16 "ITEMERR)" RPI 662
    		  "=F'27'",          //17 "PGMIDERR)"
    		  "=F'28'",          //18 "TRANSIDERR"  RPI 928
    		  "=F'29'",          //19 "ENDDATA"     RPI 928
    		  "=F'31'",          //20 "EXPIRED)"  RPI 751
    		  "=F'36'",          //21 "MAPFAIL)"  RPI 841
    		  "=F'38'",          //22 "INVMPSZ)"  RPI 841
    		  "=F'40'",          //23 "OVERFLOW)" RPI 841
    		  "=F'44'",          //24 "QIDERR)"   RPI 662
    		  "=F'55'",          //25 "ENQBUSY)"  RPI 951
    		  "=F'56'",          //26 "ENVDEFERR)" RPI 928
    		  "=F'61'",          //27 "NOTALLOC)"  RPI 1079
    		  "=F'72'",          //28 "SUPPRESSED)" RPI 1057
    		  "=F'83'",          //29 "END)"      RPI 1057
    		  "=F'84'",          //30 "DISABLED)" RPI 687
    		  "=F'109'",         //31 "ACTIVITYERR)"  RPI 1168
    		  "=F'110'",         //32 "CONTAINERERR)" RPI 1168
    		  "=F'112'",         //33 "TOKENERR)"     RPI 1168
    		  "=F'122'",         //33 "CHANNELERR)",  RPI 1168
    		  };
  /*
   * EXEC CICS DFHVALUE(type) literal substitution
   */
      String[] dfhvalue_type = {
              "NOTAPPLIC)",       // 1 - =F'1'
              "VSAM)",            // 2 - =F'3'
              "ESDS)",            // 3 - =F'5'
              "KSDS)",            // 4 - =F'6'
              "RRDS)",            // 5 - =F'7'
              "BASE)",            // 6 - =F'10'
              "PATH)",            // 7 - =F'11'
              "FIXED)",           // 8 - =F'12'
              "VARIABLE)",        // 9 - =F'13'
              "OPEN)",            //10 - =F'18'
              "CLOSED)",          //11 - =F'19'
              "ENABLED)",         //12 - =F'23'
              "DISABLED)",        //13 - =F'24'
              "UNENABLED)",       //14 - =F'33'
              "READABLE)",        //15 - =F'35'
              "NOTREADABLE)",     //16 - =F'36'
              "UPDATABLE)",       //17 - =F'37'
              "NOTUPDATABLE)",    //18 - =F'38'
              "BROWSABLE)",       //19 - =F'39'
              "NOTBROWSABLE)",    //20 - =F'40'
              "ADDABLE)",         //21 - =F'41'
              "NOTADDABLE)",      //22 - =F'42'
              "DELETABLE)",       //23 - =F'43'
              "NOTDELETABLE)",    //24 - =F'44'
              "VRRDS)",           //25 - =F'732'
              };
  String[] dfhvalue_lit = {
              "=F'1'",            // 1 "NOTAPPLIC)"
              "=F'3'",            // 2 "VSAM)"
              "=F'5'",            // 3 "ESDS)"
              "=F'6'",            // 4 "KSDS)"
              "=F'7'",            // 5 "RRDS)"
              "=F'10'",           // 6 "BASE)"
              "=F'11'",           // 7 "PATH)"
              "=F'12'",           // 8 "FIXED)"
              "=F'13'",           // 9 "VARIABLE)"
              "=F'18'",           //10 "OPEN)"
              "=F'19'",           //11 "CLOSED)"
              "=F'23'",           //12 "ENABLED)"
              "=F'24'",           //13 "DISABLED)"
              "=F'33'",           //14 "UNENABLED)"
              "=F'35'",           //15 "READABLE)"
              "=F'36'",           //16 "NOTREADABLE)"
              "=F'37'",           //17 "UPDATABLE)"
              "=F'38'",           //18 "NOTUPDATABLE)"
              "=F'39'",           //19 "BROWSABLE)"
              "=F'40'",           //20 "NOTBROWSABLE)"
              "=F'41'",           //21 "ADDABLE)"
              "=F'42'",           //22 "NOTADDABLE)"
              "=F'43'",           //23 "DELETABLE)"
              "=F'44'",           //24 "NOTDELETABLE)"
              "=F'732'",          //25 "VRRDS)"
              };
  /* 
   * end of global az390 class data and start of procs
   */
public static void main(String[] args) {
  /*
   * main is entry when executed from command line
   * Create instance of az390 class and pass
   * parms to az390 like z390 does.
   */
      az390 pgm = new az390();
	  pgm.init_az390(args,null,null,null);
      pgm.process_az390();
}
public void start_az390_thread(String[] args,JTextArea z390_log, RandomAccessFile mz390_systerm_file,RandomAccessFile mz390_stats_file){
	/*
	 * initialize z390 when called from mz390
	 * to receive bal directly and share the
	 * symbol table with mz390.
	 */
	mz390_call = true;
	init_az390(args, null, mz390_systerm_file, mz390_stats_file);
	az390_thread = new Thread(this);
    az390_running = true;
    az390_thread.start();
    set_sym_lock("az390 startup");    // proceed to waiting for bal and lock sym table
    lookahead_mode  = true; // lookahead done during mz390 load_mac
    reset_sym_lock();  // allow use of DS/DC/EQU during lookahead RPI 448
	cur_esd = tz390.opt_maxesd - 1; // lookahead dummy section # for all ds/dc/equ
    cur_esd_sid = tz390.opt_maxsym-1;
    sym_type[tz390.opt_maxsym-1] = sym_cst;
}
public void finish_az390(String[] mac_file_path,int[] mac_file_errors){
	/*
	 * save xref file names and error counts for
	 * cross reference at end of PRN
	 */
	xref_file_path = mac_file_path;
	xref_file_errors = mac_file_errors;
}
public void run() {
	if (az390_thread == Thread.currentThread()){
		if (tz390.opt_trap){ // RPI 423
			try {
				process_az390();
			} catch (Exception e){
				abort_error(158,"internal system exception - " + e.toString());
			}
		} else {
			process_az390();
		}
		lock.lock(); // RPI 415
	   	try {
			az390_running = false;
	   	    lock_condition.signalAll();
	   	} catch (Exception e) {
	   		abort_error(159,"thread ending interruption");
	   	} finally {
	   		lock.unlock();
	   	}
	}
}
private void process_az390(){
   /*
    *  assemble bal source file into
    *  relocatable OBJ file and
    *  generate optional PRN file.
    *
    * Notes;
    *   1.  az390 may be called from:
    *       a. z390 GUI Windows command via main();
    *       b. Windows command prompt via main();
    *       c. mz390 call via process_az390_call();
    *   2.  If called from z390 GUI Windows command, the
    *       console output will be redirected to
    *       to the z390 GUI log.
    *   3.  If called from mz390 via process_az390_call,
    *       az390 process will run on separate
    *       thread and the get_bal_line and
    *       receive_bal_line methods will
    *       synchronize passing bal record
    *       from mz390 to az390 process.
    */
    	if (tz390.opt_trap){
     	   try {
        	    load_bal();
                process_bal();
     	   } catch (Exception e){
     		   abort_error(79,"internal system exception - " + e.toString());
     	   }
     	} else {
        	load_bal();
     		process_bal();
     	}
	    exit_az390();
}
private void init_az390(String[] args, JTextArea log_text,
                        RandomAccessFile systerm,
                        RandomAccessFile stats) {
	/*
	 * 1.  initialize log routing
	 * 2.  set options
	 * 3.  compile regular expression parsers
	 * 4.  open bal and obj buffered I/O files
	 * 5.  Init ascii/ebcdic translation table
	 */
	    if  (log_text != null){
	    	z390_log_text = log_text;
	    }
    	tz390 = new tz390();
    	tz390.init_tz390();  // RPI 1080
    	if (!tz390.check_java_version()){ // RPI 1175
    		abort_error(88,"unknown java version "
    	    + tz390.java_vendor + " " + tz390.java_version);  
    	}
    	tz390.init_options(args,tz390.bal_type);
    	tz390.systerm_file = systerm;  // share the mz390 ERR file
    	tz390.stats_file = stats;  // RPI 737
    	tz390.systerm_prefix = tz390.left_justify(tz390.pgm_name,9) + " AZ390 ";
    	if (!mz390_call){
   			tz390.open_systerm("AZ390");
   		} else {
   			tz390.systerm_start = System.currentTimeMillis();
   			tz390.started_msg = mz390_started_msg;
   		}
   		tz390.init_codepage(tz390.codepage);  // RPI 1069
	    if (!tz390.init_opcode_name_keys()){
	    	abort_error(87,"opcode key search table exceeded");
	    }
        init_arrays();
	    init_push_pop();
		open_files();
		tz390.force_nocon = true;   // RPI 755
		put_log(tz390.started_msg); // RPI 755
		tz390.force_nocon = false;  // RPI 755
		put_copyright();
        compile_patterns();
        tod_time_limit = tz390.max_time_seconds * 1000 + tod_start;
        if (tz390.opt_optable_list.equals("LIST")) // RPI 1209A
           {gen_list_mnemonics();                  // RPI 1209A
            }                                      // RPI 1209A
}
private void init_push_pop(){
	/*
	 * init push/pop using and print
	 */
    using_start[0] = 0;
    using_end[0]   = 0;
    print_on[0] = true;
    print_gen[0] = true;
    print_data[0] = false;
}
@SuppressWarnings("unchecked")
private void init_arrays(){
	/*
	 * initialize arrays using tz390.opt_max???
	 */
	/*
	 * opt_maxcall - maximum call/push/using
	 */
    push_cur_use_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    push_cur_use_end   = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    use_lab      = new String[tz390.opt_maxcall];
    use_base_esd = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    use_base_loc = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    use_base_len = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    use_reg      = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    use_reg_loc  = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    using_start = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    using_end   = (int[])Array.newInstance(int.class,tz390.opt_maxcall);
    print_on   = (boolean[])Array.newInstance(boolean.class,tz390.opt_maxcall);
    print_gen  = (boolean[])Array.newInstance(boolean.class,tz390.opt_maxcall);
    print_data = (boolean[])Array.newInstance(boolean.class,tz390.opt_maxcall);
    xref_file_name   = new String[tz390.opt_maxfile]; 
    xref_file_type   = new char[tz390.opt_maxfile]; // RPI 549 + or =
    xref_file_path   = new String[tz390.opt_maxfile];  // RPI 425
    xref_file_errors = (int[])Array.newInstance(int.class,tz390.opt_maxfile);
    /*
	 * opt_maxesd - maximum sections
	 */
    esd_sid   = (int[])Array.newInstance(int.class,tz390.opt_maxesd);
    esd_base  = (int[])Array.newInstance(int.class,tz390.opt_maxesd); // RPI 301
    esd_loc   = (int[])Array.newInstance(int.class,tz390.opt_maxesd); // RPI 778 cur loc within section for continue
    /*
	 * opt_maxline - maximum BAL loaded in memory
	 */
    bal_line_text = new String[tz390.opt_maxline]; //logical bal line from 1 or more physical lines
    bal_line_num = (int[])Array.newInstance(int.class,tz390.opt_maxline); //starting physical line #
    bal_line_xref_file_num  = (int[])Array.newInstance(int.class,tz390.opt_maxline); //starting physical line #
    bal_line_xref_file_line = (int[])Array.newInstance(int.class,tz390.opt_maxline); //starting physical line #
    /*
	 * opt_maxrld - relocation definitions
	 */
    rld_fld_esd = (int[])Array.newInstance(int.class,tz390.opt_maxrld);
    rld_fld_loc = (int[])Array.newInstance(int.class,tz390.opt_maxrld);
    rld_fld_len = (byte[])Array.newInstance(byte.class,tz390.opt_maxrld);
    rld_fld_sgn = (char[])Array.newInstance(char.class,tz390.opt_maxrld);
    rld_xrf_esd = (int[])Array.newInstance(int.class,tz390.opt_maxrld);
    /*
	 * opt_maxsym - symbols and literals
	 */
    sym_name         = new String[tz390.opt_maxsym];
    sym_def          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_type         = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym);
    sym_dc_type      = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym); // RPI 790
    sym_dc_type_sfx  = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym); // RPI 790
    sym_attr         = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym);
    sym_attr_elt     = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym);
    sym_attrp        = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_scale        = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
	sym_attra        = new String[tz390.opt_maxsym];
    sym_esd          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_loc          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_max_loc      = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_len          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_sect_type    = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_sect_prev    = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_sect_next    = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    sym_xref = (TreeSet<Integer>[])Array.newInstance(TreeSet.class,tz390.opt_maxsym);
    lit_name         = new String[tz390.opt_maxsym];
    lit_pool         = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_line         = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_line_loc     = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_esd          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_loc          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_len          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_dup          = (int[])Array.newInstance(int.class,tz390.opt_maxsym); // RPI 1200    
    lit_scale        = (int[])Array.newInstance(int.class,tz390.opt_maxsym); // RPI 790
    lit_dc_type      = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym); // RPI 790
    lit_dc_type_sfx  = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym); // RPI 790
    lit_gen          = (byte[])Array.newInstance(byte.class,tz390.opt_maxsym);
    lit_def          = (int[])Array.newInstance(int.class,tz390.opt_maxsym);
    lit_xref = (TreeSet<Integer>[])Array.newInstance(TreeSet.class,tz390.opt_maxsym);
}
private void compile_patterns(){
	/* 
	 * compile regular expression parsers
	 */
	/*
     * label_pattern  .lll
     */
    	try {
    	    label_pattern = Pattern.compile(
    			"([a-zA-Z$@#_][a-zA-Z0-9$@#_]*)"   // RPI 253        
			  );
    	} catch (Exception e){
    		  abort_error(1,"label pattern errror - " + e.toString());
    	}
    	/*
         * extrn and entry pattern
         */
        	try {
        	    extrn_pattern = Pattern.compile(
        			"([a-zA-Z$@#_][a-zA-Z0-9$@#_]*)" // RPI 253
        	       +"|([,\\s])" //RPI181
    			  );
        	} catch (Exception e){
        		  abort_error(1,"extrn pattern errror - " + e.toString());
        	}
        /*
         * expression pattern
         *   1. B'01', C'ABC', X'0F' sdts
         *   2. USING label.
         *   3. symbolst  
         *   3. + - * / ( ) L'
         */
        	try {
        	    exp_pattern = Pattern.compile(
      				"([0-9]+([.][0-9]*)*([eE]([\\+]|[\\-])*[0-9]+)*)" // RPI 232 fp/int
     		      + "|([\\s,'\\+\\-\\*\\/\\(\\)=])"        // RPI 159, RPI181
  		    	  + "|([bB]['][0|1]+['])"                  // sdt B'01'
  		    	  +	"|([cC][aAeE]*[']([^']|(['][']))*['])" // ebcdic/ascii mode //RPI 270
  		    	  +	"|([cC][!]([^!]|([!][!]))*[!])"        // ebcdic always
  		    	  +	"|([cC][\"]([^\"]|([\"][\"]))*[\"])"   // ascii  always
	    		  +	"|([xX]['][0-9a-fA-F]+['])"            // sdt X'1F'
        		  + "|([iIlLsS]['])"                       // length op  RPI9 RPI 790 add I',S' ops
        		  +	"|([a-zA-Z$@#_][a-zA-Z0-9$@#_]*[\\.]?)" // labeled using or symbol  RPI 253
        	    );
        	} catch (Exception e){
        		  abort_error(2,"expression pattern errror - " + e.toString());
        	}
            /*
             * define exp_class with operator
             * precedence classes indexed by 
             * expression operator
             */
             exp_op_class['+'] = 1;
             exp_op_class['-'] = 1;
             exp_op_class['*'] = 2;
             exp_op_class['/'] = 2;
             exp_op_class['('] = 3;
             exp_op_class[')'] = 4;
             exp_op_class['I'] = 5; // Integer pfx // RPI 790
             exp_op_class['L'] = 5; // length  pfx
             exp_op_class['S'] = 5; // Scale   pfx // RPI 790
             exp_op_class['U'] = 5; // unary   pfx
             exp_op_class[' '] = 6;
             exp_op_class[','] = 6;
             exp_op_class['~'] = 6;
             /*
              * dcc_sq_pattern for quoted string:
              *   1.  '...''...'
              * */
         	try {
         	    dcc_sq_pattern = Pattern.compile(
         	        "([']['])"
         	      + "|([&][&])" //RPI192
				  + "|(['&])"   //RPI192
       			  + "|([^'&]+)" 
     			  );
         	} catch (Exception e){
         		  abort_error(1,"dcc pattern errror - " + e.toString());
         	}
            /*
             * dcc_dq_pattern for quoted string:
             *   1.  "...""..."
             * */
        	try {
        	    dcc_dq_pattern = Pattern.compile(
        	        "([\"][\"])"
            	  + "|([']['])"	//RPI192
         	      + "|([&][&])" //RPI192
				  + "|([\"'&])" //RPI192
      			  + "|([^\"]+)" 
    			  );
        	} catch (Exception e){
        		  abort_error(1,"dcc pattern errror - " + e.toString());
        	}
            /*
             * dcc_eq_pattern for quoted string:
             *   1.  !...!!...!
             * */
        	try {
        	    dcc_eq_pattern = Pattern.compile(
        	        "([!][!])"
                  + "|([']['])"	//RPI192
         	      + "|([&][&])" //RPI192
				  + "|([!'&])"  //RPI192
      			  + "|([^!]+)" 
    			  );
        	} catch (Exception e){
        		  abort_error(1,"dcc pattern errror - " + e.toString());
        	}

}
private void open_files(){
	/*
	 * 1.  Set trace file name
	 * 2.  Open obj and prn files
	 */
	    if (tz390.trace_file_name == null){  // RPI 719
	    	tz390.trace_file_name = tz390.dir_trc + tz390.pgm_name + tz390.tra_type;
	    } else {
	    	tz390.trace_file_name = tz390.trace_file_name + tz390.tra_type;
	    }
       	if (tz390.opt_obj){  // RPI 694
       		try {
       			obj_file = new RandomAccessFile(tz390.get_first_dir(tz390.dir_obj) + tz390.pgm_name + tz390.obj_type,"rw"); 
       		} catch (Exception e){
       			abort_error(4,"I/O error on obj open - " + e.toString());
       		}
       	}
       	if (tz390.opt_list){
       		String prn_file_name = tz390.get_file_name(tz390.dir_prn,tz390.pgm_name,tz390.prn_type); // RPI 866
            try {
            	prn_file = new File(prn_file_name); // RPI 908 catch null error
       	        prn_file_buff = tz390.getWriterForDefaultCharset(prn_file);
       	    } catch (Exception e){
       		    abort_error(4,"I/O error on prn open - " + e.toString());
       	    }
       	}
}
private void process_bal(){
	/* 
	 * assemble bal source into obj relocatable
	 * object code file                           
	 *   
	 */
	     resolve_symbols();
	     list_bal_line = true; // RPI 891 
	     gen_obj_esds();
	     gen_obj_text();
	     gen_obj_rlds();
	     gen_obj_end();   // RPI 1197
	     if (tz390.opt_list){
	    	list_bal_line = true; // RPI 891  
	     	gen_sym_list();
	     	gen_lit_xref_list(); //RPI198
	     }	     
}
private void resolve_symbols(){
	/*
	 * if errors occurred during loading of bal
	 * repeat symbol update passes until there 
	 * are no errors or minimum error or max
	 * passes are reached.
	 */
	reset_lits();
	tz390.reset_opsyn();
    if  (az390_errors > 0 || sect_change){ // RPI 605 
    	 int prev_az390_errors = az390_errors + 1;
    	 while (cur_pass <= tz390.opt_maxpass + tot_loc_stmt // RPI 920
    	 		&& (sect_change 
    	 			|| (az390_errors > 0 && az390_errors < prev_az390_errors) // RPI 632 repeat until 0 or no change
    	 			|| cur_pass <= 1  // RPI 264, RPI 632 was <=2
    	 			)
    	 		){
    		 report_label_changes = true; // RPI 632
    		 report_equ_changes   = true; // RPI 632
    	 	 prev_az390_errors = az390_errors;
    	 	 az390_errors = 0;
    	 	 az390_rc = 0; // RPI 1062
    	 	 cur_pass++;
    	     update_symbols();
    	     if (tz390.opt_tracea){
    	    	 String pass_msg = "PASS " + cur_pass + "  TOTAL ERRORS " + az390_errors;
    	    	 tz390.put_trace(pass_msg);
    	    	 tz390.put_systerm(pass_msg); // RPI 605
    	     }
    		 reset_lits();
    		 tz390.reset_opsyn();
         }
    }
    az390_errors = 0;  // RPI 920 restore from prev.
    az390_rc = 0; // RPI 1062
    bal_abort = false; // RPI 920 prevent obj ESD error
	cur_pass++;  // incr to last pass
}
private void update_symbols(){
    /*
     * scan bal source and update symbols
     */
         loc_ctr = 0;
         cur_lit_pool = 1;
         cur_esd = 0;
         bal_eof = false;
         end_found = false;
         bal_line_index = 1;
	     while (!bal_eof){
	    	  check_timeout();
		      if  (bal_line_index == tot_bal_line){
	           	  bal_eof = true;
		      } else {
	               bal_line = bal_line_text[bal_line_index];
	               xref_bal_index = bal_line_index;
	               parse_bal_line();
	               bal_op_index = find_bal_op();
	               if (bal_op_index > -1){  // RPI 274
	           	      process_bal_op();    
	               }
			       bal_line_index++;
	          }
	     }
	     if (!end_found){
	    	 process_end();
	     }
}
private void check_timeout(){
	/*
	 * check if timeout expired
	 */
	if (tz390.opt_time){
		cur_date = new Date();
		tod_end = cur_date.getTime();
		if (tod_end > tod_time_limit){
			abort_error(80,"time limit exceeded");
		}
	}
}
private void update_sects(){
	/*
	 * update each section starting address
	 * and max length, and reset current length
	 * and current esd_loc
	 * 
	 * Notes:
	 *   1.  If any section start address or 
	 *       max length changes issue error
	 *       to force additional passes.
	 *   2.  sym_cst CSECT's start at 0 and are
	 *       contiguous within LOCTR's
	 *   3.  Each new CSECT is aligned to *8
	 *   4.  sym_dst DSECT's always start at 0
	 *   5.  Set esd_base to root section
	 *       for cst, dst, and loctors
	 **/
	if (tot_loc_stmt > 0 && cur_pass == 1){ // RPI 632 
		sect_change_error();  // RPI 632 force first 2 passes if LOCTR found
	} else {
		sect_change = false;
	}
	int cst_ctr = 0;
	int index = 1;
	while (index <= tot_esd){
		cur_sid = esd_sid[index];
		if (sym_type[cur_sid] == sym_cst
			&& sym_sect_prev[cur_sid] == 0){
			// new CSECT/RSECT aligned to double word
			if (!tz390.opt_thread){
				cst_ctr = 0; // RPI 1186 NOTHREAD starts all CSECTS at 0
			}
			loc_ctr = (cst_ctr+7)/8*8;  // RPI 632
			esd_loc[index] = loc_ctr; // RPI 778
			if (sym_loc[cur_sid] != loc_ctr){
				sect_change_error();;
				bal_abort = false; // force all change errors
				log_error(91,"csect start change error - " 
						      + sym_name[cur_sid]
							  + " old start=" + tz390.get_hex(sym_loc[cur_sid],6)
							  + " new start=" + tz390.get_hex(loc_ctr,6));
			}
			sym_loc[cur_sid] = loc_ctr;
			if (sym_sect_next[cur_sid] == 0){
			    loc_ctr = (loc_ctr + sym_len[cur_sid]+7)/8*8;
			} else {
				loc_ctr = loc_ctr + sym_len[cur_sid];
			}
			if (sym_max_loc[cur_sid] != loc_ctr
				&& tot_esd > 1){
				sect_change_error();
				bal_abort = false; // force all change errors
				log_error(92,"csect end   change error - " 
						     + sym_name[cur_sid]
							 + " old end =" + tz390.get_hex(sym_max_loc[cur_sid],6)
							 + " new end =" + tz390.get_hex(loc_ctr,6)); 
			}	
			sym_max_loc[cur_sid] = loc_ctr;
			sym_len[cur_sid] = loc_ctr - sym_loc[cur_sid];
			update_loctrs();
			sym_len[cur_sid] = 0;
            cst_ctr = loc_ctr; // save end of CSECT
		} else if (sym_type[cur_sid] == sym_dst              
			       && sym_sect_prev[cur_sid] == 0){
			loc_ctr = 0;
			sym_loc[esd_sid[index]] = loc_ctr;
			esd_loc[sym_esd[cur_sid]] = loc_ctr; // RPI 778
			loc_ctr = loc_ctr + sym_len[cur_sid];
			if (sym_max_loc[cur_sid] != loc_ctr){
				sect_change_error();
				bal_abort = false; // force all change errors
				log_error(93,"dsect end   change error - " 
						     + sym_name[cur_sid]
							 + " old end  =" + tz390.get_hex(sym_max_loc[cur_sid],6)
							 + " new end  =" + tz390.get_hex(loc_ctr,6));
			}
			sym_max_loc[cur_sid] = loc_ctr;
			sym_len[cur_sid] = loc_ctr - sym_loc[cur_sid];
			update_loctrs();
			sym_len[cur_sid] = 0;
		}
		index++;
	}
	loc_ctr = 0;
}
private void sect_change_error(){
	/*
	 * set sect_change 
	 */
	sect_change = true;  // RPI 632
}
private void update_loctrs(){
	/*
	 * update loctr sections with contiguous
	 * starting addresses from CSECT/DSECT
	 * and issue errors if any start address
	 * or length changes and reset length for
	 * next pass.
	 */
	int index = cur_sid;
	while (sym_sect_next[index] > 0){
		index = sym_sect_next[index];
		if (sym_loc[index] != loc_ctr){
			sect_change_error();
			bal_abort = false; // force all change errors
			log_error(94,"loctr section start change error - " 
					   + sym_name[index]
					   + " old start=" + tz390.get_hex(sym_loc[cur_sid],6)
					   + " new start=" + tz390.get_hex(loc_ctr,6)
						);
		}
		sym_loc[index] = loc_ctr;
		esd_loc[sym_esd[index]] = loc_ctr; // RPI 778
        loc_ctr = loc_ctr + sym_len[index];
		if (loc_ctr != sym_max_loc[index]){
			sect_change_error();
			bal_abort = false; // force all change errors
			log_error(95,"loctr section end   change error - " 
					   + sym_name[index]
					   + " old end  =" + tz390.get_hex(sym_max_loc[cur_sid],6)
					   + " new end  =" + tz390.get_hex(loc_ctr,6) 
			           );
		}
		sym_max_loc[index] = loc_ctr;
		sym_len[index] = 0;
	}
}
private void reset_lits(){
	/*
	 * reset lit_gen flags to force reallocation
	 * on each pass
	 */
	if (tot_lit > 0){
		Arrays.fill(lit_gen,0,tot_lit,(byte)0); // RPI 411
	}
}



    /** // #500
     * Lists all mnemonics in the selected opcode table
     *
     * All mnemonics from the current opcode table are converted into report entries
     * The report entries are sorted
     * The sorted entries are printed (three on a print line) preceded by a header line
     *
     */
private void gen_list_mnemonics() // Routine added for RPI 1209A
   {int     index;
    String  entry;
    String[] report_entries = null; // See process_opcodes() RPI 1209G
    /* Create Array of strings listing the opcodes supported by current optable */
    /* each entry consists of four subfields: Mnemonic Format HexOP Operands    */     // #500
    /* The logic breaks down into three sections:                               */     // #500
    /* 1. for each instruction select the proper value for each subfield        */     // #500
    /*    internal instructions are suppressed                                  */     // #500
    /* 2. for each instruction format the four subfields into a complete entry  */     // #500
    /* 3. sort the entries, then print three entries per line                   */     // #500
    String my_mnemonic;                                                                // #500
    String my_format;                                                                  // #500
    String my_hexop;                                                                   // #500
    String my_operands;                                                                // #500
    String my_hdr = "Mnemonic Frmt HexOP Operands           ";                         // #500
    String my_spc = "                                       ";                         // #500
    int    mnem_len   =8;                   // length of mnemonic column               // #500
    int    format_len =4;                   // length of format   column               // #500
    int    hexop_len  =4;                   // length of hexop    column               // #500
    int    operand_len=20;                  // length of operands column               // #500
    String my_prn_line;                     // print line being constructed            // #500
    int    my_entry_count;                  // nr of entries on current print line     // #500
    int    short_op;                        // short or full opcode                    // #554
    report_entries = new String[tz390.op_name.length];
    index=0;
    while (index < tz390.op_name.length)
       {if (tz390.op_type[index] >= 100)                                               // #500
           {my_mnemonic=tz390.op_name[index];                                          // #500
            my_format="HLASM";                                                         // #500
            my_hexop="";                                                               // #500
            my_operands="";                                                            // #500
            my_mnemonic=(my_mnemonic+my_spc).substring(0,mnem_len   );                 // #500
            entry=my_mnemonic+" "+my_format+" "+my_hexop+" "+my_operands;              // #500
            entry=(entry+my_spc).substring(0,mnem_len+format_len+hexop_len+operand_len+3); // #500
            }
        else   // types 0-99 are for normal instructions                               // #500
           {my_mnemonic=tz390.op_name[index];                                          // #500
            my_format="";                                                              // #500
            my_hexop=tz390.op_code[index];                                             // #500
            my_operands="";                                                            // #500
            switch (tz390.op_type[index])                                              // #500
               {case 0: // comment lines
                    break;
                case 1:
                    my_format="E";                                                     // #500
                    break;
                case 2:
                    my_format="RR";                                                    // #500
                    my_operands="R1,R2";                                               // #500
                    if (my_mnemonic.equals("SPM"))                                     // #500
                       {my_operands="R1";                                              // #500
                        }
                    else if (tz390.op_trace_type[index]==30)
                       {my_operands="R2";                                              // #500
                        my_hexop=my_hexop+".";                                         // #500
                        }
                    break;
                case 3:
                    my_format="RR";                                                    // #500
                    my_operands="M1,R2";                                               // #500
                    break;
                case 4:
                    my_format="RR";                                                    // #500
                    my_operands="I1";                                                  // #500
                    break;
                case 5:
                    my_format="RX";                                                    // #500
                    if (my_mnemonic.equals("BC"))                                      // #500
                       {my_operands="M1,D2(X2,B2)";                                    // #500
                        }
                    else
                       {my_operands="R1,D2(X2,B2)";                                    // #500
                        }
                    break;
                case 6:
                    my_format="RX";                                                    // #500
                    my_hexop=my_hexop+".";                                             // #500
                    my_operands="D2(X2,B2)";                                           // #500
                    break;
                case 7:
                    my_format="S";                                                     // #500
                    if (my_mnemonic.equals("IPK")                                      // #500
                    ||  my_mnemonic.equals("PTLB")                                     // #500 #543
                    ||  my_mnemonic.equals("CSCH")                                     // #543
                    ||  my_mnemonic.equals("HSCH")                                     // #543
                    ||  my_mnemonic.equals("RCHP")                                     // #543
                    ||  my_mnemonic.equals("RSCH")                                     // #543
                    ||  my_mnemonic.equals("SAL")                                      // #543
                    ||  my_mnemonic.equals("SCHM")                                     // #543 #554
                    ||  my_mnemonic.equals("TEND")                                     // #613
                    ||  my_mnemonic.equals("XSCH")                                     // #554
                        )                                                              // #554
                       {my_operands="";                                                // #500
                        }                                                              // #500
                    else if (my_mnemonic.equals("LPSW")                                // #500
                         ||  my_mnemonic.equals("SSM")                                 // #500
                         ||  my_mnemonic.equals("TS"))                                 // #500
                       {my_format="SI";                                                // #500
                        my_hexop=my_hexop.substring(0,2);                              // #500
                        my_operands="D1(B1)";                                          // #500
                        }                                                              // #500
                    else                                                               // #500
                       {my_operands="D2(B2)";                                          // #500
                        }
                    break;
                case 8: // Diagnose instruction
                    my_format="DM";                                                    // #500
                    break;
                case 9:
                    my_format="RSI";                                                   // #500
                    my_operands="R1,R3,I2";                                            // #500
                    break;
                case 10:
                    my_format="RS";                                                    // #500
                    my_operands="R1,R3,I2";                                            // #500
                    if (tz390.op_trace_type[index]==101)
                       {my_operands="R1,M3,D2(B2)";                                    // #500
                        }
                    else if (tz390.op_trace_type[index]==102)
                       {my_operands="R1,D2(B2)";                                       // #500
                        }
                    else
                       {my_operands="R1,R3,D2(B2)";                                    // #500
                        }
                    break;
                case 11:
                    my_format="SI";                                                    // #500
                    my_operands="D1(B1),I2";                                           // #500
                    if (my_mnemonic.equals("LPSW"))                                    // #500
                       {my_operands="D1(B1)";                                          // #500
                        }                                                              // #500
                    break;
                case 12:
                    my_format="RI";                                                    // #500
                    my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2);        // #500
                    my_operands="R1,I2";                                               // #554
                    break;
                case 13:
                    // Our op_code table has the last two nibbles swapped
                    my_format="RI";                                                    // #500
                    if (my_hexop.length() == 3) // mask excluded?                      // #554
                       {my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2,3);  // #554
                        short_op = 1;           // mark short opcode                   // #554
                        }                                                              // #554
                    else                        // mask included!                      // #554
                       {my_hexop=my_hexop.substring(0,2)+my_hexop.substring(3)+my_hexop.substring(2,3); // #554
                        short_op = 0;           // mark full opcode                    // #554
                        }                                                              // #554
                    if (tz390.op_trace_type[index]==130)                               // #554
                       {if (short_op == 1)      // short opcode                        // #554
                           my_operands="M1,RI2";                                       // #554
                        else                    // full opcode                         // #554
                           my_operands="RI2";   // mask implied by mnemonic            // #554
                        }
                    else                                                               // #554
                       {my_operands="R1,RI2";                                          // #554
                        }                                                              // #554
                    break;
                case 14:
                    my_format="RRE";                                                   // #500
                    my_operands="R1,R2";                                               // #500
                    if (tz390.op_trace_type[index]==140)                               // #500
                       {if (tz390.op_name[index].equals("EPAR")
                        ||  tz390.op_name[index].equals("ESAR")
                        ||  tz390.op_name[index].equals("ETND")                        // #613
                        ||  tz390.op_name[index].equals("IAC")
                        ||  tz390.op_name[index].equals("SSAR")
                        ||  tz390.op_name[index].equals("IPM")
                        ||  tz390.op_name[index].equals("MSTA")
                            )
                           {my_operands="R1";                                          // #500
                            }
                        else if (tz390.op_name[index].equals("PALB"))
                           {my_operands="";                                            // #554
                            }                                                          // #554
                        else if (tz390.op_name[index].equals("IPTE"))
                           {if (tz390.opt_optable_optb_nr >= 10)  // ZS6=Z12 and above // #613
                               {my_format="RRF";                                       // #613
                                my_operands="R1,R2<,R3<,M4>>";                         // #613
                                }                                                      // #613
                            }                                                          // #613
                        else if (tz390.op_name[index].equals("CUTFU")                  // #573
                             ||  tz390.op_name[index].equals("CUUTF")                  // #573
                             ||  tz390.op_name[index].equals("CU12")                   // #573
                             ||  tz390.op_name[index].equals("CU21")                   // #573
                             ||  tz390.op_name[index].equals("SSKE"))                  // #573
                           {if (tz390.opt_optable_optb_nr >= 7) // Z9 and above        // #573
                               {my_format="RRF";                                       // #573
                                my_operands="R1,R2<,M3>";                              // #573
                                }                                                      // #573
                            }                                                          // #573
                        }
                    else if (tz390.op_trace_type[index]==142)
                       {if (tz390.op_name[index].equals("EFPC")
                        ||  tz390.op_name[index].equals("LZDR")
                        ||  tz390.op_name[index].equals("LZER")
                        ||  tz390.op_name[index].equals("LZXR")
                        ||  tz390.op_name[index].equals("SFPC")
                        ||  tz390.op_name[index].equals("SFASR")
                            )
                           {my_operands="R1";                                          // #500
                            }
                        }
                    else if (tz390.op_trace_type[index]==143)                          // #573
                       {if (tz390.op_name[index].equals("TROO")                        // #573
                        ||  tz390.op_name[index].equals("TROT")                        // #573
                        ||  tz390.op_name[index].equals("TRTO")                        // #573
                        ||  tz390.op_name[index].equals("TRTT")                        // #573
                            )                                                          // #573
                           {if (tz390.opt_optable_optb_nr >= 7) // Z9 and above        // #573
                               {my_format="RRF";                                       // #573
                                my_operands="R1,R2<,M3>";                              // #573
                                }                                                      // #573
                            }                                                          // #573
                        }                                                              // #573
                    else if (tz390.op_trace_type[index]==144)
                       {if (tz390.op_name[index].equals("ESEA")
                        ||  tz390.op_name[index].equals("EPAIR")
                        ||  tz390.op_name[index].equals("ESAIR")
                        ||  tz390.op_name[index].equals("SSAIR")
                            )
                           {my_operands="R1";                                          // #500
                            }
                        else if (tz390.op_name[index].equals("LPTEA"))
                           {my_operands="R1,R3,R2,M4";                                 // #500
                            }
                        else if (tz390.op_name[index].equals("PCC")
                             ||  tz390.op_name[index].equals("PCKMO")
                             ||  tz390.op_name[index].equals("NNPA")                   // #617
                                 )
                           {my_operands="";                                            // #500
                            }                                                          // #500
                        else if (tz390.op_name[index].equals("CU14")                   // #573
                             ||  tz390.op_name[index].equals("CU24"))                  // #573
                           {if (tz390.opt_optable_optb_nr >= 7) // Z9 and above        // #573
                               {my_format="RRF";                                       // #573
                                my_operands="R1,R2<,M3>";                              // #573
                                }                                                      // #573
                            }                                                          // #573
                        }
                    else if (tz390.op_trace_type[index]==147)
                       {my_operands="R1";                                              // #500
                        }
                    break;
                case 15:
                    my_format="RRD";                                                   // #500 #554
                    my_operands="R1,R3,R2";                                            // #500
                    break;
                case 16:
                    my_format="RIL";                                                   // #500
                    if (my_hexop.length() == 3) // mask excluded?                      // #554
                       {my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2,3);  // #554
                        short_op = 1;           // mark short opcode                   // #554
                        }                                                              // #554
                    else                        // mask included!                      // #554
                       {short_op = 0;           // mark full opcode                    // #554
                        }                                                              // #554
                    if (tz390.op_trace_type[index]==330                                // #554 #602
                    ||  tz390.op_trace_type[index]==169)                               // #554 #602
                       {if (short_op == 1)      // short opcode                        // #554
                           my_operands="M1,RI2";                                       // #554
                        else                    // full opcode                         // #554
                           my_operands="RI2";   // mask implied by mnemonic            // #554
                        }                                                              // #554
                    else if (tz390.op_trace_type[index]==160                           // #573
                         ||  tz390.op_trace_type[index]==161)                          // #573
                       {my_operands="R1,I2";                                           // #573
                        }                                                              // #573
                    else if (tz390.op_trace_type[index]==163)                          // #554
                       {my_operands="R1,RI2";                                          // #554
                        }                                                              // #554
                    else                                                               // #554
                       if(tz390.opt_optable_optb_nr >= 4) // ESA and above             // #554
                          my_operands="R1,RI2";                                        // #554
                       else                                                            // #554
                          my_operands="R1,I2";                                         // #500
                    break;
                case 17:
                    my_format="SS";                                                    // #500
                    if (tz390.op_name[index].equals("PKU"))
                       {my_operands="D1(B1),D2(L2,B2)";                                // #500
                        }
                    else if (tz390.op_name[index].equals("MVCK")
                    ||  tz390.op_name[index].equals("MVCP")
                    ||  tz390.op_name[index].equals("MVCS"))
                       {my_operands="D1(R1,B1),D2(B2),R3";                             // #500
                        }
                    else
                       {my_operands="D1(L,B1),D2(B2)";                                 // #500
                        }
                    break;
                case 18:
                    my_format="RXY";                                                   // #500
                    if(   tz390.opt_optable_optb_nr == 4  // ESA or ZOP use RXE, but   //      #568
                       || tz390.opt_optable_optb_nr == 5) // YOP ff. revert to RXY     // #554 #568
                       {my_format="RXE";                                               // #554
                        }                                                              // #554
                    if (tz390.op_trace_type[index]==189)
                       {my_operands="M1,D2(X2,B2)";                                    // #500
                        }
                    else
                       {my_operands="R1,D2(X2,B2)";                                    // #500
                        if (tz390.op_name[index].length() >= 2)                        // #615
                           {if (tz390.op_name[index].substring(0,2).equals("BI"))      // #615
                               {if (tz390.op_name[index].equals("BIC"))                // #615
                                   {my_operands="M1,D2(X2,B2)";                        // #615
                                    }                                                  // #615
                                else                                                   // #615
                                   {my_operands="D2(X2,B2)";                           // #615
                                    }                                                  // #615
                                }                                                      // #615
                            }                                                          // #615
                        }
                    break;
                case 19:
                    my_format="SSE";                                                   // #500
                    my_operands="D1(B1),D2(B2)";                                       // #500
                    break;
                case 20:
                    my_format="RSE";                                                   // #500 #554
                    my_operands="D1(B1),D2(B2)";                                       // #500
                    if(tz390.opt_optable_optb_nr >= 6) // YOP ff. change to RSY        // #568
                       {my_format="RSY";                                               // #568
                        }                                                              // #568
                    if (tz390.op_trace_type[index]==201)                               //      #613
                       {if (tz390.op_name[index].equals("CLGT")                        //      #613
                        ||  tz390.op_name[index].equals("CLMH")                        //      #613
                        ||  tz390.op_name[index].equals("CLT")                         //      #613
                        ||  tz390.op_name[index].equals("ICMH")                        //      #613
                        ||  tz390.op_name[index].equals("STCMH")                       //      #613
                            )                                                          //      #613
                           {my_operands="R1,M3,D2(B2)";                                //      #613
                            }                                                          //      #613
                        else                                                           //      #613
                           {my_operands="R1,D2(B2)";                                   // #500 #613
                            }                                                          //      #613
                        }                                                              //      #613
                    else if (tz390.op_trace_type[index]==202)                          //      #613
                       {my_operands="R1,M3,D2(B2)";                                    // #500
                        }
                    else
                       {my_operands="R1,R3,D2(B2)";                                    // #500 #554
                        }
                    break;
                case 21:
                    my_format="SIY";                                                   // #500
                    if (tz390.op_name[index].equals("LPSWEY"))                         //      #617
                       {my_operands="D1(B1)";                                          //      #617
                        }                                                              //      #617
                    else                                                               //      #617
                       {my_operands="D1(B1),I2";                                       // #500 #617
                        }                                                              //      #617
                    break;
                case 22:
                    my_format="RSL";                                                   // #500
                    my_operands="D1(L1,B1)";                                           // #500
                    if (tz390.op_trace_type[index]==230)                               // #613
                       {my_operands="R1,D2(L2,B2),M3";                                 // #613
                        }                                                              // #613
                    break;
                case 23:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,R3,RI2";                                           // #500 #561
                    if (tz390.op_trace_type[index]==230)                               // #614
                       {if (tz390.op_name[index].equals("LOCGHI")                      // #614
                        ||  tz390.op_name[index].equals("LOCHHI")                      // #614
                        ||  tz390.op_name[index].equals("LOCHI")                       // #614
                            )                                                          // #614
                           {my_operands="R1,I2,M3";                                    // #614
                            }                                                          // #614
                        else if (tz390.op_name[index].length() >= 6)                   // #614
                           {if (tz390.op_name[index].substring(0,6).equals("LOCGHI")   // #614
                            ||  tz390.op_name[index].substring(0,6).equals("LOCHHI")   // #614
                            ||  tz390.op_name[index].substring(0,5).equals("LOCHI")    // #614
                                )                                                      // #614
                               {my_operands="R1,I2";                                   // #614
                                }                                                      // #614
                            }                                                          // #614
                        }                                                              // #614
                    break;
                case 24:
                    my_format="RXE";                                                   // #500
                    my_operands="R1,D2(X2,B2)";                                        // #500
                    if (tz390.op_trace_type[index]==240)                               // #614
                       {if (tz390.op_name[index].equals("LCBB")                        // #614
                            )                                                          // #614
                           {my_operands="R1,D2(X2,B2),M3";                             // #614
                            }                                                          // #614
                        }                                                              // #614
                    break;
                case 25:
                    my_format="RXF";                                                   // #500
                    my_operands="R1,R3,D2(X2,B2)";                                     // #500
                    break;
                case 26:
                    my_format="SS";                                                    // #500
                    my_operands="D1(L1,B1),D2(L2,B2)";                                 // #500
                    break;
                case 27:
                    my_format="SS";                                                    // #500
                    my_operands="R1,D2(B2),R3,D4(B4)";                                 // #500
                    break;
                case 28:
                    my_format="SS";                                                    // #500
                    my_operands="R1,R3,D2(B2),D4(B4)";                                 // #500
                    break;
                case 29:
                    my_format="SS";                                                    // #500
                    my_operands="D1(L1,B1),D2(B2),I3";                                 // #500
                    break;
                case 30:
                    my_format="RRF";                                                   // #500
                    my_operands="R1,R3,R2,M4";                                         // #554
                    if (tz390.op_trace_type[index]==300                                // #617
                    &&  tz390.op_name[index].equals("RDP")                             // #617
                        )                                                              // #617
                       {my_operands="R1,R3,R2<,M4>";                                   // #617
                        }                                                              // #617
                    else if (tz390.op_trace_type[index]==301                           // #573 #612 #617
                    ||  tz390.op_trace_type[index]==303                                //      #612
                    ||  tz390.op_trace_type[index]==304                                //      #612
                    ||  tz390.op_trace_type[index]==305                                //      #612
                    ||  tz390.op_trace_type[index]==306                                //      #612
                    ||  tz390.op_trace_type[index]==342                                //      #612
                        )                                                              //      #612
                       {my_operands="R1,M3,R2,M4";                                     // #573
                        }                                                              // #573
                    break;
                case 31:
                    my_format="SS";                                                    // #500
                    my_operands="D1(B1),D2(L2,B2)";                                    // #500
                    break;
                case 32:
                    my_format="SSF";                                                   // #500
                    my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2);        // #500
                    my_operands="D1(B1),D2(B2),R3";                                    // #500
                    break;
                case 33:
                    // Our op_code table has the last two nibbles swapped
                    my_format="RIL";                                                   // #500
                    if (my_hexop.length() == 3) // mask excluded?                      // #561
                       {my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2,3);  // #561
                        short_op = 1;           // mark short opcode                   // #561
                        }                                                              // #561
                    else                        // mask included!                      // #561
                       {my_hexop=my_hexop.substring(0,2)+my_hexop.substring(3)+my_hexop.substring(2,3); // #500 #561
                        short_op = 0;           // mark full opcode                    // #561
                        }                                                              // #561
                    if (short_op == 1)          // short opcode                        // #561
                       my_operands="M1,RI2";                                           // #561
                    else                        // full opcode                         // #561
                       my_operands="RI2";       // mask implied by mnemonic            // #561
                    break;
                case 34:
                    my_format="RRF";                                                   // #500
                    my_operands="R1,M3,R2";                                            // #500
                    if (tz390.op_trace_type[index]==340)
                       {if (tz390.op_name[index].equals("IDTE")
                        ||  tz390.op_name[index].equals("CPSDR")
                            )
                           {my_operands="R1,R3,R2";                                    // #500
                            }
                        }
                    else if (tz390.op_trace_type[index]==343)
                       {my_operands="R1,R3,R2";                                        // #500
                        }
                    break;
                case 35:
                    my_format="RRF";                                                   // #500
                    my_operands="R1,R2,M4";                                            // #500
                    break;
                case 36:
                    // Alternate formats have mnemonics ending in 'A'
                    if (my_mnemonic.substring(my_mnemonic.length()-1).equals("A"))     // #500
                       {my_format="RRF";                                               // #500 #573
                        my_operands="R1,R2,R3,M4";                                     // #500
                        }
                    else
                       {my_format="RRF";                                               // #500 #573
                        my_operands="R1,R2,R3";                                        // #500
                        }
                    break;
                case 37: // ASSIST instructions
                    my_format="RX";                                                    // #500
                    my_operands="R1,S2";                                               // #500
                    break;
                case 38: // ASSIST instructions
                    my_format="RXSS";                                                  // #500
                    my_operands="S1(X1),S2";                                           // #500
                    break;
                case 39:
                    if (tz390.op_trace_type[index]==140)
                       {my_format="RRE";                                               // #500
                        my_operands="R1,R2";                                           // #500
                        }
                    else if (tz390.op_trace_type[index]==141)                          // #612
                       {if (tz390.op_name[index].equals("LOCGR"))                      // #612
                           {my_format="RRF";                                           // #612
                            my_operands="R1,R2,M3";                                    // #612
                            }                                                          // #612
                        else                                                           // #612
                           {my_format="RRF";                                           // #612
                            my_operands="R1,R2";                                       // #612
                            }                                                          // #612
                        }                                                              // #612
                    else if (tz390.op_trace_type[index]==142)                          // #612
                       {if (tz390.op_name[index].equals("LOCR"))                       // #612
                           {my_format="RRF";                                           // #612
                            my_operands="R1,R2,M3";                                    // #612
                            }                                                          // #612
                        else                                                           // #612
                           {my_format="RRF";                                           // #612
                            my_operands="R1,R2";                                       // #612
                            }                                                          // #612
                        }                                                              // #612
                    else if (tz390.op_trace_type[index]==144)
                       {if (tz390.op_name[index].equals("CHLR"))
                           {my_format="RRE";                                           // #500
                            my_operands="R1,R2";                                       // #500
                            }
                        else
                           {my_format="RRF";                                           // #500
                            my_operands="R1,R2<,M3>";                                  // #500
                            }
                        }
                    else if (tz390.op_trace_type[index]==151)                          // #612
                       {if (tz390.op_name[index].equals("POPCNT"))                     // #612
                           {// Introduced with ZS5=Z11; changed in ZS9=Z15             // #616
                            if (tz390.opt_optable_optb_nr < 13) // 13=ZS9/Z15          // #616
                               {my_format="RRE";          // z11 thru z14              // #612 #616
                                my_operands="R1,R2";                                   // #612
                                }                                                      // #612
                            else                                                       // #616
                               {my_format="RRF";          // z15 and higher            // #616
                                my_operands="R1,R2<,M3>";                              // #616
                                }                                                      // #616
                            }                                                          // #616
                        else                                                           // #612
                           {my_format="RRF";                                           // #612
                            my_operands="R1,R2,M3";                                    // #612
                            }                                                          // #612
                        }                                                              // #612
                    else if (tz390.op_trace_type[index]==153)                          // #612 #614
                       {if (tz390.op_name[index].equals("LOCFHR"))                     // #612 #614
                           {my_format="RRF";                                           // #612 #614
                            my_operands="R1,R2,M3";                                    // #612 #614
                            }                                                          // #612 #614
                        else if (tz390.op_name[index].equals("NOTGR"))                 //      #616
                           {my_format="RRF";                                           //      #616
                            my_operands="R1,R2";                                       //      #616
                            }                                                          //      #616
                        else if (tz390.op_name[index].length() >= 6                    //      #614
                             &&  tz390.op_name[index].substring(0,6).equals("LOCFHR")  //      #614
                                 )                                                     //      #614
                           {my_format="RRF";                                           //      #614
                            my_operands="R1,R2";                                       //      #614
                            }                                                          //      #614
                        else                                                           // #612 #614
                           {my_format="RRF";                                           // #612 #614
                            my_operands="R1,R2,R3";                                    // #612 #614
                            }                                                          // #612 #614
                        }                                                              // #612 #614
                    else if (tz390.op_trace_type[index]==154                           // #612 #614
                         ||  tz390.op_trace_type[index]==410                           // #612
                             )
                       {my_format="RRF";                                               // #612
                        my_operands="R1,R2,R3";                                        // #612
                        if (tz390.op_name[index].equals("NOTR"))                       //      #616
                           {my_format="RRF";                                           //      #616
                            my_operands="R1,R2";                                       //      #616
                            }                                                          //      #616
                        }
                    else
                       {my_format="RRF";                                               // #500
                        my_operands="R1,R2,M3";                                        // #500
                        }
                    break;
                case 40:
                    my_format="RRF";                                                   // #500
                    my_operands="R1,R2";                                               // #500
                    if (tz390.op_trace_type[index]==151)                               // #613
                       {if (tz390.op_name[index].equals("PPA"))                        // #613
                           {my_operands="R1,R2,M3";                                    // #613
                            }                                                          // #613
                        }                                                              // #613
                    break;
                case 41:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,I2,M3";                                            // #500
                    break;
                case 42:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,I2";                                               // #500
                    break;
                case 43:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,I2,M3,RI4";                                        // #500 #602
                    break;
                case 44:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,I2,RI4";                                           // #500 #602
                    break;
                case 45:
                    my_format="RRS";                                                   // #500
                    my_operands="R1,R2,M3,D4(B4)";                                     // #500
                    break;
                case 46:
                    my_format="RRS";                                                   // #500
                    my_operands="R1,R2,D4(B4)";                                        // #500
                    break;
                case 47:
                    my_format="RIS";                                                   // #500
                    my_operands="R1,I2,M3,D4(B4)";                                     // #500
                    break;
                case 48:
                    my_format="RIS";                                                   // #500
                    my_operands="R1,I2,D4(B4)";                                        // #500
                    break;
                case 49:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,R2,M3,RI4";                                        // #500 #602
                    break;
                case 50:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,R2,RI4";                                           // #500 #602
                    break;
                case 51:
                    my_format="SIL";                                                   // #500
                    my_operands="D1(B1),I2";                                           // #500
                    break;
                case 52:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,R2";                                               // #500
                    if (my_hexop.indexOf("$") == -1)                                   // #500
                       {my_operands="R1,R2,I3,I4<,I5>";
                        }
                    else if (tz390.op_name[index].equals("SLLHH")                      // #617
                         ||  tz390.op_name[index].equals("SLLHL")                      // #617
                         ||  tz390.op_name[index].equals("SLLLH")                      // #617
                         ||  tz390.op_name[index].equals("SRLHH")                      // #617
                         ||  tz390.op_name[index].equals("SRLHL")                      // #617
                         ||  tz390.op_name[index].equals("SRLLH")                      // #617
                             )                                                         // #617
                       {my_operands="R1,R2,I3";                                        // #617
                        }                                                              // #617
                    break;
                case 53:
                    // Alternate formats have mnemonics ending in 'A'
                    if (my_mnemonic.substring(my_mnemonic.length()-1).equals("A"))     // #500
                       {my_format="RRF";                                               // #500
                        my_operands="R1,M3,R2,M4";                                     // #500
                        }
                    else
                       {my_format="RRE";                                               // #500
                        my_operands="R1,R2";                                           // #500
                        }
                    break;
                case 54:
                    // Alternate formats have mnemonics ending in 'A'
                    if (my_mnemonic.substring(my_mnemonic.length()-1).equals("A"))     // #500
                       {my_format="RRF";                                               // #500
                        my_operands="R1,M3,R2,M4";                                     // #500
                        }
                    else
                       {my_format="RRF";                                               // #500
                        my_operands="R1,M3,R2";                                        // #500
                        }
                    if (tz390.op_trace_type[index]==340)                               // #568
                       {if (my_mnemonic.equals("IDTE"))                                // #568
                           {if (tz390.opt_optable_optb_nr >= 10)  // ZS6=Z12 and above // #613
                               {my_operands="R1,R3,R2<,M4>";                           // #613
                                }                                                      // #613
                            else                                                       // #613
                               {my_operands="R1,R3,R2";                                // #568 #613
                                }                                                      // #568 #613
                            }                                                          // #613
                        else if (my_mnemonic.equals("KMA"))                            // #615
                           {my_operands="R1,R3,R2";                                    // #615
                            }                                                          // #615
                        }                                                              // #568
                    else if (tz390.op_trace_type[index]==344)                          // #613
                       {my_operands="R1,R3,R2<,M4>";                                   // #613
                        }                                                              // #613
                    break;
                case 55:
                    my_format="SSF";                                                   // #500
                    my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2);        // #500
                    my_operands="R3,D1(B1),D2(B2)";                                    // #500
                    break;
                case 56:
                    my_format="RSY";                                                   // #500
                    my_operands="R1,D2(B2)";                                           // #500 #612 #614
                    if (tz390.op_trace_type[index]==207)                               //           #614
                       {if (tz390.op_name[index].equals("LOCFH")                       //      #612 #614
                        ||  tz390.op_name[index].equals("LOCG")                        //      #612 #614
                        ||  tz390.op_name[index].equals("STOCFH")                      //      #612 #614
                        ||  tz390.op_name[index].equals("STOCG")                       //      #612 #614
                            )                                                          //      #612 #614
                           {my_operands="R1,D2(B2),M3";                                // #500 #612 #614
                            }                                                          //      #612 #614
                        }                                                              //           #614
                    else if (tz390.op_trace_type[index]==209)                          //           #614
                       {if (tz390.op_name[index].equals("LOC")                         //      #612 #614
                        ||  tz390.op_name[index].equals("STOC")                        //      #612 #614
                            )                                                          //      #612 #614
                           {my_operands="R1,D2(B2),M3";                                // #500 #612 #614
                            }                                                          //      #612 #614
                        }                                                              //           #614
                    break;
                case 57:
                    my_format="RIE";                                                   // #500
                    my_operands="R1,R3,I2";                                            // #500
                    break;
                case 58:
                    my_format="QST";                                                   // #500
                    my_operands="VR1,QR3,RS2(RT2)";                                    // #500
                    if (tz390.op_trace_type[index]==581)                               // #533
                       {my_operands="M1,QR3,RS2(RT2)";                                 // #533
                        }                                                              // #533
                    break;
                case 59:
                    my_format="QV";                                                    // #500
                    my_operands="VR1,QR3,VR2";                                         // #500
                    if (tz390.op_trace_type[index]==591)                               // #533
                       {my_operands="M1,QR3,VR2";                                      // #533
                        }                                                              // #533
                    else if (tz390.op_trace_type[index]==592)                          // #533
                       {my_operands="VR1,QR2";                                         // #533
                        }                                                              // #533
                    break;
                case 60:
                    my_format="VST";                                                   // #500
                    my_operands="VR1,VR3,RS2(RT2)";                                    // #500
                    if (tz390.op_trace_type[index]==601)                               // #533
                       {my_operands="VR1,RS2(RT2)";                                    // #533
                        }                                                              // #533
                    else if (tz390.op_trace_type[index]==602)                          // #533
                       {my_operands="M1,VR3,RS2(RT2)";                                 // #533
                        }                                                              // #533
                    break;
                case 61:
                    my_format="VV";                                                    // #500
                    my_operands="VR1,VR2";                                             // #500
                    if (tz390.op_trace_type[index]==611)                               // #533
                       {my_operands="VR1,VR3,VR2";                                     // #533
                        }                                                              // #533
                    else if (tz390.op_trace_type[index]==612)                          // #533
                       {my_operands="M1,VR3,VR2";                                      // #533
                        }                                                              // #533
                    else if (tz390.op_trace_type[index]==613)                          // #533
                       {my_operands="VR1";                                             // #533
                        }                                                              // #533
                    break;
                case 62:
                    my_format="RRE";                                                   // #500
                    my_operands="R1";                                                  // #500
                    if (tz390.op_trace_type[index]==621)                               // #533
                       {my_operands="";                                                // #533
                        }                                                              // #533
                    break;
                case 63:
                    my_format="RSEv";                                                  // #500
                    my_operands="VR1,VR3,D2(B2)";                                      // #500
                    break;
                case 64:
                    my_format="S";                                                     // #500
                    my_operands="D2(B2)";                                              // #500
                    break;
                case 65:
                    my_format="VR";                                                    // #500
                    my_operands="VR1,QR3,R2";                                          // #500
                    if (tz390.op_trace_type[index]==651)                               // #533
                       {my_operands="VR1,QR2";                                         // #533
                        }                                                              // #533
                    if (tz390.op_trace_type[index]==652)                               // #533
                       {my_operands="VR1";                                             // #533
                        }                                                              // #533
                    break;
                case 66:
                    my_format="VS";                                                    // #500
                    my_operands="RS2";                                                 // #500
                    break;
                case 67:
                    my_format="RR";                                                    // #500
                    my_operands="R1,R2";                                               // #500
                    break;
                case 68:
                    my_format="RR";                                                    // #500
                    my_operands="R1,R2";                                               // #500
                    break;
                case 69:
                    my_format="RR";                                                    // #500
                    my_operands="M1,R2";                                               // #500
                    break;
                case 70:
                    my_format="R";                                                     // #500
                    my_operands="R1";                                                  // #500
                    if (my_mnemonic.equals("SPM"))                                     // #500
                       {my_format="RR";                                                // #500
                        }                                                              // #500
                    break;    
                case 71:
                    my_format="RR";                                                    // #500
                    my_operands="R1,R2";                                               // #500
                    break;
                case 72:
                    my_format="RR";                                                    // #500
                    my_hexop=my_hexop+".";                                             // #500
                    my_operands="R2";                                                  // #500
                    break;
                case 73:
                    my_format="RI";                                                    // #500
                    if(tz390.opt_optable_optb_nr >= 4)  // ESA and above               // #554
                       {my_hexop=my_hexop.substring(0,2)+"."+my_hexop.substring(2,3);  // #554
                        }                                                              // #554
                    my_operands="R1,I2";                                               // #500
                    break;    
                case 74:
                    my_format="RRF";                                                   // #500 #616
                    if (my_mnemonic.equals("SELFHR")                                   // #500 #616
                    ||  my_mnemonic.equals("SELGR")                                    //      #616
                    ||  my_mnemonic.equals("SELR")                                     //      #616
                        )                                                              //      #616
                       {my_operands="R1,R2,R3,M4";                                     // #500
                        }                                                              // #500
                    else                                                               //      #616
                       {my_operands="R1,R2,R3";                                        // #500 #616
                        }                                                              // #500
                    break;    
                case 75:                                                               // #613
                    my_format="IE";                                                    // #613
                    my_operands="I1,I2";                                               // #613
                    break;                                                             // #613
                case 76:                                                               // #613
                    my_format="MII";                                                   // #613
                    my_operands="M1,RI2,RI3";                                          // #613
                    break;                                                             // #613
                case 77:                                                               // #613
                    my_format="SMI";                                                   // #613
                    my_operands="M1,RI2,D3(B3)";                                       // #613
                    break;                                                             // #613
                case 78:                                                               // #614
                    my_format="VRX";                                                   // #614
                    switch (tz390.op_trace_type[index])                                // #614
                       {case 734:                                                      // #614
                             my_operands="V1,D2(X2,B2),M3";                            // #614
                             break;                                                    // #614
                        case 739:                                                      // #614
                             my_operands="V1,D2(X2,B2)<,M3>";                          // #614
                             break;                                                    // #614
                        case 740:                                                      // #614
                             my_operands="V1,D2(X2,B2)";                               // #614
                             break;                                                    // #614
                        case 741:                                                      // #614
                             my_operands="V1,R3,D2(B2)";                               // #614
                             break;                                                    // #614
                        default:                                                       // #614
                             my_operands="*Unknown";                                   // #614
                        }                                                              // #614
                    break;                                                             // #614
                case 79:                                                               // #615
                    my_format="VSI";                                                   // #615
                    my_operands="V1,D2(B2),I3";                                        // #615
                    break;                                                             // #615
                case 80:                                                               // #614
                    my_format="VRS";                                                   // #614
                    switch (tz390.op_trace_type[index])                                // #614
                       {case 736:                                                      // #614
                             my_operands="V1,V3,D2(B2),M4";                            // #614
                             break;                                                    // #614
                        case 742:                                                      // #614
                             my_operands="V1,V3,D2(B2)<,M4>";                          // #614
                             break;                                                    // #614
                        case 743:                                                      // #614
                             my_operands="V1,V3,D2(B2)";                               // #614
                             break;                                                    // #614
                        case 744:                                                      // #614
                             my_operands="R1,V3,D2(B2),M4";                            // #614
                             break;                                                    // #614
                        case 745:                                                      // #614
                             my_operands="R1,V3,D2(B2)";                               // #614
                             break;                                                    // #614
                        case 746:                                                      // #614
                             my_operands="V1,R3,D2(B2),M4";                            // #614
                             break;                                                    // #614
                        case 747:                                                      // #614
                             my_operands="V1,R3,D2(B2)";                               // #614
                             break;                                                    // #614
                        default:                                                       // #614
                             my_operands="*Unknown";                                   // #614
                        }                                                              // #614
                    break;                                                             // #614
                case 81:                                                               // #614
                    my_format="VRI";                                                   // #614
                    switch (tz390.op_trace_type[index])                                // #614
                       {case 737:                                                      // #614
                             my_operands="V1,V2,V3,I4,M5";                             // #614
                             break;                                                    // #614
                        case 748:                                                      // #614
                             my_operands="V1,V2,V3,I4";                                // #614
                             break;                                                    // #614
                        case 749:                                                      // #614
                             my_operands="V1,V2,I3,M4,M5";                             // #614
                             break;                                                    // #614
                        case 750:                                                      // #614
                             my_operands="V1,V2,I3";                                   // #614
                             break;                                                    // #614
                        case 751:                                                      // #614
                             my_operands="V1,I2,M3";                                   // #614
                             break;                                                    // #614
                        case 752:                                                      // #614
                             my_operands="V1,I2";                                      // #614
                             break;                                                    // #614
                        case 753:                                                      // #614
                             my_operands="V1,V3,I2,M4";                                // #614
                             break;                                                    // #614
                        case 754:                                                      // #614
                             my_operands="V1,V3,I2";                                   // #614
                             break;                                                    // #614
                        case 755:                                                      // #614
                             my_operands="V1,I2,I3,M4";                                // #614
                             break;                                                    // #614
                        case 756:                                                      // #614
                             my_operands="V1,I2,I3";                                   // #614
                             break;                                                    // #614
                        case 757:                                                      // #614
                             my_operands="V1";                                         // #614
                             break;                                                    // #614
                        case 780:                                                      // #615
                             my_operands="V1,R2,I3,M4";                                // #615
                             break;                                                    // #615
                        case 783:                                                      // #615
                             my_operands="V1,V2,I3,I4,M5";                             // #615
                             break;                                                    // #615
                        default:                                                       // #614
                             my_operands="*Unknown";                                   // #614
                        }                                                              // #614
                    break;                                                             // #614
                case 82:                                                               // #614
                    my_format="VRR";                                                   // #614
                    switch (tz390.op_trace_type[index])                                // #614
                       {case 738:                                                      // #614
                             my_operands="V1,V2,V3";                                   // #614
                             break;                                                    // #614
                        case 758:                                                      // #614
                             my_operands="V1,V2,V3,M4";                                // #614
                             break;                                                    // #614
                        case 759:                                                      // #614
                             my_operands="V1,V2,V3,M4,M5";                             // #614
                             break;                                                    // #614
                        case 760:                                                      // #614
                             my_operands="V1,V2,V3,M4<,M5>";                           // #614
                             break;                                                    // #614
                        case 761:                                                      // #614
                             my_operands="V1,V2,V3,M4<,M5>";                           // #614
                             break;                                                    // #614
                        case 762:                                                      // #614
                             my_operands="V1,V2,V3,V4,M5";                             // #614
                             break;                                                    // #614
                        case 763:                                                      // #614
                             my_operands="V1,V2,V3,V4,M5,M6";                          // #614
                             break;                                                    // #614
                        case 764:                                                      // #614
                             my_operands="V1,V2,V3,V4,M5<,M6>";                        // #614
                             break;                                                    // #614
                        case 765:                                                      // #614
                             my_operands="V1,V2,V3,M4,M5";                             // #614
                             break;                                                    // #614
                        case 766:                                                      // #614
                             my_operands="V1,V2,V3,M4,M5,M6";                          // #614
                             break;                                                    // #614
                        case 767:                                                      // #614
                             my_operands="V1,V2,M3";                                   // #614
                             break;                                                    // #614
                        case 768:                                                      // #614
                             my_operands="V1,V2,M3,M4";                                // #614
                             break;                                                    // #614
                        case 769:                                                      // #614
                             my_operands="V1,V2,M3,M4,M5";                             // #614
                             break;                                                    // #614
                        case 770:                                                      // #614
                             my_operands="V1,V2,M5";                                   // #614
                             break;                                                    // #614
                        case 771:                                                      // #614
                             my_operands="V1,V2,V3,V4";                                // #614
                             break;                                                    // #614
                        case 772:                                                      // #614
                             my_operands="V1,V2";                                      // #614
                             break;                                                    // #614
                        case 773:                                                      // #614
                             my_operands="V1,R2,R3";                                   // #614
                             break;                                                    // #614
                        case 774:                                                      // #614
                             my_operands="V1,V2,M4,M5";                                // #614
                             break;                                                    // #614
                        case 775:                                                      // #614
                             my_operands="V1,V2,V3<,M5>";                              // #614
                             break;                                                    // #614
                        case 776:                                                      // #614
                             my_operands="V1,V2<,M5>";                                 // #614
                             break;                                                    // #614
                        case 777:                                                      // #614
                             my_operands="V1,V2,V3,V4<,M6>";                           // #614
                             break;                                                    // #614
                        case 778:                                                      // #614
                             my_operands="V1,V2,M3<,M5>";                              // #614
                             break;                                                    // #614
                        case 779:                                                      // #615
                             my_operands="R1,V2,M3";                                   // #615
                             break;                                                    // #615
                        case 781:                                                      // #615
                             my_operands="V1,V2,V3,M6";                                // #615
                             break;                                                    // #615
                        case 782:                                                      // #615
                             my_operands="V1,V2,V3,V4,M6";                             // #615
                             break;                                                    // #615
                        case 784:                                                      // #615
                             my_operands="V1";                                         // #615
                             break;                                                    // #615
                        case 785:                                                      // #615
                             my_operands="R1,V2,M3<,M4>";                              // #615
                             break;                                                    // #615
                        case 786:                                                      // #617
                             my_operands="V1,V2,V3,M5";                                // #617
                             break;                                                    // #617
                        default:                                                       // #614
                             my_operands="*Unknown";                                   // #614
                        }                                                              // #614
                    break;                                                             // #614
                case 83:                                                               // #614
                    my_format="VRV";                                                   // #614
                    my_operands="V1,D2(V2,B2),M3";                                     // #614
                    break;                                                             // #614
                default:
                    my_format="???";                                                   // #500
                    my_operands="*Unknown";                                            // #500
                } // end of switch statemennt                                          // #500
            // Build entry from the individual columns, aligning properly                 #500
            my_mnemonic=(my_mnemonic+my_spc).substring(0,mnem_len   );                 // #500
            my_format  =(my_format  +my_spc).substring(0,format_len );                 // #500
            my_hexop   =(my_hexop   +my_spc).substring(0,hexop_len  );                 // #500
            my_operands=(my_operands+my_spc).substring(0,operand_len);                 // #500
            entry=my_mnemonic+" "+my_format+" "+my_hexop+" "+my_operands;              // #500
            } // end of test on tz390.op_type[index] boundary value of 100             // #500
        if (tz390.op_type[index] == 0     // comment lines
        ||  tz390.op_type[index] == 122   //   empty lines (not used)
            )
           {report_entries[index] = " ";
            }
        else
           {report_entries[index] = entry;
            }
    index++;
    }
    Arrays.sort(report_entries, new ReportEntryComparator());                      // #568
    // print the array in lines of three entries each                              // #500
    put_prn_line(" "+my_hdr+" "+my_hdr+" "+my_hdr.trim());                         // #500
    index = 0;
    my_prn_line    = "";                                                           // #500
    my_entry_count = 0;                                                            // #500
    while (index < report_entries.length)                                          // #500
       {if (!report_entries[index].equals(" ")) // skip empty                         #500
           {my_entry_count++;                                                      // #500
            if (my_entry_count < 3) // for first and second entries:                  #500
               {// add entry to line (including whitespace)                           #500
                my_prn_line = my_prn_line+" "+report_entries[index];               // #500
                }                                                                  // #500
            else  // for third entry:                                                 #500
                { // print, adding third entry without padding                        #500
                put_prn_line(my_prn_line+" "+report_entries[index].trim());        // #500
                my_entry_count = 0;                                                // #500
                my_prn_line = "";                                                  // #500
                }                                                                  // #500
            }
        index++;
        }
    // print final line with remaining entries                                     // #500
    if (my_entry_count != 0)                                                       // #500
       {put_prn_line(my_prn_line);                                                 // #500
        }                                                                          // #500
    }



/**
 * Comparator for mnemonics to mimic EBCDIC collating sequence
 * The mnemonic portion occupies the first 8 positions of the ReportEntry
 * and contains only uppercase characters and digits.
 * Digits should sort higher than alphabetic characters.
 */
public class ReportEntryComparator implements Comparator<String>                   // #568
 {@Override                                                                        // #568
  public int compare(String str1, String str2)                                     // #568
   {// special case: entries consisting of a single space sort first!              // #568
    if (str1.charAt(0) == ' ') return 1;  // blank entry always smaller            // #568
    if (str2.charAt(0) == ' ') return -1; // blank entry always smaller            // #568
    for (int i=0; i<8; i++)                                                        // #568
     {char c1 = str1.charAt(i);                                                    // #568
      char c2 = str2.charAt(i);                                                    // #568
      // if c1 == c2 then we continue on to the next character                     // #568
      if (c1 != c2)                                                                // #568
       {if (c1 == ' ') return -1; // Mnemonic 2 is longer                          // #568
        if (c2 == ' ') return 1;  // Mnemonic 1 is longer                          // #568
        if (c1 <= '9')                                                             // #568
         {if (c2 <= '9') return (c1-c2); // compare two digits                     // #568
          else return 1;                 // digit always larger                    // #568
          }                                                                        // #568
        else // c1 is not numeric                                                  // #568
         {if (c2 <= '9') return -1; // digit always larger                         // #568
          else return (c1-c2);      // compare two alphabetic chars                // #568
          }                                                                        // #568
        }                                                                          // #568
      }                                                                            // #568
    return 0; // First 8 characters are equal                                      // #568
    }                                                                              // #568
  }                                                                                // #568



private void gen_obj_esds(){
	/*
	 * write ESD's for CSECTS, EXTRNS, and ENTRIES
	 * to the OBJ file in ascii hex 
	 * and list on PRN if option LIST
	 */
	xref_bal_index = -1;
	if (tot_esd > 0 && tz390.opt_list){
		put_prn_line("External Symbol Definitions");
	}
	cur_esd = 1;
	while (cur_esd <= tot_esd){
        if (sym_type[esd_sid[cur_esd]] == sym_cst // RPI 459
        	&& sym_sect_prev[esd_sid[cur_esd]] == 0){
    		String esd_code = 
    			" ESD=" + tz390.get_hex(sym_esd[esd_sid[cur_esd]],4)
    		  + " LOC=" + tz390.get_hex(sym_loc[esd_sid[cur_esd]],8)
    		  + " LEN=" + tz390.get_hex(get_sym_len(esd_sid[cur_esd]),8)
    		  + " TYPE=" + get_esd_type()
    		  + " NAME=" + sym_name[esd_sid[cur_esd]]
    		  ;
        	if (tz390.opt_list){	
                put_prn_line(esd_code);
    		}
        	put_obj_line(".ESD" + esd_code);
        }
		cur_esd++;
	}
	cur_esd = 1;
	while (cur_esd <= tot_esd){
        if (sym_type[esd_sid[cur_esd]] != sym_cst
        	&& sym_type[esd_sid[cur_esd]] != sym_dst	
        	&& sym_sect_prev[esd_sid[cur_esd]] == 0){
    		String esd_code = 
    			" ESD=" + tz390.get_hex(sym_esd[esd_sid[cur_esd]],4)
    		  + " LOC=" + tz390.get_hex(sym_loc[esd_sid[cur_esd]],8)
    		  + " LEN=" + tz390.get_hex(get_sym_len(esd_sid[cur_esd]),8)
    		  + " TYPE=" + get_esd_type()
    		  + " NAME=" + sym_name[esd_sid[cur_esd]]
    		  ;
        	if (tz390.opt_list){	
                put_prn_line(esd_code);
    		}
        	put_obj_line(".ESD" + esd_code);
        }
		cur_esd++;
	}
}
private String get_esd_type(){
	/*
	 * return esd type
	 */
	String esd_type = sym_type_desc[sym_type[esd_sid[cur_esd]]];
    if (esd_type.equals("REL")){
    	esd_type = "ENT";
    }
	return esd_type;
}
private void gen_obj_text(){
	/*
	 * generate object code for bal instructions
	 * in CSECT's on final pass
	 */
	gen_obj_code = true;
	tz390.systerm_prefix = tz390.left_justify(tz390.pgm_name,9) + " " + "AZ390 "; // RPI 755 RPI 938 switch from mz390
	put_prn_line("Assembler Listing");
	loc_ctr = 0;
    cur_lit_pool = 1;
	cur_esd = 0;
    bal_eof = false;
    end_found = false;
    bal_line_index = 1;
    while (!bal_eof){
	      if  (bal_line_index == tot_bal_line){
          	  bal_eof = true;
	      } else {
              bal_line = bal_line_text[bal_line_index];
              xref_bal_index = bal_line_index;
              parse_bal_line();
              bal_op_index = find_bal_op();
              if (bal_op_index > -1){  // RPI 274 OPYSN cancel -2  
          	     process_bal_op();    
              }
		      bal_line_index++;
         }
    }
	if (!end_found){
		if (tz390.opt_profile.length() == 0){	
			bal_line_index = tot_bal_line-1;
			if (mz390_abort){  // RPI 433
				log_error(165,"input truncated due to mz390 abort"); // RPI 935 
			} else {
				list_bal_line = false; // RPI 1169
				process_end();         // RPI 1169
			}
		} else {
			list_bal_line = false; // RPI 891 no end stmt 3
			process_end();
		}
	}
	list_bal_line = true; // RPI 891 restore 
}
private void process_bal_op(){
	/*
	 * allocate or generate object code for bal op
	 * 
	 * 1.  Note op_type index values must match
	 *     op_name array values.  
	 * 2.  Indexes < 100 and CNOP are machine instr. types RPI 743
	 * 3.  Indexes > 100 are assembler instr.
	 *
	 */
	loc_len = 0;
	exp_text  = bal_parms;
	exp_index = 0;
	obj_code = "";
	list_obj_code = "";
	hex_bddd1_loc = "      ";
	hex_bddd2_loc = "      ";
	if (gen_obj_code && tz390.opt_list){ // RPI 484
		list_bal_line = true;
	} else {
		list_bal_line = false;
	}
	loc_start = loc_ctr;
	list_obj_loc = loc_start; // RPI 265
    dc_lit_ref = false;  // RPI12
    dc_lit_gen = false;
	bal_op_ok = false;   // assume opcode undefined
	bal_label_ok = true;    // assume label updates ok RPI 451
	cur_sym_sect = false;   // assume RX/ABS label RPI 553
	int index = tz390.op_type[bal_op_index];
	if (index < tz390.max_ins_type || bal_op.equals("CNOP")){ // RPI 340 RPI 743
		if (index > 0 && mac_inline_level == 0){
			check_private_csect(); // rpi 747
		}
		bal_lab_attr = tz390.ascii_to_ebcdic['I']; 
	} else {
		bal_lab_attr = tz390.ascii_to_ebcdic['U'];
	}
	bal_lab_attr_elt = sym_attr_elt_def;
    if (mac_inline_level > 0 
    	&& index != mac_inline_op_macro  // MACRO
    	&& index != mac_inline_op_mend){ // MEND
    	index     = mac_inline_op_other; // RPI 581 print inline source
    }
	switch (index){ 
	case 0:  // * comments 
		bal_op_ok = true;
		if (bal_line.length() > 0 && bal_line.charAt(0) != '*'){
			log_error(190,"Comment must start with * in position 1");  // RPI 609
		}
    	if (gen_obj_code 
    		&& bal_line.length() > mcall_bal_index){  
       		if (bal_line.substring(0,9)
       			.equals("*MCALL #=")){
       			if (bal_line.substring(mcall_lv_index,mcall_lv_index+5)
       				.equals("LV= 1")){ // RPI 891
       				mac_call_first = true; // RPI 891 switching to GEN code			
       			    list_bal_line = true;  // RPI 891 
       			} else if (!tz390.opt_mcall){
       				// supress nested macro call listing
       				list_bal_line = false; // RPI 891 
       			}
       			if (!tz390.opt_mcall){ // RPI 511
       				bal_line = bal_line.substring(mcall_bal_index); //strip * call prefix and level RPI 233 RPI 581
       			}
       		} else if (bal_line.substring(0,9).equals("*MEXIT #=")){
       			if (bal_line.substring(mcall_lv_index,mcall_lv_index+5)
       				.equals("LV= 1")){
       				mac_call_last = true; // RPI 891 switching to open code
       			}
       			if (!tz390.opt_mcall){
       				// supress all macro exit listings
       				list_bal_line = false; // RPI 891 
       			} else {
       				list_bal_line = true;  // RPI 891 
       			}
       		} else if (bal_line.substring(0,8)
					   .equals("* MZ390E")){
       			force_list_bal = true;
       		}
       	}
		break;
    case 1:  // "E" 8 PR oooo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
	    get_hex_op(1,4); // rpi 743 remove op check
    	put_obj_text();
    	break;
    case 2:  // "RR" 60  LR  oorr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
    	get_hex_op(1,2); 
    	get_hex_reg();
    	if (obj_code.substring(0,2).equals("04")){ // SPM
    		obj_code = obj_code.concat("0");
    	} else {
    	    skip_comma();
    	    get_hex_reg();
    	}
    	check_end_parms();
 	    put_obj_text();
    	break;
    case 3:  // "BRX" 16  BER oomr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
    	get_hex_op(1,3);  // BCR OP includes mask
    	get_hex_reg();
    	check_end_parms();
 	    put_obj_text();
    	break;
    case 4:  // "I" 1 SVC 00ii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
	    get_hex_op(1,2);
    	get_hex_byte();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 5:  // "RX" 52  L  oorxbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2);
    	get_hex_reg();
    	skip_comma();
    	get_hex_xbddd();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 6:  // "BCX" 16 BE  oomxbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,3); // BCX op includes mask
    	get_hex_xbddd();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 7:  // "S" 43 SPM oo00bddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	if (bal_op.equals("CSCH")    // RPI 296
    		|| bal_op.equals("IPK")
    		|| bal_op.equals("PTLB")
    		|| bal_op.equals("RSCH")
    		|| bal_op.equals("SAL")
    		|| bal_op.equals("SCHM")
    		|| bal_op.equals("XSCH")
    		|| bal_op.contentEquals("HSCH") // rpi 2202
    		|| bal_op.contentEquals("RCHP") // rpi 2202
			|| bal_op.contentEquals("TEND") // rpi 2202
    		){  // RPI 277
    		obj_code = obj_code + "0000";
    	} else {
    		get_hex_bddd2(true);
        	check_end_parms();  // RPI 493 only if operands
    	}
    	put_obj_text();
    	break;
    case 8:  // "DM" 1  DIAGNOSE 83000000
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
    	get_hex_zero(6);  // RPI 946
    	put_obj_text();
    	break;
    case 9:  // "RSI" 4 BRXH  oorriiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
    	get_hex_relative_offset(16);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 10:  // "RS" 25  oorrbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_reg();
    	skip_comma();
    	if (hex_op.compareTo("88") >=0
    		&& hex_op.compareTo("8F") <= 0){
    		obj_code = obj_code + "0"; // r3=0 for shift
    	} else {
    	    get_hex_reg();
        	skip_comma();
    	}
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 11:  // "SI" 9 CLI  ooiibddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
    	get_hex_bddd2(true);
    	skip_comma();
    	get_hex_byte();
    	check_end_parms();
    	obj_code = obj_code.substring(0,2) + obj_code.substring(6,8) + obj_code.substring(2,6);
    	put_obj_text();
    	break;
    case 12:  // "RI" 37 IIHH  ooroiiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2); 
       	get_hex_reg(); 
       	get_hex_op(3,1);
    	skip_comma();
    	get_hex_int(16);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 13:  // "BRCX" 31 BRE  oomoiiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,2);       // oo
		if ((tz390.op_code[bal_op_index].length() == 4)){ // rpi 2221 support JC M1,I2
     		  get_hex_op(4,1); // m3 extended op
     	} else {
     		  get_hex_reg();    // m3
			  skip_comma();
     	}
       	get_hex_op(3,1);        // last op nibble such A74 JC rpi 2221
    	get_hex_relative_offset(16);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 14:  // "RRE" 185  MSR oooo00rr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_zero(2);
    	if (bal_op.equals("PALB") // RPI 277
    		|| bal_op.equals("PCKMO") // RPI 1125
	        || bal_op.equals("PCC")){ // RPI 1125
    		get_hex_zero(2);
    	} else {
    		get_hex_reg();
    		if (exp_index >= exp_text.length()
    			|| exp_text.charAt(exp_index) != ','){ 
    			obj_code = obj_code.concat("0"); // IPM,EFPC,SFPC,PTF RPI 817
    		} else {
    			skip_comma();
    			get_hex_reg();
    		}
        	if (bal_op.equals("TROO")
        			|| bal_op.equals("TROT")
        			|| bal_op.equals("TRTO")
        			|| bal_op.equals("TRTT")
        			|| bal_op.equals("CU21")    // rpi 2202
        			|| bal_op.equals("CU24")    // rpi 2202
        			|| bal_op.equals("CUUTF") // rpi 2202
        			|| bal_op.equals("CU12")    // rpi 2202
        			|| bal_op.equals("CUTFU") // rpi 2202
        			|| bal_op.equals("CU14") // rpi 2202
        			|| bal_op.equals("IPTE") // rpi 2202
        			|| bal_op.equals("SSKE") // rpi 2202
    	       ){
        		    if (!bal_abort && exp_next_char(',')){ //RPI 577
        		    	skip_comma();
        		    	get_hex_reg(); // RPI 454
        		    	obj_code = obj_code.substring(0,4) + obj_code.substring(8,9) + obj_code.substring(5,8);
        		    }
        		    if (!bal_abort && exp_next_char(',')){ //RPI 2202 optional m3 for IPTE
        		    	skip_comma();
        		    	get_hex_reg(); // RPI 454
        		    	obj_code = obj_code.substring(0,5) + obj_code.substring(8,9) + obj_code.substring(6,8);
        		    }
        	}
    		check_end_parms();
    	}
    	put_obj_text();
    	break;
    case 15:  // RPI 206 "RRF1" MA?R, MS?R, MY?R (r1,r3,r2 maps to oooo1032)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_reg();
    	get_hex_zero(1);
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 16:  // "RIL" 6  BRCL  oomollllllll
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_reg();   // r1 or m1
        if (tz390.op_code[bal_op_index].length() > 2) {
       		 get_hex_op(3,1); // m1 or 3rd opcode nibble
		} else {
 	         get_hex_zero(1);
		}
       	skip_comma();
		if (tz390.op_name[bal_op_index].substring(0,1).equals("B") // RIL branch relative
		 || tz390.op_name[bal_op_index].substring(0,1).equals("J") // RIL jump relative
		 || tz390.op_name[bal_op_index].substring(tz390.op_name[bal_op_index].length()-2).equals("RL")){ // other RIL relative such as LARL, LHRL, etc.
			 get_hex_relative_offset_long(); // gen rel half word offset or abs value
	     } else {
           get_hex_rld_long(); // gen 32 bit RLD or abs value		  
	    }
    	check_end_parms();
    	put_obj_text();
    	break;
    case 17:  // "SS" 32  MVC oollbdddbddd  rpi 2202 support SSD and PKU with LL from S2, plus MVCK, MVCP, MVCS with d1(r1,b1), d2(b2),r3
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);      
    	 if (obj_code.contentEquals("E1") || obj_code.contentEquals("E9")){    // rpi 2202  ooll11112222 from d1(b1),d2(l2,b2) PKU and PKA
      	    get_hex_bddd2(true);  // d1(b1) bddd without length
      	    skip_comma();
      	    get_hex_llbddd();                // d2(l2,b2) llbddd with length
           	hex_bddd1     = hex_bddd;
           	hex_bddd1_loc = hex_bddd_loc; 
      	    obj_code = obj_code + hex_ll + hex_bddd1;
            obj_code = obj_code.substring(0,2)  // oo 
        		         + obj_code.substring(6,8)       // ll from L2
        		         + obj_code.substring(2,6)       // d1(b1)
        				 + obj_code.substring(8,12);    //  b2(b2)    
    	 } else if (obj_code.contentEquals("D9") || obj_code.contentEquals("DB") || obj_code.contentEquals("DA")){ // MVCK, MVCS, MVCP d1(r1,b1),d2(b2),r3 to oo13bdd1bdd2
        	get_hex_xbddd();           	
           	skip_comma();
        	get_hex_bddd2(true);
        	skip_comma();
  	     	get_hex_reg();  // optional r3 for MVCK, MVCP, MVCS
  	        obj_code = obj_code.substring(0,3)  // oo1 
   		         + obj_code.substring(11,12)         // r3
  	   			 + obj_code.substring(3,11);          // b1d1, b2d2	     			
  		} else {
           	get_hex_llbddd();           	
           	hex_bddd1     = hex_bddd;
           	hex_bddd1_loc = hex_bddd_loc; 
    		obj_code = obj_code + hex_ll + hex_bddd1;
           	skip_comma();
        	get_hex_bddd2(true);  						
     	}
    	check_end_parms();
    	put_obj_text();
    	break;
    case 18:  // "RXY" 76 MLG oorxbdddhhoo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
		if (  tz390.op_code[bal_op_index].length() == 5 // m1 
		   && tz390.op_code[bal_op_index].substring(0,4).equals("E347")){   // E347 BIC  M1,D2(X2,B2)
		    get_hex_op(5,1);    // M1 BIE D2(X2,B2)
		    get_hex_xbdddhh2(); // d2(x2,b2)
		} else {
		   get_hex_reg();   // R1  CG R1,D2(X2,B2) 
		   skip_comma();
   	       get_hex_xbdddhh2(); 
	    }		   
    	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 19:  // "SSE" 5  LASP  oooobdddbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,4); 
       	get_hex_bddd2(true);
       	skip_comma();
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 20:  // "RSY" 31  LMG r1,r3,s2 oorrbdddhhoo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
	    if   (tz390.op_code[bal_op_index].substring(0,4).equals("EB23")   // EB23 RSYb CLT  R1,M3,D2(B2)
		   || tz390.op_code[bal_op_index].substring(0,4).equals("EB2B")   // EB2B RSYb CLGT R1,M3,D2(B2)
		   || tz390.op_code[bal_op_index].substring(0,4).equals("EB17")){ // EB17 RSYb STCCTM R1,M3,D2(B2)
			 if ( tz390.op_code[bal_op_index].length() == 5){ // m1 found
       	        get_hex_reg();   // R1
				skip_comma();
			    get_hex_op(5,1); // M3 CLT, CLGT R1,M3,D2(B2)
				get_hex_bdddhh2();
		    } else {
			    get_hex_reg();     	       
			    skip_comma();
       	        get_hex_reg();  // M3
				skip_comma();
				get_hex_bdddhh2();
		    }
		} else {
			get_hex_reg();     	       
			skip_comma();
       	    get_hex_reg(); 
            skip_comma();
			get_hex_bdddhh2();
		}
    	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 21:  // "SIY" 6  TMY  ooiibdddhhoo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_bdddhh2();
    	skip_comma();
    	get_hex_byte();
    	obj_code = obj_code.substring(0,2) + obj_code.substring(8,10) + obj_code.substring(2,8); 
       	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 22:  // "RSL" 1  TP  oor0bddd00oo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
	 if (tz390.op_code[bal_op_index].substring(0,3).equals("EDA")){ // RSLb CZDT ETC
		  get_hex_reg();     // R1
		  skip_comma();
		  get_hex_llbddd();  // D2(L2,B2)
	      obj_code = obj_code.substring(0,2) + hex_ll + hex_bddd   // oolldbbb
		    + obj_code.substring(2,3);                  // r1
		  skip_comma();
		  get_hex_reg();  // M3	
	 } else if (tz390.op_code[bal_op_index].substring(0,2).equals("ED")){
		  // RSLg z15 convert to zoned etc.  RPI 2202
		  get_hex_reg();     // R1
		  skip_comma();
		  get_hex_llbddd();  // D2(L2,B2)
	      obj_code = obj_code + hex_ll + hex_bddd;  // oolldbbb
		  skip_comma();
		  get_hex_reg();  // M3		  
	} else {
		// all other RSL with length <= 16
	    get_hex_llbddd();  // D2(L2,B2)
       	if (hex_ll.charAt(0) == '0'){
       		hex_len1 = hex_ll.substring(1);
       		hex_bddd1     = hex_bddd;
       		hex_bddd1_loc = hex_bddd_loc;
       	} else {
       		log_error(69,"field 1 hex length > 16 = " + hex_ll);
       	}
       	obj_code = obj_code + hex_len1 + "0" + hex_bddd1 + "00";
	}
       	get_hex_op(3,2);
    	check_end_parms();
       	put_obj_text();
    	break;
    case 23:  // "RIE" 4  BRXLG  oorriiii00oo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_reg(); 
    	skip_comma();
		if (tz390.op_code[bal_op_index].substring(0,4).equals("EC42")   // EC42 LOCHI  R1,I2,M3
		 || tz390.op_code[bal_op_index].substring(0,4).equals("EC46")   // EC46 LOCGHI R1,I2,M3
		 || tz390.op_code[bal_op_index].substring(0,4).equals("EC4E")){ // EC42 LOCHHI R1,I2,M3
			get_hex_int(16);
			if (tz390.op_code[bal_op_index].length() == 5){
			    get_hex_op(5,1); // M3 for extended mnemonic like LOCHIE R1,I2
		    } else {
				skip_comma();
				get_hex_reg(); // M3
			}
			// move 001iiii3 to 0013iiii
			obj_code = obj_code.substring(0,3)  // oo1 
		         + obj_code.substring(7,8)      // m3
				 + obj_code.substring(3,7);  // iiii
		} else {
       	    get_hex_reg();
       	    skip_comma();
       	    get_hex_relative_offset(16);
		}
       	get_hex_zero(2);
       	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 24:  // "RXE" 28  ADB oorxbddd00oo
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_reg(); 
    	skip_comma();
       	get_hex_xbddd();
       	if (!bal_abort && exp_next_char(',')){   // E727 RXE LCBB R1,D2(X2,B2),M3 RPI 2202
     		skip_comma();
     		get_hex_reg();  // optional m3
     	} else {
     		get_hex_zero(1); // RPI 817 optional m3 PFMF 
     	}
       	get_hex_zero(1);
       	get_hex_op(3,2);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 25:  // "RXF" 8   MAE  oorxbdddr0oo (note r3 before r1)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
       	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
       	get_hex_xbddd();
        get_hex_zero(1);
       	get_hex_op(3,2);  
       	obj_code = obj_code.substring(0,2)  // oo 
		         + obj_code.substring(3,4)  // r3
				 + obj_code.substring(4,9)  // xbddd
				 + obj_code.substring(2,3)  // r1
				 + obj_code.substring(9);   // 0oo
    	check_end_parms();
       	put_obj_text();
    	break;
    case 26:   // AP SS2  oollbdddbddd
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_llbddd();
       	if (hex_ll.charAt(0) == '0'){
       		hex_len1 = hex_ll.substring(1);
       		hex_bddd1     = hex_bddd;
       		hex_bddd1_loc = hex_bddd_loc;
       	} else {
       		log_error(69,"field 1 hex length > 16 = " + hex_ll);
       	}
       	skip_comma();
    	get_hex_llbddd();
       	if (hex_ll.charAt(0) == '0'){
       		hex_len2 = hex_ll.substring(1);
       		hex_bddd2     = hex_bddd;
       		hex_bddd2_loc = hex_bddd_loc;
       	} else {
       		log_error(70,"field 2 hex length > 16 = " + hex_ll);
       	}
       	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;
    	check_end_parms();
       	put_obj_text();
    	break;
    case 27:   // PLO SS3  oorrbdddbddd  r1,s2,r3,s4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	hex_len1 = get_hex_nib();
    	skip_comma();
       	get_hex_bddd2(false);          // RPI 613
 		hex_bddd1     = hex_bddd2;     // RPI 613
       	hex_bddd1_loc = hex_bddd2_loc; // RPI 613
       	skip_comma();
      	hex_len2 = get_hex_nib();
    	skip_comma();
    	get_hex_bddd2(false);  // RPI 613
      	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;
    	check_end_parms();
      	put_obj_text();
    	break;
    case 28:   // LMD SS4  oorrbdddbddd  r1,r3,s2,s4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
    	skip_comma();
       	get_hex_bddd2(true);
       	skip_comma();
    	get_hex_bddd2(true);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 29:   // SRP SS5  oolibdddbddd s1(l1),s2,i3
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_llbddd();
       	if (hex_ll.charAt(0) == '0'){
       		hex_len1 = hex_ll.substring(1);
       		hex_bddd1     = hex_bddd;
       		hex_bddd1_loc = hex_bddd_loc;
       	} else {
       		log_error(69,"field 1 hex length > 16 = " + hex_ll);
       	}
       	skip_comma();
    	get_hex_bddd2(false); //RPI120 get bddd2 to add later
        skip_comma();
        hex_len2 = get_hex_nib();
    	obj_code = obj_code + hex_len1 + hex_len2 + hex_bddd1 + hex_bddd2;  //RPI120
    	check_end_parms();
    	put_obj_text();
    	break;
    case 30:  // RPI 206 "RRF3" 30 DIEBR/DIDBR oooormrr (r1,r3,r2,m4 maps to oooo3412) 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
		skip_comma();
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
       	obj_code = obj_code.substring(0,4)  // oooo 
        + obj_code.substring(5,6)  // r3
		+ obj_code.substring(7,8)  // m4
		+ obj_code.substring(4,5)  // r1
		+ obj_code.substring(6,7); // r2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 31:  // "SS" PKA oollbdddbddd  ll from S2  
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_bddd2(false);          // RPI 613
       	hex_bddd1     = hex_bddd2;     // RPI 613
       	hex_bddd1_loc = hex_bddd2_loc; // RPI 613
       	skip_comma();
    	get_hex_llbddd();
    	hex_bddd2 = hex_bddd;
    	hex_bddd2_loc = hex_bddd_loc;
    	obj_code = obj_code + hex_ll + hex_bddd1 + hex_bddd2;
    	check_end_parms();
    	put_obj_text();
    	break;
    case 32:   //  MVCOS oor0bdddbddd (s1,s2,r3) "C80" 32 Z9-41
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_bddd2(false);          // RPI 606
       	hex_bddd1     = hex_bddd2;     // RPI 606
       	hex_bddd1_loc = hex_bddd2_loc; // RPI 606
       	skip_comma();
    	get_hex_bddd2(false); 
        skip_comma();
        get_hex_reg(); // RPI 1152
    	get_hex_op(3,1);
    	obj_code = obj_code + hex_bddd1 + hex_bddd2; 
    	check_end_parms();
    	put_obj_text();
    	break;
    case 33:   // "BLX" branch relative on condition long RPI199
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); //BRCL C0 OP
		if ((tz390.op_code[bal_op_index].length() == 4)){ // rpi 2221 support JLC M1,I2
     		  get_hex_op(4,1); // m3 extended op
     	} else {
     		  get_hex_reg();    // m3
			  skip_comma();
     	}
    	get_hex_op(3,1); //BRCL 4  OP
    	get_hex_relative_offset_long();
    	check_end_parms();
    	put_obj_text();
    	break;
    case 34:   // RPI 206 "RRF2" CG?R, CF?R, FI?R, IDTE, TB?R RRF2 34 (r1,m3,r2 maps to oooo3012)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
     	get_hex_reg();
     	get_hex_zero(1);
     	skip_comma();
     	get_hex_reg();
     	skip_comma();
     	get_hex_reg();
    	obj_code = obj_code.substring(0,4)  // oooo
     	    + obj_code.substring(6,7)   // m3
	    	+ "0"
	    	+ obj_code.substring(4,5)  // r1
	    	+ obj_code.substring(7,8); // r2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 35:  // RPI 407 "CSDTR" "RRF4" 35 oooo0mrr (r1,r2,m4 maps to oooo0412) 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
     	get_hex_zero(1);
    	get_hex_reg();
    	skip_comma();
    	get_hex_reg();
		skip_comma();
    	get_hex_reg();
       	obj_code = obj_code.substring(0,5)  // oooo 
        + obj_code.substring(7,8)  // m4
		+ obj_code.substring(5,7);  // r1,r2
    	check_end_parms();
    	put_obj_text();
    	break;	
    case 36:  // RPI 407 "ADTR" "RRR"  R1,R2,R3 to oooo3012 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
      	if (bal_op.charAt(bal_op.length()-1) == 'A'){ // RPI 1125    	
      		get_hex_reg();
      		skip_comma();
      	   	get_hex_reg();
      	   	skip_comma();
      	   	get_hex_reg();
      	   	skip_comma();
      	   	get_hex_reg();
      	   	obj_code = obj_code.substring(0,4)  // oooo
      	   	  + obj_code.substring(6,7)  // r3
	    	  + obj_code.substring(7,8)  // m4
	    	  + obj_code.substring(4,5)  // r1
    	      + obj_code.substring(5,6); // r2
      	} else {
      		get_hex_reg();
      	   	get_hex_zero(1);
      	   	skip_comma();
      	   	get_hex_reg();
      	   	skip_comma();
      	   	get_hex_reg();
      	   	obj_code = obj_code.substring(0,4)  // oooo
      	   	  + obj_code.substring(7,8)  // r3
	    	  + "0"
	    	  + obj_code.substring(4,5)  // r1
    	      + obj_code.substring(6,7); // r2
      	}
	    check_end_parms();
    	put_obj_text();
    	break;
    case 37:  // "RXAS" XDECI for ASSIST RPI 812
    	if (tz390.opt_assist){
    		bal_op_ok = true;
    		loc_ctr = (loc_ctr+1)/2*2;
    		loc_start = loc_ctr;
    		loc_len = 4;
    		get_hex_op(1,2);
    		get_hex_reg();
    		skip_comma();
    		get_hex_xbddd();
    		check_end_parms();
    		put_obj_text();
    	}
    	break;
    case 38:  // "RXSS" XREAD for ASSIST RPI 812
    	if (tz390.opt_assist){
    		bal_op_ok = true;
    		loc_ctr = (loc_ctr+1)/2*2;
    		loc_start = loc_ctr;
    		loc_len = 6;
    		get_hex_op(1,3);
    		if ((bal_parms == null
    			|| bal_parms.length() == 0
    			|| bal_parms.charAt(0) == ',')
    			&& bal_op.equals("XDUMP")){
    			exp_text = "0,0"; // force zeros for default XDUMP	
    		}
    		get_hex_xbddd();
    		if (exp_index < exp_text.length() 
    			&& exp_text.charAt(exp_index) == ','){
    			skip_comma();
    			if(exp_index < exp_text.length() 
    				&& exp_text.charAt(exp_index) == '('){
    				exp_text = "0" + exp_text.substring(exp_index);
    				exp_index = 0; // allow (reg) vs 0(reg)
    			}
    			get_hex_bddd2(true);
    		} else {
    			get_hex_zero(4);
    		}
    		check_end_parms();
    		put_obj_text();
    	} else if (tz390.op_name[bal_op_index].equals("PKU")){
        	bal_op_ok = true;
        	loc_ctr = (loc_ctr+1)/2*2;
        	loc_start = loc_ctr;
        	loc_len = 6;
        	get_hex_op(1,2); 
           	get_hex_llbddd();
           	hex_bddd1     = hex_bddd;
           	hex_bddd1_loc = hex_bddd_loc;
        	obj_code = obj_code + hex_ll + hex_bddd1;
           	skip_comma();
        	get_hex_bddd2(true);
        	check_end_parms();
        	put_obj_text();	    		
    	}
    	break;
    // 39 to 40 rpi 2202
    case 39:   // RPI 817 "RRF5" CRT (r1,r2,m3 maps to oooom012)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
     	get_hex_reg();   // R1 MOVE
     	get_hex_zero(1);
     	skip_comma();
     	get_hex_reg();    // R2 MOVE 
     	if (!bal_abort && exp_next_char(',')){
     		skip_comma();
     		get_hex_reg();  // M3
     	} else {
			if ((tz390.op_code[bal_op_index].length() == 5)){  
   		       get_hex_op(5,1);  // M3
    	    } else {
			   if (tz390.op_code[bal_op_index].substring(0,4).equals("B966")   // NOTGR - NORGK r1,r2,r2
		        || tz390.op_code[bal_op_index].substring(0,4).equals("B976")){ // NOTR  = MORK  r1,r2,r2 
				  // set r3 = r2 for NOT
				  obj_code = obj_code.substring(0,7)  // oooo102
     	                   + obj_code.substring(6,7); // set r3=r2
			   } else {	
     		      get_hex_zero(1);  // m3=0 
     	       }
			}
		}
    	obj_code = obj_code.substring(0,4)  // oooo
     	    + obj_code.substring(7,8)   // m3
	    	+ "0"
	    	+ obj_code.substring(4,5)  // r1
	    	+ obj_code.substring(6,7); // r2
    	check_end_parms();
    	put_obj_text();
    	break;
        // 39 to 40 rpi 2202
    case 40:   // RPI 817 "RRF6" CRT (r1,r2,m3 or r1,r2 with extended op maps to oooom012) 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);    // oooo
     	get_hex_reg();        // r1
     	get_hex_zero(1);
     	skip_comma();
     	get_hex_reg();       // r2 or r3 if IDTE
		if (tz390.op_code[bal_op_index].substring(0,4).equals("B98E")   // IDTE
		 || tz390.op_code[bal_op_index].substring(0,4).equals("B98F")){ // CRDTE
		   skip_comma();
		   get_hex_reg(); // r2 third parm
		   if ((tz390.op_code[bal_op_index].length() == 5)){
     		  get_hex_op(5,1); // m3 extended op
     	   } else {
     		  skip_comma();
     		  get_hex_reg();  // m3
     	   }
		   obj_code = obj_code.substring(0,4)  // oooo
         	    + obj_code.substring(5,6)      // r3 second op
    	    	+ obj_code.substring(7,8)      // m4
				+ obj_code.substring(4,5)      // r1
				+ obj_code.substring(6,7);     // r2
	  } else { // not IDTE
		if ((tz390.op_code[bal_op_index].length() == 5) && (tz390.op_code[bal_op_index].charAt(4) != 'F')){
     		get_hex_op(5,1); // m3 extended op
     	} else {
     		skip_comma();
     		get_hex_reg();  // m3
     	}
		if (tz390.op_code[bal_op_index].substring(0,4).equals("B2E8") // PPA
		 || tz390.op_code[bal_op_index].substring(0,4).equals("B960")   // CGRT
		 || tz390.op_code[bal_op_index].substring(0,4).equals("B961")   // CLGRT
		 || tz390.op_code[bal_op_index].substring(0,4).equals("B972")   // CRT
		 || tz390.op_code[bal_op_index].substring(0,4).equals("B973")){ // CLRT
		   obj_code = obj_code.substring(0,4)  // oooo
         	    + obj_code.substring(7,8)      // m3
				+ "0"                          // 0
    	    	+ obj_code.substring(4,5)      // r1
				+ obj_code.substring(6,7);     // r2
		} else {
		   obj_code = obj_code.substring(0,6)  // oooo10
         	    + obj_code.substring(7,8)          // m3
    	    	+ obj_code.substring(6,7);
	    }
	  }		
    	check_end_parms();
    	put_obj_text();
    	break;
        // 41 to 42 rpi 2202
    case 41:  // "RIE2" CIT oor0iiiim0oo
    	// r1,i2,m3
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // OP1
    	get_hex_reg();    // R1
    	get_hex_zero(1);
       	skip_comma();
       	get_hex_int(16);    // I2
       	skip_comma();
       	get_hex_reg();    // M3
       	get_hex_zero(1);
       	get_hex_op(3,2);  // OP2
    	check_end_parms();
    	put_obj_text();
    	break;
    	// 41 to 42 rpi 2202
    case 42:  // "RIE3" CITE oor0iiiim0oo
    	// r1,i2.,m3 or r1,i2 with extended op // rpi 2202
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // op1
    	get_hex_reg();      // r1
    	get_hex_zero(1);
       	skip_comma();
       	get_hex_int(16);       // i2
       	if ((tz390.op_code[bal_op_index].length() == 5) && (tz390.op_code[bal_op_index].charAt(4) != 'F')){
       		get_hex_op(5,1);  // m3 from opcode
       	} else {
           	skip_comma();
           	get_hex_reg();    // m3
       	}
       	get_hex_zero(1);  // 0
       	get_hex_op(3,2);  // op2
    	check_end_parms();
    	put_obj_text();
    	break;	
    	// 43 to 44 rpi 2202
    case 43:  // "RIE4" CIJ/CGIJ/CLIJ/CLGIJ oo13444422oo
    case 44:                                 // #602
    	// r1,i2,m3,i4 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // op1
    	get_hex_reg();    // r1 
       	skip_comma();
       	if (tz390.op_name[bal_op_index].charAt(1) == 'L'){ // RPI 1146
       		get_hex_byte();   // i2
       	} else {
       		get_hex_byte_signed(); // i2
       	}
       	if ((tz390.op_code[bal_op_index].length() == 5) && (tz390.op_code[bal_op_index].charAt(4) != 'F')) {
       		get_hex_op(5,1); // m3 extended
       	} else {
            skip_comma();
            get_hex_reg();    // m3
       	}
       	skip_comma();
       	get_hex_relative_offset(16);    //i4 rel rpi 2225 
       	get_hex_op(3,2);
    	obj_code = obj_code.substring(0,3)  // op1+r1
 	    + obj_code.substring(5,6)          // m3
    	+ obj_code.substring(6,10)          // i4 4444 rel
    	+ obj_code.substring(3,5)           // i2 22 byte
    	+ obj_code.substring(10,12); // op2
    	check_end_parms();
    	put_obj_text();
    	break;
    	// 43 to 44 rpi 2202
    	//  45 to 46 rpi 2202
    case 45:  // "RRS1" CGIB oorrbdddm0oo
    	// r1,r2,m3,s4 
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);
    	get_hex_reg(); // r1
        skip_comma();
        get_hex_reg(); // r2 
       	skip_comma();
       	get_hex_reg(); // m3
       	get_hex_zero(1);
       	skip_comma();
       	get_hex_bddd2(true); // bddd4
       	get_hex_op(3,2);
    	obj_code = obj_code.substring(0,4)  // op1+r1+r2
    	+ obj_code.substring(6,10)          // s4 bddd    
        + obj_code.substring(4,6)           // m3 + 0
    	+ obj_code.substring(10,12); // op2
    	check_end_parms();
    	put_obj_text();
    	break;
    	//  45 to 46 rpi 2202
    case 46:  // "RRS2" CGRBE oorriiiim0oo
    	// r1,r2,s4 or r1,r2,m3,s4 with extended op m3  rpi 2202
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); // op1
    	get_hex_reg();   // r1
    	skip_comma();
    	get_hex_reg();   // r2
       	if ((tz390.op_code[bal_op_index].length() == 5) && (tz390.op_code[bal_op_index].charAt(4) != 'F')) {
       		get_hex_op(5,1);
       	} else {
           	skip_comma();
       		get_hex_reg(); // m3 
       	}
   		skip_comma();
       	get_hex_bddd2(true);  // s4 bddd
       	get_hex_zero(1); // 0
       	get_hex_op(3,2); // op2
    	// map oo12344440oo  to oo12444430oo
    	obj_code = obj_code.substring(0,4)  // op1+r1+r2
    	    	+ obj_code.substring(5,9)          // s4 bddd    
    	        + obj_code.substring(4,5)           // m3 + 0
    	    	+ obj_code.substring(9,12); // 0, op2
    	check_end_parms();
    	put_obj_text();
    	break;
    	//  47 to 48 rpi 2202
    case 47:  // "RRS3" CGIB oo13bddd22oo
    	// r1,i2,m3,i4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); // op1
    	get_hex_reg();   // r1 
       	skip_comma();
       	get_hex_byte();  // i2
       	skip_comma();
    	get_hex_reg();   // m3
       	skip_comma();
       	get_hex_bddd2(true);  // s4 bddd 
       	get_hex_op(3,2); // op2
    	obj_code = obj_code.substring(0,3)  // op1+r1
 	    + obj_code.substring(5,6)           // m3
    	+ obj_code.substring(6,10)          // s4 bddd 
    	+ obj_code.substring(3,5)           // i2 22 byte
    	+ obj_code.substring(10,12);        // op2
    	check_end_parms();
    	put_obj_text();
    	break;
    	//  47 to 48 rpi 2202
    case 48:  // "RRS4" CGIBE oormbddd22oo r1,i2,m3,s4  or r1,i2,s4 with extended op m4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // op1
    	get_hex_reg();    // r1 
    	skip_comma();
       	get_hex_byte();   // i2
       	if ((tz390.op_code[bal_op_index].length() == 5) && (tz390.op_code[bal_op_index].charAt(4) != 'F')) {
    		get_hex_op(5,1);  // m3 from opcode
    	} else {
    		skip_comma();
    		get_hex_reg(); // m3
    	}
       	skip_comma();
       	get_hex_bddd2(true);   // s4 bddd 
       	get_hex_op(3,2);  // op2
    	// map 0012234444oo to oo13444422oo
    	obj_code = obj_code.substring(0,3)  // op1+r1
    	+ obj_code.substring(5,6)             // m3
    	+ obj_code.substring(6,10)          // i4 4444 rel
    	+ obj_code.substring(3,5)           // i2 22 byte
    	+ obj_code.substring(10,12); // op2
    	check_end_parms();
    	put_obj_text();
    	break;
    	//  49 to 50 rpi 2202
    case 49:  // "RIE6" CGRJ oorriiiim0oo
    	// r1,r2,m3,s4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // OP1
    	get_hex_reg();    // R1
    	skip_comma();
    	get_hex_reg();    // r2
       	skip_comma();
       	get_hex_reg();    // m3
       	skip_comma();
       	get_hex_relative_offset(16);    // I2 rpi 2225
       	get_hex_zero(1);
       	get_hex_op(3,2);  // OP2
    	obj_code = obj_code.substring(0,4)  // op1+r1+r2
    	+ obj_code.substring(5,9)          // s4 bddd    
        + obj_code.substring(4,5)           // m3 + 0
    	+ obj_code.substring(9,12);        // op2
    	check_end_parms();
    	put_obj_text();
    	break;
    	//  49 to 50 rpi 2202
    case 50:  // "RIE7" CGRJE oorriiiim0oo
    	// r1,r2,M3,I4 or r1,r2,i4 with m3 from extended opcode
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // OP1
    	get_hex_reg();    // R1
    	skip_comma();
    	get_hex_reg();    // R2
    	skip_comma();
    	if ((tz390.op_code[bal_op_index].length() == 5) && (tz390.op_code[bal_op_index].charAt(4) != 'F')) {  //  rpi 2202 merge case 49+50 for CGRJ
   		    get_hex_op(5,1);  // M3
    	} else {
    		get_hex_reg(); // M3
    		skip_comma();  
    	}
  		get_hex_relative_offset(16);  // i2 rpi 2225
    	obj_code = obj_code.substring(0,4)  // op1+r1+r2
       	    	+ obj_code.substring(5,9)          // i4    
       	        + obj_code.substring(4,5);           // m3
    	get_hex_zero(1);  // 0
       	get_hex_op(3,2);  // OP2       	
    	check_end_parms();
    	put_obj_text();
    	break;
    case 51: // "SIL" "MVHHI" d1(b1),i2 oooobdddiiii
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,4); 
    	get_hex_bddd2(true);
    	skip_comma();
    	get_hex_int(16);
    	check_end_parms();
    	put_obj_text();
    	break;
    case 52: // "RIE8" "RNSBG" oo12334455oo RPI 817
        // r1,r2,i3,i4[,i5] or extended op r1,r2 RPI 1164
    	// i3 high bit for test RNSBGT, RXSBGT
    	// i4 high bit for zero RISBGZ
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);  // OP1
    	get_hex_reg();    // R1
       	skip_comma();
       	get_hex_reg();    // R2
       	if (tz390.op_code[bal_op_index].length() == 11){ //$i3i4i5 extension
       		// extended op (get i3,i4,i5 from op_code "oooo$i3i4i5" RPI 1164
       		int i3 = Integer.valueOf(tz390.op_code[bal_op_index].substring(5,7));
       		int i4 = Integer.valueOf(tz390.op_code[bal_op_index].substring(7,9));
       		int i5 = Integer.valueOf(tz390.op_code[bal_op_index].substring(9,11));
       		obj_code = obj_code + tz390.get_hex(i3,2);
       		if (tz390.op_name[bal_op_index].charAt(0) == 'L'){
       			obj_code = obj_code + tz390.get_hex(i4 + 0x80,2);
       		} else {
       			obj_code = obj_code + tz390.get_hex(i4,2);
       		}
       		obj_code = obj_code + tz390.get_hex(i5,2);
       	} else {
       		skip_comma();
       		get_hex_byte();   // I3
			if (tz390.opt_traceall){ // DSHX debug RXSBGT
				tz390.put_trace("RNSBG/RXSBG I3=" + exp_val + "obj_code=" + obj_code);
			}
       		if (!bal_abort 
       			&& bal_op.length() == 6 
       			&& bal_op.charAt(5) == 'T'){ // turn on test bit if OP=?????T
       			obj_code = obj_code.substring(0,obj_code.length()-2) + tz390.get_hex(((int)exp_val | (int)0x80),2); // DSH z16 #423 change + to |
       		}
       		skip_comma();
       		get_hex_byte();   // I4
       		if (!bal_abort                                          // RPI 1164 WAS 6
       			&& bal_op.charAt(bal_op.length()-1) == 'Z'){ // turn on XZERO bit if OP=??????Z  // RPI 1164 RISBG/RISBGN
       			obj_code = obj_code.substring(0,obj_code.length()-2) + tz390.get_hex(((int)exp_val | (int)0x80),2); // DSH z16 #423 change + to |
       		}
       		if (exp_text.length() > exp_index 
       			&& exp_text.charAt(exp_index) == ','){
       			skip_comma();
       			get_hex_byte();   // I5
       		} else {
       			get_hex_zero(2);  // I5 default 0 rotate RPI 1164 was 1
       		}
       	}
       	get_hex_op(3,2);  // OP2
       	check_end_parms();
       	put_obj_text();
    	break;
    case 53:   // LEDBR?, LDXBR?, LEXBR? RPI 1125 oo0012/oo3412 r1,r2/r1,m3,r2,m4
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);
      	if (bal_op.charAt(bal_op.length()-1) == 'A'){ // RPI 277
      		get_hex_reg();  // r1
   			skip_comma();
   			get_hex_reg();  // m3
   			skip_comma();
   			get_hex_reg();  // r2
   			skip_comma();
   			get_hex_reg();  // m4
   	    	obj_code = obj_code.substring(0,4)  // op1
   	    	+ obj_code.substring(5,6)           // m3    
   	        + obj_code.substring(7,8)           // m4
   	        + obj_code.substring(4,5)           // r1
   	    	+ obj_code.substring(6,7);          // r2
    	} else {
        	get_hex_zero(2);
    		get_hex_reg();
   			skip_comma();
   			get_hex_reg();        	
    	}
		check_end_parms();
    	put_obj_text();
    	break;
    case 54:   // FIEBR?, FIDBR?, FIXBR? RPI 1125 	r1,m3,r2 or r1,m3,r2,m4  maps to 00003012 or 00003412 
    	// RPI 2202 generalize to support RRF-b IDTE
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);  // oooo
  		get_hex_reg();     // r1
		skip_comma();
		get_hex_reg();    // m3
		skip_comma();
		get_hex_reg();    // r2
     	if (!bal_abort && exp_next_char(',')){
     		skip_comma();
     		get_hex_reg();  // optional m4
     	} else {
     		get_hex_zero(1); // RPI 2202 optional m4 for IDTE RRF-b
     	}
    	obj_code = obj_code.substring(0,4)  // oooo
     	    + obj_code.substring(5,6)             // m3
     	    + obj_code.substring(7,8)             // m4 or 0
	    	+ obj_code.substring(4,5)             // r1
	    	+ obj_code.substring(6,7);            // r2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 55:   // SSF LPD/LPDG oor0bdddbddd (r3,s1,s2) RPI 1125
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
    	get_hex_reg();
    	get_hex_op(3,1);
    	skip_comma();        
    	get_hex_bddd2(false);          // RPI 606
       	hex_bddd1     = hex_bddd2;     // RPI 606
       	hex_bddd1_loc = hex_bddd2_loc; // RPI 606
       	skip_comma();
    	get_hex_bddd2(false);  
    	obj_code = obj_code + hex_bddd1 + hex_bddd2; 
    	check_end_parms();
    	put_obj_text();
    	break;
    case 56:  // "RSY" 207/209 LOCG r1,s2,m3 oormbdddhhoo RPI 1125
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2); 
       	get_hex_reg();       	
       	skip_comma();
		get_hex_bdddhh2(); 
		if (tz390.op_code[bal_op_index].substring(0,4).equals("EBF3")   // EBF3 STOC   R1,D2(B2),M3
		 || tz390.op_code[bal_op_index].substring(0,4).equals("EBE3")   // EBE3 STOCG  R1,D2(B2),M3
		 || tz390.op_code[bal_op_index].substring(0,4).equals("EBE1")   // EBE1 STOCFH R1,D2(B2),M3
	     || tz390.op_code[bal_op_index].substring(0,4).equals("EBF2")   // EBF2 LOCC   R1,D2(B2),M3
		 || tz390.op_code[bal_op_index].substring(0,4).equals("EBE2")   // EBE2 LOGG   R1,D2(B2),M3
		 || tz390.op_code[bal_op_index].substring(0,4).equals("EBE0")){ // EBE0 LOCFH  R1,D2(B2),M3
			if (tz390.op_code[bal_op_index].length() == 5
			 && tz390.op_code[bal_op_index].substring(4,5) != "F"){
			    get_hex_op(5,1); // M3 for extended mnemonic like STOCE R1,D2(B2)
		    } else {
				skip_comma();
				get_hex_reg(); // M3
			}
		} else {
       	    skip_comma();
       	    get_hex_int(16);
		}
		// move 001BDDDDD3 to 0013BDDDDD
		obj_code = obj_code.substring(0,3)   // oo1 
		         + obj_code.substring(9,10)  // m3
				 + obj_code.substring(3,9);  // BDDDDD
		get_hex_op(3,2); 
    	check_end_parms();
    	put_obj_text();
    	break;
    case 57: // "RIE9" "AHIK" oo13IIII00Oo RPI 1125
        // r1,r3,i2
    	bal_op_ok = true;
    	loc_len = 6;
    	get_hex_op(1,2);  // OP1
    	get_hex_reg();    // R1
       	skip_comma();
       	get_hex_reg();    // R3
       	skip_comma();
       	get_hex_int(16);    // I2
   		get_hex_zero(2);  
       	get_hex_op(3,2);  // OP2
    	check_end_parms();
    	put_obj_text();
    	break;	
    case 58:  // "V-QST" VAS VR1,QR3,RS2(RT2) --> ooooqtvs RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        get_hex_reg();    // VR1
        skip_comma();
        get_hex_reg();    // QR3
        skip_comma();
        get_hex_reg();    // RS2
        get_exp_x();      // RT2
        // We now have oooovqst --> ooooqtvs
        obj_code = obj_code.substring(0,4)  // oooo
        + obj_code.substring(5,6)           // QR3
        + obj_code.substring(7,8)           // RT2
        + obj_code.substring(4,5)           // VR1
        + obj_code.substring(6,7);          // RS2
        check_end_parms();
        put_obj_text();
        break;
    case 59:  // "V-QV" RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        // Process operands
        check_end_parms();
        put_obj_text();
        break;
    case 60:  //60 "V-VST" VAE VR1,VR3,RS2(RT2) --> oooovtvs RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        get_hex_reg();    // VR1
        skip_comma();
        get_hex_reg();    // VR3
        skip_comma();
        get_hex_reg();    // RS2
        get_exp_x();      // RT2
        // We now have oooovvst --> oooovtvs
        obj_code = obj_code.substring(0,4)  // oooo
        + obj_code.substring(5,6)           // VR3
        + obj_code.substring(7,8)           // RT2
        + obj_code.substring(4,5)           // VR1
        + obj_code.substring(6,7);          // RS2
        check_end_parms();
        put_obj_text();
        break;
    case 61:  // "V-VV" RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        // Process operands
        check_end_parms();
        put_obj_text();
        break;
    case 62:  // "V-RRE" RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        // Process operands
        check_end_parms();
        put_obj_text();
        break;
    case 63:  // "V-RSE" RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 6;
        get_hex_op(1,4);
        // Process operands
        check_end_parms();
        put_obj_text();
        break;
    case 64:  // "V-S" VRCL D2(B2) --> oooobddd RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        get_hex_bddd2(true);
        check_end_parms();
        put_obj_text();
        break;
    case 65:  // "V-VR" RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        // Process operands
        check_end_parms();
        put_obj_text();
        break;
    case 66:  // "V-VS" RPI VF01
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 4;
        get_hex_op(1,4);
        // Process operands
        check_end_parms();
        put_obj_text();
        break;
    case 67:  // RR with 2 GPRs           // RPI 1209N
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 2;
        get_hex_op(1,2); 
        // MR, DR require operand1 even, all others dont care RPI 1209O
        if (obj_code.substring(0,2).equals("1C") // RPI 1209O
         || obj_code.substring(0,2).equals("1D") // RPI 1209O
            )                                    // RPI 1209O
           {get_hex_reg_even();                  // RPI 1209O
            }                                    // RPI 1209O
        else                                     // RPI 1209O
           {get_hex_reg();                       // RPI 1209O
            }                                    // RPI 1209O
        skip_comma();
        get_hex_reg();
        check_end_parms();
        put_obj_text();
        break;
    case 68:  // RR with 2 FPRs           // RPI 1209N
        // Even with old optables (e.g. DOS) HLASM will accept any register numbers!
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 2;
        get_hex_op(1,2);
        get_hex_reg();
        skip_comma();
        get_hex_reg();
        check_end_parms();
        put_obj_text();
        break;
    case 69:  // RR with 1 mask and 1 GPR // RPI 1209N
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 2;
        get_hex_op(1,2); 
        get_hex_reg();
        skip_comma();
        get_hex_reg();
        check_end_parms();
        put_obj_text();
        break;
    case 70:  // RR with 1 GPR            // RPI 1209N
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 2;
        get_hex_op(1,2); 
        get_hex_reg();
        obj_code = obj_code.concat("0");
        check_end_parms();
        put_obj_text();
        break;
    case 71:  // RR with 2 pairs of GPRs  // RPI 1209N
        // Register numbers must be even
        bal_op_ok = true;
        loc_ctr = (loc_ctr+1)/2*2;
        loc_start = loc_ctr;
        loc_len = 2;
        get_hex_op(1,2); 
        get_hex_reg_even();
        skip_comma();
        get_hex_reg_even();
        check_end_parms();
        put_obj_text();
        break;
    case 72:  // "BRX" 16  BER oomr
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 2;
    	get_hex_op(1,3);  // BCR OP includes mask
    	get_hex_reg();
    	check_end_parms();
 	    put_obj_text();
    	break;
    case 73:  // "RI-a" LLILL  ooroiiii //  RPI 1522
    	bal_op_ok = true;               //  RPI 1522
    	loc_ctr = (loc_ctr+1)/2*2;      //  RPI 1522
    	loc_start = loc_ctr;            //  RPI 1522
    	loc_len = 4;                    //  RPI 1522
    	get_hex_op(1,2);                //  RPI 1522
       	get_hex_reg();                  //  RPI 1522
       	get_hex_op(3,1);                //  RPI 1522
    	skip_comma();                   //  RPI 1522
    	get_hex_halfword();             //  RPI 1522
    	check_end_parms();              //  RPI 1522
    	put_obj_text();                 //  RPI 1522
    	break;                          //  RPI 1522
    case 74:   // RPI 2202 "RRR" SELRm (r1,r2,r3,m4 maps to oooo3m12)
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);   //  0 oooo
     	get_hex_reg();       //  4 r1
     	skip_comma();
     	get_hex_reg();       //  5 r2
     	skip_comma();
     	get_hex_reg();       // 6 r3
     	if (!bal_abort && exp_next_char(',')){
     		skip_comma();
     		get_hex_reg();   // 7 m4
     	} else {
     		if ((tz390.op_code[bal_op_index].length() == 5)){  
   		       get_hex_op(5,1);  // M3
    	    } else {
     		   get_hex_zero(1);  // m3=0 
     	    }
     	}
    	obj_code = obj_code.substring(0,4)  // oooo
     	    + obj_code.substring(6,7)   // r3
	    	+ obj_code.substring(7,8)   // m4
	    	+ obj_code.substring(4,5)  // r1
	    	+ obj_code.substring(5,6); // r2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 75:   // RPI 2202 "IE" NIAI I1,I2 maps to oooo0012
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 4;
    	get_hex_op(1,4);   //  0 oooo
     	get_hex_zero(2);       //  4 r1
     	get_hex_reg();       //  i1
     	skip_comma();
     	get_hex_reg();       // i2
    	check_end_parms();
    	put_obj_text();
    	break;	
    case 76: // C5 mii BPRP r1,i2,i3 RPI 2202
    	// map r1,i2,i3 into 001222333333
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  0 oo
    	get_hex_reg();                // r1        
    	skip_comma();       
     	get_hex_relative_offset(12);       //  i2 12 bits
     	skip_comma();
     	get_hex_relative_offset(24);       // i3  24 bits
    	check_end_parms();
    	put_obj_text();
    	break;
    case 77: // C7 smi BPP r1,i2,d3(b3) RPI 2202
    	// map m1,i2,d3(b3) into 0010bddd2222
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  0 oo
    	get_hex_reg();       // r1        
     	get_hex_zero(1);     //  0
     	skip_comma();
		get_hex_relative_offset(16);        // i2 16 bits
		skip_comma();
		get_hex_bddd2(true);     	
     	obj_code = obj_code.substring(0,4)  // oo1o
         	    + obj_code.substring(8,12)   // bddd
    	    	+ obj_code.substring(4,8); // i2
    	check_end_parms();
    	put_obj_text();
    	break;
    case 78: // "E601=VLEBRH,78,734", // E601 VRXVLEBRH V1,D2(X2,B2),M3 RPI 2202
    	// map V1,D2(X2,B2),M3  into 001xbddd3x00
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  oo
    	get_hex_vreg(1);       //  v1
    	skip_comma();
    	get_hex_xbddd(); // xbddd
    	if (!bal_abort && exp_next_char(',')){
     		skip_comma();
     		get_hex_reg();   // m4
     	} else {
     		if (tz390.op_code[bal_op_index].length() < 5) {
     			get_hex_zero(1);
     	    } else {
     		    get_hex_op(5,1); // m4
     	    }
     	}
    	get_hex_vreg_rxb();    // RXB
     	get_hex_op(3,2);         // oo
    	check_end_parms();
    	put_obj_text();
    	break;
    case 79: // "E634=VPKZ,79,735", // E634 VPKZ V1,D2(B2),I3 RPI 2202
    	// map V1,D2(B2),I3 from oo1bdddii into 0033bddd1x00
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  oo
    	get_hex_vreg(4);       //  V1 in pos 4 RPI 2225
    	skip_comma();
    	get_hex_bddd2(true); // bddd
   		skip_comma();
   		get_hex_byte();   // I3
   		// oo1bddd33
     	obj_code = obj_code.substring(0,2)  // oo
     			+ obj_code.substring(7,9)          // i2
         	    + obj_code.substring(3,7)         // bddd
    	    	+ obj_code.substring(2,3);        // v1
    	get_hex_vreg_rxb();    // RXB
     	get_hex_op(3,2);         // oo
    	check_end_parms();
    	put_obj_text();
    	break;
    case 80: // "E637=VLRLR,80,736", // E637 VRSd VLRLR V1,R3,D2(B2) RPI 2202
    	// map V1,R3,D2(B2)  into oo03bddd1x00
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  oo
		if ( tz390.op_code[bal_op_index].substring(0,4).equals("E721")){         // E721 VRSc VLGV R1,V3,D2(B2),M4 RPI 2202
           // r1, v3 
    	   		get_hex_reg();        // R1
    	   		skip_comma();
    	   		get_hex_vreg(2);       // V3 in v2 rxb position RPI 2216 
       		 skip_comma();
       		 get_hex_bddd2(true); // bddd
         	if (!bal_abort && exp_next_char(',')){
         		skip_comma();
         		get_hex_reg();   // M4
         	} else {
				if ((tz390.op_code[bal_op_index].length() > 4)) {
     	           get_hex_op(5,1);   //  M4
			    } else {
				   get_hex_zero(1);   // M4
		    	}
			}
	} else if ( tz390.op_code[bal_op_index].substring(0,4).equals("E73F")    // E73F VRSb VSTL V1,R3,D2(B2) RPI 2202
	         || tz390.op_code[bal_op_index].substring(0,4).equals("E737")    // E737 VRSb VLL  V1,R3,D2(B2) RPI 2202	
   	         || tz390.op_code[bal_op_index].substring(0,4).equals("E722")) { // E722 VRSc VLVG V1,R3,D2(B2),M4 RPI 2202	 
    	   		get_hex_vreg(1);        // V1
    	   		skip_comma();
    	   		get_hex_reg();          // R3
       		 skip_comma();
       		 get_hex_bddd2(true); // bddd
         	if (!bal_abort && exp_next_char(',')){
         		skip_comma();
         		get_hex_reg();   // M4
         	} else {
				if ((tz390.op_code[bal_op_index].length() > 4)) {
     	           get_hex_op(5,1);   //  M4
			    } else {
				   get_hex_zero(1);   // M4
		    	}
         	}       	
			
	} else if ( tz390.op_code[bal_op_index].substring(0,4).equals("E73E")   // E73E VRSa VSTM V1,V3,D2(B2),,M4 RPI 2202
	         || tz390.op_code[bal_op_index].substring(0,4).equals("E736")){ // E736 VRSa VLM  V1,V3,D2(B2),,M4 RPI 2202
    	   		get_hex_vreg(1);        // V1
    	   		skip_comma();
    	   		get_hex_vreg(2);       // V3 in rxb v2 pos RPI 2216
       		 skip_comma();
       		 get_hex_bddd2(true); // bddd
         	if (!bal_abort && exp_next_char(',')){
         		skip_comma();
         		get_hex_reg();   // M4
         	} else {
				if ((tz390.op_code[bal_op_index].length() > 4)) {
     	           get_hex_op(5,1);   //  M4
			    } else {
				   get_hex_zero(1);   // M4
		    	}
         	}
    	 } else if ( tz390.op_code[bal_op_index].substring(0,4).equals("E730")     // E730 VRSa VERL  V1,V3,D2(B2),M4 RPI 2202
		          || tz390.op_code[bal_op_index].substring(0,4).equals("E733")     // E733 VRSa VERLL V1,V3,D2(B2),M4 RPI 2202
    			  ||  tz390.op_code[bal_op_index].substring(0,4).equals("E738")    // E738 VRSa VESRL V1,V3,D2(B2),M4 RPI 2202
    			  ||  tz390.op_code[bal_op_index].substring(0,4).equals("E73A")){  // E73A VRSa VESRA V1,V3,D2(B2),M4 RPI 2202
    		 get_hex_vreg(1);   // V1
 	   		skip_comma();
 	   		get_hex_vreg(2);    // V3 in rxb v2 pos RPI 2216
    		 skip_comma();
    		 get_hex_bddd2(true); // bddd
      	     if (!bal_abort && exp_next_char(',')){
      		    skip_comma();
      		    get_hex_reg();   // M4
      	     } else {
      	     	if ((tz390.op_code[bal_op_index].length() == 5)) {
           		   get_hex_op(5,1); // M4
           	    } else {
                   skip_comma();   
                   get_hex_reg();      // M4
           	    }  
      	     }
    	 } else {
    		 get_hex_zero(1);   //  0
    		 get_hex_vreg(4);     //  V1 in v4 pos RPI 2225 for VLRLR
    		 skip_comma();
    		 get_hex_reg();       // R3
    		 skip_comma();
    		 get_hex_bddd2(true); // bddd
    	    // oo013bddd MOVE v1 after bddd
     	    obj_code = obj_code.substring(0,3)  // oo0
     			+ obj_code.substring(4,5)        // R3
         	    + obj_code.substring(5,9)         // bddd
    	    	+ obj_code.substring(3,4);         // v1
    	 }
    	get_hex_vreg_rxb();    // RXB
     	get_hex_op(3,2);         // oo
    	check_end_parms();
    	put_obj_text();
    	break;
    case 81: // "E649=VLIP,81,737", // E649 VRIh VLIP V1,I2,I3 RPI 2202
    	// map V1,R3,D2(B2)  into oo03bddd1x00
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  oo
    	get_hex_vreg(1);       //  V1
	     if (tz390.op_code[bal_op_index].substring(0,4).equals("E772")     // E772 VRId VERIM V1,V2,V3,I4,M5 RPI 2202
		  || tz390.op_code[bal_op_index].substring(0,4).equals("E786")     // E786 VRId VSLD  V1,V2,V3,I4 RPI 2202
		  || tz390.op_code[bal_op_index].substring(0,4).equals("E787")) {  // E787 VRId VSRD  V1,V2,V3,I4 RPI 2202
         skip_comma();   
         get_hex_vreg(2);      // V2
         skip_comma();   
         get_hex_vreg(3);      // V3
         get_hex_zero(1);  
         skip_comma();
         get_hex_byte();      // I4
        	if ((tz390.op_code[bal_op_index].length() == 5)) {
        		get_hex_op(5,1); // M5
        	} else {
				if (!bal_abort && exp_next_char(',')){
      		       skip_comma();
      		       get_hex_reg();   // M4
      	        } else {
      	     	   get_hex_zero(1);       
      	        }
        	}
	     } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E777")) {  // E777 VLId VSLDB V1,V2,V3,I4 RPI 2202
   	         skip_comma();   
   	         get_hex_vreg(2);      // V2
   	         skip_comma();   
   	         get_hex_vreg(3);      // V3
   	         get_hex_zero(1);  
   	         skip_comma();
   	         get_hex_byte();      // I4
   	         get_hex_zero(1);
	     } else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E658") // E658 VRIi VCVD V1,R2,I3,M4
        	|| tz390.op_code[bal_op_index].substring(0,4).equals("E65A")) { // E65A VRIi VCVDG
        	skip_comma();
             get_hex_reg();        // r2
             get_hex_zero(2);      // 00   
             skip_comma();
             get_hex_byte();       // I3
             skip_comma();
             get_hex_reg();        // M4
             // swap I3 and M4 oo1200334 to oo1200433
          	obj_code = obj_code.substring(0,6)  // oo1200
         			+ obj_code.substring(8,9)          // M3
             	    + obj_code.substring(6,8);         // I3
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E659") // E659 VRIg VSRP V1,V2,I3,I4,M5
        		||    tz390.op_code[bal_op_index].substring(0,4).equals("E65B")) {  // E65B VRIg VPSOP
        	skip_comma();
            get_hex_vreg(2);       // V2
            skip_comma();   
            get_hex_byte();      // I3
            skip_comma();
            get_hex_byte();      // I4
            skip_comma();
            get_hex_reg();       // M5
            // move I4 before M5 then i3  001233445 to oo1244533
          	obj_code = obj_code.substring(0,4)  // oo12
         			+ obj_code.substring(6,9)   // I4,M5
             	    + obj_code.substring(4,6);  // I3
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E74D")) { // E74D VRIc VREP V1,V3,I2,M4
        	skip_comma();
            get_hex_vreg(2);       // V3 in v2 rxb pos  RPI 2216
            skip_comma();   
            get_hex_int(16);      // I2
           	if ((tz390.op_code[bal_op_index].length() == 5)) {
           		get_hex_op(5,1); // M4
           	} else {
                skip_comma();   
                get_hex_reg();      // M4
           	}   	
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E74A")) { // E74A VRIe VFTCI V1,V2,I3,M4,M5
        	skip_comma();
            get_hex_vreg(2);       // V2
            skip_comma();   
            get_hex_int(12);      // I3
           	if ((tz390.op_code[bal_op_index].length() == 6)) {
           		get_hex_op(5,2); // M4,M5
           	} else {
                skip_comma();   
                get_hex_reg();      // M4
                skip_comma();
                get_hex_reg();      // M5
           	} 
		    obj_code = obj_code.substring(0,7)  // oo12333 RPI 2225 reverse m4 and m5
		     			+ obj_code.substring(8,9)          // M5
		         	    + obj_code.substring(7,8);         // M4			
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E740") // E740 VRIa VLEIB V1,I2,M3 RPI 2202
        		|| tz390.op_code[bal_op_index].substring(0,4).equals("E745") // E745 VRIa VREPI V1,I2,M3 RPI 2202
        		|| tz390.op_code[bal_op_index].substring(0,4).equals("E741") // E741 VRIa VLEIH V1,I2,M3 RPI 2202
        		||    tz390.op_code[bal_op_index].substring(0,4).equals("E742")  // E742 VRIa VLEIG V1,I2,M3 RPI 2202
        		||    tz390.op_code[bal_op_index].substring(0,4).equals("E743")) {  // E743 VRIa VLEIF V1,I2,M3 RPI 2202
            get_hex_zero(1);   //  0
            skip_comma();   
            get_hex_int(16);      // I2
           	if ((tz390.op_code[bal_op_index].length() == 5)) {
           		get_hex_op(5,1); // M3
           	} else {
                skip_comma();   
                get_hex_reg();      // M3
           	}      
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E744")) {  // E744 VRIa VGBM V1,I2 RPI 2202
            get_hex_zero(1);   //  0
           	if ((tz390.op_code[bal_op_index].length() == 5)) {
				if (tz390.op_code[bal_op_index].charAt(4) == '1'){ 
			       obj_code = obj_code + "FFFF";   // vone v1
			    } else {
				   obj_code = obj_code + "0000";  // vzero v1
				}
           	} else {
                skip_comma();   
                get_hex_int(16);      // I2
           	}
            get_hex_zero(1);       // 0 	
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E746")) {  // E746 VRIb VGM V1,I2,I3,M4 RPI 2202
            get_hex_zero(1);   //  0
            skip_comma();   
            get_hex_byte();      // I2
            skip_comma();   
            get_hex_byte();      // I3
           	if ((tz390.op_code[bal_op_index].length() == 5)) {
           		get_hex_op(5,1); // M4
           	} else {
                skip_comma();   
                get_hex_reg();      // M4
           	}
       
        } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E671")        // E671 VRIf VAP    v1,v2,v3,i4,m5
        	   ||      tz390.op_code[bal_op_index].substring(0,4).equals("E673")    // E673 VRIf VSP
        	   ||      tz390.op_code[bal_op_index].substring(0,4).equals("E678")    // E678 VRIf VMP
     	       ||      tz390.op_code[bal_op_index].substring(0,4).equals("E679")    // E679 VRIf VMSP
     	       ||      tz390.op_code[bal_op_index].substring(0,4).equals("E67A")    // E67A VRIf VDP
     	       ||      tz390.op_code[bal_op_index].substring(0,4).equals("E67B")    // E67B VRIf VRP
  	           ||      tz390.op_code[bal_op_index].substring(0,4).equals("E67E")){  // E67E VRIf VSDP
		    	skip_comma();
		        get_hex_vreg(2);       // V2
		        skip_comma();   
		        get_hex_vreg(3);       // V3
		        get_hex_zero(1);   //  0
		        skip_comma();
		        get_hex_byte();      // I4
		        skip_comma();
		        get_hex_reg();       // M5
		        // move m5 before i4
		      	obj_code = obj_code.substring(0,6)  // oo1230
		     			+ obj_code.substring(8,9)          // M5
		         	    + obj_code.substring(6,8);         // I4
        }  else {  // E649 VRIh VLIP
        	get_hex_zero(1);   //  0
       		skip_comma();
       		get_hex_int(16);       // I2
        	skip_comma();
        	get_hex_int(4);       // I3
        }
    	get_hex_vreg_rxb();    // RXB
     	get_hex_op(3,2);         // oo
    	check_end_parms();
    	put_obj_text();
    	break;
    case 82: // "E650=VCVB,82,738", // E650 VRRi VCVB R1,V2,M3,M4 RPI 2202
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  oo
      if ( tz390.op_code[bal_op_index].substring(0,4).equals("E650")     // E650 VRRi VCVB R1,V2,M3,M4
    	 || tz390.op_code[bal_op_index].substring(0,4).equals("E652")) { // E652 VRRi VCVBG R1,V2,M3,M4
   		get_hex_reg();        // R1
   		skip_comma();
   		get_hex_vreg(2);       // V2
    	get_hex_zero(2);   //  00
    	skip_comma();
    	get_hex_reg();       // M3
    	if (!bal_abort && exp_next_char(',')){
     		skip_comma();
     		get_hex_reg();   // M4
     	} else {
     		get_hex_zero(1); // M4
     	}
    	get_hex_zero(1);    // 0
      } else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E65F")) { // E65F VRRg VTP V1
          get_hex_zero(1);
          get_hex_vreg(2);        // V1
          get_hex_zero(5);
      } else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E677")) {     // E677 VRRh VCP V1,V2,M3
          get_hex_zero(1);
    	  get_hex_vreg(2);        // V1 in v2 pos RPI 2225
          skip_comma();
          get_hex_vreg(3);        // V2 in v3 pos rpi 2225
          get_hex_zero(1);
          skip_comma();
          get_hex_reg();        // M3
          get_hex_zero(2);
      } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E750")         // E750 VRRa VPOPCT V1,V2,M3
    		     ||  tz390.op_code[bal_op_index].substring(0,4).equals("E752")     // E752 VRRa VCTZ V1,V2,M3
    		     ||  tz390.op_code[bal_op_index].substring(0,4).equals("E753")     // E752 VRRa VCLZ V1,V2,M3
    	         ||  tz390.op_code[bal_op_index].substring(0,4).equals("E75F")){   // E75F VRRa VSEG V1,V2,M3 
    	  get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          get_hex_zero(4);
          if (!bal_abort && exp_next_char(',')){
       		skip_comma();
       		get_hex_reg();   // M3
       	  } else {
        	get_hex_op(5,1);   //  M3
       	  }
      } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E760")           // E760 VRRc VMRL V1,V2,V3,M4
 	             ||  tz390.op_code[bal_op_index].substring(0,4).equals("E761")       // E761 VRRc VMRH V1,V2,V3,M4
 	             ||  tz390.op_code[bal_op_index].substring(0,4).equals("E764")       // E764 VRRc VSUM V1,V2,V3,M4
    	         ||  tz390.op_code[bal_op_index].substring(0,4).equals("E765")       // E765 VRRc VSUMG V1,V2,V3,M4
 	             ||     tz390.op_code[bal_op_index].substring(0,4).equals("E767")    // E767 VRRc VSUMQ V1,V2,V3,M4 RPI 2202
 	             ||     tz390.op_code[bal_op_index].substring(0,4).equals("E770")    // E770 VRRc VESLV V1,V2,V3,M4 RPI 2202
 	             ||     tz390.op_code[bal_op_index].substring(0,4).equals("E773")    // E773 VRRc VERLLV V1,V2,V3,M4 RPI 2202
    	         ||     tz390.op_code[bal_op_index].substring(0,4).equals("E778")    // E778 VRRc VERSRLV V1,V2,V3,M4 RPI 2202
	             ||     tz390.op_code[bal_op_index].substring(0,4).equals("E77A")    // E77A VRRc VERSRAV V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E784")    // E784 VRRc VPDI    V1,V2,V3,M4 RPI 2202
	             ||     tz390.op_code[bal_op_index].substring(0,4).equals("E785")    // E785 VRRc VBPERM  V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E794")    // E794 VRRc VPK    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A1")    // E7A1 VRRc VMLH   V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A2")    // E7A2 VRRc VML    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A3")    // E7A3 VRRc VMH    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A4")    // E7A4 VRRc VMLE   V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A5")    // E7A5 VRRc VMLO   V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A6")    // E7A6 VRRc VME    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7A7")    // E7A7 VRRc VMO    V1,V2,V3,M4 RPI 2202 
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7B4")    // E7B4 VRRc VGFM   V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7F0")    // E7F0 VRRc VFAVGL V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7F1")    // E7F1 VRRc VFACC  V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7F2")    // E7F2 VRRc VFAVG  V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7F3")    // E7F3 VRRc VFA    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7F5")    // E7F5 VRRc VFSCBI V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7F7")    // E7F7 VRRc VFS    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7FC")    // E7FC VRRc VMNL   V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7FD")    // E7FD VRRc VMXL   V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7FE")    // E7FE VRRc VMN    V1,V2,V3,M4 RPI 2202
				 ||     tz390.op_code[bal_op_index].substring(0,4).equals("E7FF")) { // E7FF VRRc VMX    V1,V2,V3,M4 RPI 2202
	   get_hex_vreg(1);        // V1
       skip_comma();
       get_hex_vreg(2);        // V2
       skip_comma();
       get_hex_vreg(3);        // V3
       get_hex_zero(3);
       if (!bal_abort && exp_next_char(',')){
    		skip_comma();
    		get_hex_reg();   // M4
       } else {
			if ((tz390.op_code[bal_op_index].length() == 5)) {
     	       get_hex_op(5,1);   //  M4
			} else {
				get_hex_zero(1);
			}
    	}
	  } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E7E2")    // E7E2 VRRc VFS V1,V2,V3,M4,M5
	          || tz390.op_code[bal_op_index].substring(0,4).equals("E7E3")    // E7E3 VRRc VFA V1,V2,V3,M4,M5
			  || tz390.op_code[bal_op_index].substring(0,4).equals("E7E5")    // E7E5 VRRc VFD V1,V2,V3,M4,M5
			  || tz390.op_code[bal_op_index].substring(0,4).equals("E7E7")) { // E7E7 VRRc VFM V1,V2,V3,M4,M5
	      get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          skip_comma();
          get_hex_vreg(3);        // V3
          get_hex_zero(2);
          if (!bal_abort && exp_next_char(',')){
    		 skip_comma();
    		 get_hex_reg();   // M4
    	  } else {
			if ((tz390.op_code[bal_op_index].length() >= 5)) {
     	       get_hex_op(5,1);   //  M4
			} else {
				get_hex_zero(1);
			}
    	  }
		  if (!bal_abort && exp_next_char(',')){
    		 skip_comma();
    		 get_hex_reg();   // M5
    	  } else {
			if ((tz390.op_code[bal_op_index].length() >= 6)) {
     	       get_hex_op(6,1);   //  M5
			} else {
				get_hex_zero(1);
			}
    	  }
		  // move I4 before M5 001230034 to oo124054
          	obj_code = obj_code.substring(0,7)  // oo12300
         			+ obj_code.substring(8,9)   // M5
             	    + obj_code.substring(7,8);  // M4
	  } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E7E8")       // E7E8 VRRc VFCE  V1,V2,V3,M4,M5,M6
              || tz390.op_code[bal_op_index].substring(0,4).equals("E7EA")       // E7E8 VRRc VFCHE V1,V2,V3,M4,M5,M6  
			  || tz390.op_code[bal_op_index].substring(0,4).equals("E7EB")       // E7EB VRRc VFCH  V1,V2,V3,M4,M5,M6 
	          || tz390.op_code[bal_op_index].substring(0,4).equals("E7EE")       // E7EE VRRC VFMIN V1,V2,V3,M4,M5,M6 
			  || tz390.op_code[bal_op_index].substring(0,4).equals("E7EF")) {    // E7EF VRRc VFMAX V1,V2,V3,M4,M5,M6
	      get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          skip_comma();
          get_hex_vreg(3);        // V3
          get_hex_zero(1);
          if ((tz390.op_code[bal_op_index].length() >= 5)) {
              	get_hex_op(5,1);    // M3
          } else {  
                if (!bal_abort && exp_next_char(',')){
       		       skip_comma();
       		       get_hex_reg();   // M3
				} else {
					get_hex_zero(1); // M3 = 0?
				}
          }
		  if ((tz390.op_code[bal_op_index].length() >= 6)) {
           	  get_hex_op(6,1);    // M4
          } else { 
		     if (!bal_abort && exp_next_char(',')){
       		    skip_comma();
       		    get_hex_reg();   // M4
             } else {
                get_hex_zero(1);    // M4
             } 
          }
          if (!bal_abort && exp_next_char(',')){
       		 skip_comma();
       		 get_hex_reg();   // M5
          } else {
        		if ((tz390.op_code[bal_op_index].length() == 7)) {
               		get_hex_op(7,1);    // M5
               	} else {
                    get_hex_zero(1);    // M5
               	}
         }
		  // move 001230456 to 001230654
		  obj_code = obj_code.substring(0,6)  // op1+v1,v2,v3,0
 	      + obj_code.substring(8,9)           // m6
    	  + obj_code.substring(7,8)           // m5
		  + obj_code.substring(6,7);          // m4		  
      } else if (tz390.op_code[bal_op_index].substring(0,4).equals("E762")                // E762 VRRf VLVGP V1,R2,R3 RPI 2202 
    	      ||     tz390.op_code[bal_op_index].substring(0,4).equals("E766")            // E766 VRRc VCKSM V1,V2,V3 RPI 2202  
    	      ||     tz390.op_code[bal_op_index].substring(0,4).equals("E768")            // E768 VRRc VN V1,V2,V3 RPI 2202  
    	      ||     tz390.op_code[bal_op_index].substring(0,4).equals("E769")            // E769 VRRc VNC V1,V2,V3 RPI 2202   
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E76A")            // E76A VRRc VO V1,V2,V3 RPI 2202  
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E76B")            // E76B VRRc VNO V1,V2,V3 RPI 2202  
    	      ||     tz390.op_code[bal_op_index].substring(0,4).equals("E76C")            // E76C VRRc VNX V1,V2,V3 RPI 2202   
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E76D")            // E76D VRRc VX V1,V2,V3 RPI 2202  
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E76E")            // E76E VRRc VNN V1,V2,V3 RPI 2202  
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E76F")            // E76F VRRc VOC V1,V2,V3 RPI 2202  
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E774")            // E774 VRRc VSL V1,V2,V3 RPI 2202 
	          ||     tz390.op_code[bal_op_index].substring(0,4).equals("E775")            // E775 VRRc VSLB V1,V2,V3 RPI 2202 
              ||     tz390.op_code[bal_op_index].substring(0,4).equals("E77C")            // E77C VRRc VSRL V1,V2,V3 RPI 2202  
              ||     tz390.op_code[bal_op_index].substring(0,4).equals("E77D")            // E77D VRRc VSRLB V1,V2,V3 RPI 2202  
              ||     tz390.op_code[bal_op_index].substring(0,4).equals("E77E")            // E77E VRRc VSRA V1,V2,V3 RPI 2202 
              ||     tz390.op_code[bal_op_index].substring(0,4).equals("E77F")){          // E77F VRRc VSRAB V1,V2,V3 RPI 2202
	   get_hex_vreg(1);        // V1
       skip_comma();
       get_hex_vreg(2);        // V2
       if (!bal_abort && exp_next_char(',')){
  		  skip_comma();
  		  get_hex_vreg(3);       // V3
       } else {
		 if (tz390.op_name[bal_op_index].equals("VNOT")){ // RPI 2226 VNOT V1,V2,V2 extention of VNO
			obj_code = obj_code.substring(0,4)  // op1+v1,v2
 	          + obj_code.substring(3,4);        // v3 = v2
			if ((vreg_rxb & 4) == 4){ 
			   vreg_rxb  = vreg_rxb + 2; // set v3 rxb bit
			}
	     } else {
   	        get_hex_zero(1);  
         }
	   }		 
       get_hex_zero(4);
      } else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E756")) {     // E756 VRRa VLR V1,V2
    	  get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          get_hex_zero(5);
      } else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E75C")) {     // E75C VRRa VISTR V1,V2,M3,M5
    	  get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          get_hex_zero(2);
          if (!bal_abort && exp_next_char(',')){
       		 skip_comma();
       		 get_hex_reg();   // M3
          } else {
        	get_hex_op(5,1);   //  M3
          }
          get_hex_zero(1);    
          if (!bal_abort && exp_next_char(',')){
       		 skip_comma();
       		 get_hex_reg();   // M5
          } else {
        		if ((tz390.op_code[bal_op_index].length() == 6)) {
               		get_hex_op(6,1);    // M5
               	} else {
                    get_hex_zero(1);    // M5
               	}
          }
		  obj_code = obj_code.substring(0,6)  // op1+v1,v2,0,0
 	      + obj_code.substring(8,9)           // m5
    	  + obj_code.substring(7,8)           // m4
		  + obj_code.substring(6,7);          // m3
	  } else  if (  tz390.op_code[bal_op_index].substring(0,4).equals("E7C0")        // E7C0 VRRa VCLFP/VCLGD V1,V2,M3,M4,M5 
	             || tz390.op_code[bal_op_index].substring(0,4).equals("E7C1")        // E7C1 VRRa VCDLG V1,V2,M3,M4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7C2")        // E7C2 VRRa VCGD  V1,V2,M3,M4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7C3")        // E7C3 VRRa VCDG  V1,V2,M3,M4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7C4")        // E7C4 VRRa VFLL  V1,V2,M3,M4
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7C5")        // E7C5 VRRa VFLR  V1,V2,M3,M4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7C7")        // E7C7 VRRa VFI   V1,V2,M3,M4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7CA")        // E7CA VRRa WFK   V1,V2,M3,M4
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7CB")        // E7CB VRRa WFC   V1,V2,M3,M4
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7CC")        // E7CC VRRa VFPSO V1,V2,M3,M4
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7CE")        // E7CE VRRa VFSQ   V1,V2,M3,M4
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7D4")        // E7D4 VRRa VUPLL  V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7D5")        // E7D5 VRRa VUPLH  V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7D6")        // E7D6 VRRa VUPL   V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7D7")        // E7D7 VRRa VUPH   V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7D8")        // E7D8 VRRa VTM   V1,V2
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7D9")        // E7D9 VRRa VECL  V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7DB")        // E7DB VRRa VEC   V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7DE")        // E7DE VRRa VLC   V1,V2,M3
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7DF")) {     // E7DF VRRa VLP   V1,V2,M3
    	  get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          get_hex_zero(2);  // assume m3,m4,m5 with m3 possibly implied by extended mnemonic
		  if ((tz390.op_code[bal_op_index].length() >= 5)) {
              	get_hex_op(5,1);    // M3
          } else {  
                if (!bal_abort && exp_next_char(',')){
       		       skip_comma();
       		       get_hex_reg();   // M3
				} else {
					get_hex_zero(1); // M3 = 0?
				}
          }
		  if ((tz390.op_code[bal_op_index].length() >= 6)) {
           	  get_hex_op(6,1);    // M4 option
              if (   tz390.op_code[bal_op_index].substring(0,4).equals("E7C0")
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E7C1")
			      || tz390.op_code[bal_op_index].substring(0,4).equals("E7C2")
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E7C3")
			      || tz390.op_code[bal_op_index].substring(0,4).equals("E7C5")
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E7C7")
			     ){ // #230 OR option with M4
			     if (!bal_abort && exp_next_char(',')){
       		        skip_comma();
       		        get_hex_reg();   // M4			
                 } else {
                    get_hex_zero(1);   
				 }	                 
			     or_last_op();    // OR last op with prev op and remove last op
			  }					  
          } else { 
		     if (!bal_abort && exp_next_char(',')){
       		    skip_comma();
       		    get_hex_reg();   // M4			
             } else {
                get_hex_zero(1);    // M4
             } 
          }
          if (!bal_abort && exp_next_char(',')){
       		 skip_comma();
       		 get_hex_reg();   // M5
          } else {
        		if ((tz390.op_code[bal_op_index].length() == 7)) {
               		get_hex_op(7,1);    // M5
               	} else {
                    get_hex_zero(1);    // M5
               	}
         }
		 // map 001200345 to 001200543
		 obj_code = obj_code.substring(0,6)  // op1+v1,v2,0,0
 	      + obj_code.substring(8,9)           // m5
    	  + obj_code.substring(7,8)           // m4
		  + obj_code.substring(6,7);          // m3
      } else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E780")        // E780 VRRb VFEE  V1,V2,V3,M4,M5
                  || tz390.op_code[bal_op_index].substring(0,4).equals("E781")     // E781 VRRb VFENE V1,V2,V3,M4,M5
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E782")     // E782 VRRb VFAE  V1,V2,V3,M4,M5
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E795")     // E795 VRRb VPKLS V1,V2,V3,M4,M5
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E797")       // E797 VRRb VPKS  V1,V2,V3,M4,M5
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E7F8")       // E7F8 VRRb VCEQ  V1,V2,V3,M4,M5
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E7F9")       // E7F9 VRRb VCHL  V1,V2,V3,M4,M5
				  || tz390.op_code[bal_op_index].substring(0,4).equals("E7FB")){     // E7FB VRRb VCH  V1,V2,V3,M4,M5
          get_hex_vreg(1);        // V1
          skip_comma();
          get_hex_vreg(2);        // V2
          skip_comma();
          get_hex_vreg(3);        // V3
          get_hex_zero(1);
          if ((tz390.op_code[bal_op_index].length() == 4)) {
           		skip_comma();
        		get_hex_reg(); // M4
           		get_hex_zero(1);
				if (!bal_abort && exp_next_char(',')){
       		       skip_comma();
       		       get_hex_reg();   // M5
                } else {
        	       get_hex_zero(1);  //  M5
                }
          } else if ((tz390.op_code[bal_op_index].length() == 5)) {
       		   get_hex_op(5,1); // M4
       		   get_hex_zero(1);
               if (!bal_abort && exp_next_char(',')){
       		       skip_comma();
       		       get_hex_reg();   // M5
               } else {
        	       get_hex_zero(1);  //  M5
               }
         } else if ((tz390.op_code[bal_op_index].length() == 6)) {
      		   get_hex_op(5,1); // M4
      		   get_hex_zero(1);
			   if (!bal_abort && exp_next_char(',')){
       		       skip_comma();
       		       get_hex_reg();   // M5
               } else {
        	       get_hex_op(6,1); // M5
			   }
       	  } 
		  // MAP 001230405 TO OO1230504
		  obj_code = obj_code.substring(0,6)  // op1+v1,v2,v3,0
 	      + obj_code.substring(8,9)           // m5
    	  + obj_code.substring(5,7);          // 0,m4
		} else  if (tz390.op_code[bal_op_index].substring(0,4).equals("E78A")    // E78A VRRd VSTRC V1,V2,V3,V4,M5,M6
		         || tz390.op_code[bal_op_index].substring(0,4).equals("E78B")    // E78B VRRd VSTRS V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E78C")    // E78C VRRe VPERM V1,V2,V3,V4
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E78D")    // E78D VRRe VSEL  V1,V2,V3,V4
		         || tz390.op_code[bal_op_index].substring(0,4).equals("E78E")      // E78E VRRe VFMS  V1,V2,V3,V4,M5,M6 RPI 2225
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E78F")      // E78F VRRe VFMA  V1,V2,V3,V4,M5,M6 RPI 2225
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E79E")      // E79E VRRb VFNMS V1,V2,V3,V4,M5,M6 RPI 2225
   	             || tz390.op_code[bal_op_index].substring(0,4).equals("E79F")    // E79F VRRb VFNMA V1,V2,V3,V4,M5,M6	RPI 2225
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7A9")    // E7A9 VRRd VMALH V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7AA")    // E7AA VRRd VMAL  V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7AB")    // E7AB VRRd VMAH  V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7AC")    // E7AC VRRd VMALE V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7AD")    // E7AD VRRd VMALO V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7AE")    // E7AE VRRd VMAE  V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7AF")    // E7AF VRRe VMAO  V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7B8")    // E7B8 VRRd VMSL  V1,V2,V3,V4,M5,M6
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7B9")    // E7B9 VRRd VACCC V1,V2,V3,V4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7BB")    // E7BB VRRd VAC   V1,V2,V3,V4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7BC")    // E7BC VRRd VGFMA V1,V2,V3,V4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7BD")    // E7BD VRRd VSBCBI V1,V2,V3,V4,M5
				 || tz390.op_code[bal_op_index].substring(0,4).equals("E7BF")) { // E7BF VRRd VSBI   V1,V2,V3,V4,M5
		  // reformat OO,V1,V2,V3,V4,M5 to oo,v1,v2,v3,m5,m6,0,v4,rxb,oo
		  get_hex_vreg(1);     // V1
   		  skip_comma();
          get_hex_vreg(2);     // V2
    	  skip_comma();
    	  get_hex_vreg(3);     // V3
		  skip_comma();
    	  get_hex_vreg(4);     // V4
		  if (tz390.op_code[bal_op_index].length() > 4) {
       		 get_hex_op(5,1); // M5
		  } else {
			 if (!bal_abort && exp_next_char(',')){
  		        skip_comma();
  		        get_hex_reg();    // M5 
             } else {
   	           get_hex_zero(1);   // M5 = 0 
             }
		  }
		  if (!bal_abort && exp_next_char(',')){ 
       		 skip_comma();
             get_hex_reg();   // M6
			 if (tz390.op_code[bal_op_index].substring(0,4).equals("E78A")
			  && tz390.op_code[bal_op_index].length() > 5){ // #230 OR option with VSTRC? P6
			  get_hex_op(6,1); // get constant to OR with m6
			  or_last_op();    // OR last op with prev op and remove last op
		     }
		  } else {
		     if (tz390.op_code[bal_op_index].length() > 5) {
       		    get_hex_op(6,1); // M6
		     } else {
    	        get_hex_zero(1);   // M6
		     }
          }
		  if (tz390.op_code[bal_op_index].substring(0,4).equals("E78E")      // E78E VRRe VFMS  V1,V2,V3,V4,M5,M6 RPI 2225
		   || tz390.op_code[bal_op_index].substring(0,4).equals("E78F")      // E78F VRRe VFMA  V1,V2,V3,V4,M5,M6
		   || tz390.op_code[bal_op_index].substring(0,4).equals("E79E")      // E79E VRRb VFNMS V1,V2,V3,V4,M5,M6
		   || tz390.op_code[bal_op_index].substring(0,4).equals("E79F")){    // E79F VRRb VFNMA V1,V2,V3,V4,M5,M6
		  // map oo123456 to oo1235064xoo
          obj_code = obj_code.substring(0,5)  // op1+v1,v2,v3
 	      + obj_code.substring(7,8)           // m6 rpi 2225
		  + "0"                		          // 0
		  + obj_code.substring(6,7)           // m5 rpi 2225
    	  + obj_code.substring(5,6);          // v4
		   } else {		   
		  // map oo123456 to oo1235604xoo  E78A VSTRC?
          obj_code = obj_code.substring(0,5)  // op1+v1,v2,v3
 	      + obj_code.substring(6,8)           // m5,m6
		  + "0"                               // 0
    	  + obj_code.substring(5,6);          // v4
		   }
		}		
    	get_hex_vreg_rxb();    // RXB
     	get_hex_op(3,2);  // oo
    	check_end_parms();
    	put_obj_text();
    	break;
    case 83: // "E712=VGEG,83,739", // E712 VRV VGEG V1,D2(V2,B2),M3 RPI 2202
    	// map V1,D2(V2,B2),M3  into oo12bddd3x00
    	bal_op_ok = true;
    	loc_ctr = (loc_ctr+1)/2*2;
    	loc_start = loc_ctr;
    	loc_len = 6;
    	get_hex_op(1,2);   //  oo
   		get_hex_vreg(1);       // V1
   		skip_comma();
   		get_hex_v2xbddd();         // D2(V2,B2)
    	skip_comma();
    	get_hex_reg();       // M3
    	get_hex_vreg_rxb();    // RXB
     	get_hex_op(3,2);         // oo
    	check_end_parms();
    	put_obj_text();
    	break;
    case 101:  // CCW  0 
    case 102:  // CCW0 0
    	bal_op_ok = true;
    	bal_lab_attr = tz390.ascii_to_ebcdic['W']; // RPI 340
    	gen_ccw0(); // op8,addr24,flags8,zero8,len16 // RPI 567
    	break;
    case 103:  // CCW1 0 
    	bal_op_ok = true;
    	bal_lab_attr = tz390.ascii_to_ebcdic['W']; // RPI 340
    	gen_ccw1();  // op8,flags8,len16,bit0,addr31  // RPI 567
    	break;
    case 104:  // DC 0
    	bal_op_ok = true;
       	process_dc(1);	
    	break;
    case 105:  // DS 0 
    	bal_op_ok = true;
       	process_dc(1);	    	
    	break;
    case 106:  // ALIAS 0 
    	bal_op_ok = true;  // RPI 368 ignore
    	bal_label_ok = false; // RPI 553
    	break;
    case 107:  // AMODE 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	bal_label_ok = false; // RPI 553
    case 108:  // CATTR 0 
    	break;
    case 109:  // COM 0 
    	bal_lab_attr = tz390.ascii_to_ebcdic['J']; // RPI 340
    	bal_op_ok = true;
    	process_sect(sym_cst,bal_label);  // RPI 230
    	if (first_cst_esd == 0)first_cst_esd = cur_esd;
    	bal_label_ok = false;
    	break;
    case 110:  // CSECT 0 
    	bal_lab_attr = tz390.ascii_to_ebcdic['J']; // RPI 340
    	bal_op_ok = true;
    	process_sect(sym_cst,bal_label);
    	if (first_cst_esd == 0)first_cst_esd = cur_esd;
    	bal_label_ok = false;
    	break;
    case 111:  // CXD 0 
    	break;
    case 112:  // DSECT 0 
    	bal_lab_attr = tz390.ascii_to_ebcdic['J']; // RPI 340
    	bal_op_ok = true;
    	process_sect(sym_dst,bal_label);
    	bal_label_ok = false;
    	break;
    case 113:  // DXD 0 
    	break;
    case 114:  // ENTRY 0 
    	bal_op_ok = true;
    	process_esd(sym_ent);
    	break;
    case 115:  // EXTRN 0
    	bal_lab_attr = tz390.ascii_to_ebcdic['T']; // RPI 340
    	bal_op_ok = true;
        process_esd(sym_ext);
    	break;
    case 116:  // LOCTR 0 
    	bal_lab_attr = tz390.ascii_to_ebcdic['J']; // RPI 340
    	bal_op_ok = true;
    	if (cur_pass == 1){  // RPI 960
    		tot_loc_stmt++; // RPI 920
    	}
    	process_sect(sym_lct,bal_label);
    	bal_label_ok = false;
    	break;
    case 117:  // RMODE 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	bal_label_ok = false; // RPI 553
    	break;
    case 118:  // RSECT 0
    	bal_lab_attr = tz390.ascii_to_ebcdic['J']; // RPI 340
    	bal_op_ok = true;
    	process_sect(sym_cst,bal_label);  // RPI 230
    	if (first_cst_esd == 0)first_cst_esd = cur_esd;
    	bal_label_ok = false;
    	break;
    case 119:  // START 0
    	bal_lab_attr = tz390.ascii_to_ebcdic['J']; // RPI 340
    	bal_op_ok = true;
    	process_sect(sym_cst,bal_label);  // RPI 230
    	if (first_cst_esd == 0){
    		first_cst_esd = cur_esd;
    	} else if (cur_esd != first_cst_esd){
    		log_error(204,"START must be first CSECT"); // RPI 917
    	}
        process_start();               // RPI 1523
    	bal_label_ok = false;
    	break;
    case 120:  // WXTRN 0
    	bal_lab_attr = tz390.ascii_to_ebcdic['S']; // RPI 340
    	bal_op_ok = true;
        process_esd(sym_wxt); //RPI182
    	break;
    case 121:  // XATTR 0 
    	break;
    case 123:  // DROP 0
    	bal_op_ok = true;
    	if (gen_obj_code){
            drop_using();
     	}
    	break;
    case 124:  // USING 0 
    	bal_op_ok = true;
    	bal_label_ok = false;
    	check_private_csect();
    	if (gen_obj_code){
            add_using();
     	}
     	break;
    case 125:  // AEJECT 0
    	bal_op_ok = true; //RPI122 IGNORE
    	break;
    case 126:  // ASPACE 0
    	bal_op_ok = true; //RPI122 IGNORE
    	list_bal_line = false; // RPI 289
    	break;
    case 127:  // CEJECT 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	list_bal_line = false; // RPI 289
    	break;
    case 128:  // EJECT 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	list_bal_line = false; // RPI 289
    	break;
    case 129:  // PRINT 0 
    	bal_op_ok = true; 
    	if (gen_obj_code){
    		process_print();
    	}
    	break;
    case 130:  // SPACE 0 
    	bal_op_ok = true; //RPI122 IGNORE
    	list_bal_line = false; // RPI 289
    	break;
    case 131:  // TITLE 0 
    	bal_op_ok = true;
    	bal_label_ok = false; // RPI 131
    	break;
    case 132:  // ADATA 0 
    	break;
    case 133:  // CNOP 0 
    	bal_op_ok = true;
    	process_cnop();
    	break;   
    case 135:  // END 0 
    	bal_op_ok = true;
    	end_found = true;
    	if (gen_obj_code && bal_parms != null){
    	    label_match = label_pattern.matcher(bal_parms);
    		if (!bal_abort && label_match.find()){
    		   end_entry = label_match.group();
    		   end_entry_sid = find_sym(end_entry); // RPI 1197
    		   if (end_entry_sid > 0){
    			   end_entry_found = true;
    		   } else {
    			   log_error(210,"END entry label not found - " + end_entry);
    		   }
    		}
    	}
        process_end();
    	break;
    case 136:  // EQU 0
    	bal_op_ok = true; 
    	bal_label_ok = false;
    	process_equ();
    	break;
    case 137:  // EXITCTL 0 
    	break;
    case 138:  // ICTL 0
    	bal_op_ok = true; // RPI 728
    	break;
    case 139:  // ISEQ 0
    	bal_op_ok = true; // RPI 728 ignore
    	break;
    case 140:  // LTORG 0
    	bal_op_ok = true;
   		list_bal_line();
    	if ( // RPI 1159 was tot_lit > 0
    		cur_esd > 0
    		&& sym_type[esd_sid[esd_base[cur_esd]]] == sym_cst){ // RPI 564
     	   gen_ltorg();
     	}
    	cur_lit_pool++;
    	list_bal_line = false; // RPI891 supress LTORG at end
    	break;
    case 141:  // OPSYN 0 
    	break;
    case 142:  // ORG 0 
    	bal_op_ok = true;
    	process_org();
    	break;
    case 143:  // POP 0 
    	bal_op_ok = true;
    	if (gen_obj_code){
    		process_pop();
    	}
    	break;
    case 145:  // PUSH 0 
    	bal_op_ok = true;
    	if (gen_obj_code){
    		process_push();
    	}
    	break;
    case 146:  // REPRO 0
    	break;
    case 147:  // ACONTROL  
    	bal_op_ok = true;  // RPI 368 ignore
    	break; 
    case 201:  // ACTR 0
    	break;
    case 202:  // AGO 0
    	break;
    case 203:  // AIF 0
    	break;
    case 204:  // AINSERT 0
    	break;
    case 205:  // ANOP 0
    	break;
    case 206:  // AREAD 0
    	break;
    case 207:  // GBLA 0
    	break;
    case 208:  // GBLB 0
    	break;
    case 209:  // GBLC 0
    	break;
    case 210:  // LCLA 0
    	break;
    case 211:  // LCLB 0
    	break;
    case 212:  // LCLC 0
    	break;
    case 213:  // MHELP 0 
    	break;
    case 214:  // MNOTE 0
    	bal_op_ok = true;  // pass through from mz390
    	if (gen_obj_code 
    		&& mac_inline_level == 0){ // RPI 581
        	force_list_bal = true;        // RPI 581
    		if (bal_parms != null // RPI 503
   				&& bal_parms.length() > 0
   				&& bal_parms.charAt(0) != '*'){  // RPI 444
   				tz390.put_systerm("MNOTE " + bal_parms); // RPI 440
    		}
    		if (bal_parms.length() > 0 
        		&& bal_parms.charAt(0) != '\''
        	  	&& bal_parms.charAt(0) != ','
        		&& bal_parms.charAt(0) != '*'){
    		    exp_text = bal_parms;
    		    exp_index = 0;
    		    exp_val = 0;
        		if (calc_abs_exp()){
        			if (exp_val > az390_rc){  // RPI 313
        				az390_rc = exp_val;
        		    } 
        			if (exp_val > 0){
        				if (exp_val > tz390.max_mnote_warning){
        					tot_mnote_errors++;
        				} else {
        					tot_mnote_warning++;
        				}
        			}
        			if (exp_val > max_mnote_level){
        				max_mnote_level = exp_val;
        			}
        		}
        	}
    	}
    	break;
    case 215:  // SETA 0
    	break;
    case 216:  // SETAF 0
    	break;
    case 217:  // SETB 0
    	break;
    case 218:  // SETC 0
    	break;
    case 219:  // SETCF 0
    	break;
    case 220:  // MACRO 0
    	bal_op_ok = true;  // pass true from mz390
    	mac_inline_level++; // RPI 581
    	break;
    case 221:  // MEND 0
    	bal_op_ok = true;  // pass true from mz390
    	mac_inline_level--;  // RPI 581
    	break;
    case 222:  // MEXIT 0 
        break;
    case 223:  // PUNCH 0
    	bal_op_ok = true; // pass thru after gen by mz390
    	break;
    case 224:  // COPY 0 
    	bal_op_ok = true;  // already expanded in mz390
    	break;
    case 225:  // OPSYN
    	bal_op_ok = true;
    	bal_label_ok = false;         // reset to avoid dup. label
    	tz390.update_opsyn(bal_label,bal_parms);
    	if (tz390.opt_traceall){ // RPI 403
    		tz390.put_trace("OPSYN(" + tz390.opsyn_index + ") NEW=" + opsyn_label + " OLD=" + bal_parms);
    	}
    	break;
    case 226:  // inline macro code and not MACRO or MEND
        bal_op_ok = true;
    	break;
    default:
    	// should not occur - see tz390 opcode_type table
    	abort_error(139,"invalid opcode type index");
	}
	if (mac_inline_level == 0){
		if (!bal_op_ok){
			log_error(62,"unsupported operation code " + bal_op); // RPI 563
		}
		if (bal_label != null
			&& index < tz390.max_asm_type  // RPI 926  
			&& bal_label_ok){ // RPI 451
			update_label();
		}
	}
	if (!bal_abort && bal_line != null){ // RPI 891
	    list_bal_line();
	}
	loc_ctr = loc_ctr + loc_len;
}
private void list_bal_line(){
	/*
	 * list bal line with first 8 bytes of
	 * object code if any 
	 * and turn off list_bal_line request
	 * Notes:
	 *   1.  See comments processing case 0
	 *       for update of mac_call_gen,
	 *       call reformating, and delay flags
	 *       mac_call_first and mac_call_last.
	 */
	    if (!check_list_bal_line()){ // RPI 484 RPI 891
	    	update_list_bal_line();
	    	return;
	    }
	    if (list_obj_code.length() < 16){
	    	list_obj_code = list_obj_code.concat("                ").substring(0,16);
	    } 
	    list_obj_loc = loc_start;
	    if (gen_obj_code){ // RPI 581
	    	cur_line_type     = xref_file_type[bal_line_xref_file_num[bal_line_index]];
	    	cur_line_file_num = bal_line_xref_file_num[bal_line_index];
	    	put_prn_line(tz390.get_hex(list_obj_loc,6)
    		  + " " + list_obj_code.substring(0,16) 
    		  + " " + hex_bddd1_loc 
    		  + " " + hex_bddd2_loc 
    		  + " " + tz390.get_cur_bal_line_id(cur_line_file_num,
                      bal_line_xref_file_line[bal_line_index],
                      bal_line_num[bal_line_index],
                      mac_call_gen, // RPI 891 
                      cur_line_type) 
    		  + bal_line);
	    }
	    force_list_bal = false;   // RPI 285
        update_list_bal_line();
	    if (list_use){
	    	list_use();
	    	list_use = false;
	    }
}
private boolean check_list_bal_line(){
	/*
	 * set true if ok to list BAL line
	 * on PRN file
	 */
	if (!tz390.opt_list){ 
		// no PRN file generated  // rpi 895 no ERRSUM check
		return false; 
	}
	if (force_list_bal){
		// force listing error msgs etc.
		return true;
	}
    if (list_bal_line 
    	&& print_on[print_level]){ 
    	if (mac_call_gen 
    		&& !print_gen[print_level]){
    		// suppress generated BAL
    		// for PRINT NOGEN
    		return false;
    	}
    	return true;
    } else {
    	// supress BAL for PRINT OFF
    	return false;
    }
}
private void update_list_bal_line(){
	/*
	 * update mac_call_gen after prn
	 */
    if (mac_call_first){
    	mac_call_first = false;
    	mac_call_gen   = true; // RPI 891 
    	list_bal_line = false; // rpi 891 
    } 
    if (mac_call_last){
    	mac_call_last = false;
    	mac_call_gen  = false;  // RPI 891 
        list_bal_line = true;   // RPI 891 
    }
}
private void add_rld(int exp_esd){
	/*
	 * add plus rld
	 */
	if (cur_esd == 0 || sym_type[esd_sid[esd_base[cur_esd]]] != sym_cst){
		return; // RPI 1208
	}		       
	if (tot_exp_rld_add < max_exp_rld){
		exp_rld_add_esd[tot_exp_rld_add] = exp_esd;
		tot_exp_rld_add++;
	}
}
private void sub_rld(int exp_esd){
	/*
	 * sub rld
	 */
	if (cur_esd == 0 || sym_type[esd_sid[esd_base[cur_esd]]] != sym_cst){
		return; // RPI 1208
	}	
	if (tot_exp_rld_sub < max_exp_rld){
		exp_rld_sub_esd[tot_exp_rld_sub] = exp_esd;
		tot_exp_rld_sub++;
	}
}
private void reduce_exp_rld(){
	/*
	 * reduce rld on stack 
	 */
	int index1 = 0;
	int index2 = 0;
	while (index1 < tot_exp_rld_add){
		index2 = 0;
		while (index2 < tot_exp_rld_sub){
			if (exp_rld_add_esd[index1] == exp_rld_sub_esd[index2]){
				tot_exp_rld_add--;
				if (index1 < tot_exp_rld_add){ // RPI 673
					exp_rld_add_esd[index1] = exp_rld_add_esd[tot_exp_rld_add];
				    index1--; // backup to restart on replacement
				}
				tot_exp_rld_sub--;
				if (index2 < tot_exp_rld_sub){  // RPI 673
					exp_rld_sub_esd[index2] = exp_rld_sub_esd[tot_exp_rld_sub];
				}
			    index2 = tot_exp_rld_sub;  // rpi 673 force restart
			}
			index2++;
		}
		index1++;
	}
	if ((tot_exp_rld_add + tot_exp_rld_sub) == 0){
		exp_type = sym_sdt;
		exp_esd  = 0; // RPI 673
		return;
	} else if (tot_exp_rld_add == 1 && tot_exp_rld_sub == 0){
		exp_type = sym_rel;
		exp_esd = exp_rld_add_esd[0];
	} else { 
		exp_type = sym_rld;
	}
}
private void gen_exp_rld(){
	/*
	 * generate rlds for expression
	 * Notes:
	 *   1.  convert to rel csect vs rel module
	 *       offsets for linker use. 
	 *   2.  Original exp_val saved in rld_exp_val
	 *       for use in PRN display (i.e. show addresses
	 *       relative to module versus CSECT).
	 */
	if (cur_esd == 0 || sym_type[esd_sid[esd_base[cur_esd]]] != sym_cst){ // RPI 1208
		return;
	}        
	exp_rld_mod_val = exp_val;  // RPI 632 rel module vs CSECT
	exp_rld_mod_set = true;     // RPI 632
	int index = 0;
	while (index < tot_exp_rld_add){
		if (tot_rld < tz390.opt_maxrld){
			rld_fld_esd[tot_rld] = esd_base[cur_esd]; // RPI 301
			rld_fld_loc[tot_rld] = loc_ctr - sym_loc[esd_sid[esd_base[cur_esd]]]; // RPI 564 use base 
			rld_fld_len[tot_rld] = exp_rld_len;
			rld_fld_sgn[tot_rld] = rld_add;
			rld_xrf_esd[tot_rld] = exp_rld_add_esd[index];
			exp_val = exp_val - sym_loc[esd_sid[exp_rld_add_esd[index]]]; 
			if (tz390.opt_traceall){
				tz390.put_trace("EXP RLD" // RPI 564 additional traceall info
						+ " ESD=" + tz390.get_hex(rld_fld_esd[tot_rld],4)
						+ " LOC=" + tz390.get_hex(rld_fld_loc[tot_rld],8)
						+ " LEN=" + tz390.get_hex(rld_fld_len[tot_rld],1)
						+ " SIGN=" + rld_fld_sgn[tot_rld]
						                         + " XESD=" + tz390.get_hex(rld_xrf_esd[tot_rld],4));                       
			}
			tot_rld++;
		} else {
			abort_error(103,"rld table exceeded");
		}
		index++;
	}
	index = 0;
	while (index < tot_exp_rld_sub){
		if (tot_rld < tz390.opt_maxrld){
			rld_fld_esd[tot_rld] = cur_esd;
			rld_fld_loc[tot_rld] = loc_ctr - sym_loc[esd_sid[cur_esd]]; 
			rld_fld_len[tot_rld] = exp_rld_len;
			rld_fld_sgn[tot_rld] = rld_sub;
			rld_xrf_esd[tot_rld] = exp_rld_sub_esd[index];
			exp_val = exp_val + sym_loc[esd_sid[exp_rld_sub_esd[index]]]; 
			if (tz390.opt_traceall){
				tz390.put_trace("EXP RLD" // RPI 564 additional traceall info
						+ " ESD=" + tz390.get_hex(rld_fld_esd[tot_rld],4)
						+ " LOC=" + tz390.get_hex(rld_fld_loc[tot_rld],8)
						+ " LEN=" + tz390.get_hex(rld_fld_len[tot_rld],1)
						+ " SIGN=" + rld_fld_sgn[tot_rld]
						                         + " XESD=" + tz390.get_hex(rld_xrf_esd[tot_rld],4));                       
			}
			tot_rld++;
		} else {
			abort_error(103,"rld table exceeded");
		}
		index++;
	}
}
private void gen_obj_rlds(){
	/*
	 * write RLD's to the OBJ file in ascii hex
	 */
	xref_bal_index = -1;
	if (tot_rld > 0){
		put_prn_line("Relocation Definitions");
	}
	int index = 0;
	while (index < tot_rld){
		String rld_code = 
			" ESD=" + tz390.get_hex(rld_fld_esd[index],4)
		  + " LOC=" + tz390.get_hex(rld_fld_loc[index],8)
		  + " LEN=" + tz390.get_hex(rld_fld_len[index],1)
		  + " SIGN=" + rld_fld_sgn[index]
		  + " XESD=" + tz390.get_hex(rld_xrf_esd[index],4);
        put_prn_line(rld_code);
       	put_obj_line(".RLD" + rld_code);
		index++;
	}
}
private void gen_obj_end(){
	/*
	 * write END object record with entry if specified  RPI 1197
	 */
	if (end_entry_found){
		put_obj_line(".END" + " ESD=" + tz390.get_hex(sym_esd[end_entry_sid],4)
		                    + " LOC=" + tz390.get_hex(sym_loc[end_entry_sid]-sym_loc[esd_sid[sym_esd[end_entry_sid]]],8));
	} else {
		put_obj_line(".END" + " ESD=0000" 
                            + " LOC=00000000");
	}
}
private void gen_sym_list(){
	/*
	 * list symbols in alpah order 
	 * with optional cross reference
	 */
	 put_prn_line(tz390.newline +  // RPI 500
	 		"Symbol Table Listing" + tz390.newline);
	 TreeSet<String> sort_sym = new TreeSet<String>();
	 int index = 1;
	 while (index <= tot_sym){ // RPI 415
		 if (sym_def[index] != sym_def_lookahead){ // RPI 450
			 sort_sym.add(sym_name[index] + (sort_index_bias + index));
		 }
		 index++;
	 }
	 Iterator<String> sym_key_it = sort_sym.iterator();
	 while (sym_key_it.hasNext()){
	 	String key = sym_key_it.next();
	 	// get sym index from end of sort key string
	 	index = Integer.valueOf(key.substring(key.length()-sort_index_len)) - sort_index_bias;	 	
	 	String name = sym_name[index];
	 	if (name.length() < 8){
	 		name = name.concat("       ").substring(0,8);
	 	}
	 	String sym_line = " SYM=" + name
		           + " LOC=" + tz390.get_hex(sym_loc[index],8) 
		           + " LEN=" + tz390.get_hex(get_sym_len(index),8)
		           + " ESD=" + tz390.get_hex(esd_base[sym_esd[index]],4) // RPI 301
	 			   + " TYPE=" + sym_type_desc[sym_type[index]] 
				   ; 
        if (tz390.opt_xref){
        	sym_line = sym_line + "  XREF=";
        	if (sym_def[index] > sym_def_ref){ 
        		sym_line = sym_line + bal_line_num[sym_def[index]] + " ";
        	}
        	if (sym_xref[index] != null){
        		Iterator<Integer> sym_xref_it = sym_xref[index].iterator();
        		while (sym_xref_it.hasNext()){
        			int sym_xref_num = sym_xref_it.next();
        			if (sym_xref_num != bal_line_num[sym_def[index]]){
        				sym_line = sym_line + sym_xref_num + " ";
        				if (sym_line.length() > tz390.max_line_len){
        					put_prn_line(sym_line);
        					sym_line = "  ";
        				}
        			}
        		}
        	}
        	if (sym_line.length() > 2){
        		put_prn_line(sym_line);
        	}
        } else {
        	put_prn_line(sym_line);
        }
	 }
}
private void gen_lit_xref_list(){
	/*
	 * list literals in alpha order
	 * with optional cross reference
	 */
	 put_prn_line(tz390.newline + "Literal Table Listing" + tz390.newline); // RPI 500
	 TreeSet<String> sort_lit = new TreeSet<String>();
	 int index = 0;
	 while (index < tot_lit){
		 sort_lit.add(lit_name[index] + (sort_index_bias + index));
		 index++;
	 }
	 Iterator<String> lit_key_it = sort_lit.iterator();
	 while (lit_key_it.hasNext()){
	 	String key = lit_key_it.next();
	 	// get lit table index from end of sort key string
	 	cur_lit = Integer.valueOf(key.substring(key.length()-sort_index_len)) - sort_index_bias;
 	    String lit = lit_name[cur_lit];
	 	if (lit.length() < 8){
	 		lit = lit.concat("       ").substring(0,8);
	 	}
	 	String lit_line = " LIT=" + lit 
                        + " LOC=" + tz390.get_hex(lit_loc[cur_lit],8) 
                        + " LEN=" + tz390.get_hex(lit_len[cur_lit],8)
		                + " ESD=" + tz390.get_hex(lit_esd[cur_lit],4) 
		                + " POOL=" + tz390.get_hex(lit_pool[cur_lit],4)
		                ;
        if (tz390.opt_xref  && lit_xref[cur_lit] != null){  
		    lit_line = lit_line + " XREF=";
		    Iterator<Integer> lit_xref_it = lit_xref[cur_lit].iterator();
		    while (lit_xref_it.hasNext()){
		    	int lit_xref_num = lit_xref_it.next();
		    	lit_line = lit_line + lit_xref_num + " ";
		    	if (lit_line.length() > tz390.max_line_len){
		    		put_prn_line(lit_line);
		    		lit_line = "  ";
		    	}
		    }
       	}
       	if (lit_line.length() > 2){
       		put_prn_line(lit_line);
       	}
	 }
	 put_prn_line(" ");
}
private void load_bal(){
	/*
	 * load bal source
	 * 
	 * 1.  Concatentate any continuations indicated
	 *     by non-blank in position 72.  Each 
	 *     continuation must start at position 16.
	 */
	    if (!mz390_call){ // RPI 415
       		String bal_file_name = tz390.get_file_name(tz390.dir_bal,tz390.pgm_name,tz390.pgm_type); // RPI 866
	    	bal_file = new File(bal_file_name);
     	    try {
     	    	bal_file_buff = tz390.getReaderForDefaultCharset(bal_file);
     	    } catch (Exception e){
     	    	abort_error(6,"I/O error on bal open - " + e.toString());
     	    }
	    }
		get_bal_line();
		while (!bal_eof && bal_line != null){
            save_bal_line();
			parse_bal_line();
            bal_op_index = find_bal_op();
            if (bal_op_index > -1){ // RPI 274 OPSYN cancel
	           	process_bal_op();    
	        }
 			if  (bal_line != null){
				tot_bal_line++;
				if (tot_bal_line >= tz390.opt_maxline){ // issue #343 moved here to catch before end
			        abort_error(83,"maximum source lines exceeded");
		        }
	            get_bal_line();
			}
		}
		if (!end_found){
			process_end();
		}		
        if (tz390.opt_tracea){
        	tz390.put_trace("PASS " + cur_pass + " TOTAL ERRORS = " + az390_errors);
        }
        if (!mz390_call){ // RPI 415
        	try {
        		bal_file_buff.close();
        	} catch (Exception e){
        		abort_error(7,"I/O error on BAL file close " + e.toString());
        	}
        }
}
public void pass_bal_line(String new_bal_line,String new_xref_file_name, char new_xref_file_type,int new_xref_file_num, int new_xref_line_num){
	/*
	 * 1.  pass mz390 bal_line to az390 bal_line
	 *     with synchronization of threads.
	 * 2.  ignore BAL after END
	 */
	if (sym_lock){
		abort_error(168,"bal pass sym lock error on line - " + new_bal_line);
	}
	lock.lock(); // RPI 415
   	try {
   		if (pass_bal_eof){
   			return;
   		}
   		while (az390_running && bal_line_full){
   			lock_condition.await();
   		}
   	    pass_bal_line = new_bal_line;
   	    pass_xref_file_name = new_xref_file_name;
   	    pass_xref_file_type = new_xref_file_type;
   	    pass_xref_file_num = new_xref_file_num;
   	    pass_xref_file_line = new_xref_line_num;
   	    if (pass_bal_line == null || bal_eof){
   	    	pass_bal_eof = true;
   	    }
   	    bal_line_full = true;
   	    lock_condition.signalAll();
   	} catch (Exception e) {
   		abort_error(152,"waiting for az390 to release bal line");
   	} finally {
   		lock.unlock();
   	}
}
public  void set_sym_lock(String desc){
	/*
	 * 1.  Block mz390 until az390 is waiting
	 *     for next bal.
	 * 2.  Set sym_lock
	 * 
	 * Notes:
	 *   1.  See az390 pass_bal for lock check.
     *   2,  See mz390 put_bal_line for lock reset. 
	 */
	    if (tz390.opt_traceall){ // RPI 403
    		tz390.put_trace("az390 set symbol lock" + desc);
    	}
	    sym_lock_desc = desc;
        // wait for az390 to processing pending bal
	    while (az390_running 
        		&& (bal_line_full // RPI 485
        			|| !az390_waiting)){
        	Thread.yield();
        }
		if (!lookahead_mode 
			&& az390_thread != Thread.currentThread()){
			sym_lock = true;
    	} else {
			abort_error(167,"invalid set sym lock request - " + sym_lock_desc);
	    }
}
public  void reset_sym_lock(){
	/*
	 * reset sym lock at next mz390 bal line
	 * and at start of lookahead mode.
	 */
	sym_lock = false;
}
private void get_bal_line(){
	/*
	 * get next bal line from bal file
	 * or from mz390 parallel thread pass
	 */
	check_timeout();
	if (mz390_call){
		lock.lock();
		try {
			az390_waiting = true;
			while (!bal_line_full){
				lock_condition.await();
			}
			az390_waiting = false;
	        bal_line = pass_bal_line;
	        bal_xref_file_name = pass_xref_file_name;
	        bal_xref_file_type = pass_xref_file_type;
	        bal_xref_file_num  = pass_xref_file_num;
	        bal_xref_file_line = pass_xref_file_line;
	    	if (bal_xref_file_name != null){
	    		xref_file_name[bal_xref_file_num] = bal_xref_file_name;
	    		xref_file_type[bal_xref_file_num] = bal_xref_file_type;
	    	}
			bal_line_full      = false;
			lock_condition.signalAll();
		} catch(Exception e){
			abort_error(151,"waiting for mz390 to pass bal line " + e.toString());
		} finally {
			lock.unlock();
		}
		if (lookahead_mode){
			abort_error(157,"invalid pass bal record during lookahead - " + bal_line);
		}
        tz390.inc_cur_bal_line_num(bal_line);
        return;
	}
	String temp_line;
    try {
    	tz390.systerm_io++;
        temp_line = bal_file_buff.readLine();
        tz390.cur_bal_line_num++;
        tz390.prev_bal_cont_lines = 0;
        save_bal_line(); // RPI 274
    	if  (temp_line == null){
    			bal_line = null;
   		} else if (temp_line.length() < tz390.bal_ictl_end + 1  // RPI 437 RPI 728
   				   || temp_line.charAt(tz390.bal_ictl_end) <= ' '){  //RPI181 RPI 728
   			bal_line = tz390.trim_trailing_spaces(temp_line,tz390.bal_ictl_end + 1);  //RPI124 RPI 728
    	    if (!tz390.verify_ascii_source(bal_line)){
    	    	log_error(116,"invalid ascii source line " + tz390.cur_bal_line_num + " in " + bal_file.getAbsolutePath());  // RPI 694 RPI 769
    	    }
   		} else {
   		    bal_line = temp_line.substring(0,tz390.bal_ictl_end);  // RPI 728
   		    bal_line = tz390.trim_continue(bal_line,tz390.split_first,tz390.bal_ictl_end,tz390.bal_ictl_cont); // RPI 728
            boolean bal_cont = true;
   		    while (bal_cont){  //RPI181  // RPI 315
            	    tz390.systerm_io++;
            	    temp_line = bal_file_buff.readLine();
            	    if (temp_line == null){
            	    	abort_error(117,"missing continue source line " + tz390.cur_bal_line_num + " in " + bal_file.getAbsolutePath());
            	    }
            	    temp_line = tz390.trim_trailing_spaces(temp_line,tz390.bal_ictl_end + 1);  // RPI 728
            	    if (!tz390.verify_ascii_source(temp_line)){
            	    	abort_error(118,"invalid ascii source line " + tz390.cur_bal_line_num + " in " + bal_file.getAbsolutePath()); // RPI 694 RPI 769
            	    }
            	    if (temp_line.length() < tz390.bal_ictl_end + 1 || temp_line.charAt(tz390.bal_ictl_end) <= ' '){ //RPI181 RPI 278
            	    	bal_cont = false; // RPI 315
            	    	temp_line = tz390.trim_trailing_spaces(temp_line,tz390.bal_ictl_end + 1); //RPI124 RPI 728
            	    }
            	    tz390.prev_bal_cont_lines++;
            	    save_bal_line(); // RPI 274
            	    if  (temp_line.length() >= tz390.bal_ictl_cont  // RPI 728
            	    	&& temp_line.substring(tz390.bal_ictl_start - 1,tz390.bal_ictl_cont - 1).trim().equals("")){ // RPI167  RPI 728 no char preceeding cont  
            	    	bal_line = bal_line + tz390.trim_continue(temp_line,tz390.split_cont,tz390.bal_ictl_end,tz390.bal_ictl_cont); // RPI 315, RPI 463 RPI 728
            	    } else { 
            	    	log_error(8,"continuation line < " + tz390.bal_ictl_cont + " characters - " + temp_line);
            	    }
            }   
   		}
    } catch (Exception e){
       	abort_error(9,"I/O error on file read " + e.toString());
    }
}
private void save_bal_line(){
	/* 
	 * save bal line during loading for log_error use
	 */
	bal_line_index = tot_bal_line;
	bal_line_text[tot_bal_line] = bal_line;
	bal_line_num[tot_bal_line] = tz390.cur_bal_line_num;
	bal_line_xref_file_num[tot_bal_line] = bal_xref_file_num;
	bal_line_xref_file_line[tot_bal_line] = bal_xref_file_line;
    xref_bal_index = tot_bal_line; // for error xref during lookahead
}
private void parse_bal_line(){
	/*
	 * set bal_label and bal_op
	 */
	if (tz390.opt_tracea){
		if (bal_line_xref_file_num[bal_line_index] == 0){
			trace_pfx = "OPEN CODE" + tz390.right_justify("" + bal_line_xref_file_line[bal_line_index],6) + tz390.right_justify("" + bal_line_num[bal_line_index],7) + " ";
		} else {
			trace_pfx = tz390.left_justify(get_base_name(xref_file_name[bal_line_xref_file_num[bal_line_index]]),9) + tz390.right_justify("" + bal_line_xref_file_line[bal_line_index],6) + tz390.right_justify("" + bal_line_num[bal_line_index],7) + " ";
		}
		tz390.put_trace(trace_pfx + tz390.get_hex(loc_ctr,6) + " " + bal_line); // RPI 605
	}
	bal_abort = false;
	bal_label = null;
	bal_op    = null;
	bal_parms = null;
	if  (bal_line == null 
		 || bal_line.length() == 0
		 || bal_line.charAt(0) == '*'){
		return;
	} 
    tz390.split_line(bal_line);
    bal_label = tz390.split_label;
    if (tz390.split_op != null){
    	bal_op = tz390.split_op.toUpperCase();
    } else {
    	bal_op = null;
    }
    bal_parms = tz390.split_parms;
}
private String get_base_name(String file_name){
	/*
	 * return base file name from path\file.sfx
	 */
	int index1 = file_name.lastIndexOf(File.separator);
    int index2 = file_name.lastIndexOf('.');
    if (index2 <= index1){  // RPI 1210 
    	index2 = file_name.length();
    }
    return file_name.substring(index1+1,index2);
}
private int find_bal_op(){
	/*
	 * return index of bal operation 
	 * or return -1 if undefined operation
	 * or return -2 if cancelled OPSYN
	 * 
	 * return 0 for comments
	 */
	int index = 0;
	if  (bal_op != null 
		 && bal_op.length() > 0){
		String key = bal_op;
		index = tz390.find_key_index('R',key);
		if (index >= 0 && tz390.opsyn_old_name[index] != null){
			key = tz390.opsyn_old_name[index];  /// RPI 306
		    bal_op = tz390.opsyn_old_name[index]; // RPI 493
		}
		index = tz390.find_key_index('O',key);
		if (index > -1){ // RPI 274 OPYSN cancel
			return index;
		} else if (mac_inline_level > 0){
			return mac_inline_op_other; // rpi 581
		}
		label_match = label_pattern.matcher(bal_op);  // RPI 253
		if (!label_match.find()){
			log_error(196,"invalid character in opcode - " + bal_op);  // RPI 659
		} else {
			if (!tz390.opt_errsum){
				tz390.init_errsum(); // RPI 694
			}
			log_error(29,"ERRSUM missing macro = " + bal_op); // RPI 694 rpi 1051
		}
	    return -1;
	} 
	if (bal_line.length() == 0 || bal_line.charAt(0) == '*'){
		return 0;
	} else {
		log_error(71,"missing opcode - " + bal_line); // RPI 1034
		return - 1;
	}
}
private void process_esd(byte esd_type){
	/*
	 * process EXTRN, ENTRY, or WXTRN statement
	 */
	String token = null;
	boolean extrn_eod = false;
    extrn_match = extrn_pattern.matcher(bal_parms);
	while (!bal_abort && !extrn_eod
			&& extrn_match.find()){
	       token = extrn_match.group();
	       switch (token.charAt(0)){
	       case ',':
	    	   break;
	       case '\t':  //tab  RPI181
	       case '\r':  //cr
	       case '\n':  //lf
	       case ' ':   //space
	           extrn_eod = true;
	           break;
	       default:
	    	   switch (esd_type){
	    	   case 3: // sym_ent
	    	   	   cur_sid = find_sym(token);
    	    	   if (cur_sid != -1 // RPI 489
    	    		   && sym_def[cur_sid] > sym_def_ref){ 
	        	       add_entry(token);
	        	   } else {
	        		   log_error(156,"ENTRY not found - " + token);
	        	   }
	               break;
    	       case 4: // sym_ext
    	    	   cur_sid = find_sym(token);
    	    	   if (!lookahead_mode
    	    		   && (cur_sid == -1
    	    		       || sym_def[cur_sid] == sym_def_lookahead) // RPI 415 
    	    		       || sym_def[cur_sid] != sym_cst){ // RPI 1044
    	    		   cur_sid = add_extrn(token);
    	    	   }
    	    	   break;
    	       case 8: // sym_wxt
    	    	   cur_sid = find_sym(token);
    	    	   if (!lookahead_mode
    	    		   && (cur_sid == -1
    	    		       || sym_def[cur_sid] == sym_def_lookahead) // RPI 415 
    	    		       || sym_def[cur_sid] != sym_cst){ // RPI 1044
    	    		   cur_sid = add_wxtrn(token);
    	    	   }
    	    	   break;
	    	   }
	       }
	}
}
private int add_extrn(String token){
	/*
	 * add EXTRN using V vs S key index
	 */
	   int index  = tz390.find_key_index('V',token.toUpperCase());
	   if (index == -1){
		   index = add_sym(token);
	   }
	   if (index >= 1){
		   if (sym_def[index] == sym_def_ref
			   && sym_attr[index] == tz390.ascii_to_ebcdic['U']){ 			   
			   sym_type[index] = sym_ext;
			   sym_attr[index] = tz390.ascii_to_ebcdic['T']; // RPI 415
			   sym_esd[index] = add_esd(index,sym_ext);
		   }
	   } else {
		   abort_error(153,"symbol table error on add extrn " + token);
	   }
	   return index; // RPI 1044
}
private int add_wxtrn(String token){
	/*
	 * add WXTRN using V vs S key index
	 */
	   int index  = tz390.find_key_index('V',token.toUpperCase());
	   if (index == -1){
		   index = add_sym(token);
	   }
	   if (index >= 1){
		   if (sym_def[index] == sym_def_ref
			   && sym_attr[index] == tz390.ascii_to_ebcdic['U']){ 			   
			   sym_type[index] = sym_wxt;
			   sym_attr[index] = tz390.ascii_to_ebcdic['S']; // RPI 415
			   sym_esd[index] = add_esd(index,sym_wxt);
		   }
	   } else {
		   abort_error(154,"symbol table error on add wxtrn " + token);
	   }
	   return index; // RPI 1044
}
private void add_entry(String token){
	/*
	 * add ENTRY 
	 */
	   if (sym_type[cur_sid] == sym_rel 
			|| sym_type[cur_sid] == sym_cst){ // RPI 288
           int index = 1;
           while (index <= tot_esd){ 
        	   if (esd_sid[index] == cur_sid){
        		   return;  // ESD already defined
        	   }
        	   index++;
           }
		   add_esd(cur_sid,sym_ent);
       } else {
		   log_error(97,"invalid entry type symbol - " + sym_name[cur_sid]);
		   cur_sid = -1;
       }
}
private void process_sect(byte sect_type,String sect_name){
	/*
	 * add or update csect, dsect, or loctr
	 * indicated by sym_cst, sym_dst, or sym_lct type parm
	 * Steps:
	 *   1.  Update previous section if any with 
	 *       max length and any loctr pointers
	 *   2.  If name omitted used private cst/dst  RPI 254
	 *   3.  Add new section if not found or external
	 *       reference found as local label.
	 *   4.  Reset location counter to end of 
	 *       current section.
	 *   5.  Update prev section type and sid for
	 *       use in processing sym_lct sections.
	 */
	cur_sym_sect = true;  // RPI 553
	if (cur_esd_sid > 0){
		update_sect();
	}
	if (sect_name == null 
		|| sect_name.length() == 0){
		sect_name = private_csect;  // private code
		if (mz390_call && cur_pass == 1){
			az390_private_sect = true; // RPI 995
		}
	}
	cur_esd_sid = find_sym(sect_name);
	if (cur_esd_sid < 1 
		|| sym_def[cur_esd_sid] == sym_def_ref){  
	   // new section RPI 415 
	   if (cur_esd_sid == -1){ 
		   // add for first time 
		   cur_sid = add_sym(sect_name);
	   } else {
		   // replacing existing symbol
		   cur_sid = cur_esd_sid;
		   cur_esd_sid = -1;
	   }
	   if (cur_sid >= 1
		   && sym_def[cur_sid] <= sym_def_ref){  
		   if (sym_type[cur_sid] != sym_ext 
			   && sym_type[cur_sid] != sym_wxt
			  ){
			   cur_esd = add_esd(cur_sid,sect_type);
			   if (sect_type == sym_dst){  // RPI 44
				   loc_ctr = 0;  // reset for first time dsect
			   }
		   } else {
			   cur_esd = sym_esd[cur_sid]; 
		   }
	   }
	} else if (sym_def[cur_esd_sid] <= sym_def_ref){ 
		cur_sid = cur_esd_sid;
		cur_esd_sid = -1;
		if (sect_type == sym_dst){  // RPI 44
		    loc_ctr = 0;  // reset for first time dsect
		}
		init_sym_entry();
		if (!lookahead_mode){ 
			cur_esd = add_esd(cur_sid,sect_type);
		}
	}
	if  (!lookahead_mode){ 
		if (cur_esd_sid < 1    // new section or extrn redefine
	        || sym_def[cur_esd_sid] == sym_def_ref){  //RPI182
			if (sect_type != sym_lct){
				loc_ctr = (loc_ctr + 7)/8*8;
			}
			if (cur_esd_sid < 1){
				cur_esd_sid = cur_sid; // new sect sid
			} else {
				cur_sid = cur_esd_sid;          // cvt ext to csect
				cur_esd = sym_esd[cur_esd_sid]; 
				sym_def[cur_sid] = bal_line_index; 
			}
			esd_sid[cur_esd]  = cur_sid;
			sym_esd[cur_sid]  = cur_esd;
			sym_def[cur_sid]  = bal_line_index;
			sym_attr[cur_sid] = bal_lab_attr; 
			sym_type[cur_sid] = sect_type;
			sym_loc[cur_sid]  = loc_ctr;
			sym_len[cur_sid]  = 0;
			add_sym_xref(cur_sid);
			if (sect_type == sym_lct){
				if (prev_sect_type != 0){
					while (sym_sect_next[prev_sect_sid] > 0){
						// RPI 372 chain new loctr to end of loctrs 
						prev_sect_sid = sym_sect_next[prev_sect_sid];
					}
					sym_sect_prev[cur_esd_sid] = prev_sect_sid;
					sym_sect_next[prev_sect_sid] = cur_esd_sid;
					sym_type[cur_esd_sid] = prev_sect_type;
					esd_base[cur_esd] = esd_base[prev_sect_esd]; // RPI 301
				} else {
					log_error(90,"LOCTR must follow CSECT or DSECT");
					sym_type[cur_esd_sid]  = sym_cst;
				}
			}
		} else if (sect_type == sym_type[cur_esd_sid]
		           || sect_type == sym_lct
		           || sym_type[cur_esd_sid] == sym_lct
                  ){ // RPI 553  
			// update prev section
			cur_esd = sym_esd[cur_esd_sid];
			loc_ctr = esd_loc[cur_esd]; // rpi 778
            if (loc_ctr < start_loc)           // RPI 1523
               {loc_ctr = start_loc;           // RPI 1523
                start_loc = 0;                 // RPI 1523
                }                              // RPI 1523
			} else {
			log_error(182,"Duplicate section name of different type");
		}
		prev_sect_type = sym_type[cur_esd_sid];
		prev_sect_esd  = sym_esd[cur_esd_sid];
		prev_sect_sid = cur_esd_sid;
		loc_start = loc_ctr;
	}
}
public int find_sym(String name){ // RPI 415 public
	/*
	 * 1.  Return defined symbol index else -1
	 * 2.  If not lookahead mode
	 *        if found, add xref
	 *        else if vcon mode, add extrn
	 * 
	 */
	int index = 0;
	if (dcv_type){
		index  = tz390.find_key_index('V',name.toUpperCase());
		if (index == -1){
			index = add_extrn(name);
		}
		add_sym_xref(index);
		return index;
	}
	index  = tz390.find_key_index('S',name.toUpperCase());
	if (!lookahead_mode){
		if (index != -1
			&& sym_def[index] != sym_def_lookahead){ // RPI 415 
			add_sym_xref(index);
			if (sym_type[index] == sym_und){  // RPI 694
			  	log_error(198,"symbol not defined " + sym_name[index]);
			} else if (exp_equ && bal_line_index == sym_def[index]){
				log_error(200,"circular EQU expression error for " + sym_name[index]); // RPI 749
			} else if (exp_lit_mod && bal_line_index < sym_def[index]){
				log_error(201,"literal modifier forward reference for " + sym_name[index]); // RPI 749
			}
		} else if (exp_rld_len > 0){
			index  = tz390.find_key_index('V',name.toUpperCase());
		}
	}
	return index;
}

public void update_label(){ // RPI 415
	/*
	 * add or update relative labels
	 * and exclude CST, DST, EQU, USING symbols
	 */
	label_match = label_pattern.matcher(bal_label);  // RPI 253
	if (!label_match.find()
	   || !label_match.group().equals(bal_label)){
	   log_error(141,"invalid symbol - " + bal_label);
	   return;
	}
	cur_sid = find_sym(bal_label);
	if (cur_sid < 1){
	   if (bal_op.equals("USING")){
		   return;
	   }
	   cur_sid = add_sym(bal_label);
       init_sym_entry();
	} else if (sym_def[cur_sid] <= sym_def_ref){
		init_sym_entry();
	} else if (sym_def[cur_sid] == bal_line_index){
		if (sym_type[cur_sid] == sym_rel
		    && !bal_op.equals("EQU")){	
			if (sym_loc[cur_sid] != loc_start){ // RPI 605
				sect_change_error();
				if (tz390.opt_trace // RPI 726 
					&& gen_obj_code 
					&& report_label_changes){
					report_label_changes = false;
				    log_error(187,"first label address change for " + bal_label + " from " + tz390.get_hex(sym_loc[cur_sid],6) + " to " + tz390.get_hex(loc_start,6));
				}
			}
	 	    sym_loc[cur_sid] = loc_start;
	   	    if (loc_len == 0){
	   	        sym_len[cur_sid] = dc_first_len;
	   	        sym_scale[cur_sid] = dc_first_scale; // RPI 481
	   	        sym_dc_type[cur_sid]     = (byte) dc_first_type; // RPI 790
	   	        sym_dc_type_sfx[cur_sid] = (byte) dc_first_type_sfx; // RIP 790
	   	    } else {
	   	        sym_len[cur_sid] = loc_len;
	   	    }
	   	}
	} else if (sym_def[cur_sid] > sym_def_ref 
			   && sym_attr[cur_sid] != tz390.ascii_to_ebcdic['M']   // allow redefine macro label
			   && (sym_attr[cur_sid] != tz390.ascii_to_ebcdic['J']  // RPI 182 
			       || (!cur_sym_sect                                // RPI 553 don't allow sect + RX
			    	   && sym_loc[cur_sid] != loc_ctr               // unless RX address = section start           
			    	  )
			      )                                          
              ){
		duplicate_symbol_error();
	}
}
private void init_sym_entry(){
	/*
	 * init sym variables for new or 
	 * existing lookahead symbol table entry
	 */
	   if (lookahead_mode){
		   sym_def[cur_sid] = sym_def_lookahead;
	   } else {
		   sym_def[cur_sid] = bal_line_index;
	   }
	   sym_type[cur_sid]  = sym_rel;
	   sym_attr[cur_sid]  = bal_lab_attr;
	   sym_attr_elt[cur_sid] = bal_lab_attr_elt;
	   sym_esd[cur_sid]   = cur_esd;
	   sym_loc[cur_sid]   = loc_start;
	   if (loc_len == 0){
		   sym_len[cur_sid]   = dc_first_len;
		   sym_scale[cur_sid] = dc_first_scale; // RPI 481
		   sym_dc_type[cur_sid]     = (byte)dc_first_type; // RPI 790
		   sym_dc_type_sfx[cur_sid] = (byte)dc_first_type_sfx;
	   } else {
		   sym_len[cur_sid] = loc_len;
	   }
}
private int get_sym_len(int index){
	/*
	 * return total length of csect or dsect
	 */
    switch (sym_type[index]){
    case 1: // CSECT or CSECT LOCTR
    case 2: // DSECT or DSECT LOCTR
    	int tot_len = sym_max_loc[index] - sym_loc[index];
	    if (sym_sect_prev[index] == 0){
    	   while (sym_sect_next[index] > 0){
		         index = sym_sect_next[index];
		         tot_len = tot_len + sym_max_loc[index] - sym_loc[index];
	       }
    	   tot_len = (tot_len + 7)/8*8;
	    }
	    return tot_len;
    default: // other types (abs, rel, etc.)
    	return sym_len[index];
    }
}
private void update_sect(){
	/*
	 * update length of current section
	 * and save current loc_ctr for continue
	 */
	 if (loc_ctr - sym_loc[cur_esd_sid] > sym_len[cur_esd_sid]){
	 	sym_len[cur_esd_sid] = loc_ctr - sym_loc[cur_esd_sid];
	 }
	 esd_loc[cur_esd] = loc_ctr; // rpi 778
}
public void process_dc(int request_type){ // RPI 415
    /*
     * processing by request type:
     * 1.  parse ds/dc bal statement and allocate
     *     or gen data items including alignment
     *     bytes where required.
     * 2.  find or add literal table entry using
     *     ds/dc type single parm following = parm
     * 3.  generate literal pool dc using loc_ctr
     *     set to referenced instruction.
     * 
     * if LTORG, gen lits
     * 
     * if not DC/DS/LTORG set dc_lit and process first
     * field as literal and update exp_index
     */
	 switch (request_type){
	     case 1: // process ds/dc statements
	    	 check_private_csect(); 
      	     dc_field = bal_parms;
      	     if (dc_field == null){ // RPI 978
      	    	 log_error(205,"DS/DC missing operand");
      	     }
	 	     dc_index = 0;
	         dc_lit_ref = false;
	         dc_lit_gen = false;
	         if (bal_op.equals("DC")
	 	         && gen_obj_code
		         && sym_type[esd_sid[esd_base[cur_esd]]] == sym_cst){ // RPI 564
		 	 	dc_op = true;
		 	 } else { 
			 	dc_op = false;
	         }
	         break;
	     case 2:  // find or add literal table entry 
	    	 lit_loc_ref = false;
	 	     dc_field = exp_text;
		     dc_index = exp_index + 1;
	 	     dc_lit_ref = true;
	 	     dc_lit_gen = false;
	 	     dc_op = false;
		     dc_lit_index_start = dc_index;
		     break;
		 case 3:  // generate literal table entry
			 check_private_csect(); 
		 	 dc_field = lit_name[cur_lit];
		 	 dc_index = 0;
		 	 obj_code = "";
		 	 list_obj_code = "";
		 	 dc_lit_ref = false;
		 	 dc_lit_gen = true;
		 	 if (gen_obj_code 
		 		 && sym_type[esd_sid[esd_base[cur_esd]]] == sym_cst){ // RPI 564
		 	 	dc_op = true;
		 	 } else { 
		 	 	dc_op = false;
		 	 }
		 	 break;
	 }
	 dc_first_field  = true;
	 dc_bit_len      = false; // RPI 417
	 dc_bit_tot      = 0;
	 dc_len_explicit = false;
	 while (!bal_abort 
			&& dc_index < dc_field.length()){
	       if (dc_field.charAt(dc_index) == ',' 
	       	   && !dc_lit_ref){
	       	  dc_index++;
	       } else if (dc_field.charAt(dc_index) <= ' '){ //RPI181
	       	  dc_index = dc_field.length();  // flush trailing comments
	       }
	       if (dc_lit_ref){
	    	   exp_lit_mod = true; // RPI 749
	       }
	       get_dc_field_dup();
	       get_dc_field_type();
	       get_dc_field_modifiers(); // RPI 368
	       exp_lit_mod = false; // RPI 749
	       if  (dc_index < dc_field.length() 
	       		&& dc_field.charAt(dc_index) != ','
	       	    && dc_field.charAt(dc_index) > ' '){ //RPI181
	    	   // process field data
	       	   if (bal_abort || dc_field.charAt(dc_index) 
	       	  		!= dc_type_delimiter[dc_type_index]){
	       		  if (dc_type != 'C' || 
	       				  (dc_field.charAt(dc_index) != '"'      //RPI5
	       			       && dc_field.charAt(dc_index) != '!')){ //RPI73  
	       			  log_error(45,"invalid dc delimiter for type - " + dc_field.substring(0,dc_index+1));
	       		      return;
	       		  }
	       	   }
	       	   dc_eod = false;
	           switch (dc_type){
		          case 'A': // (exp1,,expn)
		       	     process_dca_data();
   	  	             break;
   	  	       	  case 'B': // '0|1,0|1'
  	  	       	  	 process_dcb_data();
  	  	       	  	 break;
   	  	          case 'C': // 'text'
   	  	       	     process_dcc_data();
   	  	       	     break;
   	  	       	  case 'D': // 'fp,fp'
  	  	       	  	 process_dc_fp_data();
  	  	       	  	 break;
   	  	       	  case 'E': // 'fp,fp'
  	  	       	  	 process_dc_fp_data();
  	  	       	  	 break;
   	  	       	  case 'F': // 'int,int'
   	  	       	  	 process_dcf_data();
   	  	       	  	 break;
   	  	       	  case 'H': // 'int,int'
  	  	       	  	 process_dch_data();
  	  	       	  	 break;
   	  	       	  case 'L': // 'fp,fp'
 	  	       	  	 process_dc_fp_data();
 	  	       	  	 break;
  	  	       	  case 'P': // 'int,int'
  	  	       	  	 process_dcp_data();
  	  	       	  	 break;
  	  	       	  case 'S': // (exp1,expn)
  	  	       		 if (dc_bit_len){
  	  	       			  log_error(172,"DC S invalid length");
  	  	       			  return;
  	  	       		 }
  	  	       	     process_dcs_data();
  	  	       	     break;
  	  	       	  case 'V': // (exp1,expn)
  	  	       		  if (dc_bit_len){
  	  	       			  log_error(173,"DC V invalid length");
  	  	       			  return;
  	  	       		  } 
  	  	      		  if (cur_esd > 0 && sym_type[esd_sid[esd_base[cur_esd]]] == sym_cst){ // RPI 564
  	  	       			 dcv_type = true;
  	  	       			 process_dca_data();
  	  	       			 dcv_type = false;
  	  	      		 } else {
  	  	      			 dc_op = false;
  	  	       			 process_dca_data();
  	  	      		 }
   	  	       	  	 break;	 
   	  	       	  case 'X': // 'int,int'
  	  	       	  	 process_dcx_data();
  	  	       	  	 break;
		          case 'Y': // (exp1,,expn) length 2
			       	     process_dca_data();
	   	  	             break;
   	  	          case 'Z': // 'zoned decimals'
    	  	       	     process_dcz_data();
    	  	       	     break;
   	  	          default:
   	  	       	     log_error(44,"invalid dc type delimiter");
   	  	       	     break;
	           }
	       } else { 
	    	    // no field data so fill with zeros
	    	    if (dc_op && dc_dup > 0){
	    		   log_error(189,"DC field with no data"); // RPI 609
	    	    }
                dc_fill(dc_dup * dc_len); // RPI 265 align within ds/dc
	    	    dc_len = 0;
	       }
	       dc_first_field = false;
	       if (dc_lit_ref || dc_lit_gen){
	   	      if (dc_lit_gen && !gen_obj_code){
		   	     lit_loc[cur_lit] = loc_start;
		      }
	 	   	  exp_index = dc_index;
	 	   	  dc_lit_ref = false;
			  dc_lit_gen = false;
			  return;
	       }
	       if (!(dc_index < dc_field.length()) 
	       		|| dc_field.charAt(dc_index) <= ' '  //RPI181
	       		|| dc_field.charAt(dc_index) == dc_type_delimiter[dc_type_index]
			  ){
	    	  if (dc_bit_len){
	            	 flush_dc_bits(); // RPI 417
	          }
	       	  return;

			 // #325 code added to detect invalid connector/terminator on DC
			} else if (dc_field.charAt(dc_index) != ','
		               && dc_field.charAt(dc_index) != '\''){
			        log_error(54,"invalid dc field terminator =" + dc_field
					  + " len=" + dc_field.length()
					  + " char=" + dc_field.charAt(dc_index)
					  ); 
		            }
			}
			// #325 end of code
	
	 if (dc_bit_len){
     	 flush_dc_bits(); // RPI 417
	 }
	 if (gen_obj_code && loc_ctr != cur_text_loc){
		 log_error(174,"location counter / hex object code error");
	 }
}
private boolean calc_abs_exp(){
	/*
	 * calculate abs value else abort
	 */
	if (calc_exp()
			&& exp_esd == 0){
		return true;
	} else {
		log_error(32,"invalid absolute value");
	}
	return false;
}
private boolean calc_rel_exp(){
	/*
	 * calculate rel value else abort
	 */
	if (calc_exp()
			&& exp_esd > 0){
		return true;
	} else {
		log_error(33,"invalid relative value");
	}
	return false;
}
private boolean calc_dca_exp(){
	/*
	 * set dca_ignore_refs for A and V type
	 * symbol refs if DS or DSECT
	 */
	if (!dc_op || dcv_type){ 
		dca_ignore_refs = true;
		boolean temp_rc = calc_exp();
		dca_ignore_refs = false;
		return temp_rc;
	} else {
		return calc_exp();
	}
}
private boolean calc_exp(){
	/*
	 * parse abs/rel expression starting at
	 * exp_text.charAt(exp_index)
	 * return true if ok and set
	 * 1. exp_val = abs or rel offset
	 * 2. exp_esd = abs 0 or cst/dst esd
	 * 3. exp_index = index to terminator
	 * which can be end of string, or (),
	 */
	   if (exp_text == null || exp_index > exp_text.length()){
	   	   exp_text = ""; // RPI 356
		   return false;
	   }
       exp_match = exp_pattern.matcher(exp_text.substring(exp_index));
       exp_state = 1;
       exp_term = false;
       exp_eot  = false;
       exp_first_sym_len = true; // is this first exp symbol length
       exp_use_lab = null; // RPI 375
       exp_len = 1;
       tot_exp_stk_sym = 0;
       tot_exp_stk_op  = 0;
       tot_exp_rld_add = 0;
       tot_exp_rld_sub = 0;
       exp_sym_pushed = false;
       exp_sym_last = false;
   	   exp_level = 0;
   	   exp_op = " ";
   	   exp_type = sym_sdt;
   	   exp_attr = tz390.ascii_to_ebcdic['U'];
	   while (!exp_term && !bal_abort){
	   	   if (!exp_op.equals(exp_term_op) && exp_match.find()){
	          exp_token = exp_match.group();
	          exp_index = exp_index + exp_token.length();
	   	   } else {
	   	   	  exp_token = "" + exp_term_op;
	   	   	  exp_eot = true;
	   	   }
		   proc_exp_token();
	   }
	   if (!bal_abort){
		   if (!exp_eot){
			   exp_index--;  // backup to terminator
		   }
   	      return true;
       } else {
       	  return false;
       }
}
private void proc_exp_token(){
	/*
	 * parse general expression tokens
	 *   1. push sym or sdt
	 *   2. exec or push operations + - * /
	 *   3. terminate on end of string or (),
	 */
	check_prev_op = true;
	while (check_prev_op && !bal_abort){
	    exp_op = exp_token.toUpperCase();
	    switch (exp_op.charAt(0)){
	        case '+':
	        	if (!exp_sym_pushed){
	        		exp_token = "U+";
	        		exp_op = exp_token;
	        	}
	        	proc_exp_op();
	            break;
	        case '-':
	        	if (!exp_sym_pushed){
	        		exp_token = "U-";
	        		exp_op = exp_token;
	        	}
	        	proc_exp_op();
	            break;
	        case '*':
	        	if  (exp_sym_last){
	        	    proc_exp_op();
	        	} else {
	        		proc_loc_ctr();
	        	}
	            break;
	        case '/':
	        	proc_exp_op();
	            break;
	        case '(':
	        	if (exp_level == 0
	        		&& exp_sym_last
	        		&& tot_exp_stk_sym > 0){
	        		exp_op = exp_term_op;
	        	}
	        	proc_exp_op();
	            break;
	        case ')':
	        	proc_exp_op();
	            break;
	        case '\t': //tab  RPI181
	        case '\r': //cr
	        case '\n': //lf
	        case ' ':  //space
	        	exp_op = exp_term_op;
	        	proc_exp_op();
	            break;
	        case ',':
	        case '\'': // terminator for DCF, DCH, expressions
	        	exp_op = exp_term_op;
	        	proc_exp_op();
	            break;
	        case '~':  // terminator
	        	proc_exp_op();
	            break;
	        case 'B':
	        	if (exp_token.length() > 2 && exp_token.charAt(exp_token.length()-1) == '\''){ // RPI 270
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'C':
	        	if (exp_token.length() > 1 
	        		&& (exp_token.charAt(exp_token.length()-1) == '\''      //RPI 270
	        			|| exp_token.charAt(exp_token.length()-1) == '"'    //RPI5
	        		    || exp_token.charAt(exp_token.length()-1) == '!')){ //RPI73,RPI90
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'I':  // RPI 790 I' operator
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
	        	   proc_exp_op();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'L':
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
	        	   proc_exp_op();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'S':  // RPI 790 S' operator
	        	if (exp_token.length() > 1 && exp_token.charAt(1) == '\''){
	        	   proc_exp_op();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case 'U':
	        	if (exp_token.length() == 2
	        		&& (exp_token.charAt(1) == '-'
	        			|| exp_token.charAt(1) == '+')){
		        	   proc_exp_op();
		        	} else {
		        	   proc_exp_sym();
		        	}
		        	break;
	        case 'X':
	        	if (exp_token.length() > 1 && exp_token.charAt(exp_token.length()-1) == '\''){ //RPI 270
	        	   proc_exp_sdt();
	        	} else {
	        	   proc_exp_sym();
	        	}
	            break;
	        case '=':
                push_exp_lit(); // RPI 365
	        	break;
	        default:
		        if (exp_op.charAt(0) <= '9' && exp_op.charAt(0) >= '0'){
		        	proc_exp_sdt();
		        } else {
	        	    proc_exp_sym();
		        }
	            break;
	    }
	}
}
private void proc_loc_ctr(){
	/*
	 * push current location counter on stack
	 */
    exp_sym_last = true;
    check_prev_op = false;
	if (inc_tot_exp_stk_sym()){
	   if (cur_esd > 0){ 
          exp_stk_sym_esd[tot_exp_stk_sym-1]= esd_base[cur_esd];  // RPI 301
          if (dc_lit_ref || dc_lit_gen){
          	 lit_loc_ref = true;
          	 exp_stk_sym_val[tot_exp_stk_sym-1] = lit_line_loc[cur_lit] + dc_dup_loc;
          } else {
             exp_stk_sym_val[tot_exp_stk_sym-1] = loc_ctr;
          }
	   } else {
	   	  log_error(27,"location counter undefined");
	   }
    }
}
private void proc_exp_sym(){
	if (exp_token.length() > 1 && exp_token.charAt(exp_token.length()-1) == '.'){
		if (gen_obj_code){
		   exp_use_lab = exp_token.substring(0,exp_token.length()-1).toUpperCase(); //RPI 987 add uc
		}
	} else {
        exp_sym_last = true;
        push_exp_sym();
	}
    check_prev_op = false;
}
private void proc_exp_sdt(){
	exp_sym_last = true;
    push_exp_sdt(exp_token);  // RPI 415 (was exp_op in caps)
    check_prev_op = false;
}
private void proc_exp_op(){
	if  (tot_exp_stk_op > 0){
	    exp_prev_op = exp_stk_op[tot_exp_stk_op -1];
	} else {
		exp_prev_op = exp_start_op;
	}
    if (tz390.opt_traceall){
    	tz390.put_trace("EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_sym + " PREV OP = " + exp_prev_op +  " NEXT OP = " + exp_token);
    }
	int prev_op_class = exp_op_class[exp_prev_op.charAt(0)];
	if  (prev_op_class == 0){
		log_error(11,"invalid operator class for - " + exp_prev_op);
		return;
	} 
	int next_op_class = exp_op_class[exp_op.charAt(0)];
	if  (next_op_class == 0){
		log_error(12,"invalid operator class - " + exp_op);
	    return;
	}
    int action = exp_action[tot_classes*(prev_op_class-1)+next_op_class-1];
    if (tz390.opt_traceall){
    	tz390.put_trace("EXP OPS=" + tot_exp_stk_op + " VARS=" + tot_exp_stk_sym + " ACTION = " + action + " PREV CLASS = " + prev_op_class + " NEXT CLASS = " + next_op_class);
    }
    switch (action){
    case 1: // add/sub
       if (exp_prev_op.equals("+")){
          exp_add();
       } else {
       	  exp_sub();
       }
	   break;
    case 2:  // mpy/div
       if (exp_prev_op.equals("*")){
          exp_mpy();
       } else {
       	  exp_div();
       }
  	   break;
    case 3: // (
  	   exp_push_op();
  	   if (exp_op.charAt(0) == '('){
  	      exp_level++;
  	   }
  	   check_prev_op = false;
  	   break;
    case 4: // )
  	   exp_pop_op();
  	   exp_level--;
  	   if (exp_level == 0 && bal_op.equals("AIF")){
  	   	  exp_op = exp_term_op;
  	   	  exp_term();
  	   }
  	   check_prev_op = false;
       break;
    case 5: //  PFX operators (I', L', S', or U+ or U-) RPI 790
    	 exp_pop_op();          //RPI9
    	 switch (exp_stk_op[tot_exp_stk_op].charAt(0)){
    	 case 'I': // Integer operator
    		 exp_integer_op();
    		 break;
    	 case 'L': // length operator
    	 	 exp_len_op();
    	 	 break;
    	 case 'S': // Scale operator
    		 exp_scale_op();
    		 break;
    	 case 'U': // unary operator
    		 if (exp_sym_pushed){
    			 if (exp_stk_op[tot_exp_stk_op].charAt(1) == '-'){
    				 exp_stk_sym_val[tot_exp_stk_sym-1] = - exp_stk_sym_val[tot_exp_stk_sym -1];
    			 }
    		 } else if (exp_token.charAt(0) == 'U'){
    			 if (exp_token.charAt(1) == exp_stk_op[tot_exp_stk_op].charAt(1)){
    				 exp_token = "U+";
    			 } else {
    				 exp_token = "U-";
    			 }
    			 exp_op = exp_token;
    		 } else {
    			 log_error(123,"missing unary operand value");
    		 }
    		 break;
    	 default:
    		 log_error(124,"invalid prefix operator type");
    	 }
         break;
    case 6: // terminator space, comma, unmatched )
  	   exp_term();
  	   check_prev_op = false;
  	   break;
  	case 7: // check if ( is terminator after value
  	   if (exp_sym_last){
   	       exp_term();
  	   } else {
   	       exp_push_op();
           exp_level++;
  	   }
  	   check_prev_op = false;
  	   break;
    default:
    	log_error(13,"expression parsing error"); // RPI 260
    }
}
private void exp_add(){
	/* add top of stack value to prev. value
	 * and pop the top stack value off
	 */
	get_stk_sym();
	if  (sym_esd1 != 0 || sym_esd2 != 0){
	    if  ((sym_esd1 > 0) && (sym_esd2 > 0)){
		    add_rld(sym_esd1);
		    add_rld(sym_esd2);
		    sym_type1 = sym_rld;
		    sym_esd1  = esd_cpx_rld;
	    } else if ((sym_esd1 == esd_cpx_rld) || (sym_esd2 == esd_cpx_rld)){
		    if (sym_esd1 > 0){
		       add_rld(sym_esd1);
		    } else if (sym_esd2 > 0){
			   add_rld(sym_esd2);
		    }
		    sym_type1 = sym_rld;
		    sym_esd1  = esd_cpx_rld;
	    } else {
	    	if (sym_esd2 > 0){
	    		sym_esd1 = sym_esd2;
	    	}
	    }
	}
	sym_val1 = sym_val1 + sym_val2;
	put_stk_sym();
}
private void exp_sub(){
	/* sub top of stack value from prev. value
	 * and pop the top stack value off
	 */
	get_stk_sym();
	if  (sym_esd1 > 0 || sym_esd2 > 0){
	    if  (sym_esd1 > 0 && sym_esd2 > 0){
		    if (sym_esd1 == sym_esd2){
		 	   sym_esd1 = 0;
		 	   sym_esd2 = 0;
		 	   sym_type1 = sym_sdt;
		 	   sym_type2 = sym_sdt;
		    } else {
		       add_rld(sym_esd1);
		       sub_rld(sym_esd2);
		       sym_type1 = sym_rld;
		       sym_esd1  = esd_cpx_rld;
		    }
	    } else if (sym_esd1 == esd_cpx_rld 
			|| sym_esd2 == esd_cpx_rld
			|| sym_esd2 > 0){
		    if (sym_esd1 > 0){
		       add_rld(sym_esd1);
		    } else if (sym_esd2 > 0){
			   sub_rld(sym_esd2);
		    }
		    sym_type1 = sym_rld;
		    sym_esd1  = esd_cpx_rld;
	    }
	}
	sym_val1 = sym_val1 - sym_val2;
	put_stk_sym();
}
private void exp_mpy(){
	/* mpy top of stack value to prev. value * and pop the top stack value off
	 */
	get_stk_sym();
	if (sym_esd1 != 0 || sym_esd2 != 0){
		log_error(58,"invalid rld multiplication - " + exp_text.substring(0,exp_index));
		return;
	}
	sym_val1 = sym_val1 * sym_val2;
	put_stk_sym();
}
private void exp_div(){
	/* div top of stack value into prev. value
	 * and pop the top stack value off
	 */
	get_stk_sym();
	if (sym_esd1 != 0 || sym_esd2 != 0){
		log_error(59,"invalid rld division - " + exp_text.substring(0,exp_index));
		return;
	}
	if (sym_val2 == 0 && tz390.opt_allow){ 
		log_error(60,"invalid rld division - " + exp_text.substring(0,exp_index));
		return;
	}
	if (sym_val2 != 0){ 
	    sym_val1 = sym_val1 / sym_val2;
	} else {
		sym_val1 = 0;  // by definition for HLASM
	}
	put_stk_sym();
}
private void exp_integer_op(){ // RPI 790
	/*
	 * replace sym or lit on stack
	 * with integer I' attribute value
	 */
	if (tot_exp_stk_sym >= 1){
		int temp_int = -1;
		if (cur_sid >  0){
			temp_int = get_int_pfx(sym_dc_type[cur_sid],sym_dc_type_sfx[cur_sid],sym_len[cur_sid],sym_scale[cur_sid]);
		} else if (cur_lit >= 0){
			temp_int = get_int_pfx(lit_dc_type[cur_lit],lit_dc_type_sfx[cur_lit],lit_len[cur_lit],lit_scale[cur_lit]);
		}
		if (temp_int >= 0){
		   exp_stk_sym_val[tot_exp_stk_sym - 1] = temp_int;
		   exp_stk_sym_esd[tot_exp_stk_sym - 1] = sym_sdt;
		} else {
		   log_error(25,"invalid symbol for length attribute operator");
		}
	} else {
		log_error(26,"missing symbol for integer attribute");
	}
}
private void exp_len_op(){
	/*
	 * replace sym or lit on stack
	 * with length value
	 */
	if (tot_exp_stk_sym >= 1){
		int temp_len = -1;
		if (cur_sid >  0){
			temp_len = sym_len[cur_sid];
		} else if (cur_lit >= 0){
			temp_len = lit_len[cur_lit];
		}
		if (temp_len >= 0){
		   exp_stk_sym_val[tot_exp_stk_sym - 1] = temp_len;
		   exp_stk_sym_esd[tot_exp_stk_sym - 1] = sym_sdt;
		} else {
		   log_error(25,"invalid symbol for length attribute operator");
		}
	} else {
		log_error(26,"missing symbol for length attribute");
	}
}
private void exp_scale_op(){ // RPI 790
	/*
	 * replace sym or lit on stack
	 * with scale value
	 */
	if (tot_exp_stk_sym >= 1){
		int temp_scale = -1;
		if (cur_sid >  0){
			temp_scale = sym_scale[cur_sid];
		} else if (cur_lit >= 0){
			temp_scale = lit_scale[cur_lit];
		}
		if (temp_scale >= 0){
			exp_stk_sym_val[tot_exp_stk_sym - 1] = temp_scale;
			exp_stk_sym_esd[tot_exp_stk_sym - 1] = sym_sdt;
		} else {
			   log_error(25,"invalid symbol for scale attribute operator");
		}
	} else {
		log_error(26,"missing symbol for length attribute");
	}
}
private void get_stk_sym(){
	/*
	 * set stk_value1 & 2 from top of stack
	 */
	if (tot_exp_stk_sym >= 2){
	    sym_esd1 = exp_stk_sym_esd[tot_exp_stk_sym - 2];
		sym_val1 = exp_stk_sym_val[tot_exp_stk_sym - 2];
	    if (sym_esd1 > 0){ // RPI 1198
	    	sym_type1 = sym_rel;
	    } else {
	    	sym_type1 = sym_sdt;
	    }
	    sym_esd2 = exp_stk_sym_esd[tot_exp_stk_sym - 1];
	    sym_val2 = exp_stk_sym_val[tot_exp_stk_sym - 1];
	    if (sym_esd2 > 0){
	    	sym_type2 = sym_rel;
	    } else {
	    	sym_type2 = sym_sdt;
	    }
	} else {
		log_error(17,"expression parsing error"); // RPI 260
	}
}
private void put_stk_sym(){
	/*
	 * pop operator from op stack
	 * pop sym_val2 off var stack
	 * replace original sym_val1 
	 * on top of stack with result
	 */
	if ((tot_exp_stk_sym >= 2) && (tot_exp_stk_op > 0)){
		tot_exp_stk_op--;
		tot_exp_stk_sym--;
	    exp_stk_sym_esd[tot_exp_stk_sym - 1] = sym_esd1;
		exp_stk_sym_val[tot_exp_stk_sym - 1] = sym_val1;
	} else {
		log_error(18,"expression parsing error");
	}
	exp_sym_last = true;
}

private void exp_push_op(){
	/*
	 * put current op on stack
	 * 
	 * if unary minus push 0 var first
	 * if unary plus skip the push
	 */
   	if (tot_exp_stk_op > max_exp_stk){
   		abort_error(20,"stack operation size exceeded");
   	}
   	exp_stk_op[tot_exp_stk_op] = exp_op; // RPI 270 was exp_token with lc l'
   	tot_exp_stk_op++;
   	exp_sym_pushed = false;
   	exp_sym_last = false;
}
private void exp_pop_op(){
	/*
	 * pop current op on stack
	 */
      tot_exp_stk_op--;
      if (tot_exp_stk_op < 0){
      	 log_error(21,"expression parsing error"); // RPI 260
      }
}
private void exp_term(){
	/*
	 * terminate expression returning
	 * value on stack if no errors
	 */
	if (tot_exp_stk_sym == 1 && tot_exp_stk_op == 0){
		exp_term = true;
    	exp_val = exp_stk_sym_val[0];
        exp_esd = exp_stk_sym_esd[0];
        if (exp_esd == esd_cpx_rld){
        	reduce_exp_rld();
        }
        if (exp_esd == esd_sdt){
           	exp_type = sym_sdt;
        } else if (exp_esd == esd_cpx_rld){
        	if (exp_rld_len > 0){ // RPI 893
        		if (gen_obj_code){
        			gen_exp_rld();
        		}    
            } else {
            	log_error(61,"invalid complex rld expression: " + exp_text.substring(0,exp_index)); // RPI 1034
            }
        } else {  
        	if (gen_obj_code){
        		if (exp_rld_len > 0){ // RPI 894
        			exp_rld_add_esd[0] = exp_esd;
        			tot_exp_rld_add = 1;
        			gen_exp_rld();
        		}
        	}
            exp_type = sym_rel;
        }
	} else {
		log_error(35,"expression parsing error"); // RPI 260
	}
}
private void push_exp_sym(){
	/*
	 * push symbol on stack else abort
	 * set cur_sid > 0 used by L'
	 */
	if (inc_tot_exp_stk_sym()){
	   cur_sid = find_sym(exp_token);
	   if (cur_sid > 0 && sym_type[cur_sid] == sym_und){
		   log_error(98,"symbol not found - " + exp_token); // #239 DSH
	   }
	   if (cur_sid > 0 
		   && (sym_def[cur_sid] >= sym_def_ref || lookahead_mode)){  // RPI 488
	   	  if (exp_first_sym_len){
	   	  	 exp_first_sym_len = false;
	   	  	 exp_len = sym_len[cur_sid];
	   	  }
          exp_stk_sym_esd[tot_exp_stk_sym-1]  = esd_base[sym_esd[cur_sid]]; // RPI 301
          exp_stk_sym_val[tot_exp_stk_sym-1]  = sym_loc[cur_sid];
	   } else {
		  if (dca_ignore_refs){
	          exp_stk_sym_esd[tot_exp_stk_sym-1]  = sym_sdt;
	          exp_stk_sym_val[tot_exp_stk_sym-1]  = 0;
		  } else {
			  log_error(98,"symbol not found - " + exp_token);
			  if (cur_sid < 0 && cur_pass > 1){
				  tot_missing_sym++;
				  cur_sid = add_sym(exp_token); // RPI 694
				  sym_type[cur_sid] = sym_und;  // RPI 694 
			      sym_loc[cur_sid]  = -1;       // RPI 694
			      sym_len[cur_sid]  = -1;       // RPI 694
			  }
		  }
	   }
    }
}
private void push_exp_lit(){  // RPI 365
	/*
	 * push literal on stack else abort
	 * and set cur_lit >= 0 and cur_sit = -1
	 * for L' to determine that literal is on
	 * the stack
	 * Note:
	 *  1. Literal must be first term in exp
	 *     since it may use calc_exp during
	 *     DC processing and then resets
	 *     exp stack with lit address
	 *     
	 */
	cur_sid = -1; // RPI 365
	if (inc_tot_exp_stk_sym()){
	   exp_index--;	
	   get_lit_addr();
	   if (!exp_match.find()){  // skip lit exp term
	       log_error(111,"invalid literal token");
	   }
	   if (cur_lit >= 0){ 
    	  exp_len = lit_len[cur_lit];
	   	  exp_stk_sym_esd[tot_exp_stk_sym-1]  = esd_base[lit_esd[cur_lit]]; // RPI 301
          exp_stk_sym_val[tot_exp_stk_sym-1]  = lit_loc[cur_lit];
	   } else {
		  if (dca_ignore_refs){
	          exp_stk_sym_esd[tot_exp_stk_sym-1]  = sym_sdt;
	          exp_stk_sym_val[tot_exp_stk_sym-1]  = 0;
		  } else {
			  log_error(110,"literal not found - " + exp_token);
		  }
	   }
    }
	exp_sym_last = true;
	check_prev_op = false;
}
private boolean inc_tot_exp_stk_sym(){
	/*
	 * check if room to add to exp_stack
	 * and return true else abort
	 */
	if (tot_exp_stk_sym < max_exp_stk){
		tot_exp_stk_sym++;
		exp_sym_pushed = true;
		return true;
	} else {
		abort_error(22,"maximum stack variables exceeded");
	    return false;
	}
}
private void push_exp_sdt(String sdt){
	/*
	 * push self defining abs term on stack
	 */
    	if (inc_tot_exp_stk_sym()){
           exp_stk_sym_esd[tot_exp_stk_sym-1] = sym_sdt;
           try {
        	   switch (sdt.toUpperCase().charAt(0)){
        	   case 'B': // B'11000001' binary
        		   exp_stk_sym_val[tot_exp_stk_sym-1] = Long.valueOf(sdt.substring(2,sdt.length()-1),2).intValue(); // RPI 1099 
        		   break;
        	   case 'C': //RPI192
        		   if (!tz390.get_sdt_char_int(sdt)){
        			   log_error(138,"invalid character sdt " + sdt);
        		   }
        		   exp_stk_sym_val[tot_exp_stk_sym-1] = tz390.sdt_char_int; 
        		   break;
        	   case 'X': // X'C1' hex
        		   exp_stk_sym_val[tot_exp_stk_sym-1] = Long.valueOf(sdt.substring(2,sdt.length()-1),16).intValue();
        		   break;
        	   default:
        		   exp_stk_sym_val[tot_exp_stk_sym-1] = (int) Double.valueOf(sdt).longValue();  // RPI 232 RPI 1101 
               	   break;
        	   }
           } catch (Exception e){
        	   log_error(163,"invalid sdt constant - " + sdt);
        	   exp_stk_sym_val[tot_exp_stk_sym-1] = 0;
           }
    	}
}

public void exit_az390(){
	/*
	 * put stats and display total errors
	 * after mz390 is done and close files.
	 * Note:
	 *   1.  return az390 return code for use by mz390
	 *       when called from mz390 when mfc option on.
	 */
      if (!mz390_call){ // RPI 935 let mz390 do it if running
    	  put_stats(); // rpi 846
      }
	  if (tz390.opt_errsum){
		report_critical_errors();
	  }
	  if (!mz390_call){ // RPI 935
		  close_files(); // RPI 935 let mz390 close after stats
	  }
	  if (mz390_call){ // RPI 415
		  return;
	  }
   	  System.exit(az390_rc);
}
public void put_stats(){
	/*
	 * display statistics on STA and 
	 * totals on STA and TRM including
	 * MZ and AZ totals.
	 */
	force_list_bal = true; // RPI 285
	tz390.force_nocon = true; // RPI 755
	if (tz390.opt_stats){  // RPI 453
	   put_stat_line("BAL lines             = " + (tot_bal_line-1));
	   put_stat_line("symbols               = " + tot_sym);
	   put_stat_line("Literals              = " + tot_lit);
	   put_stat_line("alloc passes          = " + (cur_pass-1));
	   put_stat_line("Keys                  = " + tz390.tot_key);
	   put_stat_line("Key searches          = " + tz390.tot_key_search);
	   if (tz390.tot_key_search > 0){
	       tz390.avg_key_comp = tz390.tot_key_comp/tz390.tot_key_search;
	   }
	   put_stat_line("Key avg comps         = " + tz390.avg_key_comp);
	   put_stat_line("Key max comps         = " + tz390.max_key_comp);
	   put_stat_line("ESD symbols           = " + tot_esd);
	   put_stat_line("object bytes          = " + tot_obj_bytes);
	   put_stat_line("object rlds           = " + tot_rld);
	   if (tz390.opt_timing){
	      cur_date = new Date();
	      tod_end = cur_date.getTime();
	      tot_sec = (tod_end - tod_start)/1000;
	   }
	}
	int index = 0;
	while (index < tot_xref_files){
		if (mz390_call && xref_file_errors[index] > 0){  // RPI 935
			String xref_msg = "FID=" + tz390.right_justify(""+(index+1),3) 
					        + " ERR=" + tz390.right_justify(""+xref_file_errors[index],4) 
 	                        + " " + xref_file_name[index];
			put_log(msg_id + xref_msg);
		    tz390.put_systerm(msg_id + xref_msg);
		}
		index++;
	}
	if (tz390.opt_stats){
		put_stat_line("total mnote warnings  = " + tot_mnote_warning); // RPI 402
		put_stat_line("total mnote errors    = " + tot_mnote_errors);
		put_stat_line("max   mnote level     = " + max_mnote_level);
		if (mz390_call){
			put_stat_line("total mz390 errors    = " + mz390_errors); // RPI 659
		}
		put_stat_line("total az390 errors    = " + az390_errors); // RPI 659
	}
	put_log(msg_id + "total mnote warnings = " + tot_mnote_warning); // RPI 402
	put_log(msg_id + "total mnote errors   = " + tot_mnote_errors);
	put_log(msg_id + "max   mnote level    = " + max_mnote_level);
	if (mz390_call){
		put_log(msg_id + "total mz390 errors   = " + mz390_errors); // RPI 659
	}
	put_log(msg_id + "total az390 errors   = " + az390_errors); // RPI 659
	if (mz390_call){
		tz390.systerm_prefix = tz390.left_justify(tz390.pgm_name,9) + " " + "MZ390 "; // RPI 755 RPI 1047
	}
	tz390.force_nocon = false; // RPI 755
}
private void put_stat_line(String msg){
	/*
	 * routine statistics line to PRN or STATS(file)
	 */
	if (tz390.stats_file != null){
		tz390.put_stat_line(msg);
	} else {
		put_log(msg_id + msg);
	}
}
public void close_files(){
	/*
	 * close output obj, prn, err, tra
	 */
	  if (obj_file != null){
	  	  try {
	  	  	  obj_file.close();
	  	  } catch (Exception e){
	  	  	  tz390.abort_error(24,"I/O error on obj close - " + e.toString());
	  	  }
	  }
	  if (!mz390_call){ // RPI 415 let mz390 close it
		  tz390.close_systerm(az390_rc);
	  } else {
		  if (mz390_rc > az390_rc){
			  az390_rc = mz390_rc;
		  }
		  tz390.set_ended_msg(az390_rc);
	  }
      tz390.force_nocon = true;
	  put_log(tz390.ended_msg);
	  if (!mz390_call && tz390.opt_tracea){ // RPI 935 RPI 1050 moved within NOCON
		  tz390.put_trace(tz390.ended_msg);
	  }
	  tz390.force_nocon = false;
	  if  (tz390.opt_list){
		  if (prn_file != null && prn_file.isFile()){
		  	  try {
		  	  	  prn_file_buff.close();
		  	  } catch (Exception e){
		  	  	  tz390.abort_error(24,"I/O error on prn close - " + e.toString());
		  	  }
		  }
	    }
	  tz390.close_trace_file();
}
private void log_error(int error,String msg){
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 * Notes:
	 *   1.  Set bal_abort if not set else exit
	 *   2.  supress if not gen_obj and not trace
	 *   3.  print bal line first if list on
	 */
	  if (bal_abort)return; // only 1 error per line
	  bal_abort = true;
	  az390_errors++;
	  if (gen_obj_code && az390_rc < 12){ // RPI 1062
		  az390_rc = 12;
	  }
	  if (gen_obj_code){ // RPI 484
		 if (!mz390_abort){ // RPI 433 don't duplicate mz error line
			 force_list_bal = true;  // RPI 285
			 list_bal_line();
		 }
   	     force_list_bal = true;  // RPI 285
         set_file_line_xref();
	     if (tz390.opt_errsum){  // RPI 694 (see mz390 log_error also
		     if (error == 29){   // undefined opcode
	             if (!add_missing_macro(bal_op)){
	            	 abort_error(199,"max missing macro/copy exceeded"); //  rpi 2202 raise max limit and description
	             }
		     }
	     } // RPI 895
    	 String error_msg = "AZ390E error " + tz390.right_justify("" + error,3); // RPI 1031
	     if (dc_lit_gen){ // RPI 1031
	    	 xref_file_line = " (" + (bal_line_xref_file_num[lit_line[cur_lit]]+1) + "/" + bal_line_xref_file_line[lit_line[cur_lit]] + ")";
	    	 error_msg = error_msg + tz390.right_justify(xref_file_line + bal_line_num[lit_line[cur_lit]],15) + "   " + bal_line_text[lit_line[cur_lit]];
	     } else {
	    	 error_msg = error_msg + tz390.right_justify(xref_file_line + bal_line_num[bal_line_index],15) + "   " + bal_line_text[bal_line_index];
	     }
	     put_log(error_msg);
	     tz390.put_systerm(error_msg);
	     error_msg = msg_id + msg;
	     tz390.force_nocon = true; // RPI 935 
	     put_log(error_msg);
	     tz390.force_nocon = false; // RPI 935
	     tz390.put_systerm(error_msg);
	     force_list_bal = false;  // RPI 285
	     list_bal_line = false; // RPI 891 suppress defail bal line 
	  }
	  if (gen_obj_code && tz390.max_errors != 0 && az390_errors > tz390.max_errors){
	  	 abort_error(49,"max errors exceeded");	 
	  }
}
private void set_file_line_xref(){
	/*
	 * set xref_file_line passed from mz390
	 * if available for use in error messages
	 */
	     if (mz390_call && xref_bal_index > -1){  // RPI 425 RPI 935
	    	 if (gen_obj_code){  // RPI 935 
	    		 xref_file_errors[bal_line_xref_file_num[xref_bal_index]]++;
	    	 }
   	    	 xref_file_line = " (" + (bal_line_xref_file_num[xref_bal_index]+1) + "/" + bal_line_xref_file_line[xref_bal_index] + ")";
   	     } else {
   	    	 xref_file_line = "";
   	     }
}
private synchronized void abort_error(int error,String msg){ // RPI 646
	/*
	 * issue error msg to log with prefix and
	 * inc error total
	 */
	  az390_errors++;
	  az390_rc = 16; // RPI 1062
	  if (az390_recursive_abort || tz390.z390_abort){
		 msg = msg_id + "aborting due to recursive abort error " + error + " - " + msg;
		 System.out.println(msg);
		 tz390.put_systerm(msg);
		 if (tz390.opt_errsum){
			report_critical_errors();
		 }
		 if (mz390_call){  // RPI 1062
			 tz390.systerm_prefix = tz390.left_justify(tz390.pgm_name,9) + " MZ390 ";
		 }
		 tz390.close_systerm(az390_rc);
		 bal_line_full = false;
	  	 System.exit(az390_rc);
	  }
	  az390_recursive_abort = true; // RPI 1062
	  bal_abort = true;        // RPI 415
	  tz390.z390_abort = true;
	  tz390.opt_con = true;    // RPI 453
	  force_list_bal = true;      // RPI 285
	  list_bal_line();
	  force_list_bal = true; // RPI 285
	  String error_msg = "AZ390E abort " + error + " on line " + bal_line_num[bal_line_index] + " " + bal_line_text[bal_line_index];
	  put_log(error_msg);
	  tz390.put_systerm(error_msg);
	  error_msg = msg_id + msg;
	  put_log(error_msg);
	  tz390.put_systerm(error_msg);
      exit_az390();
}
private void put_copyright(){
	   /*
	    * display az390 version, timestamp,
	    * and copyright if running standalone
	    */
	    tz390.force_nocon = true; // RPI 755
	   	if  (z390_log_text == null){
			put_log(msg_id + "Copyright (c) 2021 z390 Assembler LLC");
			put_log(msg_id + "z390 comes with ABSOLUTELY NO WARRANTY;");   
			put_log(msg_id + "This is free software, and you are welcome to redistribute it");
			put_log(msg_id + "under certain conditions; see included LICENSE file for details.");
	   	}
	   	put_log(msg_id + "program = " + tz390.dir_mlc + tz390.pgm_name);
	   	put_log(msg_id + "options = " + tz390.cmd_parms);
	    tz390.force_nocon = false; // RPI 755
       }
	   private synchronized void put_log(String msg) {
	   	/*
	   	 * Write message to z390_log_text or console
	   	 * if running standalone
	   	 * 
	   	 */
   	    	put_prn_line(msg);
   	    	if (tz390.force_nocon){
   	    		return; // RPI 755
   	    	}
	        if  (z390_log_text != null){
  	        	z390_log_text.append(msg + "\n");
   	        } else {
   	        	if (tz390.opt_con){ // RPI 453
   	    	        System.out.println(msg);
   	        	}
   	        }
	   }
	   private void put_prn_line(String msg){
	   /*
	    * put line to prt listing file
	    * if print_on and not surpressed by nogen.
	    * if print data, print all data.
	    */
		   String temp_hex;
		   if (tz390.opt_tracea){
		   	   tz390.put_trace(msg); // RPI 564 additional tracea info
		   }
	   	   if (check_list_bal_line()){ // RPI 484  RPI 694 RPI 891
               String str;  // replace non-printable chars with '.'
	   	       try {
	   	    	  tz390.systerm_io++;
				  if (tz390.opt_writenonprintable) { // write all characters
	   	    	      prn_file_buff.write(msg + tz390.newline); // RPI 500
	   	    	  } else {
	   	    	      str = tz390.replaceNonPrintableChars(msg, tz390.ascii_charset_name);
	   	    	      prn_file_buff.write(str + tz390.newline);
	   	    	  }
	   	          if (prn_file.length() > tz390.max_file_size){
	   	        	  abort_error(118,"maximum prn file size exceeded");
	   	          }
	   	          int index = 16;
	   	          while (print_data[print_level]
	   	                 && index < list_obj_code.length()){
	   	        	  list_obj_loc = list_obj_loc + 8;
	   	        	  if (index + 16 > list_obj_code.length()){
	   	        		  temp_hex = list_obj_code.substring(index);
	   	        	  } else {
	   	        		  temp_hex = list_obj_code.substring(index,index+16);
	   	        	  }	   	        	  
	   	        	  String data_line = tz390.get_hex(list_obj_loc,6) + " " + temp_hex;
	   	        	  if (tz390.opt_tracea){
	   	        		  tz390.put_trace(data_line); // RPI 564 additional tracea info
	   	        	  }
	   	        	  tz390.systerm_io++;
	   	        	  // no non-printable characters on these lines
   	    	          str = tz390.replaceNonPrintableChars(data_line, tz390.ascii_charset_name);
	   	    	      prn_file_buff.write(str + tz390.newline);  // RPI 500
		   	          if (prn_file.length() > tz390.max_file_size){
		   	        	  abort_error(118,"maximum prn file size exceeded");
		   	          }
	   	        	  index = index + 16;
	   	          }
	   	       if (mnote_warning_msg.length() > 0){ // RPI 1056
                   mnote_warning_msg = "AZ390E " + mnote_warning_msg;
				   if (tz390.opt_writenonprintable) { // write all characters
	   	    	       prn_file_buff.write(mnote_warning_msg + tz390.newline); // RPI 500
	   	    	   } else {
	   	    	       str = tz390.replaceNonPrintableChars(mnote_warning_msg, tz390.ascii_charset_name);
	   	    	       prn_file_buff.write(str + tz390.newline);
	   	    	   }
                   tot_mnote_warning++;
                   if (az390_rc < 4){
                           az390_rc = 4;
                   }
                   if (max_mnote_level < 4){
                           max_mnote_level = 4;
                   }
                   tz390.put_systerm(mnote_warning_msg); // RPI 1056
                   mnote_warning_msg = "";
           }
	   	      } catch (Exception e){
	              az390_errors++;
	   	      }
	   	   }
	   }
	   private void check_end_parms(){
		   /*
		    * check if generated object code equals instruction length
		    * and
		    * check for extra parms beyond end
		    * of last instruction parm and issue error
		    */
		   if (obj_code.length() != (2*loc_len)) {
			   log_error(122,"hex code not equal instruction length");
		   }
		   if (!exp_eot 
			   && exp_index < exp_text.length()
			   && exp_text.charAt(exp_index) > ' '){  //RPI181
			   log_error(122,"extra parameter found - " + exp_text.substring(exp_index));
		   }
	   }
	   private void put_obj_line(String msg){
		   /*
		    * put object code to obj file in
		    * hex or binary format
		    */
		    if (obj_file == null || tz390.z390_abort){
		    	return;
		    }
		    if (tz390.opt_traceall){
		    	tz390.put_trace(msg); // RPI 564 additional traceall info
		    }
		   	try {
		   		if (tz390.opt_objhex){
		   			tz390.systerm_io++;
		   			obj_file.writeBytes(msg + tz390.newline); // RPI 500
		   		} else {
		   			cvt_obj_hex_to_bin(msg);
		   			if (!bal_abort){
		   				tz390.systerm_io++;
		   				obj_file.write(bin_byte);
		   			}
		   		}
		   		if (obj_file.length() > tz390.max_file_size){
	   	       	  abort_error(119,"maximum obj file size exceeded");
		   		}
		   	} catch (Exception e){
		   	    abort_error(28,"I/O error on OBJ file write - " + e.toString());
		   	}
		   }
	   private void cvt_obj_hex_to_bin(String hex_rcd){
		   /*
		    * convert ascii hex object string to
		    * binary 80 byte EBCDIC format for 
		    * mainframe compatiblity.  See DFSMS
		    * Program Management Manual reference.
		    */
		try {
		   int index = 0;
		   int index1 = 0;
		   bin_byte[0] = 0x02;        // 1    binary OBJ ID code
		   String type = hex_rcd.substring(1,4);
		   if (type.equals("ESD")){   // 2-4 .ESD
			    bin_byte[1] = tz390.ascii_to_ebcdic['E'];
				bin_byte[2] = tz390.ascii_to_ebcdic['S'];
				bin_byte[3] = tz390.ascii_to_ebcdic['D'];
				Arrays.fill(bin_byte,4,14,ebcdic_space);
  		                             // 15-16 ESD ID
				bin_byte[14] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
				bin_byte[15] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
				index = 16;          // 17-72 up to 3 ESD entries
				index1 = 54;
				while (index < 24){  // name at entry 1 - 8
					if (index1 < hex_rcd.length()){
						bin_byte[index] = tz390.ascii_to_ebcdic[hex_rcd.charAt(index1)];
						index1++;
					} else {
						bin_byte[index] = ebcdic_space;
					}
					index++;
				}
				if (hex_rcd.substring(45,48).equals("CST")){ 
					bin_byte[10] = 0;    // 11-12 SD entry bytes
					bin_byte[11] = 16;
					bin_byte[24] = 0x00; // SD type at entry 9
					                     // 24 bit address at entry 10-12
					if (!hex_rcd.substring(18,20).equals("00")){
						abort_error(132,"SD invalid 24 bit address - " + hex_rcd);
					}
					bin_byte[25] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
					bin_byte[26] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
					bin_byte[27] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
					bin_byte[28] = 0x07;  // double word align at entry 13
					if (!hex_rcd.substring(31,33).equals("00")){
						log_error(133,"SD invalid 24 bit length - " + hex_rcd); // RPI 851
					    return;
					}
					bin_byte[29] = (byte)Integer.valueOf(hex_rcd.substring(33,35),16).intValue();
					bin_byte[30] = (byte)Integer.valueOf(hex_rcd.substring(35,37),16).intValue();
					bin_byte[31] = (byte)Integer.valueOf(hex_rcd.substring(37,39),16).intValue();
					Arrays.fill(bin_byte,32,80,ebcdic_space);
				} else if (hex_rcd.substring(45,48).equals("EXT")){ 
					bin_byte[10] = 0;    // 11-12 ER entry bytes
					bin_byte[11] = 13;
					bin_byte[24] = 0x02; // ER type at entry 9
					Arrays.fill(bin_byte,25,28,ebcdic_space); // blank address at entry 10
					bin_byte[28] = 0x00; // byte alignment at entry 13
					Arrays.fill(bin_byte,29,80,ebcdic_space);
				} else if (hex_rcd.substring(45,48).equals("WXT")){ 
					bin_byte[10] = 0;    // 11-12 WX entry bytes
					bin_byte[11] = 13;
					bin_byte[24] = 0x0A; // WX type at entry 9
					Arrays.fill(bin_byte,25,28,ebcdic_space); // blank address at entry 10
					bin_byte[28] = 0x00; // byte alignment at entry 13
					Arrays.fill(bin_byte,29,80,ebcdic_space);
				} else if (hex_rcd.substring(45,48).equals("ENT")){ 
					bin_byte[10] = 0;    // 11-12 :D entry bytes
					bin_byte[11] = 16;
					bin_byte[24] = 0x01; // LD type at entry 9
					Arrays.fill(bin_byte,25,28,ebcdic_space); // blank ESD type 12-14 for LD
					if (!hex_rcd.substring(18,20).equals("00")){
						log_error(134,"LD invalid 24 bit address - " + hex_rcd); // RPI 851
					    return;
					}
					bin_byte[25] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
					bin_byte[26] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
					bin_byte[27] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
					bin_byte[28] = 0x00; // byte align at entry 13}
					bin_byte[29] = 0x00; // SD identifier for entry
					bin_byte[30] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
					bin_byte[31] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
					Arrays.fill(bin_byte,32,80,ebcdic_space);
				} else {
					log_error(131,"invalid ESD type " + hex_rcd); // RPI 851
					return;
				}
		   } else if (type.equals("TXT")){
				bin_byte[1] = tz390.ascii_to_ebcdic['T'];
				bin_byte[2] = tz390.ascii_to_ebcdic['X'];
				bin_byte[3] = tz390.ascii_to_ebcdic['T'];
				bin_byte[4] = ebcdic_space;
				                        // 6-8 address at 
				if (!hex_rcd.substring(18,20).equals("00")){
					log_error(134,"TXT invalid 24 bit address - " + hex_rcd);
					return;
				}
				bin_byte[5] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
				bin_byte[6] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
				bin_byte[7] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
				bin_byte[8] = ebcdic_space;
				bin_byte[9] = ebcdic_space;
				bin_byte[10] = 00;       // 11-12 number of text bytes
				bin_byte[11] = (byte)Integer.valueOf(hex_rcd.substring(31,33),16).intValue();
				bin_byte[12] = ebcdic_space;
				bin_byte[13] = ebcdic_space;
				                         // 15-16 SD type
				bin_byte[14] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
				bin_byte[15] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
				index = 16;
				index1 = 34;
				int count = bin_byte[11];
				while (count > 0){
					bin_byte[index] = (byte)Integer.valueOf(hex_rcd.substring(index1,index1+2),16).intValue();
				    index++;
				    index1 = index1+2;
				    count--;
				}
				Arrays.fill(bin_byte,index,80,ebcdic_space);
		   } else if (type.equals("RLD")){
				bin_byte[1] = tz390.ascii_to_ebcdic['R'];
				bin_byte[2] = tz390.ascii_to_ebcdic['L'];
				bin_byte[3] = tz390.ascii_to_ebcdic['D'];
				Arrays.fill(bin_byte,4,10,ebcdic_space);
				bin_byte[10] = 0;        // 11-12 number of bytes for RLD entries
				bin_byte[11] = 8;
				Arrays.fill(bin_byte,12,16,ebcdic_space);
                                         // 17-18 ESD ID of referenced ESD
				bin_byte[16] = (byte)Integer.valueOf(hex_rcd.substring(45,47),16).intValue();
				bin_byte[17] = (byte)Integer.valueOf(hex_rcd.substring(47,49),16).intValue();
                                         // 19-20 ESD ID of SD containing RLD field
				bin_byte[18] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
				bin_byte[19] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
                                         // 20 flags TTTTLLSN
				int  rld_len =  Integer.valueOf(hex_rcd.substring(31,32),16).intValue()
				             - 1; // rld field len -1  4=3, 3=2, 2=1, 8=0 RPI 270 RPI 894
				if (rld_len == 7){
					rld_len = 0;  // RPI 270 RPI 894 
				}
				char rld_sign = hex_rcd.charAt(38);
				if (rld_sign == '+'){
					bin_byte[20] = (byte)(rld_len << 2); // pos rld
				} else {
					bin_byte[20] = (byte)(rld_len << 2 + 2); // neg rld
				}
				                         // 22-24 address at 
				if (!hex_rcd.substring(18,20).equals("00")){
					log_error(135,"RLD invalid 24 bit address - " + hex_rcd); // RPI 851
					return;
				}
				bin_byte[21] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
				bin_byte[22] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
				bin_byte[23] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
				Arrays.fill(bin_byte,24,80,ebcdic_space);
		   } else if (type.equals("END")){
				bin_byte[1] = tz390.ascii_to_ebcdic['E'];
				bin_byte[2] = tz390.ascii_to_ebcdic['N'];
				bin_byte[3] = tz390.ascii_to_ebcdic['D'];				
				Arrays.fill(bin_byte,4,80,ebcdic_space);
                // 15-16 ESD ID of referenced ESD  RPI 1197
		        bin_byte[14] = (byte)Integer.valueOf(hex_rcd.substring(9,11),16).intValue();
		        bin_byte[15] = (byte)Integer.valueOf(hex_rcd.substring(11,13),16).intValue();
                // 6-8 relative entry address in ESD  RPI 1197
		        if (!hex_rcd.substring(18,20).equals("00")){
					log_error(211,"END invalid 24 bit entry address - " + hex_rcd); // RPI 851
					return;
				}
				bin_byte[5] = (byte)Integer.valueOf(hex_rcd.substring(20,22),16).intValue();
				bin_byte[6] = (byte)Integer.valueOf(hex_rcd.substring(22,24),16).intValue();
				bin_byte[7] = (byte)Integer.valueOf(hex_rcd.substring(24,26),16).intValue();
				
		   } else {
			   log_error(130,"invalid object record - " + hex_rcd); // RPI 851
			   return;
		   }
		} catch (Exception e){
			if (az390_errors == 0){ // ignore if prior errors
				log_error(136,"Invalid ascii hex object code - " + hex_rcd);
			}
		}
	   }
	   private String get_long_hex(long work_long) {
	   	/*
	   	 * Format long into 16 byte hex string
	   	 */
	   	    String work_hex = Long.toHexString(work_long);
			return ("0000000000000000" + work_hex).substring(work_hex.length()).toUpperCase();
	   }
	   private String string_to_hex(String text,boolean ascii){
	   	/*
	   	 * Format text string into hex string
	   	 * If ascii_req true, gen ascii else ebcdic hex
	   	 */
		    int work_int = 0;
            StringBuffer hex = new StringBuffer(2 * text.length());
            int index = 0;
            while (index < text.length()){
            	if (ascii){
            		work_int = text.charAt(index) & 0xff;
            	} else {
            		work_int = tz390.ascii_to_ebcdic[text.charAt(index) & 0xff] & 0xff; // RPI 737
            	}
				String temp_string = Integer.toHexString(work_int);
                if  (temp_string.length() == 1){
                	hex.append("0" + temp_string);
                } else {
                	hex.append(temp_string);
                }
				index++;
            }
            return hex.toString().toUpperCase();            
	   } 
private String bytes_to_hex(byte[] bytes,int byte_start,int byte_length,int chunk){
	   	/*
	   	 * Format bytes into hex string
	   	 * If chuck > 0 insert space after each chuck
	   	 */
	        StringBuffer hex = new StringBuffer(72);
	        int index1 = 0;
	        int hex_bytes = 0;
	        while (index1 < byte_length){
	        	int work_int = bytes[byte_start + index1] & 0xff;
				String temp_string = Integer.toHexString(work_int);
	            if  (temp_string.length() == 1){
	            	hex.append("0" + temp_string);
	            } else {
	            	hex.append(temp_string);
	            }
	            if (chunk > 0){
	               hex_bytes++;
	               if (hex_bytes >= chunk){
	            	  hex.append(" ");
	            	  hex_bytes = 0;
	               }
	            }
			    index1++;
	        }
	        return hex.toString().toUpperCase();   
}
private void put_obj_text(){
	/*
	 * 1.  Append obj_code to list_obj_code for 
	 *     print line (reguired by mult DC calls).
	 * 2.  Exit if gen_obj_code not on or not CSECT
	 * 3.  Buffer output of ojbect text code for
	 *     contiguous data in same ESD.
	 * 4.  Called from END processing with BAL_EOF
	 *     to flush buffer.
	 * 5.  Reset obj_code for use by DC routines
	 */
	 if (tz390.z390_abort || mz390_abort){
		 return;
	 }
	 if (!bal_eof){ // flush buffer if bal_eof
		 if (!gen_obj_code){
			 return;
		 }
		 if (cur_esd == 0 || sym_type[esd_sid[esd_base[cur_esd]]] != sym_cst){  // RPI 564, RPI 301
		  	 return;
		 }
	 }
	 String temp_obj_line;
	 if (exp_rld_mod_set){
		 list_obj_code = list_obj_code.concat(tz390.get_hex(exp_rld_mod_val,2*exp_rld_len)); // RPI 632
		 exp_rld_mod_set = false;
	 } else {
		 list_obj_code = list_obj_code.concat(obj_code);
	 }
	 int obj_code_len = obj_code.length()/2;
	 tot_obj_bytes = tot_obj_bytes + obj_code_len;
	 if (cur_text_len > 0
	 	&& (bal_eof 
	 		|| cur_text_esd != esd_base[cur_esd] // RPI 301
		 	|| cur_text_loc != loc_ctr)){
		cur_text_loc = cur_text_loc - cur_text_len;
		temp_obj_line = ".TXT ESD=" + tz390.get_hex(cur_text_esd,4) + " LOC=" + tz390.get_hex(cur_text_loc - sym_loc[esd_sid[cur_text_esd]],8) + " LEN=" + tz390.get_hex(cur_text_len,2) + " " + cur_text_buff;
		put_obj_line(temp_obj_line);
		cur_text_len = 0; 
	}
	if (bal_eof || bal_abort)return;  // rpi 851
	if (cur_text_len == 0){
		cur_text_esd = esd_base[sym_esd[esd_sid[cur_esd]]]; // RPI 301
		cur_text_loc = loc_ctr;
		cur_text_buff = "";
	} 
	cur_text_buff = cur_text_buff.concat(obj_code);
	cur_text_len = cur_text_len + obj_code_len;
	cur_text_loc = cur_text_loc + obj_code_len;
	while (cur_text_len >= max_text_buff_len){	 	 
        cur_text_loc = cur_text_loc - cur_text_len;
	 	temp_obj_line = ".TXT ESD=" + tz390.get_hex(cur_text_esd,4) 
	 			   + " LOC=" + tz390.get_hex(cur_text_loc - sym_loc[esd_sid[cur_text_esd]],8) 
	 			   + " LEN=" + tz390.get_hex(max_text_buff_len,2) 
	 			   + " " + cur_text_buff.substring(0,2*max_text_buff_len);
	 	put_obj_line(temp_obj_line);
		cur_text_loc = cur_text_loc + cur_text_len;
	 	cur_text_buff = cur_text_buff.substring(2*max_text_buff_len);
	 	cur_text_len = cur_text_buff.length()/2;
	}
	obj_code = "";
}
private void check_private_csect(){
	/*
	 * start private csect if no csect or dsect
	 */
    if (cur_esd == 0){
     	process_sect(sym_cst,"");
    	first_cst_esd = cur_esd;
    }
}
private void add_using(){
	/*
	 * add or replace USING for code generation
	 */
	if (bal_parms == null){ // RPI 651
		log_error(195,"missing USING parms");
		return;
	}
	get_use_range();
	if (bal_label != null){
		cur_use_lab = bal_label.toUpperCase();
        drop_cur_use_label();
	} else {
		cur_use_lab = "";
	}
	use_eof = false;
	use_domain_tot = 0; // rpi 776
    get_use_domain();
    while (!use_eof){
    	use_domain_tot++;
    	if (cur_use_lab.length() == 0 
    		&& !cur_use_depend){
    		drop_cur_use_reg(); // RPI 629
    	}
      	add_use_entry();
       	get_use_domain();
       	cur_use_base_loc = cur_use_base_loc + 4096;
    }
    if (use_domain_tot == 0){ // RPI 776
    	log_error(202,"USING missing domain operand");
    }
    if (tz390.opt_listuse){
    	list_use = true;
    }
}
private void get_use_range(){
	/*
	 * set cur_use_base esd,loc, and len
	 */
	cur_use_base_esd = 0;
	cur_use_base_loc = 0;
	cur_use_base_len = 0;
	exp_text = bal_parms.toUpperCase();  // RPI 776
	exp_index = 0;
	if (exp_text.length() > 1){  // RPI 776
		if (exp_text.charAt(0) == '('){
			exp_index = 1;
			if (calc_exp()){ // RPI 1148 allow abs base
				cur_use_base_esd = exp_esd;
				cur_use_base_loc = exp_val;
			}
			if (exp_text.charAt(exp_index) == ','){
                exp_index++;
			} else {
				log_error(104,"missing domain for using");
				return;
			}
			if (calc_rel_exp() && cur_use_base_esd == exp_esd){
				cur_use_base_len = exp_val - cur_use_base_loc;
			} else {
				log_error(103,"missing end of range value");
				return;
			}
			if (exp_text.length() > exp_index
				&& exp_text.charAt(exp_index) == ')'){
				exp_index++;
			} else {
				log_error(103,"missing end of range value");
				return;
			}
		} else {
			if (calc_exp()){ // RPI 1148 allow abs base
				cur_use_base_esd = exp_esd;
				cur_use_base_loc = exp_val;
			}
			cur_use_base_len = 4096;
		}
	}
}
private void get_use_domain(){
	/*
	 * set cur_use_reg and cur_use_off
	 * from exp_text at exp_index set by get_use_range
	 * Notes:
	 *   1.  get_rel_exp_bddd is called for dependent
	 *       using expressions to find reg and loc
	 */
	cur_use_depend = false;
	cur_use_reg = 0;
	cur_use_off = 0;
	if (exp_text.length() > exp_index 
		&& exp_text.charAt(exp_index) == ','){
        exp_index++;  // RPI 776
		if (calc_exp()){
			if (exp_type == sym_sdt){
				cur_use_reg = exp_val;
			} else if (exp_type == sym_rel
					   || exp_type == sym_cst
					   || exp_type == sym_dst){ // RPI 274
				cur_use_depend =true;
				if (cur_use_lab == ""){
					drop_dependent_using(); // RPI 1056
				}
				hex_bddd = get_exp_bddd();
			}
		}
	} else {
		use_eof =true;
	}	
}
private void drop_dependent_using(){ // RPI 1056
	/*
	 * drop duplicate unlabeled dependent
	 * USING for same section and offset
	 */
	int index = cur_use_start;
	while (index < cur_use_end){  
		if (use_lab[index] == ""
			&& cur_use_base_esd == use_base_esd[index]
			&& cur_use_base_loc == use_base_loc[index]){
			cur_use_end--;
			if (index < cur_use_end){ // RPI 431 was <
				move_use_entry(cur_use_end,index);
			}
		}
		index++;
	}
}
private void drop_using(){
	/*
	 * drop one or more using 
	 * registers or labeled using
	 */
	if (tz390.opt_listuse){
		list_use = true;
	}
	if  (bal_parms == null 
			|| bal_parms.length() == 0
			|| bal_parms.charAt(0) == ','){
		cur_use_end = cur_use_start; 
		return;
	}
	tz390.parm_match = tz390.parm_pattern.matcher(bal_parms);
	while (tz390.parm_match.find()){
		cur_use_lab = tz390.parm_match.group().toUpperCase();  // RPI 431, RPI 987
		if (cur_use_lab.charAt(0) != ','){
           if (cur_use_lab.charAt(0) > ' '){
   		      if (tz390.find_key_index('U',cur_use_lab) != -1){
			      drop_cur_use_label();
   		      } else {
   		    	  exp_text = bal_parms.substring(tz390.parm_match.start()); // RPI 673
   		    	  exp_index = 0;
   		    	  if (calc_abs_exp()){ 
   		    		  cur_use_reg = exp_val;
   		    		  explicit_drop_reg = true; // RPI 1052
   		    		  drop_cur_use_reg();
   		    		  explicit_drop_reg = false; // RPI 1052
   		    		  if (tz390.parm_match.start() + exp_index < bal_parms.length()){  // RPI 1052
   		    			  bal_parms = bal_parms.substring(tz390.parm_match.start() + exp_index);
   		    			  tz390.parm_match = tz390.parm_pattern.matcher(bal_parms);
   		    		  } else {
   		    			  return;
   		    		  }
   		    	  } else {
   		    		  log_error(101,"invalid register expression - " + exp_text);
   		    	  }
   		      }
           } else {
        	   return; // end of parms at white space
           }
		}
	}
}
private void drop_cur_use_label(){
	/*
	 * remove labeled using if found
	 */
	int index = cur_use_start;
	while (index < cur_use_end){  
		if (use_lab[index] != null && use_lab[index].equals(cur_use_lab)){
			cur_use_end--;
			if (index < cur_use_end){ // RPI 431 was <
				move_use_entry(cur_use_end,index);
			}
		}
		index++;
	}
	
}
private void drop_cur_use_reg(){
	/*
	 * Remove cur_use_reg entries if found
	 * but not labeled usings.
	 */
	int index = cur_use_start;
	int dropped = 0; // RPI 1206
	while (index < cur_use_end){
		if (use_lab[index] == ""  // RPI 431, RPI 451
			&& use_reg[index] == cur_use_reg){
			cur_use_end--;
			dropped++; // RPI 1206			
			if (index < cur_use_end){
 		        move_use_entry(cur_use_end,index);
				index--; // RPI 1206 backup to check moved entry
			}
			// RPI 1205 return; // RPI 1052 exit after removing one
		}
		index++;
	}
	if (explicit_drop_reg == true && dropped == 0){ // RPI 1206
		log_error(207,"Active USING not found for DROP register - " + cur_use_reg); // RPI 1052
	}
}
private void move_use_entry(int index1,int index2){
	/*
	 * move use entry from index1 to index2
	 */
	use_lab[index2] = use_lab[index1];
	use_base_esd[index2] = use_base_esd[index1];
	use_base_loc[index2] = use_base_loc[index1];
	use_base_len[index2] = use_base_len[index1];
	use_reg[index2] = use_reg[index1];
	use_reg_loc[index2] = use_reg_loc[index1];
}
private void add_use_entry(){
	/*
	 * add use entry
	 */
	if (cur_use_end < tz390.opt_maxcall){
		cur_use = cur_use_end;
		cur_use_end++;
		use_lab[cur_use] = cur_use_lab;
		if (cur_use_lab.length() > 0 
			&& tz390.find_key_index('U',cur_use_lab) == -1){
			// create key to indicate using label
			if (!tz390.add_key_index(0)){
			    abort_error(87,"key search table exceeded");
			}
		}
		use_base_esd[cur_use] = cur_use_base_esd;
		use_base_loc[cur_use] = cur_use_base_loc;
		use_base_len[cur_use] = cur_use_base_len;
		use_reg[cur_use] = cur_use_reg;
		use_reg_loc[cur_use] = cur_use_off;
	} else {
		log_error(100,"maximum active using table exceeded");
	}
}
private void list_use(){
	/*
	 * list current use table if LISTUSE
	 */
	int index = cur_use_start;
	boolean none = true;
	while (index < cur_use_end){
		none = false;
		list_bal_line = true; // RPI 891 
		put_prn_line("LISTUSE " + tz390.left_justify(sym_name[esd_sid[use_base_esd[index]]],8)
				+ " ESD=" + tz390.get_hex(use_base_esd[index],4)
				+ " LOC=" + tz390.get_hex(use_base_loc[index],8)
				+ " LEN=" + tz390.get_hex(use_base_len[index],5)	
				+ " REG=" + tz390.get_hex(use_reg[index],1)
				+ " OFF=" + tz390.get_hex(use_reg_loc[index],5)
				+ " LAB=" + use_lab[index]
				);
		index++;
	}
	if (none){
		list_bal_line = true; // RPI 891
		put_prn_line("LISTUSE NONE");
	}
}
private void get_hex_op(int op_offset, int op_len){
	/*
	 * initialize object code with op code
	 * and initialize exp parser with parms
	 * if op_offset = 1
	 */
	hex_op = tz390.op_code[bal_op_index].substring(op_offset-1,op_offset - 1 + op_len);
	obj_code = obj_code + hex_op;
	if (op_offset == 1){
	   exp_text = bal_parms;
	   exp_index = 0;
	}
}
private void or_last_op(){
	// dsh for issue #230 or last op added to prev op added and remove last op (see operation E78A
    int obj_len = obj_code.length();
    if (tz390.opt_traceall){ // dsh issue #230
	   tz390.put_trace("obj_len=" + obj_len + "p6=" + obj_code.substring(obj_len-2,obj_len-1));
    }
    obj_code = obj_code.substring(0,obj_len-2)
          	 + tz390.get_hex(
          	    Integer.parseInt(obj_code.substring(obj_len-2,obj_len-1),16)
			    | 
				Integer.parseInt(obj_code.substring(obj_len-1,obj_len),16)
			   ,1);	
}
private String get_hex_nib(){
	/*
	 * return single hex nibble char 0-f
	 */
	if (calc_abs_exp()){
		if (exp_val >= 0 && exp_val <= 15){
		    return tz390.get_hex(exp_val,1);
		} else {
       		log_error(81,"invalid field value 0-15 " + exp_val);
		}
	} else {
		log_error(82,"invalid field");
	}
	return "h";
}
private void get_hex_vreg(int vreg){
	/*
	 * append vector hex reg low 4 bits from next parm and set corresponding vreg_rxb high bit for 16-31 vregs 
	 */
	if (calc_abs_exp()){
		if (exp_val >= 0 && exp_val <= 15){
		    obj_code = obj_code + tz390.get_hex(exp_val,1);
			return;
		} else if (exp_val >= 16 && exp_val <= 31){
			obj_code = obj_code + tz390.get_hex(exp_val-16,1);
			switch (vreg){
				case  1:
				   vreg_rxb = 8;
				   break;
				case  2:
				   vreg_rxb = vreg_rxb + 4;
				   break;
				case  3:
				   vreg_rxb  = vreg_rxb + 2;
				   break;
				case  4:
				   vreg_rxb = vreg_rxb + 1;
				   break;
				default:
				   log_error(55,"invalid vector register number 1-4");
			}
		} else {	
			log_error(55,"invalid vector register expression - " + exp_val);
			obj_code = obj_code + "r";
		}
	} else {
		log_error(41,"invalid vector register value");
		obj_code = obj_code + "r";
	}
}
private void get_hex_reg(){
	/*
	 * append hex reg from next parm
	 */
	if (calc_abs_exp()){
		if (exp_val >= 0 && exp_val <= 15){
		    obj_code = obj_code + tz390.get_hex(exp_val,1);
		} else {
			log_error(55,"invalid register expression - " + exp_val);
			obj_code = obj_code + "r";
		}
	} else {
		log_error(41,"invalid register value");
		obj_code = obj_code + "r";
	}
}
private void get_hex_reg_even() // RPI 1209N
   {/*
     * append hex reg from next parm - if it is an even register
     */
    if (calc_abs_exp())
       {if (exp_val >= 0 && exp_val <= 15)
           {if ((exp_val/2)*2-exp_val == 0) // check for even register number
               {obj_code = obj_code + tz390.get_hex(exp_val,1);
                }
            else
               {log_error(56,"incorrect register specification - " + exp_val);
                }
            }
        else
           {log_error(55,"invalid register expression - " + exp_val);
            obj_code = obj_code + "r";
            }
        }
    else
       {log_error(41,"invalid register value");
        obj_code = obj_code + "r";
        }
    }
private void get_hex_vreg_rxb() {
	/*
	 * vector register 1-5 high bits to support registers 16-31
	 *    reset for next set of high bits
	 */
	obj_code = obj_code + tz390.get_hex(vreg_rxb,1);
	vreg_rxb = 0;
}
private void get_hex_zero(int hex_ll){
	/*
	 * append zero nibbles
	 */
	String zeros = "00000000";
	obj_code = obj_code.concat(zeros.substring(0,hex_ll));
}
private void skip_comma(){
	/*
	 * verify and skip comma
	 */
	 if (!bal_abort && exp_next_char(',')){
	 	exp_index++;
	 } else {
	 	log_error(50,"missing operand comma - " + exp_text.substring(0,exp_index));
	 }
}
private void get_hex_byte(){
	/*
	 * append hex byte from next parm
	 */
	if (calc_abs_exp() 
		&& exp_val < 256){
		obj_code = obj_code + tz390.get_hex(exp_val,2);
	} else {
		log_error(42,"invalid byte value");
		obj_code = obj_code + "ii";
	}
}
private void get_hex_halfword(){
	/*
	 * append hex halfword from next parm
	 */
	if (calc_abs_exp() 
		&& exp_val < 65536){
		obj_code = obj_code + tz390.get_hex(exp_val,4);
	} else {
		log_error(42,"invalid halfword value");
		obj_code = obj_code + "iiii";
	}
}
private void get_hex_int(int bits){
	/*
	 * append hex integer from next parm
	 */
	if (calc_exp()){
		if  (exp_type == sym_rel){
			log_error(42,"invalid immediate integer bits = " + bits + " value =" + exp_val);
		} else {
			if (bits == 4){
				obj_code = obj_code + tz390.get_hex(exp_val,1);
			} else if (bits == 8){
			   obj_code = obj_code + tz390.get_hex(exp_val,2);
			} else if (bits == 12){
			   obj_code = obj_code + tz390.get_hex(exp_val,3);
			} else if (bits == 16){
				obj_code = obj_code + tz390.get_hex(exp_val,4);
			} else if (bits == 24){
				obj_code = obj_code + tz390.get_hex(exp_val,6);
		    } else if (bits == 32){
				obj_code = obj_code + tz390.get_hex(exp_val,8);
			} else {
				log_error(42,"invalid immediate integer bit count " + bits);
			}
		}
	} else {
		log_error(42,"invalid expression");
	}
}
private void get_hex_relative_offset(int bits){
	/*
	 * append hex relative offset from current instruction
	 */
	if (calc_exp()){
		if  (exp_type == sym_rel){
			if (bits == 12){
				obj_code = obj_code + get_hex_relative_offset_12();			
			} else if (bits == 16){
				obj_code = obj_code + get_hex_relative_offset_16();
			} else if (bits == 24){
				obj_code = obj_code + get_hex_rel_offset_24();	
		    } else if (bits == 32){
				obj_code = obj_code + get_hex_relative_offset_32();
			} else {
				obj_code = obj_code + "iiiiiiii";
				log_error(42,"invalid relative offset bits " + bits);
			}
		} else {
		    obj_code = obj_code + "iiiiiiii";		
			log_error(42,"invalid relative offset bits " + bits);
		}
	} else {
		obj_code = obj_code + "iiiiiiii";		
		log_error(42,"invalid relative offset expression");
	}
}
private void get_hex_byte_signed(){
	/*
	 * append signed byte from next parm RPI 1146
	 */
	if (calc_exp() 
		&& exp_val < 128
		&& exp_val >= -128){  
		obj_code = obj_code + tz390.get_hex(exp_val,2);
	} else {
		log_error(209,"invalid signed byte value");
		obj_code = obj_code + "ii";
	}
}
private void get_hex_llbddd(){
	/*
	 * set hex_len, hex_bddd, and hex_bddd_loc
	 * from next parm
	 */
	int ll = 1;
	int b  = 0;
	int ddd = 0;
	hex_ll = "ll";
	hex_bddd = "bddd";
	hex_bddd_loc = "      ";
    calc_lit_or_exp();
	if (!bal_abort){
		if (exp_type == sym_rel){
			hex_bddd_loc = tz390.get_hex(exp_val,6);
			hex_bddd = get_exp_bddd();
			ll  = get_exp_ll();
		} else {
			ddd = exp_val;
			if (exp_next_char('(')){
				exp_index++;
				if (exp_next_char(',')){
					ll = exp_len;
					exp_index++;
					if (calc_abs_exp()){
						b = exp_val;
					}
				} else if (calc_abs_exp()){
					if (exp_next_char(',')){
					    ll = exp_val;
						exp_index++;
						if (calc_abs_exp()){
							b = exp_val;
						}
					} else {
						ll = exp_val;  // RPI 538 (was ll), RPI 613 (was b = exp_len in err)
					}
				}
				if (exp_next_char(')')){
					exp_index++;
				} else {
					log_error(192,"missing close )"); // RPI 637
				}
			}
			hex_bddd = get_exp_abs_bddd(b,ddd);
		 }
		 if (ll >= 0 && ll <= 256){
			 if (ll > 0){
				 ll--;
			 }
		     hex_ll = tz390.get_hex(ll,2);
		 } else {
			 log_error(206,"invalid length - " + ll); // RPI 1033
		 }
	}
}
private int get_exp_ll(){
	/*
	 * return explicit or implied length
	 * from exp_len
	 */
	int ll = exp_len;
	if (exp_next_char('(')){
		if (calc_abs_exp()){
			ll = exp_val;
		} else {
			return 1;
		}
	}
	if (ll >= 0 && ll <= 256){
        return ll;
	} else {
		log_error(149,"length exceeds 256 limit = " + ll); // RPI 955
		return 1;	
	}
}
private void get_hex_v2xbddd(){ // rpi 2202 syooirt for   v2 index
	/*
	 * append xbddd or xbdddhh hex object code
	 * from next parm
	 */
	String hex_xbddd = "llbddd";
    calc_lit_or_exp();
	if (!bal_abort){
		if  (exp_type == sym_rel){ // 
			hex_bddd2_loc = tz390.get_hex(exp_val,6);
			hex_bddd2 = get_exp_bddd(); // RPI 1148
			hex_xbddd = get_exp_v2x() +hex_bddd2;
		} else {
		 	hex_xbddd = get_exp_abs_v2xbddd();
		}
	}
	obj_code = obj_code + hex_xbddd;
}
private void get_hex_xbddd(){
	/*
	 * append xbddd or xbdddhh hex object code
	 * from next parm
	 */
	String hex_xbddd = "llbddd";
    calc_lit_or_exp();
	if (!bal_abort){
		if  (exp_type == sym_rel){ // 
			hex_bddd2_loc = tz390.get_hex(exp_val,6);
			hex_bddd2 = get_exp_bddd(); // RPI 1148
			hex_xbddd = get_exp_x() +hex_bddd2;
		} else {
		 	hex_xbddd = get_exp_abs_xbddd();
		}
	}
	obj_code = obj_code + hex_xbddd;
}
private String get_exp_v2x(){ // rpi 2202 support v2 as x2
	/*
	 * get hex x from next (x) else 0
	 */
	if  (exp_next_char('(')){
		exp_index++;
		if (calc_abs_exp() && exp_val >= 0 && exp_val <= 31){
			if (exp_val > 15){
				vreg_rxb = vreg_rxb + 4; // set v2 high bit
				exp_val = exp_val -16;
			}
			exp_index++;
			return tz390.get_hex(exp_val,1);
		} else {
			log_error(40,"invalid index register");
		    return "x";
		}
	} else {
		return "0";
	}
}
private String get_exp_x(){
	/*
	 * get hex x from next (x) else 0
	 */
	if  (exp_next_char('(')){
		exp_index++;
		if (calc_abs_exp() && exp_val >= 0 && exp_val <= 15){
			exp_index++;
			return tz390.get_hex(exp_val,1);
		} else {
			log_error(40,"invalid index register");
		    return "x";
		}
	} else {
		return "0";
	}
}
private void get_hex_xbdddhh2(){
	/*
	 * append xbddd hex object code from next parm
	 */
	get_bdddhh = true; // RPI 387         
	get_hex_xbddd();   //RPI161,RPI166
	get_bdddhh = false;
}
private void get_hex_bddd2(boolean add_code){
	/*
	 * if add_code
	 *    append bddd or bdddhh hex object code
	 *    from next parm 
	 * else 
	 *    just set hex_bddd2
	 */
	hex_bddd2 = null;
	calc_lit_or_exp();
	if  (!bal_abort){
		if  (exp_type == sym_rel){
			hex_bddd2_loc = tz390.get_hex(exp_val,6);
			hex_bddd2 = get_exp_bddd(); // RPI 1148
		} else {
			hex_bddd2 = get_exp_abs_bddd();
		}
	} else {
		hex_bddd2 = get_default_bddd();
	}
	if (add_code){  //RPI120
		obj_code = obj_code + hex_bddd2;
	}
}
private boolean exp_next_char(char next_char){
	/*
	 * return true if next exp_text char
	 * at exp_index is next_char
	 */
	if (exp_text != null && exp_index < exp_text.length() // RPI 822 
		&& exp_text.charAt(exp_index) == next_char){
		return true;
	} else {
		return false;
	}
}
private void get_hex_bdddhh2(){
	/*
	 * gen bdddhh where hhddd is 20 bit
	 * signed offset to b. RPI 387
	 */
	get_bdddhh = true;   // RPI 387
	get_hex_bddd2(true);
	get_bdddhh = false;
}
private void get_hex_rld_long(){
	/*
	 * gen 32 bit RLD or abs value with loc ctr at start of ins.
	 */
	 loc_ctr = loc_ctr + 2;
	 exp_rld_len = 4; // allow rld rpi 2223
	 calc_lit_or_exp();
	 exp_rld_mod_set = false; // don't try and display module rld addr instead of opcode text
	 exp_rld_len = 0;
	 hex_bddd2_loc = tz390.get_hex(exp_val,6); // RPI 949
	 obj_code = obj_code + tz390.get_hex(exp_val,8);
	 loc_ctr = loc_ctr - 2;
}
private void get_hex_relative_offset_long(){
	/*
	 * append iiiiiiii signed offset (calc for label)
	 */
    String hex_iiiiiiii = "iiiiiiii";
    calc_lit_or_exp();  // RPI 954
	if (!bal_abort){ // RPI 954
		if  (exp_type == sym_rel){
			hex_bddd2_loc = tz390.get_hex(exp_val,6); // RPI 949	
			hex_iiiiiiii = get_hex_relative_offset_32(); // 32 bit immediate offset
		} else {
		    hex_iiiiiiii = tz390.get_hex(exp_val,8);
		    hex_bddd2_loc = tz390.get_hex(exp_val,6); // RPI 949
		}
	}
	obj_code = obj_code + hex_iiiiiiii;
}
private String get_hex_relative_offset_12(){
	/*
	 * return relative 12 bit relative halfword offset
	 */
	String hex_iii = "iii";
	if (exp_esd == esd_base[cur_esd] || tz390.opt_allow){ // RPI 301 RPI 988
		if ((exp_val & 0x1) == 0){
			exp_val = (exp_val - loc_start)/2;
			hex_iii = tz390.get_hex(exp_val,3);
		} else {
			log_error(112,"relative target address odd - " + tz390.get_hex(exp_val,8));
		}
	} else {
		log_error(76,"relative offset not in same esd");
	}
	return hex_iii;
}
private String get_hex_relative_offset_16(){
	/*
	 * return relative 16 bit relative halfword offset
	 */
	String hex_iiii = "iiii";
	if (exp_esd == esd_base[cur_esd] || tz390.opt_allow){ // RPI 301 RPI 988
		if ((exp_val & 0x1) == 0){
			exp_val = (exp_val - loc_start)/2;
			hex_iiii = tz390.get_hex(exp_val,4);
		} else {
			log_error(112,"relative target address odd - " + tz390.get_hex(exp_val,8));
		}
	} else {
		log_error(76,"relative offset not in same esd");
	}
	return hex_iiii;
}
private String get_hex_relative_offset_32(){
	/*
	 * return relative signed half word offset
	 * from psw_loc to symbol in same csect at
	 * even address
	 * Notes:
	 *   1.  Error if not same csect or odd address
	 */
	String hex_llllllll= "llllllll";
	if (exp_esd == esd_base[cur_esd] || tz390.opt_allow){ // RPI 301 RPI 988
		if ((exp_val & 0x1) == 0){
			exp_val = (exp_val - loc_start)/2;
			hex_llllllll = tz390.get_hex(exp_val,8);
		} else {
			log_error(112,"relate target address odd - " + tz390.get_hex(exp_val,8));
		}
	} else {
		log_error(76,"relative offset not in same esd");
	}
	return hex_llllllll;
}
private String get_hex_rel_offset_24(){
	/*
	 * return relative 24 bit halfword offset
	 * from psw_loc to symbol in same csect at
	 * even address
	 * Notes:
	 *   1.  Error if not same csect or odd address
	 */
    String hex_llllll= "llllll";
	if (exp_esd == esd_base[cur_esd] || tz390.opt_allow){ // RPI 301 RPI 988
		if ((exp_val & 0x1) == 0){
			exp_val = (exp_val - loc_start)/2;
			hex_llllll = tz390.get_hex(exp_val,6);
		} else {
			log_error(112,"relative target address odd - " + tz390.get_hex(exp_val,8));
		}
	} else {
		log_error(76,"relative offset not in same esd");
	}
	return hex_llllll;
}
private String get_exp_bddd(){ // RPI 1148 supp 31 bit abs
	/*
	 * 1.  Return hex bddd based on exp_esd 
	 *     and exp_val set by calc_exp or calc_lit.
	 * 2.  If get_bdddhh is set,
	 *     then 20 bit signed offset will be
	 *     returned as bdddhh. RPI 387
	 * 
	 * 2.  Set cur_reg and cur_reg_loc for use
	 *     when called from dependent using with
	 *     domain expression.
	 * 3.  If exp_use_lab is not null restrict
	 *     using entries to labelled using.  
	 */
	if (!gen_obj_code){
        return get_default_bddd();
	}
	if (exp_esd == 0 && exp_val >= 0){ // RPI 1196
		cur_use_reg = 0;
		cur_use_off = 0;
		if (get_bdddhh && exp_val <= 0xfffff){ // RPI 1148 RPI 1196
			return "0" + tz390.get_hex(exp_val,3) + tz390.get_hex(exp_val >>> 12,2); 
		} else if (exp_val <= 0xfff) {                  // RPI 1196
			return "0" + tz390.get_hex(exp_val,3);
		}
	}
	cur_use_reg = -1;  // assume not found
	cur_use_off = 0x80000; 
	cur_use_neg_reg = -1;
	cur_use_neg_off = 0xfff00000;
	int test_offset = 0;
	int test_len = 0;
	int index = cur_use_start;
	cur_esd_base = exp_esd; // RPI 301
	while (index < cur_use_end){ 
		if (use_base_esd[index] == cur_esd_base // RPI 301
			&& ((exp_use_lab != null 
				 && use_lab[index].equals(exp_use_lab))  // RPI 274
			    || (exp_use_lab == null  // RPI 609
			        && use_lab[index] == ""))
			){
			test_offset = exp_val - use_base_loc[index];
			if (get_bdddhh){
				test_len = max_hh;
			} else {
				test_len = use_base_len[index];
			}
			if (test_offset <= cur_use_off  // RPI 982
					&& test_offset + use_reg_loc[index] >= 0  // RPI 994
					&& test_offset < test_len){
				if (cur_use_lab == ""
					&& test_offset == cur_use_off){
					 mnote_warning_msg = "MNOTE 4,'Duplicate USING ranges found for - " 
						 + use_reg[index] + " and " + cur_use_reg 
						 + " using highest'"; // RPI 1056
				}
				if (test_offset < cur_use_off 
					|| use_reg[index] > cur_use_reg){ // RPI 982
					cur_use_reg = use_reg[index];
					cur_use_off = test_offset + use_reg_loc[index];
				}
			} else if (get_bdddhh
					&& test_offset >= cur_use_neg_off  // RPI 982
					&& test_offset < 0
					){
				if (test_offset > cur_use_off 
						|| use_reg[index] > cur_use_reg){ // RPI 982
					cur_use_neg_reg = use_reg[index];
					cur_use_neg_off = test_offset + use_reg_loc[index];
				}
			}
		}
		index++;
	}
	exp_use_lab = null;
	if (cur_use_reg >= 0){  // RPI 465
	    return get_exp_abs_bddd(cur_use_reg,cur_use_off);
	} else if (cur_use_neg_reg > 0){
		cur_use_reg = cur_use_neg_reg;
		cur_use_off = cur_use_neg_off;
		return get_exp_abs_bddd(cur_use_reg,cur_use_off);
	} else {
		log_error(144,"no base register found");
	    exp_use_lab = null;
		return get_default_bddd();
	}
}
private String get_exp_abs_bddd(){
	/*
	 * return bddd or bdddhh from
	 * explicit ddd(b) with ddd in exp_val
	 */
	int b   = 0;
	int ddd = exp_val;
	if (exp_next_char('(')){
		exp_index++;	
		if (exp_next_char(',')){
			exp_index++; // ignore , in (,b)
			log_error(183,"no index or length comma allowed"); // RPI 588
		}
		if (calc_abs_exp()){
			b = exp_val; 
		}
		if (exp_next_char(')')){
			exp_index++;
		} else {
			log_error(193,"unexpected character before close )");  // RPI 637 RPI 1034
		}
	} else {
		return get_exp_bddd(); // RPI 1148
	}
	return get_exp_abs_bddd(b,ddd);
}
private String get_exp_abs_v2xbddd(){ // ROI 2202 support v2 as x in abs ddd(x,b)
	/*
	 * return xbddd or xbdddhh from
	 * explicit ddd(x,b) with ddd in exp_val
	 */
	int x  = 0;
	int b   = 0;
	int ddd = exp_val;
	if (exp_next_char('(')){
		exp_index++;
		if (exp_next_char(',')){
			exp_index++;
			if (calc_abs_exp()){
				b = exp_val;
			}
		} else if (calc_abs_exp() && exp_val >= 0 && exp_val <= 31){
			x = exp_val;
			if (x > 15){
				vreg_rxb = vreg_rxb + 4; // set V2 reg 16-31 bit
				x = x - 16;
			}
			if (exp_next_char(',')){
				exp_index++;
				if (calc_abs_exp()){
					b = exp_val;
				}
			}
		} else {
			log_error(194,"invalid index register");
		}
		if (exp_next_char(')')){
			exp_index++;
		} else {
			log_error(194,"missing close ) ");  // RPI 637
		}
	} else {		
		return "0" + get_exp_bddd(); // RPI 1148
	}
	return tz390.get_hex(x,1) + get_exp_abs_bddd(b,ddd);
}
private String get_exp_abs_xbddd(){
	/*
	 * return xbddd or xbdddhh from
	 * explicit ddd(x,b) with ddd in exp_val
	 */
	int x  = 0;
	int b   = 0;
	int ddd = exp_val;
	if (exp_next_char('(')){
		exp_index++;
		if (exp_next_char(',')){
			exp_index++;
			if (calc_abs_exp()){
				b = exp_val;
			}
		} else if (calc_abs_exp()){
			if (exp_next_char(',')){
				x = exp_val;
				exp_index++;
				if (calc_abs_exp()){
					b = exp_val;
				}
			} else {
				x = exp_val;  // RPI 612 
			}
		}
		if (exp_next_char(')')){
			exp_index++;
		} else {
			log_error(194,"missing close ) ");  // RPI 637
		}
	} else {		
		return "0" + get_exp_bddd(); // RPI 1148
	}
	return tz390.get_hex(x,1) + get_exp_abs_bddd(b,ddd);
}
private String get_exp_abs_bddd(int b,int dddhh){
	/*
	 * return bddd or bdddhh 
	 * using exp_val displacement
	 */
	if (b < 0 || b > 15){
		log_error(146,"base out of range = " + b);
	    return get_default_bddd();
	}
	if (get_bdddhh){
	    if (dddhh >= min_hh && dddhh < max_hh){
	    	return (tz390.get_hex(b,1) 
	    	        + tz390.get_hex(dddhh & 0xfff,3)
	    	        + tz390.get_hex((dddhh >> 12) & 0xff,2)
	    	       ).toUpperCase();
	    } else {
	    	log_error(147,"displacement dddhh out of range = " + dddhh);
	        return get_default_bddd();
	    }
	} else {
		if (dddhh >= 0 && dddhh < 4096){
		    return (tz390.get_hex(b,1)
				   + tz390.get_hex(dddhh,3)
		           ).toUpperCase();
		} else {
			log_error(148,"displacement ddd out of range = " + dddhh);
			return get_default_bddd();
		}
	}
}
private void get_dc_field_dup(){
    /*
     * return dup factor for dc_field else 1
     */
	 dc_dup_loc = 0;
	 dc_dup = 1;
	 if (dc_index >= dc_field.length()){
		 return;
	 }
     if (dc_field.charAt(dc_index) == '('){
     	exp_text = dc_field;
     	exp_index = dc_index + 1;
     	if (calc_abs_exp()){  
     		dc_index = exp_index + 1;
     		dc_dup = exp_val;
     	} else {
     		dc_dup = -1;
     	}
     } else {
        dc_dup = get_dc_int(dc_index);
     }
     if (dc_dup < 0){ // RPI 327
    	 log_error(43,"invalid dc duplication factor");
     }
}
private void get_dc_field_type(){
	/* 
	 * set dc_type and dc_type_index 
	 * and verify valid type else abort

	 */
	  if (bal_abort || dc_index >= dc_field.length()){
		  dc_type_index = -1;
		  log_error(145,"missing DC field type");
		  return;
	  }
      dc_type = dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0);
      dc_index++;
      dc_type_index = dc_type_table.indexOf(dc_type);
      if (dc_type_index == -1){
      	 log_error(51,"invalid dc type - " + dc_field.substring(0,dc_index));
      }      	 
}
private void get_dc_field_modifiers(){
	/*
	 * 1.  Set L, S, E defaults
	 * 2.  Check modifiiers based on type
	 *     a.  if DEL check for B/D/H and set tz390.fp_type
	 *     b.  if C check for A/E and set dc_type_sfx  // RPI 270
	 *     c.  if AFV check for D and set dc_type_sfx  // RPI 270
	 *     d.  if LQ ignore the Q for 16 byte default
	 *     e.  If S check for Y and set length 3
	 * 3.  Process explicit L length, S scale, and E exponent
	 *     modifiers in any order
	 * 4.  Align and save first length if req'd
	 */
	if (dc_type_index != -1){
		dc_attr_elt = sym_attr_elt_def; // default for not explicit length
    	dc_len = dc_type_len[dc_type_index];
	} else {
	 	dc_len = 1;
	}
	dc_len_explicit = false;
	dc_scale_explicit = false; // RPI 777
	dc_exp_explicit = false;   // RPI 777
	dc_scale = 0; // 2**N  mantissa multiplier
	dc_exp   = 0; // 10**N exponent offset
	/*
	 * set type suffix if any
	 */
	if (dc_index < dc_field.length()){
      	 	dc_type_sfx = dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0);
      	 	switch (dc_type){
      	 	case 'A': // RPI 270
      	 		if (dc_type_sfx == 'D'){
      	 			dc_index++;
      	 			dc_len = 8; // RPI 270
      	 		}
      	 		break;
      	 	case 'C': // RPI 270
      	 		if (dc_type_sfx == 'A'){ 
      	 		    dc_index++;
      	 		} else {
      	 			if (dc_type_sfx == 'E'){
      	 				dc_index++;
      	 			}
      	 		}
      	 		break;
      	 	case 'D':
      	 		if (dc_type_sfx == 'B'){
      	 			tz390.fp_type = tz390.fp_db_type;
      	 			dc_index++;
      	 		} else if (dc_type_sfx == 'D'){
      	 			tz390.fp_type = tz390.fp_dd_type; // RPI 407
      	 			dc_index++;
      	 		} else {
      	 			tz390.fp_type = tz390.fp_dh_type;
      	 			if (dc_type_sfx == 'H'){
      	 			   dc_index++;
      	 			}
      	 		}
      	 		break;
      	    case 'E':
      	 		if (dc_type_sfx == 'B'){
      	 			tz390.fp_type = tz390.fp_eb_type;
      	 			dc_index++;
      	 		} else if (dc_type_sfx == 'D'){
      	 			tz390.fp_type = tz390.fp_ed_type; // RPI 407
      	 			dc_index++;
      	 		} else {
      	 			tz390.fp_type = tz390.fp_eh_type;
      	 			if (dc_type_sfx == 'H'){
       	 			   dc_index++;
       	 			}
      	 		}
      	 		break;
      	 	case 'F': // RPI 270
      	 		if (dc_type_sfx == 'D'){
      	 			dc_index++;
      	 			dc_len = 8; // RPI 270
      	 		}
      	 		break;
      	    case 'L':
      	 		if (dc_type_sfx == 'B'){
      	 			tz390.fp_type = tz390.fp_lb_type;
      	 			dc_index++;
      	 		} else if (dc_type_sfx == 'D'){
      	 			tz390.fp_type = tz390.fp_ld_type; // RPI 407
      	 			dc_index++;	
      	 		} else if (dc_type_sfx == 'Q'){ // RPI 555
      	 			tz390.fp_type =tz390.fp_lq_type; // RPI 1108 LQ
      	 			dc_index++;	
      	 		} else {
      	 			tz390.fp_type = tz390.fp_lh_type;
      	 			if (dc_type_sfx == 'H'){
       	 			   dc_index++;
       	 			}
      	 		}
      	 		break;
      	    case 'S': // RPI 893
      	 		if (dc_type_sfx == 'Y'){
      	 			dc_index++;
      	 			dc_len_explicit = true; // no SY alignment
      	 			dc_len = 3;
      	 		}
      	 		break;
      	 	case 'V': // RPI 270
      	 		if (dc_type_sfx == 'D'){
      	 			dc_index++;
      	 			dc_len = 8; // RPI 270
      	 		}
      	 		break;	
      	 	}
     } else {
         dc_type_sfx = ' '; // RPI 388
     }	 
	/*
	 * set explicit length, scale, exponent if any
	 */
	boolean check_mod = true;
	while (!bal_abort && check_mod){
		 if (dc_index < dc_field.length() 
			 && dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0) == 'L'){
			 // explicit length
			 dc_len_explicit = true;
			 if (!bal_abort){
				 dc_attr_elt = tz390.ascii_to_ebcdic[dc_type_explicit.charAt(dc_type_index) & 0xff];  // RPI 737
			 }
			 if (dc_index+1 < dc_field.length() && dc_field.charAt(dc_index+1) == '.'){
				 dc_index++;  // RPI 438 limited bit lenght support
                 if (!dc_bit_len){
                	 dc_bit_len = true; // RPI 417
			         dc_bit_buff = BigInteger.valueOf(0);
			         dc_bit_tot = 0;
                 }
             } else if (dc_bit_len){
            	 flush_dc_bits(); // RPI 417
             }
			 dc_len = get_dc_mod_int();
			 if (dc_len < 0){
				 log_error(185,"DS/DC negative length -" + dc_len);
			 }
		 } else if (dc_index < dc_field.length() 
			 && dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0) == 'S'){
			 // explicit scale
			 dc_scale_explicit = true; // RPI 777
			 dc_scale = get_dc_mod_int();
		 } else if (dc_index < dc_field.length() 
			 && dc_field.substring(dc_index,dc_index+1).toUpperCase().charAt(0) == 'E'){
			 // explicit exponent
			 dc_exp_explicit = true;
			 dc_exp = get_dc_mod_int();
		 } else {
			 check_mod = false;
		 }
	 }
	 /*
	  * align and save first field attr.
	  */
	 if (!dc_lit_ref 
		&& !dc_len_explicit){ // RPI 265 align within DS/DC
        if (tz390.opt_align || dc_dup == 0){ // RPI 1073
        	if (dc_len == 16){
        		if (tz390.fp_type == tz390.fp_lq_type){ // RPI 1108
        			dc_align(16);
        		} else {
        			dc_align(8);
        		}
        	} else {
        		dc_align(dc_len);
        	}
        }
	 }
	 if (dc_first_field){
		dc_first_type  = dc_type;
		dc_first_type_sfx = dc_type_sfx; // RPI 790
		bal_lab_attr   = tz390.ascii_to_ebcdic[dc_type];
		dc_first_attr_elt = dc_attr_elt;
		bal_lab_attr_elt  = dc_attr_elt;
	 	if (dc_bit_len){ // RPI 417
	 		dc_first_len = (dc_len + 7)/8;
	 	} else {
	 		dc_first_len = dc_len;
	 	}
	 	dc_first_scale = dc_scale;
	 	dc_first_dup   = dc_dup; // RPI 1200
	 	loc_start = loc_ctr;
	 }
}
private void dc_align(int align_len){
	/*
	 * align to mult of align_len from loc_ctr
	 * If align_len > 8 use 8  RPI 373 removed by RPI 1108
	 */
	 dc_fill((loc_ctr + align_len -1)/align_len*align_len - loc_ctr);
}
private void flush_dc_bits(){
	/*
	 * flush any bits in dc_bit_buff to 
	 * align to byte boundary for next field
	 * or end of DS.DC
	 */
	if (dc_bit_tot > 0){
		dc_bit_byte_len = (dc_bit_tot + 7)/8;
		if (dc_op){
			dc_bit_fill = dc_bit_tot - (dc_bit_tot/8)*8;
			if (dc_bit_fill > 0){
				dc_bit_buff = dc_bit_buff.shiftLeft(8 - dc_bit_fill);
			}
			dc_bit_bytes = dc_bit_buff.toByteArray();
			dc_bit_hex = "";
			int index = 0;
			if (dc_bit_byte_len > dc_bit_bytes.length){
				while (index < dc_bit_byte_len - dc_bit_bytes.length){
					dc_bit_hex = dc_bit_hex + "00";
					index++;
				}
				index = 0;
			} else if (dc_bit_byte_len < dc_bit_bytes.length){
				index = 1;
			}
			while (index < dc_bit_bytes.length){
				String dc_hex_byte = Integer.toHexString(dc_bit_bytes[index] & 0xff).toUpperCase();
			    if (dc_hex_byte.length() < 2){
			    	dc_bit_hex = dc_bit_hex + "0" + dc_hex_byte;
			    } else {
			    	dc_bit_hex = dc_bit_hex + dc_hex_byte;
			    }
				index++;
			}
		    obj_code = obj_code + dc_bit_hex;
			put_obj_text();
		}
		loc_ctr = loc_ctr + dc_bit_byte_len;
	}
	dc_bit_len = false;
}
private void dc_fill(int fill_len){
	/*
	 * 1.  increment loc_ctr by bytes if not bit mode
	 *     else shift bits by bit length
	 * 2.  if DC and not first field fill with zeros 
	 */
	  if (dc_bit_len){
		  dc_bit_buff = dc_bit_buff.shiftLeft(fill_len);
		  dc_bit_tot  = dc_bit_tot + fill_len;
		  return;
	  }
	  int prev_loc_ctr = loc_ctr;
	  loc_ctr = loc_ctr + fill_len;
	  if (!dc_first_field && dc_op){
		  if (prev_loc_ctr < loc_ctr){
			  list_obj_code = list_obj_code + tz390.get_dup_string("0",2*(loc_ctr-prev_loc_ctr)); // RPI 411
		  }
	  }
}
private void process_dca_data(){
	/*
	 * alloc or gen DS/DC A/V/Y type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	dc_data_start = dc_index; 
	if (dc_op
		&& !dc_bit_len   // RPI 417
		&& ((dc_len >= 2       // RPI 893 add AL2(RLD) > BDDD support
			&& dc_len <= 4)
			|| dc_len == 8)){  //RPI182 RPI 270
		exp_rld_len = (byte) dc_len;
	} else {
		exp_rld_len = 0;
	}
	exp_index = dc_index;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (calc_dca_exp()){
			    dc_index = exp_index;
			    if (dc_bit_len){
			    	gen_dca_bits();
			    } else {
			    	gen_dca_bytes();
			    }
			    if (dc_field.length() > dc_index){
			    	if (dc_field.charAt(dc_index) == ','){
			    		exp_index++;
			    	} else if (dc_field.charAt(dc_index) == ')'){
			    		if (dc_dup > 1){         //RPI2 start
			    			dc_index = dc_data_start;
			    			exp_index = dc_index; 
			    			dc_dup--; 
			    		} else { 
			    			dc_eod = true;	
			    		}                        //RPI2
			    	} else {
			    		log_error(105,"invalid dc data terminator - " + dc_field.substring(dc_index));
			    	}
			    } else { // prevent trap on missing ) etc. RPI 817
			    	log_error(203,"missing dc data terminator");
			    	
			    }
		    }
		}
	    dc_index++; // skip dca terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void gen_dca_bits(){
	/*
	 * gen dca exp_val in dc_bit_buff
	 * Notes:
	 *   1.  Shared by gen_dcb_bits
	 */
	dc_bit_tot = dc_bit_tot + dc_len;
	if (dc_op && dc_dup > 0){
		dc_bit_buff = dc_bit_buff.shiftLeft(dc_len);
		if (exp_val >= 0){
			dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf(exp_val));
		} else {
			dc_bit_value = ((long)(-1) >>> (64-dc_len)) & (long)(exp_val);
			dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf(dc_bit_value));
		}
	} 
}
private void gen_dca_bytes(){
	/*
	 * gen dca byte field
	 */
	if (dc_op && dc_dup > 0){
		if (exp_val >= 0 || dc_len <= 4){
			obj_code = obj_code + tz390.get_hex(exp_val,2*dc_len);
		} else {
			obj_code = obj_code + ("FFFFFFFF").substring(0,2*dc_len-8) + tz390.get_hex(exp_val,8);
		}
		put_obj_text();
	} 
	if (!dc_lit_ref && dc_dup > 0){
		loc_ctr = loc_ctr + dc_len;
		dc_dup_loc = dc_dup_loc + dc_len;
	}
}
private void process_dcb_data(){
	/*
	 * alloc or gen DS/DC B type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_len
	 * Notes:
	 *   1.  binary values are right aligned in 
	 *       explicit length fields.
	 */
	dc_index++;   // start inside 'bin1,bin2,,'
    dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		int dcb_start = dc_index;
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''
				&& dc_field.charAt(dc_index) != ','){
			    dc_index++;
		}
		if (dc_index >= dc_field.length()){ 
			log_error(65,"invalid binary dc data " + dc_field.substring(dc_data_start));
			return;
		}
		dcb_bin = remove_blanks(dc_field.substring(dcb_start,dc_index)); // #233
		dcb_len = dcb_bin.length();
		dcb_pad = 8 - (dcb_len - dcb_len/8*8);
		if (dcb_pad != 8){
			dcb_bin = "00000000".substring(0,dcb_pad) + dcb_bin; // #233 
		}
        if (dc_bit_len){
           gen_dcb_bits();
        } else {
           gen_dcb_bytes();
        }
		if (dc_field.charAt(dc_index) == ','){
		   	dc_index++;
		} else if (dc_field.charAt(dc_index) == '\'') {
	        dc_index++; // skip dch terminator
	        dc_len = 0; // don't double count
	        if  (!bal_abort){
		        if  (dc_dup > 1){  //rpi2
			        dc_index = dc_data_start;
			        dcb_start = dc_index;
			        exp_index = dc_index;
			        dc_dup--;
		        } else {
			        dc_eod = true;
		        }                  //rpi2
	        }
	    } else {
	    	log_error(106,"invalid dc data terminator - " + dc_field.substring(dc_index));
	    }
	}
	exp_rld_len = 0;
}
private void gen_dcb_bits(){
	/*
	 * gen dcb type bit field in dc_bit_buff
	 */
	try {
		exp_val = Integer.valueOf(dcb_bin,2);
	} catch (Exception e){
		log_error(171,"invalid binary constant= " + dcb_bin); // DSH #233
		return;
	}
	gen_dca_bits();
}
private void gen_dcb_bytes(){
	/*
	 * gen dcb byte length field 
	 */
	int index = 0;
	dc_hex = "";
	try {
		while (index < dcb_bin.length()){
			String dcb_hex = Integer.toHexString(Integer.valueOf(dcb_bin.substring(index,index+8),2).intValue()).toUpperCase();
			if (dcb_hex.length() < 2){
				dc_hex = dc_hex + "0" + dcb_hex;
			} else {
				dc_hex = dc_hex + dcb_hex;
			}
			index = index + 8;
		}
	} catch (Exception e){
		log_error(197,"invalid binary value string - " + dcb_bin);  // RPI 667
	}
	dcb_len = dc_hex.length()/2;
	if (dc_len_explicit){
		if (dcb_len < dc_len){
			dc_hex = tz390.get_dup_string("0",2*(dc_len-dcb_len)) + dc_hex; // RPI 411
		}
		if (dcb_len > dc_len){
			dc_hex = dc_hex.substring(2*(dcb_len-dc_len));
			dcb_len = dc_len;
		}
	} else {
		dc_len = dcb_len;
	}
	if (!dc_len_explicit && dc_first_field){
		dc_first_len = dc_len;
		dc_first_field = false;
	}
	if (dc_op && dc_dup > 0){
		obj_code = obj_code + dc_hex;
		put_obj_text();
	}
	if (!dc_lit_ref && dc_dup > 0){
	   loc_ctr = loc_ctr + dc_len;
	}
}
private void process_dcc_data(){
	/*
	 * allocate or generate dc Cln'...' data
	 * using dc_dup and explicit dc_len if any
	 * Notes:
	 *   1.  C'..' default EBCDIC unless ASCII option
	 *   2.  C".." always ASCII regardless of option
	 *   3.  C!..! always EBCDIC regardless of option
	 *   4.  ''|""|!! or && replaced with single '|"|! or &
	 *   5.  CA'...' always ASCII   RPI 270
	 *   6.  CE'...' always EBCDIC  RPi 270
	 */
	dcc_text = "";
	dcc_len  = 0;
	String token = null;
	int dcc_next = 0;
	dcc_quote = dc_field.charAt(dc_index); // ',", or ! delimiter
    if (dcc_quote == '\''){
    	dcc_match = dcc_sq_pattern.matcher(dc_field.substring(dc_index + 1));
    } else if (dcc_quote == '"') {
    	dcc_match = dcc_dq_pattern.matcher(dc_field.substring(dc_index + 1));
    } else {
    	dcc_match = dcc_eq_pattern.matcher(dc_field.substring(dc_index + 1));
    }
	while (!dc_eod && !bal_abort
			&& dcc_match.find()){
	       token = dcc_match.group();
	       dcc_next = dcc_match.end();
	       if (token.charAt(0) != dcc_quote 
	    		   && token.charAt(0) != '\''  //RPI192
	    		   && token.charAt(0) != '&'){ //RPI192
	       	  dcc_text = dcc_text + token;
	       } else if (token.length() == 2){
	       	  dcc_text = dcc_text + token.charAt(0); //RPI192
	       } else if (token.charAt(0) == dcc_quote){
	       	  dc_eod = true;
	       } else {
	    	  log_error(137,"invalid single " + token.charAt(0)); 
	       }
	}
	if (!dc_eod){
		log_error(52,"invalid dc character literal - " + dc_field.substring(dc_index));
	}
	dc_index = dc_index + dcc_next + 1;
	dcc_len = dcc_text.length();
	if (dcc_len > 0 || dc_len_explicit) {                            // #509
		dcc_ascii_req = 
			 (dcc_quote == '\'' 
				 && (    (tz390.opt_ascii 
					      && dc_type_sfx != 'E'
					     )
					  || dc_type_sfx == 'A'
					)
    	     )
		     | dcc_quote == '"';  //RPI5 and RPI73
		if (dc_bit_len){
			gen_dcc_bits();
		} else {
			gen_dcc_bytes();
		}
	} else {                                                         // #509
		log_error(88,"invalid data field expression - " + dc_field); // #509
	}                                                                // #509
	dc_len = 0;
}
private void gen_dcc_bits(){
	/*
	 * gen dcc bit field
	 */
	dc_bit_tot = dc_bit_tot + dc_len;
	if (dc_op && dc_dup > 0){
		int index = 0;
		while (dc_len > 0){
			dc_bit_buff = dc_bit_buff.shiftLeft(8);
			if (index < dcc_text.length()){
				if (dcc_ascii_req){
					dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf((int)dcc_text.charAt(index) & 0xff));
				} else {
					dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf((int)tz390.ascii_to_ebcdic[dcc_text.charAt(index) & 0xff] & 0xff));
				}
			} else {
				if (dcc_ascii_req){
					dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf((int)ascii_space & 0xff));
				} else {
					dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf((int)ebcdic_space & 0xff));
				}
			}
			dc_len = dc_len -8;
			index++;
		}
		if (dc_len < 0){
			dc_bit_buff = dc_bit_buff.shiftRight(-dc_len);
		}
	} 
}
private void gen_dcc_bytes(){
	/*
	 * gen dcc bytes
	 */
	if  (dc_len_explicit){
    	if  (dc_len > dcc_len){
    		dcc_text = tz390.left_justify(dcc_text,dc_len); // RPI 411
	    } else {
	    	dcc_text = dcc_text.substring(0,dc_len);
	    }
	} else {
	    dc_len = dcc_len;
	}
	if (dc_first_field && !dc_len_explicit){
	    dc_first_len = dc_len;
		dc_first_field = false;
	}
	while (!bal_abort
		&& dc_dup > 0){
		if (dc_op){
 	         obj_code = obj_code + string_to_hex(dcc_text,dcc_ascii_req);
  		     put_obj_text();
		}
		if (!dc_lit_ref){
  	       loc_ctr = loc_ctr + dc_len;
		}
   	    dc_dup--;
	}
}
private void process_dc_fp_data(){
	/*
	 * alloc or gen DS/DC D, E, or L type data using
	 * prev settings for dc_type, dc_type_sfx,
	 * dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	exp_index = dc_index;
	dc_data_start = dc_index; 
	exp_rld_len = 0;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort 
				&& dc_field.charAt(dc_index) != '\''){
			get_dc_fp_hex(dc_field,dc_index);
            if (dc_bit_len){
            	gen_dc_fp_bits();
            } else {
            	gen_dc_fp_bytes();
            }
			if (dc_field.charAt(dc_index) == ','){
			   	exp_index++;
		    } else if (dc_field.charAt(dc_index) == '\''){
		    	if (dc_dup > 1){         //RPI 388 start
				    dc_index = dc_data_start;
				    dc_dup--; 
		    	} else { 
		    		dc_eod = true;	
		    	}
		    } else {
			    log_error(150,"invalid data field terminator - " + dc_field);
		    }
			dc_first_field = false; // RPI 790
		}
	    dc_index++; // skip dca ) terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void gen_dc_fp_bits(){
	/*
	 * gen del bit length field
	 */
	dc_bit_tot = dc_bit_tot + dc_len;
	if (dc_op && dc_dup > 0){
		int index = 0;
		while (dc_len > 0){
			dc_bit_buff = dc_bit_buff.shiftLeft(8);
			dc_bit_buff = dc_bit_buff.add(BigInteger.valueOf(Integer.valueOf(dc_hex.substring(index,index+2),16) & 0xff));
			dc_len = dc_len -8;
			index = index + 2;
		}
		if (dc_len < 0){
			dc_bit_buff = dc_bit_buff.shiftRight(-dc_len);
		}
	} 
}
private void gen_dc_fp_bytes(){
	/*
	 * gen del byte length field
	 */
	if (dc_len_explicit){
		if (dc_len * 2 <= dc_hex.length()){
			dc_hex = dc_hex.substring(0,dc_len*2);
		} else {
			while (dc_hex.length() < dc_len *2){
				dc_hex = dc_hex + "00";
			}
		}
	}
	if (dc_op && dc_dup > 0){
		obj_code = obj_code + dc_hex;
		put_obj_text();
	}
	if (!dc_lit_ref && dc_dup > 0){
	   loc_ctr = loc_ctr + dc_len;
	}
}
private boolean get_dc_bd_val(){
	/*
	 * set dc_bd_val from next floating point
	 * sdt in dc_field at dc_index
	 * Note:
	 *  1.  Apply any scale factor 
	 *      or exponent to dc_bd_value.
	 *  2.  Set dc_unsigned true/false
	 */
	dc_unsigned = false;
	if (dc_field.charAt(dc_index) == '\''){
		dc_eod = true;
		return false;
	}
	if (dc_field.charAt(dc_index) == 'U'
		|| dc_field.charAt(dc_index) == 'u'){
		dc_unsigned = true;
		dc_index++;
	}
	int fp_bd_start = dc_index;
	while (dc_index < dc_field.length()){
		if (dc_field.charAt(dc_index) == '\''
			|| dc_field.charAt(dc_index) == ','){
			try { // 
				dc_bd_val = new BigDecimal(remove_blanks(dc_field.substring(fp_bd_start,dc_index))); // #233
			} catch (Exception e){
				log_error(161,"invalid decimal constant - " + dc_field.substring(fp_bd_start,dc_index));
				dc_bd_val = BigDecimal.ZERO;
			}
			if (dc_scale != 0){ // RPI 368
	    		dc_bd_val = dc_bd_val
	    		   .multiply(fp_bd_two.pow(dc_scale))
	    		   .divideToIntegralValue(BigDecimal.ONE); 
	    	}
			if (dc_exp > 0){ // RPI 737
				dc_bd_val = dc_bd_val.movePointRight(dc_exp);
			} else if (dc_exp < 0){
				dc_bd_val = dc_bd_val.movePointLeft(-dc_exp);
				
			}
		    return true;
		} else {
			dc_index++;
		}
	}
	return false;
}
private String remove_blanks(String text_in){
	// DSH #233 remove blanks allowed within numeric DC fields BDEFHLPXZ 
	return text_in.replace(" ", ""); // #233
}
private String get_dc_fh_hex_val(){
	/*
	 * get 1-16 byte hex value for F or H
	 * constant from dc_bd_val
	 */
	try {
    	if (dc_len <= 8){  // RPI 893
	        return tz390.get_long_hex(dc_bd_val.longValueExact(),2*dc_len); 
    	} else if (dc_len <= 16 
    			   && dc_bd_val.scale() <= 0
    			   && dc_bd_val.scale() > -40){
    		dc_bi_val = dc_bd_val.toBigIntegerExact();
    		dc_byte_val = dc_bi_val.toByteArray();
    		if (dc_byte_val.length > dc_len){
    	    	log_error(129,"DC value out of range " + dc_len);
    	   	    dc_len = 0;
    	   	    return "";
    		}
    		byte pad = 0;
    		if (dc_bi_val.signum() < 0){
    			pad = -1;
    		}
    		int index = 0;
    		if (index < 16-dc_byte_val.length){
    			Arrays.fill(fp_data_byte,index,16-dc_byte_val.length,pad); // RPI 411
    			index = 16-dc_byte_val.length;
    		}
    		if (index < 16){
    			System.arraycopy(dc_byte_val,0,fp_data_byte,index,16-index);
    		}
    		return tz390.get_long_hex(fp_data_buff.getLong(0),2*dc_len-16)
    		     + tz390.get_long_hex(fp_data_buff.getLong(8),16); 
    	} else {
    		log_error(122,"DC field length out of range " + dc_len);
    	    dc_len = 0;
    	    return "";
    	}
    } catch (Exception e) {
    	log_error(128,"DC value out of range " + dc_len);
   	    dc_len = 0;
   	    return "";
    }
}
private void get_dc_fp_hex(String text,int index){
	/*
	 * set dc_hex for D, E, or L 
	 * floating point sdt starting at text index
	 */
	if (text.charAt(index) == ','){
		index++;
	}
	int text_end   = text.substring(index).indexOf('\''); // RPI 411
	int text_comma = text.substring(index).indexOf(','); // RPI 463
	if (text_comma == -1 || text_comma > text_end){
		if (text_end == -1){
			log_error(66,"invalid floating point data field");
			dc_hex = "00";
		}
	} else {
		text_end = text_comma; // rpi 463
	}
	dc_index = index + text_end;
	fp_text = text.substring(index,index+text_end); // RPI 790
	fp_get_hex();
}
private void process_dcf_data(){
	/*
	 * alloc or gen DS/DC F type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	dc_index++;   // start inside ',,,'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (get_dc_bd_val()){
                if (dc_bit_len){
                	gen_dc_fh_bits();
                } else {
                	gen_dc_fh_bytes();
                }
			    if (dc_field.charAt(dc_index) == ','){
			    	dc_index++;
			    } else if (dc_field.charAt(dc_index) == '\''){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        // RPI2 end
			    } else {
				    log_error(107,"invalid data field terminator - " + dc_field);
			    }
		    } else {
			    log_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dcf terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void gen_dc_fh_bits(){
	/*
	 * gen F or H bit field
	 */
	dc_bit_tot = dc_bit_tot + dc_len;
	if (dc_op && dc_dup > 0){
		dc_bit_buff = dc_bit_buff.shiftLeft(dc_len);
		dc_bi_val = dc_bd_val.toBigIntegerExact();
		if (dc_bi_val.signum() >= 0){
			dc_bit_buff = dc_bit_buff.add(dc_bi_val);
		} else {
			dc_bi_val = BigInteger.ONE.shiftLeft(dc_len).subtract(BigInteger.ONE).and(dc_bi_val);
			dc_bit_buff = dc_bit_buff.add(dc_bi_val);
		}
	} 
}
private void gen_dc_fh_bytes(){
	/*
	 * gen F or H byte field
	 */
    if (dc_op && dc_dup > 0){
        obj_code = obj_code + get_dc_fh_hex_val();
        put_obj_text();
    }
	if (!dc_lit_ref && dc_dup > 0){
	   loc_ctr = loc_ctr + dc_len;
	}
}
private void process_dch_data(){
	/*
	 * alloc or gen DS/DC H type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	dc_index++;   // start inside (,,,)
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (get_dc_bd_val()){
                if (dc_bit_len){
                	gen_dc_fh_bits();
                } else {
                	gen_dc_fh_bytes();
                }
			    if (dc_field.charAt(dc_index) == ','){
			    	dc_index++;
			    } else if (dc_field.charAt(dc_index) == '\''){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        // RPI2 end
			    } else {
				    log_error(108,"invalid data field terminator - " + dc_field);
			    }
		    } else {
	         	log_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dch terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void process_dcp_data(){
	/*
	 * alloc or gen DS/DC P type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	dc_index++;   // start inside delimiter 'n,n'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''){
			    dcp_sign = 'C';
				dc_dec_point = false; // RPI 777
				dc_dec_scale = 0;     // RPI 777
			    dc_digits = "";
			    while (!bal_abort  // RPI 617 
			    		&& dc_index < dc_field.length() 
			    		&& dc_field.charAt(dc_index) != ','
			    	    && dc_field.charAt(dc_index) != '\''){
			         if (dc_field.charAt(dc_index) >= '0'
			         	&& dc_field.charAt(dc_index) <= '9'){
			        	dc_digits = dc_digits + dc_field.charAt(dc_index); // RPI 777
			        	if (dc_dec_point){
			        		dc_dec_scale++; // RPI 777
			        	}
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == '.'){
			        	 dc_dec_point = true;
			        	 dc_index++;
			         } else if (dc_field.charAt(dc_index) == '+'){
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == '-'){
			         	dcp_sign = 'D';
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == ' '){
			        	 dc_index++; // RPI 840
			         } else {
			         	log_error(67,"invalid character in P type data field - " + dc_field);
			         }
			    }
				if (!dc_scale_explicit && dc_first_field){
					dc_first_scale = dc_dec_scale;
				}
			    if (dc_digits.length() - dc_digits.length()/2*2 == 0){
			    	dc_hex = '0' + dc_digits + dcp_sign;
			    } else {
			    	dc_hex = dc_digits + dcp_sign;
			    }
			    dcp_len = dc_hex.length()/2;
                if (dc_bit_len){
                	gen_dcp_bits();
                } else {
                	gen_dcp_bytes();
                }
			    if (dc_field.charAt(dc_index) == ','){
			    	dc_index++;
			    }
			    dc_first_field = false; // RPI 790
		}
	    dc_index++; // skip dcp terminator
	    if  (!bal_abort){
		    if  (dc_dup > 1){
			    dc_index = dc_data_start;
			    dc_dup--;
		    } else {
			    dc_eod = true;
		    }
	    }
	}
	dc_len = 0; // don't double count RPI 538
}
private void gen_dcp_bits(){
	/*
	 * gen P bit field
	 */
	dc_bit_tot = dc_bit_tot + dc_len;
	if (dc_op && dc_dup > 0){
		dc_bit_buff = dc_bit_buff.shiftLeft(dc_len);
		dc_bi_val = BigInteger.ZERO;
		int index = 0;
		while (index < dc_hex.length()){
			dc_bi_val = dc_bi_val.shiftLeft(8).add(BigInteger.valueOf(Integer.valueOf(dc_hex.substring(index,index+2),16)));
			index = index + 2;
		}
		dc_bit_buff = dc_bit_buff.add(dc_bi_val);
	} 
}
private void gen_dcp_bytes(){
	/*
	 * gen P byte field
	 */
    if (dc_len_explicit){
    	if (dcp_len < dc_len){
    		dc_hex = tz390.get_dup_string("0",2*(dc_len-dcp_len)) + dc_hex;
    		dcp_len = dc_len;
    	}
        if (dcp_len > dc_len){
        	dc_hex = dc_hex.substring(2*(dcp_len - dc_len));
        }
    } else {
        dc_len = dcp_len;
    }
	if (!dc_len_explicit && dc_first_field){
		dc_first_len = dcp_len;
		dc_first_field = false;
	}
    if (dc_len > 16 && dc_type == 'P'){ // allow Z > 16
       log_error(68,"P type field too long - " + dc_field);
    } else if (dc_op && dc_dup > 0){
    	obj_code = dc_hex;
		put_obj_text();
    }
    if (!dc_lit_ref && dc_dup > 0){
	   loc_ctr = loc_ctr + dc_len;
    }
}
private void process_dcz_data(){
	/*
	 * alloc or gen DS/DC Z type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	dc_index++;   // start inside delimiter 'n,n'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''){
			    dcp_sign = 'C';
				dc_dec_point = false; // RPI 777
				dc_dec_scale = 0;     // RPI 777
			    dc_digits = "";
			    while (!bal_abort  // RPI 617 
			    		&& dc_index < dc_field.length() 
			    		&& dc_field.charAt(dc_index) != ','
			    	    && dc_field.charAt(dc_index) != '\''){
			         if (dc_field.charAt(dc_index) >= '0'
			         	&& dc_field.charAt(dc_index) <= '9'){
			        	if (tz390.opt_ascii){ // RPI 777
			        		dc_digits = dc_digits + "3" + dc_field.charAt(dc_index); // RPI 777
			        	} else {
			        		dc_digits = dc_digits + "F" + dc_field.charAt(dc_index); 
			        	}
			        	if (dc_dec_point){
			        		dc_dec_scale++; // RPI 777
			        	}
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == '.'){
			        	 dc_dec_point = true;
			        	 dc_index++;
			         } else if (dc_field.charAt(dc_index) == '+'){
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == '-'){
			         	dcp_sign = 'D';
			         	dc_index++;
			         } else if (dc_field.charAt(dc_index) == ' '){
			        	 dc_index++; // RPI 840
			         } else {
			         	log_error(67,"invalid character in Z type data field - " + dc_field); // RPI 930
			         }
			    }
				if (!dc_scale_explicit && dc_first_field){
					dc_first_scale = dc_dec_scale;
				}
			    if (dc_digits.length() > 2){
			    	dc_hex = dc_digits.substring(0,dc_digits.length()-2) + dcp_sign + dc_digits.charAt(dc_digits.length()-1);
			    } else if (dc_digits.length() == 2){  // RPI 930
			    	dc_hex = "" + dcp_sign + dc_digits.charAt(dc_digits.length()-1);
			    } else { // RPI 930
			    	dc_hex = "0";
			    }
			    dcp_len = dc_hex.length()/2;
			    while (dcp_len < dc_len){
			    	dc_hex = "F0" + dc_hex; // RPI 939
			    	dcp_len++;  // RPI 930
			    }
                if (dc_bit_len){
                	gen_dcp_bits();
                } else {
                	gen_dcp_bytes();
                }
			    if (dc_field.charAt(dc_index) == ','){
			    	dc_index++;
			    }
			    dc_first_field = false; // RPI 790
		}
	    dc_index++; // skip dcp terminator
	    if  (!bal_abort){
		    if  (dc_dup > 1){
			    dc_index = dc_data_start;
			    dc_dup--;
		    } else {
			    dc_eod = true;
		    }
	    }
	}
	dc_len = 0; // don't double count RPI 538
}
private void process_dcs_data(){
	/*
	 * alloc or gen DS/DC S type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_type, dc_len
	 */
	exp_text = dc_field;
	dc_index++;   // start inside (,,,)
	exp_index = dc_index;
	dc_data_start = dc_index; 
    exp_rld_len = 0;
	while (!dc_eod && !bal_abort){
		while (!dc_eod && !bal_abort){
		    if  (calc_exp()){
			    dc_index = exp_index;
			    if  (dc_len == 2){
			    	if  (exp_type == sym_rel){ // RPI 458
			    		if (dc_op && dc_dup > 0){ //RPI 578
			    			obj_code = obj_code + get_exp_bddd();
			    		}
			    	} else {
			    		dc_hex = get_exp_abs_bddd();
			    		if (dc_op && dc_dup > 0){ // RPI 578
			    			obj_code = obj_code + dc_hex;
			    		}
			    	}
			    	dc_index = exp_index;
			    } else if (dc_len == 3){
			    	// RPI 893 SY long displacement 
			    	get_bdddhh = true;
			    	if  (exp_type == sym_rel){ // RPI 458
			    		if (dc_op && dc_dup > 0){ //RPI 578
			    			obj_code = obj_code + get_exp_bddd();
			    		}
			    	} else {
			    		dc_hex = get_exp_abs_bddd();
			    		if (dc_op && dc_dup > 0){ // RPI 578
			    			obj_code = obj_code + dc_hex;
			    		}
			    	}
			    	get_bdddhh = false;
			    	dc_index = exp_index;
			    } else {
			    	log_error(99,"invalid length for S type");
			    }
			    if (dc_op && dc_dup > 0){ //RPI 578
			    	put_obj_text();
			    }
			    if (!dc_lit_ref && dc_dup > 0){
				   loc_ctr = loc_ctr + dc_len;
				   dc_dup_loc = dc_dup_loc + dc_len;
			    }
			    if (dc_field.charAt(dc_index) == ','){
			    	exp_index++;
			    } else if (dc_field.charAt(dc_index) == ')'){
			    	if (dc_dup > 1){         //RPI2 start
					    dc_index = dc_data_start;
					    exp_index = dc_index; 
					    dc_dup--; 
			    	} else { 
			    		dc_eod = true;	
			    	}                        // RPI2 end
			    } else {
				    log_error(109,"invalid data field terminator - " + dc_field);
			    }
		    } else {
			    dc_index = exp_index;
	         	log_error(88,"invalid data field expression - " + dc_field);
		    }
		}
	    dc_index++; // skip dca terminator
	    dc_len = 0; // don't double count
	}
	exp_rld_len = 0;
}
private void process_dcx_data(){
	/*
	 * alloc or gen DS/DC X type parms using prev.
	 * settings for dc_dup and dc_len.  Also save
	 * first field dc_len
	 * Notes:
	 *   1.  hex values are right aligned in 
	 *       explicit length fields.
	 *   2.  Spaces are ignored in data RPI 371
	 */
	dc_index++;   // start inside 'hex1,hex2,,'
	dc_data_start = dc_index; 
	while (!dc_eod && !bal_abort){
		dcx_len = 0;
		dc_hex = "";
		while (!dc_eod && !bal_abort
				&& dc_index < dc_field.length()
				&& dc_field.charAt(dc_index) != '\''
				&& dc_field.charAt(dc_index) != ','){
			    char hex_code = dc_field.substring(dc_index,dc_index + 1).toUpperCase().charAt(0);
			    if ((hex_code >= '0' && hex_code <= '9')
			    	||
					(hex_code >= 'A' && hex_code <= 'F')){
			    	dcx_len++;
			    	dc_hex = dc_hex + hex_code;
			    } else if (hex_code != ' '){
			    	log_error(77,"invalid hex code " + hex_code);
			    }
			    dc_index++;
		}
		if (dc_index >= dc_field.length()){
			log_error(78,"invalid hex dc data " + dc_field.substring(dc_data_start));
			return;
		}
		if (dcx_len != dcx_len/2*2){
			dc_hex = "0" + dc_hex;
		}
		dcx_len = dc_hex.length()/2;
        if (dc_bit_len){
        	gen_dcx_bits();
        } else {
        	gen_dcx_bytes();
        }
		if (dc_field.charAt(dc_index) == ','){
		   	dc_index++;
		} else if (dc_field.charAt(dc_index) == '\''){
	        if  (dc_dup > 1){    //rpi2
		        dc_index = dc_data_start;
		        exp_index = dc_index;
		        dc_dup--;
	        } else {
		        dc_eod = true;
	        }                    //rpi2
	    } else {
	    	log_error(109,"invalid dc data terminator - " + dc_field.substring(dc_index));
	    }
	}
    dc_index++; // skip terminator
    dc_len = 0; // don't double count
}
private void gen_dcx_bits(){
	/*
	 * gen X bit field
	 */
	dc_bit_tot = dc_bit_tot + dc_len;
	if (dc_op && dc_dup > 0){
		dc_bit_buff = dc_bit_buff.shiftLeft(dc_len);
		dc_bi_val = BigInteger.ZERO;
		int index = 0;
		while (index < dc_hex.length()){
			dc_bi_val = dc_bi_val.shiftLeft(8).add(BigInteger.valueOf(Integer.valueOf(dc_hex.substring(index,index+2),16)));
			index = index + 2;
		}
		dc_bit_buff = dc_bit_buff.add(dc_bi_val);
	} 
}
private void gen_dcx_bytes(){
	/*
	 * gen X byte field
	 */
	if (dc_len_explicit){
		if (dcx_len < dc_len){ // RPI 411
			dc_hex = tz390.get_dup_string("0",2*(dc_len-dcx_len)) + dc_hex;
			dcx_len = dc_len;
		}
		if (dcx_len > dc_len){
			dc_hex = dc_hex.substring(2*(dcx_len-dc_len));
			dcx_len = dc_len;
		}
	} else {
		dc_len = dcx_len;
	}
	if (!dc_len_explicit && dc_first_field){
		dc_first_len = dc_len;
		dc_first_field = false;
	}
	if (dc_op && dc_dup > 0){
		obj_code = obj_code + dc_hex;
		put_obj_text();
	}
	if (!dc_lit_ref && dc_dup > 0){
	   loc_ctr = loc_ctr + dc_len;
	}
}
private int get_dc_mod_int(){
	/*
	 * return integer expression in (...)
	 * or decimal number for modifier
	 */
 	if (dc_field.length() <= dc_index+1){  // RPI 1077 
 		log_error(208,"DS/DC missing modifier - " + dc_field);
 	    return 1;
 	} else if (dc_field.charAt(dc_index+1) == '('){
    	exp_text = dc_field;
 	    exp_index = dc_index+2;
 	    if (!bal_abort && calc_abs_exp() // RPI 416
 	    		&& dc_field.charAt(exp_index) == ')'){
 	       dc_index = exp_index+1;
 		   return exp_val;
 	    } else {
 		   log_error(43,"invalid dc duplication factor");
 		   return -1;
 	    }
 	} else {
        return get_dc_int(dc_index+1);
 	}
}
private int get_dc_int(int index){
	/*
	 * return next number from dc_field at index
	 * else return 1 and update dc_index
	 */
	dc_index = index;
 	while (dc_index < dc_field.length() 
 			&& dc_field.charAt(dc_index) <= '9'
 		    && dc_field.charAt(dc_index) >= '0'){
 		dc_index++;
 	}
 	if (dc_index > index){
 		return Integer.valueOf(dc_field.substring(index,dc_index)).intValue(); // #233
 	} else {
 		return 1;
 	}
}
private void process_cnop(){
	/*
	 * generate one or more 0700 instr.
	 * to align to specified boundary
	 */
   	loc_ctr = (loc_ctr+1)/2*2; // skip odd byte preceeding CNOP RPI 941
    loc_start = loc_ctr;
	exp_text = bal_parms;
	exp_index = 0;
	int req_off = 0;
	int cur_off = 0;
	if (calc_abs_exp() 
			&& exp_val >= 0 
			&& exp_val <  8){ 
		 req_off = exp_val;
		 if (exp_text.charAt(exp_index) == ','){
			 exp_index++;
			 if (calc_abs_exp()  // RPI 620
					 && (exp_val == 4
					     || exp_val == 8)){
				 cur_off = loc_ctr - loc_ctr/exp_val*exp_val;
                 int gap_bytes = req_off - cur_off;
                 if (gap_bytes < 0){
                	 gap_bytes = exp_val - cur_off + req_off;
                 }
                 if ((gap_bytes & 0x1) > 0){
                	 loc_len = 1;
                	 gap_bytes--;
                	 cur_off++;
                	 obj_code = "00";
                 }
			 	 if ((gap_bytes & (4-1)) > 0){ // 2 byte alignment
             		obj_code = obj_code + "0700";
             		cur_off += 2;
             		gap_bytes -= 2;
             		loc_len += 2;
			 	 }
                 while (gap_bytes > 0){ 
	             	obj_code = obj_code + "47000700";
	             	cur_off += 4;
	             	gap_bytes -= 4;
	             	loc_len += 4;
                 }
               	 put_obj_text();
			 }
		 }
	}
}
private void process_end(){
	/*
	 * perform END processing at END 
	 * statement or end of MLC source
	 * at end of each pass
	 */
	if (cur_esd > 0){
  		update_sect();
	}
	list_bal_line();
	if (tot_lit > 0){
		cur_esd = 1;
		while (cur_esd <= tot_esd 
				&& sym_type[esd_sid[esd_base[cur_esd]]] != sym_cst){ // RPI 564
			cur_esd++;
		}
		if (cur_esd <= tot_esd){
			cur_esd_sid = esd_sid[cur_esd];
	   	    while (sym_sect_next[cur_esd_sid] > 0){
	   	    	cur_esd_sid = sym_sect_next[cur_esd_sid];
	   	    }
	   	    loc_ctr = (sym_loc[cur_esd_sid] + sym_len[cur_esd_sid] + 7)/8*8;
			gen_ltorg();
			update_sect();
		} else {
			cur_esd = 0;
		}
	}
	bal_eof = true; 
	loc_ctr = 0;
	cur_esd = 0;
	put_obj_text(); // flush buffer
	if (end_loc != loc_ctr){ // RPI 605
		sect_change_error();
		log_error(186,"end location changed from " 
				+ tz390.get_hex(end_loc,6) 
				+ " to " + tz390.get_hex(loc_ctr,6));
	}
	end_loc = loc_ctr;
	update_sects();
	list_bal_line = false; // RPI 891 supress END after auto LTORG
}
public void process_equ(){ // RPI 415
	/* 
	 * define or update symbol definition
	 *   1. Set sym_loc to first pos value
	 *   2. Set sym_len to optional
	 *      2nd pos value else 
	 *      set sym_len to 1.
	 *   3. Set sym_attr to optional
	 *      3rd pos value.
	 *   4. Set sym_attrp 4th program type
	 *   5. Set sym_attra 5th assembler type
	 */
    int index = 0;
	check_private_csect();
	loc_start = loc_ctr;
	if (bal_label != null){
		cur_sid = find_sym(bal_label);
		if (cur_sid < 1){
			cur_sid = add_sym(bal_label);
		}
		int store_sid = cur_sid;
		sym_name[store_sid] = bal_label;
		if (!lookahead_mode && sym_def[store_sid] <= sym_def_ref){ 
			sym_def[store_sid] = bal_line_index;
		} else if (!lookahead_mode && sym_def[store_sid] != bal_line_index){
			duplicate_symbol_error();
		}
		exp_text = bal_parms;
		exp_index = 0;
		exp_equ = true; // rpi 749
		if (calc_exp()){
			// equ value and defaults
			sym_type[store_sid] = exp_type;
			sym_attr[store_sid] = exp_attr;
			sym_esd[store_sid] = exp_esd;
			if (sym_loc[store_sid] != exp_val){ // RPI 605
				sect_change_error();
				if (gen_obj_code && report_equ_changes){
					report_equ_changes = false;
					log_error(188,"first equ change for " + sym_name[store_sid] + " from " + tz390.get_hex(sym_loc[store_sid],8) + " to " + tz390.get_hex(exp_val,8));
				}
			}
			sym_loc[store_sid] = exp_val;
			sym_len[store_sid] = 1;
			hex_bddd1_loc = tz390.get_hex(exp_val,8); // RPI 1099 was 6 vs 8
			hex_bddd2_loc = "    ";                   // RPI 1099
			if (exp_next_char(',')){
				// equ explicit length
				exp_text = exp_text.substring(exp_index+1);
				exp_index = 0;
				if (exp_index < exp_text.length()){
					if (exp_text.charAt(exp_index) != ','){
						if (calc_abs_exp()){ // RPI 340
							sym_len[store_sid] = exp_val;
						}
					}
				}
				if (exp_next_char(',')){
					// equ explicit attr
					exp_text = exp_text.substring(exp_index+1);
					exp_index = 0;
					if (exp_text.charAt(exp_index) != ','){
						if (exp_text.length() > 2 
							&& exp_text.substring(exp_index,exp_index+2).equals("T'")){
							index = find_sym(exp_text.substring(exp_index+2));
							if (index > 0){
								sym_attr[store_sid] = sym_attr[index];
							} else {
								sym_attr[store_sid] = tz390.ascii_to_ebcdic['U'];
							}
						} else if (calc_abs_exp()){ // RPI 340
							sym_attr[store_sid] = (byte) exp_val;
						}
					}
				}
				if (exp_next_char(',')){
					// equ 4th explicit attrp pgm attr
					exp_text = exp_text.substring(exp_index+1);
					exp_index = 0;
					if (exp_text.charAt(exp_index) != ','){
						if (calc_abs_exp()){ // RPI 340
							sym_attrp[store_sid] = exp_val;
						}
					}
				}
				if (exp_next_char(',')){
					// equ 5th explicit attra asm attr
				    String setc_value = exp_text.substring(exp_index+1).toUpperCase();
				    index = 0;  // RPI 624
				    while (index < setc_value.length()
				    		&& setc_value.charAt(index) > ' '){
				    	index++;
				    }
				    if (index > 0){
				    	setc_value = setc_value.substring(0,index);
				    }
				    sym_attra[store_sid] = setc_value; // RPI 615 remove bad code setting string length
					index = 0;
					boolean attra_found = false;
					while (!attra_found && index < sym_attra_type.length){
						if (sym_attra[store_sid].equals(sym_attra_type[index])){
							attra_found = true;
						}
						index++;
					}
					if (!attra_found){
						log_error(155,"invalid symbol assembler attribute " + sym_attra[store_sid]);
					}
				}
			}
		} else {
			log_error(53,"invalid equ expression");
		}
	} else {
		log_error(184,"missing EQU label");  // RPI 597
	}
	exp_equ = false; // RPI 749
}
private void process_org(){
	/*
	 * reset current location in same csect
	 */
	update_sect(); // RPI 340 
	loc_start = loc_ctr;
	if (bal_parms == null 
		|| bal_parms.length() == 0
		|| bal_parms.charAt(0) == ','){  // RPI 258
		if (cur_esd > 0){  //RPI10, RPI87
			// org to end of current CSECT
			loc_ctr = sym_loc[esd_sid[cur_esd]] + sym_len[esd_sid[cur_esd]];
			hex_bddd1_loc = tz390.get_hex(loc_ctr,6); // RPI 632
		} else {
			log_error(102,"org expression must be in same section");
		}
		return;
	}
	exp_text = bal_parms;
	exp_index = 0;
	if (cur_esd > 0
		&& calc_rel_exp()
		&& exp_esd == esd_base[cur_esd]){ // RPI 301
		loc_ctr = exp_val;
		hex_bddd1_loc = tz390.get_hex(loc_ctr,6); // RPI 632
		update_sect();  // RPI 10, RPI 778
	} else { 
		loc_ctr = loc_start; // rpi 851
		log_error(102,"org expression must be in same section");
	}
}
private void process_start()                                    // RPI 1523 routine copied from process_org
    /*                                                          // RPI 1523
     *                                                          // RPI 1523
     * set current location first csect                         // RPI 1523
     */                                                         // RPI 1523
   {if (bal_parms           == null                             // RPI 1523
     || bal_parms.length()  == 0                                // RPI 1523
     || bal_parms.charAt(0) == ','                              // RPI 1523
        )                                                       // RPI 1523
       {loc_ctr   = 0; // Start at address 0                    // RPI 1523
        }                                                       // RPI 1523
    else                                                        // RPI 1523
       {exp_text  = bal_parms;                                  // RPI 1523
        exp_index = 0;                                          // RPI 1523
        if (calc_abs_exp()                                      // RPI 1523
            )                                                   // RPI 1523
           {start_loc = (exp_val+7)/8*8; // Round to Dword      // RPI 1523
            }                                                   // RPI 1523
        else                                                    // RPI 1523
           {start_loc = 0;                                      // RPI 1523
            log_error(102,"start expression must be absolute"); // RPI 1523
            }                                                   // RPI 1523
        }                                                       // RPI 1523
    }                                                           // RPI 1523
private void process_push(){
	/*
	 * push print or using level if any
	 */
	init_get_next_parm(bal_parms);
	String parm = get_next_parm();
    while (parm != null){
    	if (parm.equals("NOPRINT")){
    		list_bal_line = false;
    	} else if (parm.equals("PRINT")){
			if (print_level < tz390.opt_maxcall-1){
				print_on[print_level+1] = print_on[print_level];
				print_gen[print_level+1] = print_gen[print_level];
				print_data[print_level+1] = print_data[print_level];
				print_level++;
			} else {
				log_error(126,"maximum push print exceeded");
			}
		} else if (parm.equals("USING")){
			int cur_entries = cur_use_end - cur_use_start;
			if (using_level < tz390.opt_maxcall-1
					&& cur_use_end + cur_entries <= tz390.opt_maxcall){
				push_cur_use_start[using_level] = cur_use_start;
				push_cur_use_end[using_level]   = cur_use_end;
				int index = cur_use_start;
				while (index < cur_use_end){
					move_use_entry(index,index+cur_entries);
					index++;
				}
				using_level++;
				cur_use_start = cur_use_start + cur_entries;
				cur_use_end   = cur_use_end   + cur_entries;
			} else {
				log_error(127,"maximum push using exceeded");
			}
		} else {
			log_error(129,"invalid push parm - " + parm);
		}
		parm = get_next_parm();
	}	
}
private void process_pop(){
	/*
	 * pop print or using level if any
	 */
	init_get_next_parm(bal_parms);
	String parm = get_next_parm();
    while (parm != null){
    	if (parm.equals("NOPRINT")){
    		list_bal_line = false;
    	} else if (parm.equals("PRINT")){
			if (print_level > 0){
				print_level--;
			}
		} else if (parm.equals("USING")){
			if (using_level > 0){
				using_level--;
				cur_use_start = push_cur_use_start[using_level];
				cur_use_end   = push_cur_use_end[using_level];
			}
		} else {
			log_error(125,"invalid pop parm - " + parm);
		}
		parm = get_next_parm();
	}	
}
private void process_print(){
	/*
	 * process print options
	 * (unsupported options ignored)
	 */
	init_get_next_parm(bal_parms);
	String parm = get_next_parm();
    while (parm != null){
    	if (parm.equals("NOPRINT")){  // RPI 304
    		list_bal_line = false;
    	} else if (parm.equals("ON")){
            print_on[print_level] = true;
		} else if (parm.equals("OFF") && !tz390.opt_printall){ // RPI 1127
			print_on[print_level] = false;
		} else if (parm.equals("GEN")){
			print_gen[print_level] = true;
		} else if (parm.equals("NOGEN") && !tz390.opt_printall){ // RPI 1127
			print_gen[print_level] = false;
		} else if (parm.equals("DATA")){ // RPI 588
			print_data[print_level] = true;
		} else if (parm.equals("NODATA")){
			print_data[print_level] = false;
		}
		parm = get_next_parm();
	}
}
private void init_get_next_parm(String parms){
	/*
	 * use tz390.parm_match to find and return next parm
	 * separated by commas else return null.
	 * 
	 */
	if (parms != null && parms.length() > 0){
		tz390.parm_match = tz390.parm_pattern.matcher(parms);
	} else {
		tz390.parm_match = null;
	}
}
private String get_next_parm(){
	/*
	 * use tz390.parm_match to find and return next parm
	 * in upper case else return null.
	 * 
	 */
	if (tz390.parm_match != null){
		while (tz390.parm_match.find()){
			String parm = tz390.parm_match.group().toUpperCase();
			if (parm.charAt(0) <= ' '){
				return null;
			}
			if (parm.charAt(0) != ','){
				return parm;
			}
		}
	}
	return null;
}
private void duplicate_symbol_error(){
	/*
	 * issue error for duplicate symbol definition
	 */
	log_error(72,"duplicate symbol " + sym_name[cur_sid] + " on line " + bal_line_num[bal_line_index] + " and " + bal_line_num[sym_def[cur_sid]]);
}
private void calc_lit_or_exp(){
	/*
	 * calc rel exp for lit or explicit offset
	 * for following offset(index,base)
	 */
	if (exp_next_char('=')){
		calc_lit();
	} else if (exp_text != null && exp_text.substring(exp_index).length() > 8  // RPI 626 rpi 822
			   && exp_text.substring(exp_index,exp_index+8).toUpperCase().equals("DFHRESP(")){
		String dfhresp_type_key = exp_text.substring(exp_index + 8).toUpperCase() + "         ";
		int index = 0;
		while (index < dfhresp_type.length && !dfhresp_type_key.substring(0,dfhresp_type[index].length()).equals(dfhresp_type[index])){
			index++;
		}
		if (index < dfhresp_type.length){
			exp_text = exp_text.substring(0,exp_index) + dfhresp_lit[index] + exp_text.substring(exp_index + 8 + dfhresp_type[index].length()); // RPI 635
		    calc_lit();
		} else {
			calc_exp();
		}
	} else if (exp_text != null && exp_text.substring(exp_index).length() > 9  // RPI 1057
			   && exp_text.substring(exp_index,exp_index+9).toUpperCase().equals("DFHVALUE(")){
		String dfhvalue_type_key = exp_text.substring(exp_index + 9).toUpperCase() + "         ";
		int index = 0;
		while (index < dfhvalue_type.length && !dfhvalue_type_key.substring(0,dfhvalue_type[index].length()).equals(dfhvalue_type[index])){
			index++;
		}
		if (index < dfhvalue_type.length){
			exp_text = exp_text.substring(0,exp_index) + dfhvalue_lit[index] + exp_text.substring(exp_index + 9 + dfhvalue_type[index].length()); // RPI 635
		    calc_lit();
		} else {
			calc_exp();
		}	
	} else {
		calc_exp();
	}
}
private boolean calc_lit(){
	/*
	 * 1.  Find or add literal and set 
	 *     exp_type, exp_val, and exp_esd.
     * 2.  If literal followed by '-' or '+'
     *     caculcate expression  
     *     add to lit address
     *     and return abs val else error.
	 */
    get_lit_addr();
	if (cur_lit != -1){
		if (exp_next_char('-')){
			if (calc_exp()){
				exp_val = exp_val + lit_loc[cur_lit];
				if (exp_esd == lit_esd[cur_lit]){
					exp_esd = esd_sdt;
					exp_type = sym_sdt;
				} else if (exp_esd == 0){ // RPI 501
				    exp_esd = lit_esd[cur_lit];
				    exp_type = sym_rel;
				} else {
					log_error(175,"invalid literal complex expression");
				}
			} else { // RPI 457
				log_error(169,"invalid literal expression");
			}
		} else if (exp_next_char('+')){
			if (calc_exp() && exp_esd == esd_sdt){ // RPI 457
				exp_val = exp_val + lit_loc[cur_lit];
				exp_esd = esd_base[lit_esd[cur_lit]];  // RPI 457
				exp_type = sym_rel;
			} else {
				log_error(170,"invalid literal + offset expression");
			}
		} else {
			exp_val = lit_loc[cur_lit];
			exp_esd = esd_base[lit_esd[cur_lit]];
			exp_type = sym_rel;
		}
		exp_len = lit_len[cur_lit];
		if (!bal_abort){
			return true;
		}
	}
	return false;
}
private String get_default_bddd(){
	/* 
	 * return bddd or bdddhh 
	 */
	if (get_bdddhh){
		return "bdddhh";
	} else {
		return "bddd";
	}
}
private void get_lit_addr(){
	/*
	 * find or add literal and set:
	 *   1. cur_lit = lit table index
	 *   2. exp_val = lit address
	 *   3. exp_esd = lit esd
	 */
	String lit_key = "";
	process_dc(2);
	if (!bal_abort){
		if (lit_loc_ref){
			lit_key = cur_lit_pool + ":" +bal_line_index + dc_field.substring(dc_lit_index_start,dc_index);
		} else {
			lit_key = cur_lit_pool + dc_field.substring(dc_lit_index_start,dc_index);
		}
		cur_lit = tz390.find_key_index('L',lit_key);
		if (cur_lit != -1){
			add_lit_xref(cur_lit);
			if (lit_loc_ref){
				lit_line_loc[cur_lit] = loc_ctr;
			}
			exp_esd = esd_base[lit_esd[cur_lit]]; // RPI 301
			exp_val = lit_loc[cur_lit];
            return;
		}
		if (!gen_obj_code && tot_lit < tz390.opt_maxsym){
		    cur_lit = tot_lit;
			if (!tz390.add_key_index(cur_lit)){
			    abort_error(87,"key search table exceeded");
			}
			add_lit_xref(cur_lit);
		    tot_lit++;
			lit_name[cur_lit] = dc_field.substring(dc_lit_index_start,dc_index);
			lit_pool[cur_lit] = cur_lit_pool;
			lit_line[cur_lit] = bal_line_index;
			lit_line_loc[cur_lit] = loc_ctr;
			lit_esd[cur_lit] = cur_esd;
			lit_loc[cur_lit] = -1; // set by gen_lit
			lit_len[cur_lit] = dc_first_len;
			lit_dup[cur_lit] = dc_first_dup; // RPI 1200 save for lit pool alignment sizing
			lit_scale[cur_lit] = dc_first_scale; // RPI 790
			lit_dc_type[cur_lit]  = (byte) dc_first_type;  // RPI 790
			lit_dc_type_sfx[cur_lit] = (byte) dc_first_type_sfx; // RPI 790
			lit_gen[cur_lit] = 0;  // set by gen_lit;
			exp_val = -1;
		} else {
			log_error(57,"literal table size exceeded");
		}
	}
    exp_val = 0;
    exp_esd = 0;
}
private void gen_ltorg(){
	/* 
	 * generate ltorg at current location in csect
	 */
	loc_ctr = (loc_ctr + 7)/8*8;
	gen_lit_size(8);
	gen_lit_size(4);
	gen_lit_size(2);
	gen_lit_size(1);
}
private void gen_lit_size(int size){
	/*
	 * generate literal dc's of specified size
           * force all length 1 lits to even addr for access by relative halfword offsets in LARL etc.  per issue #327
           * see regression test rt\test\TESTLITS.MLC which fails on previous releases
	 */
	cur_lit = 0;
	while (cur_lit < tot_lit){
		if (lit_len[cur_lit]*lit_dup[cur_lit] == lit_len[cur_lit]*lit_dup[cur_lit]/size*size // RPI 1200 use dup for aligning
				&& lit_gen[cur_lit] == 0
				&& lit_pool[cur_lit] == cur_lit_pool
				){			
			lit_gen[cur_lit] = 1;
			lit_esd[cur_lit] = esd_base[cur_esd]; // RPI 457
                             if   (size == 1 && loc_ctr != (loc_ctr/2*2)){
                                  loc_ctr = loc_ctr + 1;  // issue #327 force lits on even addr for ref by halfword offset counts
                             }
			process_dc(3);
			if (gen_obj_code && tz390.opt_list){ // RPI 484
				if (list_obj_code.length() < 16){
					list_obj_code = list_obj_code.concat("                ").substring(0,16);
				} 
				list_obj_loc = lit_loc[cur_lit];
				String lit_line = tz390.get_hex(list_obj_loc,6) + " " + list_obj_code.substring(0,16) + " =" + lit_name[cur_lit]; 
				if (bal_abort){
					force_list_bal = true; // RPI 1031
					loc_ctr = loc_ctr + lit_len[cur_lit]; // RPI 1031
				}
				put_prn_line(lit_line);
				bal_abort = false;         // RPI 1031
			}			
		}
		cur_lit++;
	}
}
private void add_lit_xref(int index){
	/*
	 * add literal xref
	 */
	if (!tz390.opt_xref || !gen_obj_code){  //RPI165
		return;
	}
	if (lit_xref[index] == null){
		lit_xref[index] = new TreeSet<Integer>();
	}
	lit_xref[index].add(bal_line_num[bal_line_index]);
}
private int add_esd(int sid,byte sect_type){
	/*
	 * add new esd chained to sid 
	 * and return index else abort
	 */
	   if (tot_esd < tz390.opt_maxesd-1){ // RPI 284
		   tot_esd++;
		   esd_sid[tot_esd] = sid;
		   esd_base[tot_esd] = tot_esd; // RPI 301
		   if (sect_type != sym_ent){ // may already be set to rel or cst
			   sym_esd[sid] = tot_esd;
			   sym_type[sid] = sect_type;
		   }
	   } else {
		   abort_error(96,"maximum esds exceeded");
		   return -1;
	   }
	   return tot_esd;
}
public int add_sym(String name){ // RPI 415 public
	/*
	 * add symbol table entry name and return
	 * index for use in setting remaining fields
	 * Notes:
	 *   1.  If lookahead mode, set sym_def = -1
	 */
	   if (tot_sym < tz390.opt_maxsym - 1){
		   tot_sym++;
		   sym_name[tot_sym] = name.toUpperCase(); // RPI 415
		   sym_attr[tot_sym] = tz390.ascii_to_ebcdic['U'];
		   if (!tz390.add_key_index(tot_sym)){
			   return -1;
		   }
		   if (lookahead_mode){
			   sym_def[tot_sym] = sym_def_lookahead;
		   } else {
			   add_sym_xref(tot_sym);
		   }
		   return tot_sym;
	   } else {
		   abort_error(10,"maximum symbol table size exceeded");
		   return -1;
	   }
}
private void add_sym_xref(int index){
	/*
	 * add symbol xref
	 */
	if (!tz390.opt_xref 
		|| !gen_obj_code  //RPI165
		|| (last_xref_index   == index 
			&& last_xref_line == bal_line_index)){ 
		return;
	}
	last_xref_index = index;
	last_xref_line  = bal_line_index;
	if (sym_xref[index] == null){
		sym_xref[index] = new TreeSet<Integer>();
	}
	sym_xref[index].add(bal_line_num[bal_line_index]);
}
private void gen_ccw0(){  // RPI 567
	/*
	 * generate 8 byte aligned CCW0
	 * op8,addr24,flags8,zero8,len16
	 */
	dc_align(8);
	loc_start = loc_ctr;
	exp_text = bal_parms;
	exp_index = 0;
	if (calc_abs_exp() 
		&& exp_val >= 0 
		&& exp_val <  256){ 
		obj_code = obj_code + tz390.get_hex(exp_val,2);
		loc_len  = 1;
		put_obj_text();
		loc_ctr++;
		if (exp_text.charAt(exp_index) == ','){
			 exp_index++;
			 exp_rld_len = 3;
			 if (calc_exp()){  // RPI 771
				 obj_code = obj_code + tz390.get_hex(exp_val,6);
				 put_obj_text();        // RPI 632 
				 loc_ctr = loc_ctr + 3; // RPI 632
				 if (exp_text.charAt(exp_index) == ','){
					 exp_index++;
					 exp_rld_len = 0;
					 if (calc_abs_exp()
						&& exp_val < 256){
						obj_code = obj_code + tz390.get_hex(exp_val,2);
						obj_code = obj_code + tz390.get_hex(0,2);
						put_obj_text();        // rpi 632 
						loc_ctr = loc_ctr + 2; // rpi 632
						if (exp_text.charAt(exp_index) == ','){
							 exp_index++;
							 if (calc_abs_exp()
								 && exp_val <= 0xffff){
								obj_code = obj_code + tz390.get_hex(exp_val,4);
								put_obj_text();        // rpi 632 
								loc_ctr = loc_ctr + 2; // rpi 632
							}
						}
					}
				}
			 }
		}
	}
	loc_len = 0;
	exp_rld_len = 0;
}
private void gen_ccw1(){  // RPI 567
	/*
	 * generate 8 byte aligned CCW1
	 * op8,flags8,len16,addr32
	 */
	String ccw_op    = null;
	String ccw_flags = null;
	String ccw_len   = null;
	dc_align(8);
	loc_start = loc_ctr;
	exp_text = bal_parms;
	exp_index = 0;
	if (calc_abs_exp() 
		&& exp_val >= 0 
		&& exp_val <  256){ 
		ccw_op = tz390.get_hex(exp_val,2);
		loc_ctr = loc_ctr + 4;
		if (exp_text.charAt(exp_index) == ','){
			 exp_index++;
			 exp_rld_len = 4;
			 if (calc_exp()){  // RPI 771
				 obj_code = obj_code + tz390.get_hex(exp_val,8);
				 put_obj_text();
				 exp_rld_len = 0;
				 loc_ctr = loc_ctr - 4;
				 if (exp_text.charAt(exp_index) == ','){
					 exp_index++;
					 put_obj_text();
					 if (calc_abs_exp()
						&& exp_val < 256){
						ccw_flags = tz390.get_hex(exp_val,2);
						if (exp_text.charAt(exp_index) == ','){
							 exp_index++;
							 if (calc_abs_exp()
								 && exp_val <= 0xffff){
								 ccw_len = tz390.get_hex(exp_val,4);
							 }
						}
					}
				}
			 }
		}
	}
	obj_code = obj_code + ccw_op + ccw_flags + ccw_len;
	put_obj_text();
	loc_ctr = loc_ctr + 8;
	loc_len = 0;
	exp_rld_len = 0;
	if (list_obj_code.length() == 16){
		list_obj_code = list_obj_code.substring(8)+list_obj_code.substring(0,8);
	}
}
private void fp_get_hex(){
	/*
	 * set dc_hex for floating point
	 * string fp_text
	 * in scientific notation 0.314159E1 etc.
	 * format is based on fp type 0-8 (db,dd,dh,eb,ed,eh,lb,ld,lh)
	 *
	 * Notes:
	 *   1.  This is very tricky code!
	 *   2.  Use BigDecimal for all types to 
	 *       insure DH and EH exponents beyond 
	 *       range of DB and EB will be correctly
	 *       handled without error.
	 *   3.  The fp_context is set to significant
	 *       decimal digits plus fp_guard_digts to insure 
	 *       sufficient significant bits for proper
	 *       rounding occurs.
	 *   4.  The preferred DFP exponent  
	 *       (BigDecimal scale factor) is
	 *       set based on explicit decimal poiint
	 *       with significant trailing decimal places
	 *       including zeros else use 0. RPI 790
	 * 
	 * First convert string constant to positive
	 * big_dec1 value with sufficent sig. bits.
	 * Exit with artbitrary format if zero.
	 */
	char fp_sign = '+';
	if (fp_text.charAt(0) =='-'){ // RPI 834
		fp_sign = '-';
		fp_text = fp_text.substring(1);
	}
	fp_text = remove_blanks(fp_text); // #233 
	if (fp_text.charAt(0) == '('){ // RPI 367 support (MIN) and (MAX)
		if (fp_text.toUpperCase().equals("(MAX)")){
			switch (tz390.fp_type){  // gen (max) hex for tz390.fp_type
			case 0: // tz390.fp_db_type s1,e11,m52 with assumed 1
				dc_hex = "7FEFFFFFFFFFFFFF";
			    break;
			case 1: // tz390.fp_dd_type s1,cf5,bxcf6,ccf20
			    dc_hex = "77FCFF3FCFF3FCFF"; // RPI 407
			    break;
			case 2: // tz390.fp_dh_type s1,e7,m56 with hex exp
				dc_hex = "7FFFFFFFFFFFFFFF";
				break;
			case 3: // tz390.fp_eb_type s1,e8,m23 with assumed 1
	            dc_hex = "7F7FFFFF";
	            break;
			case 4: // tz390.fp_ed_type s1,cf5,bxcf8,ccf50
			    dc_hex = "77F3FCFF"; // RPI 407
			    break;
			case 5: // tz390.fp_eh_type s1,e7,m24 with hex exp
				dc_hex = "7FFFFFFF";
				break;
			case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
				dc_hex = "7FFEFFFFFFFFFFFFFFFFFFFFFFFFFFFF";
				break;
			case 7: // tz390.fp_ld_type s1,cf5,bxcf12,ccf110
			    dc_hex = "77FFCFF3FCFF3FCFF3FCFF3FCFF3FCFF"; // RPI 407
			    break;
			case 8: // tz390.fp_lh_type s1,e7,m112 with split hex	
				dc_hex = "7FFFFFFFFFFFFFFF71FFFFFFFFFFFFFF";
				break;
			case 9: // tz390.fp_lq_type quad word RPI 1108
				dc_hex = "00000000000000000000000000000000";
				break;
			}
			if (fp_sign == '-'){
				dc_hex = "F" + dc_hex.substring(1);
			}
			return;
		} else if (fp_text.toUpperCase().equals("(MIN)")){
			switch (tz390.fp_type){  // gen (min) hex for tz390.fp_type
			case 0: // tz390.fp_db_type s1,e11,m52 with assumed 1
				dc_hex = "0010000000000000";
			    break;
			case 1: // tz390.fp_dd_type s1,cf5,bxcf8,ccf50
				dc_hex = "0000000000000001"; // RPI 407
				break;
			case 2: // tz390.fp_dh_type s1,e7,m56 with hex exp
				dc_hex = "0110000000000000";
				break;
			case 3: // tz390.fp_eb_type s1,e7,m24 with assumed 1
	            dc_hex = "00800000";
	            break;
			case 4: // tz390.fp_dd_type s1,cf5,bxcf6,ccf20
				dc_hex = "00000001"; // RPI 407
				break;
			case 5: // tz390.fp_eh_type s1,e7,m24 with hex exp
				dc_hex = "01100000";
				break;
			case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
				dc_hex = "00010000000000000000000000000000";
				break;
			case 7: // tz390.fp_ld_type s1,cf5,bxcf12,ccf110
				dc_hex = "00000000000000000000000000000001"; // RPI 407
				break;
			case 8: // tz390.fp_lh_type s1,e7,m112 with split hex	
				dc_hex = "01100000000000007200000000000000";
				break;
			case 9: // tz390.fp_lq_type quad word RPI 1108
				dc_hex = "00000000000000000000000000000000";
				break;	
			}
			if (fp_sign == '-'){
				dc_hex = "8" + dc_hex.substring(1);
			}
			return;
		} else {
			log_error(112,"unrecognized floating point constant " + fp_text);
		}
	}
	fp_context = new MathContext(tz390.fp_precision[tz390.fp_type],RoundingMode.HALF_EVEN); // RPI 843
	try { // RPI 424
		fp_big_dec1 = new BigDecimal(fp_text,fp_context);
	} catch (Exception e){
		log_error(162,"invalid decimal floating point constant= " + fp_text); // #233 show text fix
		fp_big_dec1 = BigDecimal.ZERO;
	}
	if (dc_exp > 0){ // RPI 368 adj by DC E modifer
		fp_big_dec1 = fp_big_dec1.movePointLeft(dc_exp);
	} else if (dc_exp < 0){
		fp_big_dec1 = fp_big_dec1.movePointRight(-dc_exp);		
	}
	if (fp_big_dec1.signum() > 0){
		if (fp_sign == '+'){  // RPI 834
			tz390.fp_sign = 0;
		} else {
			tz390.fp_sign = tz390.fp_sign_bit[tz390.fp_type];
		}
	} else if (fp_sign == '+'){ // RPI 834
		switch (tz390.fp_type){  // gen zero hex for tz390.fp_type
		case 0: // tz390.fp_db_type s1,e11,m52 with assumed 1
			dc_hex = "0000000000000000"; // RPI 384
			return;
		case 1: // tz390.fp_dd_type s1,cf5,bxcf8,ccf50 // RPI 407
			dc_hex = "2238000000000000"; // RPI 384 RPI 790
			return;
		case 2: // tz390.fp_dh_type s1,e7,m56 with hex exp
			dc_hex = "0000000000000000"; // RPI 384
			return;
		case 3: // tz390.fp_eb_type s1,e7,m24 with assumed 1
			dc_hex = "00000000"; // RPI 384
			return;
		case 4: // tz390.fp_ed_type s1,cf5,bxdf6,ccf20 // RPI 407
			dc_hex = "22500000"; // RPI 384 RPI 790
			return;
		case 5: // tz390.fp_eh_type s1,e7,m24 with hex exp
			dc_hex = "00000000"; // RPI 384
			return;
		case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
			dc_hex = "00000000000000000000000000000000";  // RPI 384
			return;
		case 7: // tz390.fp_ld_type s1,cf5,bxdf12,ccf110 // RPI 407	
			dc_hex = "22080000000000000000000000000000";  // RPI 384 RPI 790
			return;
		case 8: // tz390.fp_lh_type s1,e7,m112 with split hex	
			dc_hex = "00000000000000000000000000000000";  // RPI 384
			return;
		case 9: // tz390.fp_lq_type quad word RPI 1108
			dc_hex = "00000000000000000000000000000000";
			break;	
		}
	} else { // RPI 834 negative zero values
		switch (tz390.fp_type){  // gen zero hex for tz390.fp_type
		case 0: // tz390.fp_db_type s1,e11,m52 with assumed 1
			dc_hex = "8000000000000000"; // RPI 384
			return;
		case 1: // tz390.fp_dd_type s1,cf5,bxcf8,ccf50 // RPI 407
			dc_hex = "A238000000000000"; // RPI 384 RPI 790
			return;
		case 2: // tz390.fp_dh_type s1,e7,m56 with hex exp
			dc_hex = "8000000000000000"; // RPI 384
			return;
		case 3: // tz390.fp_eb_type s1,e7,m24 with assumed 1
			dc_hex = "80000000"; // RPI 384
			return;
		case 4: // tz390.fp_ed_type s1,cf5,bxdf6,ccf20 // RPI 407
			dc_hex = "A2500000"; // RPI 384 RPI 790
			return;
		case 5: // tz390.fp_eh_type s1,e7,m24 with hex exp
			dc_hex = "80000000"; // RPI 384
			return;
		case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
			dc_hex = "80000000000000000000000000000000";  // RPI 384
			return;
		case 7: // tz390.fp_ld_type s1,cf5,bxdf12,ccf110 // RPI 407	
			dc_hex = "A2080000000000000000000000000000";  // RPI 384 RPI 790
			return;
		case 8: // tz390.fp_lh_type s1,e7,m112 with split hex	
			dc_hex = "80000000000000000000000000000000";  // RPI 384
			return;
		case 9: // tz390.fp_lq_type quad word RPI 1108
			dc_hex = "00000000000000000000000000000000";
			break;	
		}
	}
	/*
	 * 1.  Convert BFP and HFP to base 2 exp
	 *     and mantissa from base 10.
	 * 2.  For DFP adjust base 10 exponent
	 *     based on explicit decimal point
	 *     and significant trailing digits
	 *     includeing zeros.
	 */
	switch (tz390.fp_type){
	case 0: // tz390.fp_db_type s1,e11,m52 with assumed 1
	    cvt_fp_exp_to_base_2();
	    fp_cvt_bd_to_hex();
	    break;
	case 1: // tz390.fp_dd_type s1,cf5,bxcf8,ccf50 // RPI 407
		fp_cvt_bd_to_hex();
		break;
	case 2: // tz390.fp_dh_type s1,e7,m56 with hex exp
	    cvt_fp_exp_to_base_2();
	    fp_cvt_bd_to_hex();
	    break;
	case 3: // tz390.fp_eb_type s1,e7,m24 with assumed 1
	    cvt_fp_exp_to_base_2();
	    fp_cvt_bd_to_hex();
	    break;
	case 4: // tz390.fp_ed_type s1,cf5,bxdf6,ccf20 // RPI 407
		fp_cvt_bd_to_hex();
		break;
	case 5: // tz390.fp_eh_type s1,e7,m24 with hex exp
	    cvt_fp_exp_to_base_2();
	    fp_cvt_bd_to_hex();
	    break;
	case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
	    cvt_fp_exp_to_base_2();
	    fp_cvt_bd_to_hex();
	    break;
	case 7: // tz390.fp_ld_type s1,cf5,bxdf12,ccf110 // RPI 407	
		fp_cvt_bd_to_hex();
		break;
	case 8: // tz390.fp_lh_type s1,e7,m112 with split hex	
	    cvt_fp_exp_to_base_2();
	    fp_cvt_bd_to_hex();
	    break;
	case 9: // tz390.fp_lq_type quad word RPI 1108
		break;
	}
}
	private void cvt_fp_exp_to_base_2(){
	/*******************************************
	 * calc tz390.fp_exp and big_dec2 such that:      
	 * big_dec1 = big_dec2 * 2  ** tz390.fp_exp      
	 *************************************** 
	 * 
	 * tz390.fp_exp = log(big_dec1) / log(2)
	 * 	 *                                           
	 * Since the exponent range of LB exceeds  
	 * native double, the log of big_dec1 is
	 * calculated using equivalent:
	 *   log(X*10**N) = log(X) + N*log(10)
	 * The exponent must then be offset by the number
	 * of bits in the required binary mantissa in 
	 * order to retain significant bits when big_dec2
	 * is converted to big_int format.  The exponent
	 * is also reduced by 1 for assumed bit in binary 
	 * formats plus 1 additional to insure rounding for
	 * irrational values is done by shifting right.
	 * 
	 */ 
	fp_big_dec1 = fp_big_dec1.stripTrailingZeros(); // RPI 821	
	int    work_scale  =  - fp_big_dec1.scale();
	double work_man    =    fp_big_dec1.multiply(
		BigDecimal.TEN.pow(-work_scale,fp_context),fp_context).doubleValue();
	tz390.fp_exp   =  (int)((Math.log(work_man) 
			           + ((double)work_scale 
			                * fp_log10))
			          / fp_log2) 
	         - tz390.fp_man_bits[tz390.fp_type] 
			 - tz390.fp_one_bit_adj[tz390.fp_type]; 
	/*
	 * Now calc big_dec2 mantissa truncated integer
	 * tz390.fp_exp calculated above.  This calculation
	 * may produce an irrational number with the 
	 * precison specified due to base 10 to base 2
	 * exponent conversion.
     *
	 * big_dec2 = big_dec1 / 2 ** tz390.fp_exp/
	 * 
	 */
	try {
	    fp_big_dec2 = fp_big_dec1.multiply(BigDecimal.valueOf(2).pow(-tz390.fp_exp,fp_context),fp_context);
	} catch (Exception e){
		log_error(89,"floating point value out of range");
		dc_hex = "FFFF000000000000";
		return;
	}
	/*
	 * retrieve fp_big_dec2 mantissa bits as big_int and
	 * adjust tz390.fp_exp by mantissa bits
	 */
	fp_big_int1 = fp_big_dec2.toBigInteger();
    tz390.fp_exp = tz390.fp_exp + tz390.fp_man_bits[tz390.fp_type];
	}
	private void fp_cvt_bd_to_hex(){
	/*
	 * 1.  BFP - Adjust mantiss and base 2 exponent
	 *     to align for assumed 1 bit.
	 * 2.  HFP - Adjust mantissa and base 2
	 *     exponent to base 16 exponent.
	 * 3.  DFP - Set base 10 exponent based on
	 *     explicit decimal point and trailing
	 *     significant digits including zeros
	 *     else use preferred exponent of 0.  RPI 790
	 * 
	 */
	switch (tz390.fp_type){  // gen hex for fp type
	case 0: // tz390.fp_db_type s1,e11,m52 with assumed 1
		fp_long1 = fp_big_int1.longValue();
		fp_round_bit = 0;
		while (fp_long1 > fp_long_db_one_bits){
			fp_round_bit = (int)(fp_long1 & 1);
			fp_long1 = fp_long1 >>> 1;
			tz390.fp_exp++;
			if (fp_long1 <= fp_long_db_one_bits){
				fp_long1 = fp_long1 + fp_round_bit;	
			}
		}
		tz390.fp_exp = tz390.fp_exp + tz390.fp_exp_bias[tz390.fp_type];
		if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[tz390.fp_type]){
			dc_hex = get_long_hex( 
			         ((long)(tz390.fp_sign | tz390.fp_exp) 
			         		<< tz390.fp_man_bits[tz390.fp_type])
		              | (fp_long1 & fp_long_db_man_bits));
		} else {
			log_error(89,"floating point value out of range");
			dc_hex = "FFFF000000000000";
		}
        break;
	case 1: // tz390.fp_dd_type s1,cf5,bxcf8,ccf50 // RPI 50
		set_dfp_preferred_exp();
		if (!tz390.fp_get_dfp_bin(tz390.fp_dd_type, fp_big_dec1)){  
        	log_error(179,"DD dfp constant out of range");
	    	dc_hex = "0000000000000000";
        } else {
        	dc_hex = tz390.get_long_hex(tz390.fp_work_reg.getLong(0),16);
        }
		break;
	case 2: // tz390.fp_dh_type s1,e7,m56 with hex exp
		fp_long1 = fp_big_int1.longValue();
		fp_round_bit = 0;
		while ((tz390.fp_exp & 0x3) != 0
	           || fp_long1 > fp_long_dh_man_bits
			   ){
			fp_round_bit = (int)(fp_long1 & 1);
			fp_long1 = fp_long1 >>> 1;
			tz390.fp_exp++;
			if (fp_round_bit == 1
				&& (tz390.fp_exp & 0x3) == 0  // RPI 821
				&& fp_long1 <= fp_long_dh_man_bits){
				fp_long1++;	
			}
		}
		tz390.fp_exp = (tz390.fp_exp >> 2) + tz390.fp_exp_bias[tz390.fp_type] + dc_scale; // RPI 368
		if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[tz390.fp_type]){
			dc_hex = get_long_hex( 
			         ((long)(tz390.fp_sign | tz390.fp_exp) 
			         		<< tz390.fp_man_bits[tz390.fp_type])
		              | fp_long1);
		} else {
			log_error(89,"floating point value out of range");
			dc_hex = "FFFF000000000000";
		}
		break;
	case 3: // tz390.fp_eb_type s1,e7,m24 with assumed 1
		fp_int1 = fp_big_int1.intValue();
		fp_round_bit = 0;
		while (fp_int1 >= fp_int_eb_one_bits){
			fp_round_bit = fp_int1 & 1;
			fp_int1 = fp_int1 >>> 1;
			tz390.fp_exp++;
			if (fp_int1 <= fp_int_eb_one_bits){
				fp_int1 = fp_int1 + fp_round_bit;	
			}
		}
		tz390.fp_exp = tz390.fp_exp + tz390.fp_exp_bias[tz390.fp_type];
		if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[tz390.fp_type]){
			dc_hex = tz390.get_hex( 
			          ((tz390.fp_sign | tz390.fp_exp) 
			          		<< tz390.fp_man_bits[tz390.fp_type])
		              | (fp_int1 & fp_int_eb_man_bits),8);
		} else {
			log_error(89,"floating point value out of range");
			dc_hex = "FF000000";
		}
		break;
	case 4: // tz390.fp_ed_type s1,cf5,bxcf6,ccf20 // RPI 407
		set_dfp_preferred_exp();
		if (!tz390.fp_get_dfp_bin(tz390.fp_ed_type, fp_big_dec1)){ // RPI 790
        	log_error(180,"ED dfp constant out of range");
	    	dc_hex = "00000000";
        } else {
        	dc_hex = tz390.get_hex(tz390.fp_work_reg.getInt(0),8);
        }
		break;
	case 5: // tz390.fp_eh_type s1,e7,m24 with hex exp
		fp_int1 = fp_big_int1.intValue();
		fp_round_bit = 0;
		while ((tz390.fp_exp & 0x3) != 0  // RPI 821
				|| fp_int1 > fp_int_eh_man_bits 
				){
			fp_round_bit = fp_int1 & 1;
			fp_int1 = fp_int1 >>> 1;
			tz390.fp_exp++;
			if (fp_round_bit == 1   // RPI 821
				&& (tz390.fp_exp & 0x3) != 0  
				&&	fp_int1 <= fp_int_eh_man_bits){
				fp_int1++;	// RPI 821
			}
		}
		tz390.fp_exp = (tz390.fp_exp >> 2) + tz390.fp_exp_bias[tz390.fp_type] + dc_scale;  // RPI 368
		if (tz390.fp_exp >= 0 && tz390.fp_exp <= 0x7f){
			dc_hex = tz390.get_hex( 
			          ((tz390.fp_sign | tz390.fp_exp) << 24)
		              | fp_int1,8);
		} else {
			log_error(89,"floating point value out of range");
			dc_hex = "00000000";
		}
	    break;
	case 6: // tz390.fp_lb_type s1,e15,m112 with assumed 1
		fp_round_bit = 0;
		while (fp_big_int1.compareTo(fp_big_int_one_bits) > 0){
			if (fp_big_int1.testBit(0)){
				fp_round_bit = 1;
			} else {
				fp_round_bit = 0;
			}
			fp_big_int1 = fp_big_int1.shiftRight(1);
			tz390.fp_exp++;
			if (fp_round_bit == 1 
				&& fp_big_int1.compareTo(fp_big_int_one_bits) <= 0){
				fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
			}
		}
		tz390.fp_exp = tz390.fp_exp + tz390.fp_exp_bias[tz390.fp_type];
		if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[tz390.fp_type]){
			fp_big_byte = fp_big_int1.toByteArray();
			int index1 = fp_big_byte.length - 1;
			int index2 = 15;
			while (index2 > 0){
				if (index1 >= 0){
					fp_data_byte[index2] = fp_big_byte[index1];
					index1--;
				} else {
					fp_data_byte[index2] = 0;
				}
				index2--;
			}
			fp_data_buff.putShort(0,(short)(tz390.fp_sign | tz390.fp_exp));
            dc_hex = bytes_to_hex(fp_data_byte,0,16,0);
		} else {
			log_error(89,"floating point value out of range");
			dc_hex = "FF00000000000000FF00000000000000";
		}
	    break;
	case 7: // tz390.fp_ld_type s1,cf5,bxcf12,ccf110 // RPI 407
		set_dfp_preferred_exp();
		if (!tz390.fp_get_dfp_bin(tz390.fp_ld_type, fp_big_dec1)){ // RPI 790
        	log_error(181,"LD dfp constant out of range");
	    	dc_hex = "00000000000000000000000000000000";
        } else {
        	dc_hex = tz390.get_long_hex(tz390.fp_work_reg.getLong(0),16)
	               + tz390.get_long_hex(tz390.fp_work_reg.getLong(8),16)	;
		}
		break;
	case 8: // tz390.fp_lh_type s1,e7,m112 with split hex
        fp_round_bit = 0;
		while ((tz390.fp_exp & 0x3) != 0  // RPI 821
				|| fp_big_int1.compareTo(fp_big_int_lx_man_bits) > 0
				){
			if (fp_big_int1.testBit(0)){
				fp_round_bit = 1;
			} else {
				fp_round_bit = 0;
			}
			fp_big_int1 = fp_big_int1.shiftRight(1);
			tz390.fp_exp++;
			if (fp_round_bit == 1 
				&& (tz390.fp_exp & 0x3) == 0  // RPI 821
				&& fp_big_int1.compareTo(fp_big_int_lx_man_bits) <= 0
				){
				fp_big_int1 = fp_big_int1.add(BigInteger.ONE);
			}
		}
		tz390.fp_exp = (tz390.fp_exp >> 2) + tz390.fp_exp_bias[tz390.fp_type] + dc_scale; // RPI 368
		if (tz390.fp_exp >= 0 && tz390.fp_exp <= tz390.fp_exp_max[tz390.fp_type]){
			fp_big_byte = fp_big_int1.toByteArray();
			int index1 = fp_big_byte.length - 1;
			int index2 = 15;
			while (index2 > 0){
				if (index2 == 8){
					index2--;  // skip dup exp byte
			    }
				if (index1 >= 0){
					fp_data_byte[index2] = fp_big_byte[index1];
					index1--;
				} else {
					fp_data_byte[index2] = 0;
				}
				index2--;
			}
			fp_data_buff.put(0,(byte)(tz390.fp_sign | tz390.fp_exp));
			if (fp_data_buff.getLong(0) == 0){
				fp_data_buff.put(8,(byte)0x00); // RPI 384
			} else {
				fp_data_buff.put(8,(byte)(tz390.fp_sign | ((tz390.fp_exp - 14) & 0x7f))); // RPI 384
			}
            dc_hex = bytes_to_hex(fp_data_byte,0,16,0);
		} else {
			log_error(89,"floating point value out of range");
			dc_hex = "FF00000000000000FF00000000000000";
		}
	    break;
	case 9: // tz390.fp_lq_type quad word RPI 1108
		break;
	}	
}
	private void set_dfp_preferred_exp(){
		/*
		 * set DFP preferred base 10 exponent
		 * for fp_big_dec1 value if explicit
		 * decimal found in fp_text.
		 */
		int tot   = 0;
		int index = fp_text.indexOf('.');
		if (index != -1){
			index++;
			while (index < fp_text.length() && fp_text.charAt(index) >= '0'
				   && fp_text.charAt(index) <= '9'){
				tot++;
				index++;
			}
			if (tot > tz390.fp_digits_max[tz390.fp_type]){
				tot = tz390.fp_digits_max[tz390.fp_type];
			}
			fp_big_dec1.setScale(tot);			
		}
		if (!dc_scale_explicit && dc_first_field){
			dc_first_scale = tot; // RPI 790
		}
		dc_scale = fp_big_dec1.scale();
	}
    public boolean add_missing_copy(String name){
	/*
	 * add missing copy file for ERRSUM
	 */
	int index = 0;
	while (index < tot_missing_copy){
		if (name.equals(missing_copy[index])){
			return true;
		}
		index++;
	}
	if (index < max_missing){
		tot_missing_copy++;
		missing_copy[index] = name;
		return true;
	} else {
		return false;
	}	
}
private boolean add_missing_macro(String name){
	/*
	 * add nussubg ciot file for ERRSUM
	 */
	int index = 0;
	while (index < tot_missing_macro){
		if (name.equals(missing_macro[index])){
			return true;
		}
		index++;
	}
	if (index < max_missing){
		tot_missing_macro++;
		missing_macro[index] = name;
		return true;
	} else {
		return false;
	}
}
public void report_critical_errors(){
	/*
	 * report critical errors on ERR file
	 * and console for ERRSUM option
	 */
	if (tot_missing_macro+tot_missing_copy+tot_missing_sym == 0){
		return; // RPI 959
	}
	tz390.opt_errsum = false; // allow printing on PRN again
	tz390.opt_list   = true;
	put_errsum("ERRSUM Critical Error Summary Option");
    put_errsum("ERRSUM Fix and repeat until all nested errors resolved");
	int index = 0;
	while (index < tot_missing_copy){
		put_errsum("ERRSUM missing copy  =" + missing_copy[index]); // RPI 1051
		index++;
	}
	index = 0;
	while (index < tot_missing_macro){
		put_errsum("ERRSUM missing macro =" + missing_macro[index]);
		index++;
	}
	if (tot_missing_macro + tot_missing_copy == 0){
		index = 1;
		while (index <= tot_sym){
			if (sym_type[index] == sym_und){
				put_errsum("ERRSUM undefined symbol = " + sym_name[index]);
			}
			index++;
		}
	}
	put_errsum("ERRSUM total missing   copy   files =" + tot_missing_copy);
	put_errsum("ERRSUM total missing   macro  files =" + tot_missing_macro);
	put_errsum("ERRSUM total undefined symbols      =" + tot_missing_sym);
	if (mz390_call){
		put_errsum("ERRSUM total mz390 errors    = " + mz390_errors); // RPI 659 RPI 945
	}
	put_errsum("ERRSUM total az390 errors    = " + az390_errors); // RPI 659 RPI 945
}
private void put_errsum(String msg){
	/*
	 * put ERRSUM msgs on ERR file and console
	 */
	msg = "AZ390E " + msg;
	System.out.println(msg);
	if (prn_file != null){ 
		 // if ERRSUM turned on after open put msgs to PRN
		 put_prn_line(msg);
	}
	tz390.put_systerm(msg);
}
public int get_int_pfx(byte type,byte type_sfx,int len,int scale){
	/*
	 * return I' integer pfx value for symbol
	 * based on length and scale per ref.
	 */
	switch (type){  // RPI 790
	case 'F':
	case 'H':
		return 8 * len - scale - 1;
	case 'D':
	case 'E':
	case 'L':
		if (type_sfx == 'D'){ // DFP
			if (len == 4){
				return 7 - scale;
			} else if (len == 8){
				return 16 - scale;
			} else {
				return 34 - scale;
			}			
		} else { // HFP and BFP
			if (len <= 8){
				return 2 * (len - 1) - scale;
			} else {
				return 2 * (len - 1) - scale - 2;
			}
		}
	case 'P':
		return 2 * len - scale - 1;
	case 'Z':
		return len - scale;
	default:
		return 0;
	}
}
/*
 *  end of az390 code 
 */
}
