import java.nio.ByteBuffer;
//!! Add boilerplate copyright disclaimer etc.

/**
*  Shadow ACB
*
*  This class shadows a real ACB that resides in Z memory and provides all 
*  services that operate on an ACB.
*
<p>
*  It is keyed on ACB address.
*
*
*  @author   Hugh Sweeney
*  @version  2.0
*  @see      "zVSAM Design and Logic"
*
*/
public class zACB {
  // !! To suppress all developer code, set this variable to false:-
  public static final boolean DEVEL = true ;   // !! RPI 1614 

  private final ByteBuffer zMemByteBuf;    // The wrapper around ...
  private final byte[]     zMemByteArr;    // ... this array.
  private final int        ACB_addr; 
  private final zFCB       fcb;            // I must instantiate this? !!

  int    last_op_completion_code = /* Initial state */ -1;
  String last_op_completion_msg;




  /**
  *  Constructor, stores the pointer to user`s ACB, and validates it.
  */
  zACB(ByteBuffer parm_memBuff, int parm_addr) {
    devLog("zACB constructor has been entered.");
    zMemByteBuf = parm_memBuff;         // = pz390.mem
    zMemByteArr = zMemByteBuf.array();  // = pz390.mem_byte
    ACB_addr = parm_addr;
    fcb = new zFCB(this);

    last_op_completion_code = 0;
    if (ID()             != IDVAL)     fail_op("!! ACBID invalid");
    if (STYPE()          != SVSAM)     fail_op("!! ACBSTYPE invalid");
    if (VER()            != V2)        fail_op("!! ACBVER invalid");
    if (last_op_completion_code > 0) return;

    //!! lots more tbd
    }



  /**
  *  Open this ACB.                                                                
  *  @return      return code (for R15)
  */
  int open() {
    devLog("zACB.open() has been entered.");
    //!! body goes here
    if ((OFLGS() & OPEN) == OPEN)      fail_op("!! ACB already open");
    fail_op("!!zACB.open() not implemented yet");
    return 16;    //!! Keeps the compiler happy.
    }




  /**
  *  Set completion status of failed operation.
  *
  *  @param    parm_msg Message detailing te failure.
  *  @return            false
  */
  boolean fail_op(String parm_msg) {
    last_op_completion_msg = parm_msg; 
    last_op_completion_code = 16;
    return false;
    }




  /**
  *  Getters for ACB fields.
  *
  *  These are the only methods that should be available to other classes to access ACB fields.
  */
  byte   ID()    { return zMemByteArr[ACB_addr + V2OFF + ID];      }
  byte   STYPE() { return zMemByteArr[ACB_addr + V2OFF + STYPE];   }
  byte   VER()   { return zMemByteArr[ACB_addr + V2OFF + VER];     }
  short  LEN()   { return zMemByteBuf.getShort(ACB_addr + V2OFF + LEN); }
  int    AMBL()  { return zMemByteBuf.getInt(ACB_addr + V2OFF + AMBL);  }
  int    IFR()   { return zMemByteBuf.getInt(ACB_addr + V2OFF + IFR);   }
  byte   MACRF() { return zMemByteArr[ACB_addr + V2OFF + MACRF];   }
  byte   MACR2() { return zMemByteArr[ACB_addr + V2OFF + MACR2];   }
  byte   MACR3() { return zMemByteArr[ACB_addr + V2OFF + MACR3];   }
  byte   OFLGS() { return zMemByteArr[ACB_addr + V2OFF + OFLGS];   }  
  String DDNAM() { try {
            return new String(zMemByteArr, ACB_addr + V2OFF + DDNAM, 8, "cp1047"); 
          } catch (java.io.UnsupportedEncodingException e) { 
            fail_op("!! can't convert DDNAM from cp1047: "+ e);  //!! Serious, but not abort?
            return ""; 
          }
  }




  /**
  *  Developer's logging function 
  *
  *  Conditionally issues a developer message.
  *
  *  @param msg Message to be displayed.
  *
  */
  private void devLog(String msg) {
    if (DEVEL) System.out.println("!! vz390: " + msg);
  }



  /**
  *  Offsets of various fields in user`s zACB. This is information about ACB internals.
  *  These offsets should be used only in getter functions, above.
  *  No information about ACB internal layout should be defined outside this class.
  */
  private static final short V2OFF = 0x00;   // !! additional offset for V2 ACB (check it!)
  private static final short ID    = 0x04;
  private static final short AMBL  = 0x04;   // !! exist?
  private static final short STYPE = 0x05;
  private static final short LEN   = 0x06;   
  private static final short DDNAM = 0x08;
  private static final short IFR   = 0x08;   // !! exist?
  private static final short MACRF = 0x10;
  private static final short MACR1 = 0x10;
  private static final short MACR2 = 0x11;
  private static final short MACR3 = 0x12;
  private static final short MACR4 = 0x13;
  private static final short OFLGS = 0x28;
  private static final short VER   = 0x2D;    

  /**
  *  Values of equated symbols in ACB DSECT.
  *
  */
  private static final byte IDVAL   = (byte) 0xA0; // ACBID: VSAM
  private static final byte SVSAM   = 0x10;        // ACBSTYPE: VSAM
  private static final byte V2      = 0x02;        // ACBVER: Version: 2
  private static final byte OPEN    = 0x02;        // ACBOFLGS: is open.



  // The above data was extracted from:
  // 16:11:03 ACBD2     MZ390 START USING z390 V1.6.00b12 ON J2SE 1.8.0_111 06/22/20
  // AZ390I Copyright 2011 Automated Software Tools Corporation
  // AZ390I z390 is licensed under GNU General Public License
  // AZ390I program = /home/hsweeney/handover/2/test/ACBD2
  // AZ390I options = 
  //   sysmac(/media/CRUZER3/Dev/zVSAM/MACVSAM2_MM+.) 
  //   sysmac(+/media/CRUZER3/Dev/z/mac) syscpy(/media/CRUZER3/Dev/z/mac+.)
  // External Symbol Definitions
  // Assembler Listing
  // 000000                                        (1/1)1          ACBD  ,
  // 000000                                       (2/32)2+IHAACB   DSECT 
  // 000000                  00000000             (2/33)3+IFGACB   EQU   IHAACB
  // 000000                                       (2/34)4+ACBEYE   DS    CL4                EYECATCHER
  // 000004                  A9C1C3C2             (2/35)5+ACBZACB  EQU   C'zACB'               EYECATCHER VALUE
  // 000004                                       (2/36)6+ACBID    DS    X                  IDENTIFIER
  // 000005                  000000A0             (2/37)7+ACBIDVAL EQU   X'A0'                 VSAM
  // 000005                                       (2/38)8+ACBSTYP  DS    X                  SUBTYPE
  // 000006                  00000005             (2/39)9+ACBSTYPE EQU   ACBSTYP
  // 000006                  00000005            (2/40)10+ACBTYPE  EQU   ACBSTYP
  // 000006                  00000010            (2/41)11+ACBSVSAM EQU   X'10'                 VSAM SUBTYPE
  // 000006                                      (2/42)12+ACBLENG  DS    XL2                ACB LENGTH
  // 000008                  00000006            (2/43)13+ACBLENG2 EQU   ACBLENG
  // 000008                  00000006            (2/44)14+ACBLEN   EQU   ACBLENG
  // 000008                  00000006            (2/45)15+ACBLEN2  EQU   ACBLENG
  // 000008                                      (2/46)16+ACBDDNM  DS    CL8                DDNAME > SET > PATH\CAT.CLUSTER
  // 000010                  00000008            (2/47)17+ACBDDNAM EQU   ACBDDNM
  // 000010                                      (2/48)18+ACBMACRF DS    0XL2               MACRO ACCESS TYPE FLAGS 1 & 2
  // 000010                                      (2/49)19+ACBMACR1 DS    B                  MACRF FLAGS BYTE 1
  // 000011                  00000080            (2/50)20+ACBKEY   EQU   X'80'                 INDEXED ACCESS BY LOGICAL KEY
  // 000011                  00000080            (2/51)21+ACBMACR1_KEY EQU ACBKEY
  // 000011                  00000040            (2/52)22+ACBADR   EQU   X'40'                 NON-INDEXED ACCESS BY ADDRESS
  // 000011                  00000040            (2/53)23+ACBADD   EQU   ACBADR
  // 000011                  00000040            (2/54)24+ACBMACR1_ADR EQU ACBADR
  // 000011                                      (2/55)25+* ACBMACR1_CNV EQU X'20'             CI ACCESS (NOT SUPPORTED)
  // 000011                  00000010            (2/56)26+ACBSEQ   EQU   X'10'                 SEQUENTIAL ACCESS
  // 000011                  00000010            (2/57)27+ACBMACR1_SEQ EQU ACBSEQ
  // 000011                  00000008            (2/58)28+ACBDIR   EQU   X'08'                 DIRECT ACCESS
  // 000011                  00000008            (2/59)29+ACBMACR1_DIR EQU ACBDIR
  // 000011                  00000004            (2/60)30+ACBIN    EQU   X'04'                 GET ALLOWED
  // 000011                  00000004            (2/61)31+ACBGET   EQU   ACBIN
  // 000011                  00000004            (2/62)32+ACBMACR1_IN  EQU ACBIN
  // 000011                  00000002            (2/63)33+ACBOUT   EQU   X'02'                 GET, PUT, ERASE ALLOWED
  // 000011                  00000002            (2/64)34+ACBPUT   EQU   ACBOUT
  // 000011                  00000002            (2/65)35+ACBMACR1_OUT EQU ACBOUT
  // 000011                                      (2/66)36+* ACBMACR1_UBF EQU X'01'             USER BUFFER MGT (NOT SUPPORTED)
  // 000011                                      (2/67)37+*
  // 000011                                      (2/68)38+ACBMACR2 DS    B                  MACRF FLAGS BYTE 2
  // 000012                  00000010            (2/69)39+ACBSKP   EQU   X'10'                 SKIP SEQUENTIAL ACCESS
  // 000012                  00000010            (2/70)40+ACBMACR2_SKP EQU ACBSKP
  // 000012                                      (2/71)41+* ACBMACR2_NLOGON EQU X'40'          NO LOGON REQUIRED (NOT SUPPORTED)
  // 000012                                      (2/72)42+* ACBRST   EQU   X'04'               DATASET REUSABLE (NOT SUPPORTED)
  // 000012                                      (2/73)43+* ACBMACR2_RST EQU ACBRST
  // 000012                                      (2/74)44+* ACBMACR2_DSN EQU X'10'             SUBTASK SHARING (NOT SUPPORTED)
  // 000012                  00000001            (2/75)45+ACBAIX   EQU   X'01'                 ACCESS THROUGH AIX
  // 000012                  00000001            (2/76)46+ACBAIXP  EQU   ACBAIX
  // 000012                  00000001            (2/77)47+ACBMACR2_AIX EQU ACBAIX
  // 000012                                      (2/78)48+*
  // 000012                                      (2/79)49+ACBMACR3 DS    B                  MACRF FLAGS BYTE 3
  // 000013                                      (2/80)50+* ACBNLW   EQU   X'80'               NO LSR EXC. CTRL WT (NOT SUPP)
  // 000013                  00000040            (2/81)51+ACBLSR   EQU   X'40'                 LOCAL SHARED RESOURCES
  // 000013                  00000040            (2/82)52+ACBMACR3_LSR EQU ACBLSR
  // 000013                  00000020            (2/83)53+ACBGSR   EQU   X'20'                 GLOBAL SHARED RESOURCES
  // 000013                  00000020            (2/84)54+ACBMACR3_GSR EQU ACBGSR
  // 000013                                      (2/85)55+* ACBMACR2_ICI EQU X'10'             IMPROVED CI PROC. (NOT ALLOWED)
  // 000013                  00000008            (2/86)56+ACBDFR   EQU   X'08'                 DEFERRED WRITES ALLOWED
  // 000013                  00000008            (2/87)57+ACBMACR3_DFR EQU ACBDFR
  // 000013                  00000004            (2/88)58+ACBSIS   EQU   X'04'                 SEQUENTIAL INSERT STRATEGY
  // 000013                  00000004            (2/89)59+ACBMACR3_SIS EQU ACBSIS
  // 000013                                      (2/90)60+* ACBMACR3_CFX EQU X'20'             FIX CBS AND BUFFS (NOT ALLOWED)
  // 000013                  00000001            (2/91)61+ACBMODE  EQU   X'01'                 BUFFERS ALLOWED ABOVE THE LINE
  // 000013                                      (2/92)62+*
  // 000013                                      (2/93)63+ACBMACR4 DS    B                  MACRF FLAGS BYTE 4
  // 000014                                      (2/94)64+* ACBRLS EQU   X'80'                 RES. LEVEL SHARING (NOT ALLOWED)
  // 000014                                      (2/95)65+*
  // 000014                                      (2/96)66+ACBBUFND DS    XL2                NO. OF DATA BUFFERS
  // 000016                                      (2/97)67+ACBBUFNI DS    XL2                NO. OF INDEX BUFFERS
  // 000018                                      (2/98)68+ACBBUFSP DS    XL4                MAX BUFFER SPACE IN BYTES
  // 00001C                                      (2/99)69+ACBLRECL DS    XL2                RECORD LENGTH FROM CATALOG
  // 00001E                                     (2/100)70+ACBPASSW DS    AL4                PASSWORD POINTER
  // 000022                                     (2/101)71+ACBEXLST DS    AL4                EXLST POINTER
  // 000026                  00000022           (2/102)72+ACBUEL   EQU   ACBEXLST
  // 000026                                     (2/103)73+*
  // 000026                                     (2/104)74+ACBINFL  DS    0BL2               INPUT FLAGS 1 & 2
  // 000026                  00000026           (2/105)75+ACBINFLG EQU   ACBINFL
  // 000026                                     (2/106)76+ACBINFL1 DS    B                  INPUT FLAG 1
  // 000027                  00000010           (2/107)77+ACBCAT   EQU   X'10'                 ACB FOR VSAM CATALOG (NOT SUPP)
  // 000027                                     (2/108)78+*
  // 000027                                     (2/109)79+ACBINFL2 DS    B                  INPUT FLAG 2
  // 000028                  00000080           (2/110)80+ACBSWARN EQU   X'80'                 SUPPRESS OPEN WARNING
  // 000028                  00000003           (2/111)81+ACBSHROP EQU   X'03'                 SHAREOPTIONS
  // 000028                  00000002           (2/112)82+ACBSHR02 EQU   X'02'                 CROSS REGION SHARE 2
  // 000028                  00000001           (2/113)83+ACBSHR01 EQU   X'01'                 CROSS REGION SHARE 1
  // 000028                                     (2/114)84+*
  // 000028                                     (2/115)85+ACBOFLGS DS    B                  OPEN FLAGS
  // 000029                  00000080           (2/116)86+ACBR31B  EQU   X'80'                 31-BIT ADDRESSING FOR BUFFERS
  // 000029                  00000040           (2/117)87+ACBR31C  EQU   X'40'                 31-BIT ADDRESSING FOR CONTROL BLKS
  // 000029                  00000020           (2/118)88+ACBEOV   EQU   X'20'                 EOV CONCATENATION
  // 000029                  00000010           (2/119)89+ACBOPEN  EQU   X'10'                 ACB CURRENTLY OPEN
  // 000029                  00000008           (2/120)90+ACBDSERR EQU   X'08'                 ERROR-ACB MUST BE CLOSED
  // 000029                  00000004           (2/121)91+ACBRECOV EQU   X'04'                 OPEN FOR RECOVERY
  // 000029                  00000002           (2/122)92+ACBEXFG  EQU   X'02'                 OFF WHEN USER EXIT IN PROGRESS
  // 000029                  00000002           (2/123)93+ACBLOCK  EQU   ACBEXFG
  // 000029                  00000001           (2/124)94+ACBIOSFG EQU   X'01'                 OPEN/CLOSE IN PROGRESS
  // 000029                  00000001           (2/125)95+ACBBUSY  EQU   ACBIOSFG
  // 000029                                     (2/126)96+*
  // 000029                                     (2/127)97+ACBERFLG DS    B                  ERROR FLAGS
  // 00002A                                     (2/128)98+*
  // 00002A                                     (2/129)99+ACBBSTNO DS    X                  NO. OF AIX CONCURRENT STRINGS
  // 00002B                                    (2/130)100+ACBSTRNO DS    X                  NO. OF CONCURRENT REQUEST STRINGS
  // 00002C                                    (2/131)101+ACBSHRP  DS    X                  LSR SHARE POOL NUMBER
  // 00002D                                    (2/133)102+ACBVER   DS    X                  zACB LAYOUT VERSION
  // 00002E                  00000002          (2/134)103+ACBV2    EQU   X'02'              zACB V2
  // 00002E                                    (2/135)104+ACBPFX   DS    AL4                ADDRESS OF DATA PREFIX BLOCK
  // 000032                                    (2/136)105+ACBXPFX  DS    AL4                ADDRESS OF INDEX PREFIX BLOCK
  // 000036                                    (2/137)106+ACBBUFD  DS    AL4                ADDRESS OF DATA BUFFERS
  // 00003A                                    (2/138)107+ACBBUFI  DS    AL4                ADDRESS OF INDEX BUFFERS
  // 00003E                                    (2/139)108+         DS    XL2                ALIGN
  // 000040                                    (2/140)109+         DS    0D
  // 000040                  00000040          (2/141)110+ACBEND   EQU   *
  // 000040                                      (1/2)112          END   ,
}
