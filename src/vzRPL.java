/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a zVSAM Request Parameter List                              |
 |                                                                            |
 |  The Request Parameter List (or RPL for short) represents in the memory    |
 |       of the emualator an operation against an open zVSAM cluster          |
 |                                                                            |
 |  Supported operations are Get, Put, Erase, Check, Point, EndReq, Verify    |
 |                           ShowCB, and TestCB                               |
 |                                                                            |
\* -------------------------------------------------------------------------- */


/* ***************************************************
 * 
 * z390 portable mainframe assembler and emulator.
 * 
 * Copyright 2011 Automated Software Tools Corporation
 * 
 * z390 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * z390 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with z390; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * zACB is part of the zVSAM v2 component of z390.
 * It is the java implementation of the ACB with its associated operations
 * and thus represents a single cluster.
 *
 ****************************************************
 * Maintenance
 ****************************************************
 * 2019/2020  Initial construction by Hugh Sweeney
 * 2021       AFK Extensive changes
 * 
 ****************************************************/

/**
 * A Java object in the emulator that shadows an RPL in z-memory. 
 * 
 * This object is created on issuance of a request against an RPL.
 * 
 * @author  Hugh Sweeney
 * 
 */
public class vzRPL extends zControlBlock
   {private static final String RPLZRPL  = "zRPL";
    private static final ebcdicStryng ebcdicEyeCatcher = new ebcdicStryng(RPLZRPL);

    private zException           my_zException; // Current exception status

    /**
    *   Offsets of fields in RPL DSECT
    */
    private static final short RPLEYE      = 0x0000;
    private static final short RPLDACB     = 0x0004;
    private static final short RPLAREA     = 0x0008;
    private static final short RPLAREAL    = 0x000c;
    private static final short RPLARG      = 0x0010;
    private static final short RPLECB      = 0x0014;
    private static final short RPLMSGAR    = 0x0018;
    private static final short RPLNXTRP    = 0x001C;
    private static final short RPLRECLN    = 0x0020;
    private static final short RPLMSGLN    = 0x0024;
    private static final short RPLKEYLN    = 0x0026;
    private static final short RPLOPTCD    = 0x0027;
    private static final short RPLOPT1     = 0x0027;
    private static final short RPLOPT2     = 0x0028;
    private static final short RPLFEEDB    = 0x0029;
    private static final short RPLEND      = 0x0030;

    /**
    *   Values of equated symbols in RPL DSECT   
    */
    private static final byte  RPLOPT1_KEY = (byte) 0x80;  // 1=KEY 0=ADR
    private static final byte  RPLOPT1_SEQ = (byte) 0x40;  // 1=SEQ 0=DIR
    private static final byte  RPLOPT1_SKP = (byte) 0x20;  // 1=SKP 0=SEQ/DIR
    private static final byte  RPLOPT1_ARD = (byte) 0x10;  // 1=ARD 0=LRD
    private static final byte  RPLOPT1_FWD = (byte) 0x08;  // 1=FWD 0=BWD
    private static final byte  RPLOPT1_SYN = (byte) 0x04;  // 1=SYN 0=ASY
    private static final byte  RPLOPT1_NUP = (byte) 0x02;  // 1=NUP 0=UPD
    private static final byte  RPLOPT1_NSP = (byte) 0x01;  // 1=NSP 0=NUP/UPD
    private static final byte  RPLOPT2_KEQ = (byte) 0x80;  // 1=KEQ 0=KGE
    private static final byte  RPLOPT2_FKS = (byte) 0x40;  // 1=FKS 0=GEN
    private static final byte  RPLOPT2_MVE = (byte) 0x20;  // 1=MVE 0=LOC
    private static final byte  RPLOPT2_RBA = (byte) 0x10;  // 1=RBA 0=XRBA
    
    private static final int    RPLOPGET   = 1;            // Get
    private static final int    RPLOPPUT   = 2;            // Put
    private static final int    RPLOPERA   = 3;            // Erase
    private static final int    RPLOPPNT   = 4;            // Point




     /**
     *
     * Constructor, stores the pointer to user's RPL, and validates it.
     *
     * Parameters: byte[]     holding the z390 memory array
     *             int        holding the offset of the RPL in the memory array
     *             int        specifying the size of the underlying byte array
     *
     */
    public vzRPL(byte[] parm_memBuff, int parm_addr, int parm_size)
       {super(parm_memBuff, parm_addr, RPLEND);
        my_zException = new zException();
        
        // Ensure the input is a valid RPL
        if (parm_addr < 0x2000)
           throw new zException("RPL must never be low storage", 8, 8);
        if (parm_addr + RPLEND - 1 > parm_size)
           throw new zException("RPL area is too small", 8, 8);
        
        // Validate contents:
        if (! ebcdicEyeCatcher.equals(parm_memBuff[parm_addr + RPLEYE]) )
            throw new zException("vzRPL: Not a zVSAM RPL eyecatcher", 8, 8);
        }




// The above data was extracted from:
// 13:52:37 RPLD      MZ390 START USING z390 V1.6.00b12 ON J2SE 1.8.0_111 10/24/20
// AZ390I Copyright 2011 Automated Software Tools Corporation
// AZ390I z390 is licensed under GNU General Public License
// AZ390I program = /media/CRUZER3/Dev/zVSAM/RPLD
// AZ390I options = sysmac(MACVSAM2_MM)
// External Symbol Definitions
// Assembler Listing
// 000000                                        (1/1)1          RPLD  ,
// 000000                                       (2/37)2+IHARPL   DSECT 
// 000000                  00000000             (2/38)3+IFGRPL   EQU   IHARPL
// 000000                                       (2/39)4+RPLEYE   DS    CL4                EYECATCHER
// 000004                  A9D9D7D3             (2/40)5+RPLZRPL  EQU   C'zRPL'               EYECATCHER VALUE
// 000004                                       (2/41)6+RPLDACB  DS    A                  ACB ADDRESS
// 000008                                       (2/42)7+RPLAREA  DS    A                  AREA ADDRESS
// 00000C                                       (2/43)8+RPLAREAL DS    XL4                AREA LENGTH
// 000010                                       (2/44)9+RPLARG   DS    A                  ARG (KEY, RRDS REC NO., RBA/XRBA)
// 000014                                      (2/45)10+RPLECB   DS    A                  ECB ADDRESS (POSTED IF PRESENT)
// 000018                                      (2/46)11+RPLMSGAR DS    A                  MESSAGE AREA
// 00001C                                      (2/47)12+RPLNXTRP DS    A                  POINTER TO RPL CHAIN
// 000020                                      (2/48)13+RPLRECLN DS    XL4                RECORD LENGTH
// 000024                                      (2/49)14+RPLMSGLN DS    XL2                MESSAGE LENGTH
// 000026                                      (2/50)15+RPLKEYLN DS    X                  KEY LENGTH
// 000027                                      (2/51)16+RPLOPTCD DS    0XL2               OPTION BYTES 1 AND 2
// 000027                                      (2/52)17+RPLOPT1  DS    X                  OPTION BYTE 1
// 000028                  00000080            (2/53)18+RPLOPT1_KEY EQU X'80'                1=KEY 0=ADR
// 000028                  00000040            (2/54)19+RPLOPT1_SEQ EQU X'40'                1=SEQ 0=DIR
// 000028                  00000020            (2/55)20+RPLOPT1_SKP EQU X'20'                1=SKP 0=SEQ/DIR
// 000028                  00000010            (2/56)21+RPLOPT1_ARD EQU X'10'                1=ARD 0=LRD
// 000028                  00000008            (2/57)22+RPLOPT1_FWD EQU X'08'                1=FWD 0=BWD
// 000028                  00000004            (2/58)23+RPLOPT1_SYN EQU X'04'                1=SYN 0=ASY
// 000028                  00000002            (2/59)24+RPLOPT1_NUP EQU X'02'                1=NUP 0=UPD
// 000028                  00000001            (2/60)25+RPLOPT2_NSP EQU X'01'                1=NSP 0=NUP/UPD
// 000028                                      (2/61)26+*
// 000028                                      (2/62)27+RPLOPT2  DS    X                  OPTION BYTE 2
// 000029                  00000080            (2/63)28+RPLOPT2_KEQ EQU X'80'                1=KEQ 0=KGE
// 000029                  00000040            (2/64)29+RPLOPT2_FKS EQU X'40'                1=FKS 0=GEN
// 000029                  00000020            (2/65)30+RPLOPT2_MVE EQU X'20'                1=MVE 0=LOC
// 000029                  00000010            (2/66)31+RPLOPT2_RBA EQU X'10'                1=RBA 0=XRBA
// 000029                                      (2/67)32+RPLFEEDB DS    XL4                RPL FEEDBACK CODES
// 00002D                                      (2/68)33+         DS    XL3                ALIGN
// 000030                                      (2/69)34+         DS    0D
// 000030                  00000030            (2/70)35+RPLEND   EQU   *
// 000030                  00000030            (2/71)36+RPLLEN   EQU   RPLEND-IHARPL
// 000030                                      (2/72)37+* FIELDS FROM HERE NOT DEFINED IN D&L AND MAY NOT BE NEEDED
// 000030                                      (2/73)38+RPLID    DS    X                  RPL ID
// 000031                                      (2/74)39+RPLSTYPE DS    X                  RPL TYPE VSAM
// 000032                                      (2/75)40+RPLLXRBA DS    XL8 XRBA OF LAST REC VES (ESDS,RRDS) OR VX0 (KSDS,VRRDS)
// 00003A                                      (2/76)41+RPLCXRBA DS    XL8 XRBA OF CURR POS VES (ESDS,RRDS) OR VX0 (KSDS,VRRDS)
// 000044                                      (2/77)42+RPLOPENC DS    F   UNIQUE ACB OPEN COUNT TO DETECT RPL REPOSITION REQ'D
// 000048                                      (2/78)43+RPLFLAG  DS    0F                 RPL FLAG BITS FOR UPD GET ETC.
// 000048                                      (2/79)44+RPLFLAG1 DS    X
// 000049                  00000080            (2/80)45+RPLF1GOK EQU   X'80'              PREV GET SUCCESSFUL
// 000049                  00000040            (2/81)46+RPLF1GNF EQU   X'40'              PREV GET RECORD NOT FOUND
// 000049                                      (2/82)47+RPLFLAG2 DS    X
// 00004A                                      (2/83)48+RPLFLAG3 DS    X
// 00004B                                      (2/84)49+RPLFLAG4 DS    X
// 00004C                                      (2/85)50+*
// 00004C                                      (2/86)51+* VSAM RPL SVC 151 FUNCTION CODES IN R0
// 00004C                                      (2/87)52+*
// 00004C                  00000001            (2/88)53+RPLOPGET EQU   1                  GET
// 00004C                  00000002            (2/89)54+RPLOPPUT EQU   2                  PUT
// 00004C                  00000003            (2/90)55+RPLOPERA EQU   3                  ERASE
// 00004C                  00000004            (2/91)56+RPLOPPNT EQU   4                  POINT
// 00004C                                       (1/2)58          END   ,


}
