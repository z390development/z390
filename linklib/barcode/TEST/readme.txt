  Application: USPS 4-State Barcode (aka Intelligent Barcode)
      Release: V1.0
       Author: Roger Williams, ABA
               (roger@accessaba.com)
    Copyright: 2009 ABA Inc
      RelDate: 05/11/2009
      License: GNU General Public License (GPL)

  The Z390 USPS Barcode implementation is written in HL/ASM390,
  supported in Z/390. All business logic was adapted from
  the original USPS reference implementation written in C.

  This implementation consists of two (2) modules:

  Source Name        Usage

  ENCODETR           The code containing the Barcode logic
                     (the DLL). This module can be invoked
                     from user-written code.

  S4BCDRVA           A driver program used to run the
                     4 Barcode self-tests supplied by the USPS.
                     This program verifies that the Barcode logic
                     is operating correctly, and prints the resulting
                     report to SYSPRINT.

*---------------------------------------------------------------------*
* Installation                                                        *
*---------------------------------------------------------------------*
  1. Unzip the S4BC.zip file to a directory on your machine.
  2. cd to <yourDirectory>/barcode.
  3. Assemble and link ENCODETR and S4BCDRVA.

*---------------------------------------------------------------------*
* Program Invocation - ENCODETR                                       *
*---------------------------------------------------------------------*

    LOAD EP=ENCODETR                  load the DLL
    ST   0,ENCEPA                     save entry point address
    MVC  ENCPARMS+00(4),=A(TRKCODE)   ptr - tracking code
    MVC  ENCPARMS+04(4),=A(ZIPCODE)   ptr - zip code
    MVC  ENCPARMS+08(4),=A(BARCODE)   ptr - generated barcode
    MVC  ENCPARMS+12(4),=A(0)         int - no intermediate results
    MVC  ENCPARMS+16(4),=A(0)         int - no intermediate table dumps
    OI   ENCPARMS+16,X'80'            mark as last parm in plist
    LA   1,ENCPARMS                   point to parameter list
    L    15,ENCEPA                    load entry point
    BASR 14,15                        invoke the service
    ...  ... ... ...
    ...  ... ... ...
    TRKCODE   DS CL20                 tracking code to be used
    ZIPCODE   DS CL12                 0-, 5-, 9-, or 11-digit zipcode
    BARCODE   DS CL65                 65-digit barcode result
    ENCPARMS  DS 5A                   ENCODETR parameter list
    ENCEPA    DS A                    ENCODETR entry point

    REGISTERS AT ENTRY
       REG      USAGE
       R0       N/A
       R1       POINTER TO A LIST OF FROM 3 to 5 FULLOWRD ENTRIES,
                AS FOLLOWS:
                OFFSET   CONTENTS
                  0      ADDRESS OF A 20-BYTE BUFFER CONTAINING
                         THE 20 TRACKING CODE DECIMAL DIGITS.
                  4      ADDRESS OF A 12-BYTE BUFFER, LEFT-JUSTIFIED
                         AND PADDED BY BLANKS, CONTAINING
                         0, 5, 9, OR 11 ROUTER (I.E., ZIPCODE) DIGITS.
                  8      ADDRESS OF A 65-BYTE BUFFER WHICH
                         WILL CONTAIN THE BARCODE RESULT.
                         CHARACTERS WILL BE 'T', 'F', 'A', OR 'D',
                         WHERE:
                         T IS TRACKER (NEITHER ASCENDER NOR DESCENDER)
                         F IS FULL (TRACKER + ASCENDER + DESCENDER)
                         A IS ASCENDER (TRACKER + ASCENDER)
                         D IS DESCENDER (TRACKER + DESCENDER)
                         BARS PROCEED FROM LEFT TO RIGHT, WITH THE
                         LEFTMOST BAR AS THE FIRST BAR IN THE BARCODE
                 12      IF NON-ZERO, DISPLAY
                         INTERMEDIATE RESULTS WHILE
                         GENERATING THE BARCODE;
                         IGNORED IF ZERO
                         (USER CODE SHOULD ALWAYS SPECIFY 0)
                 16      IF NON-ZERO, DISPLAY
                         TABLE DATA WHILE
                         GENERATING INTERNAL TABLES;
                         IGNORED IF ZERO
                         (USER CODE SHOULD ALWAYS SPECIFY 0)
                THE FIRST 3 FULLWORDS ARE REQUIRED; THE LAST LIST
                ENTRY SHOULD ITS HI-ORDER BIT ON TO DENOTE
                THE END OF THE LIST. A MAXIMUM OF 5 LIST ENTRIES
                WILL BE RECOGNIZED.
       R2-R12   N/A
       R13      SAVE AREA PTR (18 FULLWORDS)
       R14      RETURN ADDRESS
       R15      N/A

    REGISTERS AT RETURN
       REG      USAGE
       R0-R14   SAME AS AT ENTRY.
       R15      A RETURN CODE, AS FOLLOWS:
                RC   MEANING
                 0   BARCODE GENERATED.
                 1   SELF-TEST FAILED.
                 2   BAR STRING IS NULL.
                 3   BYTE CONVERSION FAILED.
                 4   RETRIEVE TABLE FAILED.
                 5   CODEWORD CONVERSION FAILED.
                 6   CHARACTER RANGE ERROR DETECTED.
                 7   TRACK STRING IS NULL.
                 8   ROUTE STRING IS NULL.
                 9   TRACK STRING HAS BAD LENGTH.
                10   TRACK STRING HAS INVALID DATA.
                11   TRACK STRING HAS INVALID DIGIT2.
                12   ROUTE STRING HAS BAD LENGTH.
                13   ROUTE STRING HAS INVALID DATA.
               100   INVALID ARGUMENT(S) DETECTED.

    REGISTER USAGE
       REG      USAGE
       R0-R8    WORK REGISTERS
       R9       I/O WORK AREA PTR
       R10      GENERAL WORK AREA PTR
       R11-R12  CSECT ADDRESSABILITY
       R13      SAVE AREA PTR
       R14-R15  WORK REGISTERS

    PROGRAM PROPERTIES:
       AMODE:        31-BIT
       RMODE:        ANY
       REENTRANT:    NO


*---------------------------------------------------------------------*
* Program Invocation - S4BCDRVA                                       *
*---------------------------------------------------------------------*

    JCL:
      //BARCODE  JOB (4444),'BARCODE',CLASS=A,MSGCLASS=X
      //BARCODE      EXEC PGM=S4BCDRVA,
      //             PARM='SEE BELOW'
      //STEPLIB  DD  DISP=SHR,DSN=ABA.BARCODE.LOAD
      //SYSPRINT DD  SYSOUT=*

    EXEC PARMS:
      PARM       MEANING
      SELFTEST   RUN THE SELF-TEST SUITE
                 (TRACKING AND ROUTE CODES BELOW ARE IGNORED)
      NNN XXX    GENERATE A BARCODE FROM "NNN" AND "XXX", WHERE:
                 NNN IS A 20-DIGIT TRACK CODE
                 XXX IS A 0-, 5-, 9-, OR 11-DIGIT ROUTE CODE
                     (I.E., A ZIP CODE)

    RETURN CODE:
                RC   MEANING
                 0   BARCODE GENERATED.
                 1   SELF-TEST FAILED.
                 2   BAR STRING IS NULL.
                 3   BYTE CONVERSION FAILED.
                 4   RETRIEVE TABLE FAILED.
                 5   CODEWORD CONVERSION FAILED.
                 6   CHARACTER RANGE ERROR DETECTED.
                 7   TRACK STRING IS NULL.
                 8   ROUTE STRING IS NULL.
                 9   TRACK STRING HAS BAD LENGTH.
                10   TRACK STRING HAS INVALID DATA.
                11   TRACK STRING HAS INVALID DIGIT2.
                12   ROUTE STRING HAS BAD LENGTH.
                13   ROUTE STRING HAS INVALID DATA.
               100   INVALID ARGUMENT(S) DETECTED.
