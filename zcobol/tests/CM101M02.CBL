000100 IDENTIFICATION DIVISION.                                         CM1014.2
000200 PROGRAM-ID.                                                      CM1014.2
000300     CM101M.                                                      CM1014.2
      ******************************************************************
      *
      * Testcase for Issue #697:
      * When a Cobol source program contains a CD declaration, the
      * generated code causes (during assembly):
      *     missing copy =CD.MAC REPLACE CD INSTRUCTION WITH CD MACRO
      *
      * Cause: the standard options file for use with zCobol specifies
      *        an incomplete concatenation of folders.
      *
      * Compiles of this test program still end with RC=8
      * The validation criterion is the content of the error message:
      *     MZ390E MNOTE 8,'CD NOT SUPPORTED YET'
      *
      ******************************************************************
002200 ENVIRONMENT DIVISION.                                            CM1014.2
002300 CONFIGURATION SECTION.                                           CM1014.2
002800 INPUT-OUTPUT SECTION.                                            CM1014.2
003200 DATA DIVISION.                                                   CM1014.2
003300 FILE SECTION.                                                    CM1014.2
004000 WORKING-STORAGE SECTION.                                         CM1014.2
022900 COMMUNICATION SECTION.                                           CM1014.2
023000 CD  CM-INQUE-1 FOR INPUT                                         CM1014.2
023100     SYMBOLIC QUEUE IS MAIN-QUEUE                                 CM1014.2
023200     SYMBOLIC SUB-QUEUE-1 IS NO-SPEC-1                            CM1014.2
023300     SYMBOLIC SUB-QUEUE-2 IS NO-SPEC-2                            CM1014.2
023400     SYMBOLIC SUB-QUEUE-3 IS NO-SPEC-3                            CM1014.2
023500     MESSAGE DATE IS DATE-RECEIVED                                CM1014.2
023600     MESSAGE TIME IS TIME-RECEIVED                                CM1014.2
023700     SYMBOLIC SOURCE IS WHERE-FROM                                CM1014.2
023800     TEXT LENGTH IS MSG-LENGTH                                    CM1014.2
023900     END KEY IS END-KEY                                           CM1014.2
024000     STATUS KEY IS STATUS-KEY                                     CM1014.2
024100     MESSAGE COUNT IS MSG-COUNT.                                  CM1014.2
024200 PROCEDURE    DIVISION.                                           CM1014.2
024300 SECT-CM101M-0001 SECTION.                                        CM1014.2
026300 GOODBYE.                                                         CM1014.2
027400         STOP RUN.                                                CM1014.2
