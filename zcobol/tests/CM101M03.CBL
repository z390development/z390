000100 IDENTIFICATION DIVISION.                                         CM1014.2
000200 PROGRAM-ID.                                                      CM1014.2
000300     CM101M.                                                      CM1014.2
002200 ENVIRONMENT DIVISION.                                            CM1014.2
002300 CONFIGURATION SECTION.                                           CM1014.2
002800 INPUT-OUTPUT SECTION.                                            CM1014.2
003200 DATA DIVISION.                                                   CM1014.2
004000 WORKING-STORAGE SECTION.                                         CM1014.2
020000 01  CCVS-E-3.                                                    CM1014.2
020100     02  FILLER PICTURE X(22) VALUE                               CM1014.2
020200     " FOR OFFICIAL USE ONLY".                                    CM1014.2
020300     02  FILLER PICTURE X(12) VALUE SPACE.                        CM1014.2
020400     02  FILLER PICTURE X(58) VALUE                               CM1014.2
020500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".CM1014.2
020600     02  FILLER PICTURE X(13) VALUE SPACE.                        CM1014.2
020700     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 CM1014.2
024200 PROCEDURE    DIVISION.                                           CM1014.2
024300 SECT-CM101M-0001 SECTION.                                        CM1014.2
024400 GOODBYE.                                                         CM1014.2
027400         STOP RUN.                                                CM1014.2
