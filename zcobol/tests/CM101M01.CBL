000100 IDENTIFICATION DIVISION.                                         CM1014.2
000200 PROGRAM-ID.                                                      CM1014.2
000300     CM101M.                                                      CM1014.2
000400 AUTHOR.                                                          CM1014.2
000500     FEDERAL COMPILER TESTING CENTER.                             CM1014.2
      ******************************************************************
      *
      * Testcase for Issue #655:
      * The INSTALLATION paragraph is supposed to contain comments only,
      * until the next paragraph or division is encountered.
      * Yet the line with the zip code (5203) is interpreted as
      * a working storage definition. Which causes an error:
      * ZC390E zcobol invalid ws level 5203 on line 11
      *
      * The code in zc390 was fixed. This test is to guard against
      * regression - program should compile okay with RC=0.
      *
      ******************************************************************
000600 INSTALLATION.                                                    CM1014.2
000700     GENERAL SERVICES ADMINISTRATION
000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.
000900     SOFTWARE DEVELOPMENT OFFICE.
001000     5203 LEESBURG PIKE  SUITE 1100
001100     FALLS CHURCH VIRGINIA 22041.
001200
001300     PHONE   (703) 756-6153
001400
001500     " HIGH       ".
001600 DATE-WRITTEN.
001700     CCVS-74 VERSION 4.0 - 1980 JULY 1.
001800     CREATION DATE     /    VALIDATION DATE
001900     "4.2 ".
002000 SECURITY.                                                        CM1014.2
002100     NONE.                                                        CM1014.2
002200 ENVIRONMENT DIVISION.                                            CM1014.2
002300 CONFIGURATION SECTION.                                           CM1014.2
002400 SOURCE-COMPUTER.                                                 CM1014.2
002500     XXXXX082.                                                    CM1014.2
002600 OBJECT-COMPUTER.                                                 CM1014.2
002700     XXXXX083.                                                    CM1014.2
002800 INPUT-OUTPUT SECTION.                                            CM1014.2
003200 DATA DIVISION.                                                   CM1014.2
004000 WORKING-STORAGE SECTION.                                         CM1014.2
024200 PROCEDURE    DIVISION.                                           CM1014.2
024300 SECT-CM101M-0001 SECTION.                                        CM1014.2
027400         STOP RUN.                                                CM1014.2
