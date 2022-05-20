## [v1.8.1]

### Added

#### Changes to fully support running zCICS in Unix based OSes

* 091712a - Enable zCICS to run on Linux/MacOS (John Ganci)
* 585e957 - Make bat/DFHALL.BAT COBOL SYSMAC and SYSCPY consistent with bash/dfhall (John Ganci)
* ab82906 - add JARPATH and Z390SEQ environment variables (John Ganci)
* d1ac309 - Update/add new copyright info (John Ganci)

#### Z390 documentation restructure

A major restructure of the online documentation available at https://z390development.github.io/z390
PDF version of the documentation included in the distribution package

* 1d1015a - z390 docs structure (#315) (Anthony Delosa)

### Changed

* 3e13457 - First change to include additional help. As yet only for ASM.BAT (Abe Kornelis)
* 2dbcdd5 - Issue374 Improve usability of bat files (#383) (Abe Kornelis)
* 622949c - rename assembler directive APARM in tz390 to ACALLPRM to allow use of APARM macro (dsh33782)
* 41c867d - rename assembler directive APARM to ACALLPRM to allow use of APARM macro (dsh33782)

### Deprecated

None this release

### Removed

None this release

### Fixed

* 0891161 - fix issue 325 to prevent az390 abort due to invalid DC field terminator (dsh33782)
* dfdf87f - Issue 398 - Typo in error message 42 - invalid relatvie offset expression (#407) (Abe Kornelis)
* b1b96ca - Update issue #233 optimizing code, adding TESTSAC1, removing TESTSUB1 (dsh33782)
* cea1061 - Sample program to reproduce/test issue 398 (#401) (Abe Kornelis)
* 646c913 - Add sample program for issue 396 (#397) (Abe Kornelis)
* 1afceaf - Remove leading and trailing spaces from z390 version (John Ganci)
* 524da31 - Remove space in build version in z390.properties (Anthony Delosa)
* 52a4a30 - Fix build error "Not a git repo." https://github.com/actions/checkout/issues/760 (Anthony Delosa)
* 4346be4 - Issue @233 add support for blanks in DC numrtiv nominal values (dsh33782)
* 3861e20 - Add double ampersands tests (#393) (John Ganci)
* 35c2a74 - Issue #215 add regression tests for handling && in SETC vs DC strings (dsh33782)
* 492ba6f - Issue #215 Correct handling of && in SETC strings (dsh33782)
* 350c0aa - correct WTO field length in TBRCTX (dsh33782)
* c7dac3c - Corret TBRCTX change to WTO to remove word after (dsh33782)
* 226b707 - cleanup comments in TBRCTX (dsh33782)
* ecc1200 - Speed up BRCTG test to avoid S422 time limit exception (dsh33782)
* d7a1284 - Issue #180 Issue error on circular reference in options file (#354) (Abe Kornelis)
* 256c597 - Add TESTLITS test (#385) (John Ganci)
* 6081303 - fix LTORG issue 327 to align odd lits to even bound (dsh33782)
* f92613a - update change date and make ACALLPRM caps (dsh33782)
* 20927f3 - fix last mpy 0 result test to set cc non-zero (dsh33782)
* 4089843 - reset cc not zero fpr mpy 0 tests (dsh33782)
* 82da894 - add mpy result 0 tests (dsh33782)
* d838d21 - add mpy espie reset and move cc3 before wto (dsh33782)
* 709683a - add mpy overflow s0c8 overflow test via espie (dsh33782)
* 57b2daf - Issue372 add new program to zCobol test suite (#373) (Abe Kornelis)
* e75dce5 - Fix issue #363 Various bat files are failing to return error code when processing has failed (#364) (Abe Kornelis)
* fc05291 - fix multiply instructions abd tests (dsh33782)
* b44f1b9 - Fix breakpoint info displayed in test mode (#362) (John Ganci)
* b28e8cb - #359 Fix tron handling in bat files (#360) (Abe Kornelis)
* a41a8ee - Issue #357 Ending apostrophe on MNOTE not recognized (#361) (Abe Kornelis)
* cdeb422 - Fix for Issue #195 (=RPI 1622) Issue S0C2 if any of the privileged instructions are used (#351) (Abe Kornelis)
