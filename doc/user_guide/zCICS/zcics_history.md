# zCics History

This is only a brief summary; please refer to the Application Programming Guide
and the System Programmers Guide for the details.

## Release highlights

| zCics | z390      | Highlight                                               |
|-------|-----------|---------------------------------------------------------|
| V 4   | V1.3.08h  | Support of VSAM base datasets for read/browse functions |
| V 5   | V1.4.02   | Basic Mapping Support supported                         |
| V 7   | V1.5.00   | COBOL supported                                         |
| V 8   | V1.5.01   | INQUIRE FILE and ASSIGN added                           |
| V 9   | V1.5.01f  | CEMT INQUIRE/SET and CEDF Phase 1                       |
| V10   | V1.5.04a  | CEDF Phase 2...Redisplay Mode                           |
| V11   | V1.5.05   | Container and Channel support                           |

## V11

### Enhancements and changes

- BMS
  - DFHMDF now supports long field names
- Z390LCL local module locator, revised index method to give performance improvement.
- FLENGTH added to READQ/WRITEQ TS, START and RETRIEVE.
  Needed for implementation of EXEC CICS START CHANNEL
- Transaction abend WTO message now has the abend code interpreted.
- Container and Channel support
  - CHANNEL parm added to ASSIGN, LINK, XCTL, RETURN and START.
  - New commands GET, PUT, DELETE, MOVE, STARTBROWSE, GETNEXT, ENDBROWSE.
  - CEDF mods for CHANNEL parameter, new intercepts for the new commands.

### Maintenance

- SEND MAP LENGTH parameter is valid but ignored, better doc and no error if omitted
- In zCOBOL, INTO and FROM parameters may result in 'base reg not found'
- Attempt to free an XCTL-copied COMMAREA twice.
- Changed many EXEC CICS macros to use LAY instead of LA when numeric lengths or implied lengths have to be used.
  This permits field sizes larger than 4K bytes. eg. LAY R1,L'&FROM
- CEDF intercept of EXEC CICS RETRIEVE may display incorrect lengths.
- READQ TS with SET can incorrectly raise LENGERR.
- Programs TESTBED9 and TESTBEC9 amended.
- Removed FREEMAIN from LCL100A (RETRIEVE) as it may cause abends.

## V10

### Enhancements and changes

- CEDF intercepts for ASSIGN, INQUIRE FILE and FORMATTIME with paging
- CEDF intercept for ABEND
- CEDF intercept for RETURN with bad response
- CEDF enhanced with the addition of Redisplay Mode to page through saved intercepts,
  together with display of Working Storage at the time of the intercept.
- See "zCICS Supplied Transactions".
- DFHREGS supported as synonym for EQUREGS

### Maintenance

- Transaction SETF not reset properly preventing repeat (fixed in V9)
- Added another SETF in the regression test
- CEMT I SYS may cause storage overlay when there is no JARPATH environment variable (fixed in V9).
- EXEC CICS SEND/RECEIVE MAP with NOHANDLE doesn't return the correct EIBRESP/EIBRESP2
- EXEC CICS SEND MAP ... CURSOR(nnn) generates the SBA,IC sequence multiple times.
- EXEC CICS READQ TS with ITEM=zero causes loop in server instead of raising ITEMERR.
- Minor changes to EXEC CICS macros to bring them up to HLASM standard and avoid using the ALLOW option.
- Change to DFHEIENT as entry to a program via EXEC CICS XCTL may cause storage overlay.
- Change to EXEC CICS SET FILE parameter list not reflected in GBL4C04 preventing some functions from working.
  SETF also showed incorrect results
- EXEC CICS HANDLE CONDITION/AID may accept invalid parameters if they begin validly.
  eg. EXEC CICS HANDLE CONDITION ERROR123(label) would be interpreted as ERROR without a label.
- CEMT SET FILE without any parameters caused ASRA abend.

- BMS
  - Symbolic cursor positioning was putting cursor under attribute, not first data byte.
  - Mapset greater than 32K may cause abends.
  - EXEC CICS RECEIVE MAP may abend MAPFAIL/7 owing to incorrect setting of the structure size.
- CEDF
  - Intercept at PROGRAM INITIATION might display bad data for EIBREQID/EIBRSRCE.
  - HANDLE CONDITION/AID may show address/offset in error.

## V9

### Enhancements and changes

- Eyecatcher added to all LCL modules
- BMS support for colour
  - DFHMSD EXTATT=, DSATTS=, MAPATTS= added
  - DFHMDI EXTATT=, DSATTS=, MAPATTS= overrides added
  - DFHMDF COLOR= added
  - DFHBMSCA for Assembler and COBOL updated to include colour
    MAPFAIL/9 added to detect invalid attribute/colour override
- BMS macros DFHMDI and DFHMDF have a new NOLABEL parameter
- Non-Terminal support
  - New INI parm MAX_NONTERMS=
  - Displayed in CEMT I SYS
  - CEMT I TER displays NONTERM
  - New HANDLE CONDITION NOTALLOC
  - Various new conditions for RETURN, SEND (all), RECEIVE (all) and ASSIGN
- Interval Control
  - EXEC CICS START will now force an IC scan. Previously IC tasks were delayed if all terminals were busy.
  - Reconstructed so that any IC scan request can start transactions for other terminals.
    Z390KCP (client) can now process unsolicited requests from the server.
  - As a result of implementing unsolicited requests, the shutdown mechanism was reconstructed and the idle tick rate reduced from 1 sec to 0.1 sec.
  - EXEC CICS DELAY reconstructed.
  - Added REQID
  - DELAY is now shipped to the server and an ICE is set up.
  - EXEC CICS CANCEL can now cancel a DELAY using REQID.
  - When a task is waiting for a DELAY, CEMT INQ TER will show the status of SUS-DELAY (suspended by delay).
  - New global module GBL1004.
- Added SET FILE and new test transaction SETF.
- CEMT INQUIRE/SET FILE reconstructed to provide an integrated process.
- CEMT INQ TERM has the extra field CONNID. When starting multiple terminals they may not be started in sequence.
- CEDF intercepts and Working Storage implemented.

### Maintenance

- DATE_FORMAT=S may display dates as YMD instead of MDY.
- Z390COMP changed to format output correctly as a result of CODEPAGE support.
- Data in a map structure starting with X'FF' was mistakenly identified as an XINIT parameter.
- Coding error in EXEC CICS ADDRESS may cause problems in zCOBOL.
- EXEC CICS LOAD, FLENGTH was being treated as the same as LENGTH.
- EXEC CICS HANDLE and IGNORE enhanced to detect no parameters.
- EXEC CICS SEND can now raise LENGERR.
- EXEC CICS SEND CONTROL wasn't setting the WCC bits correctly.
- EXEC CICS READ, READNEXT and READPREV with SET was ignoring any LENGTH parameter.

## V8

### Enhancements and changes

- FILEERTB ... added RPL feedback code X'000800C4'
- Add the DUMP parameter to all ABEND macros
- Added INQUIRE FILE
- HANDLE/IGNORE new condition ... END
- Added ASSIGN and test transaction ASGN
- CEMT I FIL now uses INQUIRE FILE and you can now cursor select a file for an expanded display.
- Z390COMP (Sequential output comparator) has been improved to display an error message and print the buffers when an error occurs.

### Maintenance

- DFHMDI wasn't generating MAPNAME.I and MAPNAME.O correctly for MODE=IN or MODE=OUT assembler maps.
- DFHMDF wasn't generating a dummy length field for MODE=OUT assembler maps leading to abends or corrupted output.
- The linkage mechanism for XCTL has been changed and DFHEIPRM eliminated.
  Although XCTL worked ok, if an abend occurred in an XCTL'd-to program, recursive abends were likely.
- zCOBOL enhancements have brought out several minor bugs in zCICS COBOL maps and programs which are now fixed.
- Test transaction IC02 stopped working owing to changes in TESTBED1 to support CWA testing. The transaction was revised.
- Z390KCPR.BAT had the NOTIME parameter added.

## V7

As of February 21, 2009 V1.5.00

## Rebranding

Z390/CICS is now known as zCICS

### Maintenance

- BMS
  - Mods made to macros and EXEC CICS SEND/RECEIVE to support COBOL maps.
  - Added PICIN/PICOUT support for COBOL maps.
- Revised all EXEC CICS commands to make them base-free, a requirement for COBOL support.
- Changes to LINK, XCTL and DFHEIENT to enter all programs with GR1 pointing to A(EIB,COMMAREA). A requirement for COBOL support.
- DFHEIGBL has been withdrawn and the code placed in DFHEIENT.
- HANDLE ABEND LABEL wasn't correctly restoring registers or properly cleaning up if the LABEL was at a higher link-level than the abending program.
- FORMATTIME wasn't returning the correct date.
- FREEMAIN DATA() corrected.
- Improvement to all commands accepting indirect adcons.

### Enhancements and changes

- BMS
  - DFHMSD LANG=COBOL will generate a COBOL map structure as mapset.CPZ
  - The BAT file for SYSPARM(MAP) generation needs the ALLOW parameter to enable the IMG file to be created correctly.
- EXEC CICS ADDRESS COMMAREA() CWA() EIB() added
- New INI parm CWASIZE=nnnnnnn Displayed in CEMT I SYS
- CEMT I TRA extended from 17 to 42 transactions.
- zCOBOL support
  - BMS LANG=COBOL already listed above
  - SET supports ADDRESS OF label
  - LENGTH and FLENGTH support LENGTH OF label
- EXEC CICS SEND MAP supports SET()

## V6

As of November 24, 2008 V1.4.04

### Maintenance

- FORMATTIME bug fixed, gave incorrect date in leap years
- Minor XCTL bug fixed, copied COMMAREA unnecessarily deleted.
- Z390KCP bug fixed, in complex situations COMMAREA is being freed causing recursive abends. Not related to XCTL bug.
- Z390CEBR not sending task number resulting in bad QNAMES log message.
- Z390CEMT had minor bug which sometimes prevented the terminal prefix from being displayed in CEMT I TER.
- Z390CICS not setting the TRACE_Z390CICS parameter properly.
- Eliminated GR2 as an internal register. It is now available as a base register but is not recommended as it may be modified by some instructions.
- A label is now allowed on EXEC CICS statements. e.g. MYLABEL EXEC CICS READ ...
- DFHEIGBL includes EQUREGS and therefore not needed in programs.
- EXEC CICS HANDLE/IGNORE CONDITION now supports DSIDERR=FILENOTFOUND
- EXEC CICS WRITEQ TS NOHANDLE not working owing to missing restore of GR15.
- EXEC CICS WRITEQ TS supports ITEM without REWRITE for compatibility.
- EXEC CICS READQ TS with SET was using LENGTH as an input field incorrectly.
- EXEC CICS GETMAIN not supporting NOHANDLE properly.
- DFHALL.BAT amended so that GBL and LCL modules are no longer linkedited.
- BMS
  - DFHMSD
    - Support for TIOAPFX=YES
    - Support and discard TERM=
  - DFHMDI
    - Bug: AGO .GOON15 should be .GOON05
    - Extra error checking for LINE/COLUMN and MAP01 corrected.
    - Support for TIOAPFX=YES
    - Support and discard JUSTIFY=
  - DFHMDF
    - Support for fields that wrap row or screen.
    - INITIAL now supports strings that contain doubled quotes or ampersands.
  - LCL1804 (SEND MAP)...
    - Obscure bug that may not generate a cursor location when in simulation mode.
    - Bug fixed which may allow X'00' to be included in a data stream.
    - Support for fields that wrap row or screen.
  - EXEC CICS SEND MAP now supports FROM() and LENGTH() parms and MAP may now be a label.
  - EXEC CICS RECEIVE MAP now supports INTO() and MAP may now be a label.
  - DFHNULL added to DFHAID.CPY
- Interval Control
  - HANDLE/IGNORE extra conditions TERMIDERR, IOERR, TRANSIDERR, ENDDATA, ENVDEFERR
- Task Control
  - HANDLE/IGNORE extra condition ... ENQBUSY
- Internal reconstruction
  - This was started in CICS V5; all EXEC CICS commands now invoke an external LCLhhhh module.
    The list can be found in zCICS Diagnosis Reference.
  - As a result some changes were made to the Application program structure:
    - DFHEIFCP and DFHEITSP removed from EISTG.
    - DFHEIGBL now only contains EIB and TCTTE structures.
    - Coding reconstructed for LINK, XCTL and RETURN.
    - DFHEIRET withdrawn, now incorporated into LCL0E08 (RETURN).
    - This means you must end a zCICS program with EXEC CICS RETURN.

### Enhancements and changes

- All EXEC CICS commands now support RESP, RESP2 and NOHANDLE.
- Added TRACE(G) to Z390KCP trace options (storage).
- Additional code added to BMS to prevent PICIN/PICOUT generating truncated data when too much data is input. MAPFAIL/8 is raised.
- EIBDS is now supported
- Support for EXEC CICS START, RETRIEVE and CANCEL
- Support for IMMEDIATE on EXEC CICS RETURN
- Support for EXEC CICS ENQ/DEQ
- CEMT I ENQ added.

## V5

As of June 27, 2008 V1.4.02

### Maintenance

- RPI833: All macros now changed for HLASM compatibility.
- RPI837: Add NOTIME parameter to Z390CICS to prevent S422 abend for terminals.
- RPI844: Use of TROT now needs aligned tables.
  - Z390CEBR and Z390COMP needed amending.
- RPI845: Recode ESTAE routines in Z390KCP to use new SDWA
- RPI946: Amend a few programs to eliminate the generation of 6's for uninitialized fields.
- TESTBED3: Bug fixed, final lines not displayed.
  - Rebuilt the sequential terminal streams for BED1.
- Z390KCP: Bug fixed, CLEAR key not passed to pseudo-conversational task.
  - Bug fixed, abend ASRA after RETURN COMMAREA received may lead to recursive abends owing to multiple FREEMAINs of the COMMAREA.

### Enhancements and changes

- CEMT conversational mode implemented.
- DFHFCT and DFHPCT have protection against missing TYPE=FINAL.
- Z390CICS future-proofed with a new modular design.
  - Global managed functions now in GBLxxxx modules
- Sequential terminal support improved:
  - Only the SEQ_TERM parameter needs to be amended.
  - Added TRACE option to set parms for trace data collection.
- HANDLE/IGNORE extra conditions
  - MAPFAIL, INVMPSZ, OVERFLOW
- New LCLxxxx modules to handle EXEC CICS processing.
  - This conversion is not complete.

### New Features

- Basic Mapping Support
- Macros DFHMSD, DFHMDI, DFHMDF.
- EXEC CICS SEND MAP
- EXEC CICS RECEIVE MAP
- EXEC CICS SEND CONTROL

### New Doc

- zCICS Basic Mapping Support.

## V4

As of January 18, 2008 V1.3.08h

- File control commands now support all fixed and variable VSAM datasets for all applicable read and browse functions.
- Interval Control
  - ASKTIME, DELAY and FORMATTIME supported
- Dump Control
  - DUMP TRANSACTION

### Enhancements and changes

- HANDLE/IGNORE extra conditions
  - ILLOGIC, EXPIRED
- Z390KCP amended to cater for conditions that default to ignore.
- INI parm DATE_FORMAT changed to single byte:
  - K=DMY (UK format)
  - S=MDY (US format)
  - R=YMD (Reverse format)
- This is also reflected in CEMT I SYS
- Implementation of new LOG() parameter.
  - When the INI parameter TRACE_LOCAL=YES is specified, the ERR, LOG and TRE files are prefixed with the terminal id and not Z390KCP.
- Sequential Terminal support
  - Enables regression testing of all transactions.
  - See the documentation about this major new feature: zCICS Sequential Terminal Support

### Bug fixes

- Abends may occur if INI parm LOCAL_TERMINALS=0
- EIBRSRCE was being used in abend messages, but this may not contain the program name. New field EIBPROG fixes this.
- Minor error in DFHHCBLK, 5 fullwords reserved for ILLOGIC instead of one.

### Other

Programs RTZCICSn are no longer supplied and are now classified 'internal use only'.

## V3

As of October 1st, 2007 V1.3.08

- Improvements to the shutdown process.
- Changes to remote terminal start procedure.
  - Z390KCPL to launch multiple remotes
  - Z390KCPR to launch one remote, used for test/trace
- Z390CICS.INI
  - JAR_PATH= and CICS_PATH= are withdrawn. The paths are extracted from the environment variables JARPATH and CICSPATH.
  - New parm, DATE_FORMAT=UK or US

### Bug fix

- EXEC CICS IGNORE not acquiring handle block.

### Enhancements and changes

- RECEIVE INTO(EIBAID) no longer supported
  - Receive into your own area, EIBAID and EIBCPOSN will be automatically extracted.
- HANDLE/IGNORE extra conditions
  - QIDERR, ITEMERR, FILENOTFOUND, NOTFND, DUPREC, DUPKEY, NOSPACE, NOTOPEN, ENDFILE, DISABLED
- CEMT I SYS
  - Terminal numbers were in error, display enhanced.
  - JAR_PATH and CICS_PATH are displayed from the environment variables.
  - DFHALLV.BAT to rebuild the test VSAM catalog and files.

### Added support for

- DELETEQ TS
- READQ TS
- WRITEQ TS

## File control commands are for VSAM ESDS read-only

- READ
- STARTBR
- READNEXT
- READPREV
- RESETBR
- ENDBR

### Added facilities

- CEBR transaction, a hybrid of CEMT I TSQ and CEBR.
- Extensive improvements.
- CEMT I FIL

### Documentation

- CEMT and CEBR are now moved to zCICS Supplied Transactions.
- New document: zCICS VSAM Guide.

## V2

As of June 25, 2007 V1.3.05

- Corrections to setting of EIBAID and EIBCPOSN.
- Maintain EIBRSRCE (program name).
- Clear prefix section of DSA, possibility of abends.
- Support of multiple base registers for code and DSA
  - See ZCICSAPP doc.
- Local terminals may not start... thanks to Don for a temporary fix.
  - Command Prompt pathname not externalized.
  - Fixed by extra INI parm CICS_PATH...see ZCICSSYS doc.
- Added DFHAID.CPY...BAT files may need SYSCPY amended if used.

### Enhancements and changes

- EXEC CICS RECEIVE
  - supports NOHANDLE
  - default INTO(EIBAID) has been withdrawn, recode to
```hlasm
         EXEC CICS RECEIVE INTO(EIBAID) LENGTH(ONE)
         ...
         ONE DC H'1'
```
- Abend handling now properly implemented.
- COMMAREA() now supports indirect addressing.
- SEND FROM() now supports indirect addressing.
- Added SHUT DOWN status to CEMT I TER.
- CEMT now accepts full wording for commands, i.e. `CEMT SET TERMINAL OUT`,
  but only the minimum number of characters of each parameter will be tested.
- CEMT P SHU and CEMT P SHU IMM are fully implemented, there are some issues, see ZCICSSYS for details.
- Added INI parameter INITIAL_TRANSID to fire a transaction when terminal is started. New LOGO transid.
- Added dummy DFHPCTUS.CPY for users to add their own PCT entries.
- INI parms JAR_PATH and CICS_PATH now support preset environment variables as %...%

### Added support for

- ABEND
- FREEMAIN
- GETMAIN
- HANDLE AID
- HANDLE ABEND
- HANDLE/IGNORE CONDITION
  - only ERROR, INVREQ, LENGERR, PGMIDERR supported
- PUSH HANDLE
- POP HANDLE
- RELEASE

## V1

As of May 1, 2007 V1.3.03b

- Provides assembly support for CICS programs.
- Includes most parameters associated with the following EXEC CICS commands...
  - LINK, LOAD, RECEIVE, RETURN, SEND, XCTL
- Some basic CEMT functions.