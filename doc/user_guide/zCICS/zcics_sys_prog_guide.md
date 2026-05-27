# zCICS System Programmer's Guide

## Introduction

This document describes the zCICS environment, how it operates and the
modifications that can be made to it.

Experienced CICS people may notice a lack of authenticity in the background
coding and formats, and some use of archaic methods. This is all intentional, and
as z390 matures, so will zCICS.

## Objectives

1) To take an existing Assembler or COBOL CICS application, re-gen it in the
   z390 environment and run it unmodified and successfully under zCICS.
2) To be able to develop an Assembler or COBOL CICS application in the z390
   environment and transfer the source to the mainframe for re-assembly and
   testing.

Only the source may be exchanged between environments.

## Version compatibility

Versions of Java and z390 are tested by the z390 support team
and there are mechanisms for checking compatibility.

zCics is integrated with z390 in a single github repository.
There should not be compatibility issues.

## Current environment

The currently supported Application environment is described in the
[zCICS Application Programming Guide](zcics_app_prog_guide.md).

Requests for commands and extra parameters are very welcome and will help to
set a priority list.

## Reentrancy

Much of the internal code does not conform to the strict (quasi)-reentrancy rules
required of mainframe CICS.

If you are developing an application for later transfer to the mainframe, you must
obey all the reentrancy rules. There is no checking yet for rule breaking.

## How it works

The zCICS environment has been split into two sections.

The primary task is `Z390CICS`, which is called the Global Manager.
It will handle all shared resources like `TS`, `FC`, etc.

Each terminal has its own Command Prompt (cmd window).
This environment runs `Z390KCP` and invokes any Application programs requested.

Each Command Prompt is effectively a single terminal, single task environment.

`Z390KCP` is therefore the Local Manager handling `EIB`, `COMMAREA`s, `DSA`s and
other task related storage.

`TCPIO SEND/RECEIVE` are used to pass requests and data between `Z390CICS`
and each `Z390KCP`, with `Z390CICS` being the server and all terminals running
`Z390KCP` as multiple clients.

## Setting it up

### Parameters

The `Z390CICS.INI` file is self-documenting.

The following BAT files may need to be modified to your own environment
- `Z390CICG` - Start up zCICS
- `Z390KCPR` - Start 1 remote terminal for testing/tracing.
- `Z390KCPL` - Start many remote terminals.

`DFHPCT.MLC` has a basic set of test transaction codes which are listed later.
Users should add their own transactions to `DFHPCTUS.CPY` and re-assemble the
`PCT`.

`DFHFCT.MLC` has a basic set of test files. Users should add their own files to
`DFHFCTUS.CPY` and re-assemble the `FCT`. File creation and setup is more fully
described in the [zCICS VSAM Guide](zcics_vsam_guide.md).

## Local and Remote terminals

A Local terminal is a Command Prompt (cmd window) that is auto-started when
`Z390CICS` is started. The number of Local terminals that are started is controlled
by the `LOCAL_TERMINALS=` parameter.

A Remote terminal must be started from a Command Prompt that you have
manually created and is initiated by using `Z390KCPR` or `Z390KCPL`. See the
[Starting it up](#starting-it-up) section.

A Remote terminal doesn't have to be on the same PC as `Z390CICS`, but can be
on another PC connected via a home network.

There is a restraint on the number of Remote terminals set by the
`REMOTE_TERMINALS=` parameter.

The `MAXTHREADS=` parameter defines the upper limit for all terminals.

## Starting it up

From the z390 GUI (recommended), or in your own Command Prompt:

1) `Z390CICG`
   - This will start the zCICS Global Manager and all Local terminals.
2) A Remote terminal may be set up as follows:
   - Method 1: One remote for testing
     - If interactive debugging of an application is wanted
     - Create a Command Prompt window and use `CD` to navigate to, and invoke
       `Z390KCPR`. Parameters like `TEST` and `TRACE` may be added.
     - Any test session will be recorded in `Z390KCP.TRE`.
     - The remote terminal will be started in the Command Prompt.
     - If `TEST` is specified, then zCICS progress cannot be made unless you give
       commands to the z390 GUI as well.
  - Method 2 : The Launcher
    - Create a Command Prompt window and use `CD` to navigate to, and invoke `Z390KCPL`.
    - `Z390KCPL` takes a single digit parameter, e.g. `Z390KCPL 2`
    - A parameter of 1 is assumed if missing.
    - `Z390KCPL` does not use the current Command Prompt, but creates the
      specified number of remote terminals in other windows.
    - The use of `TEST` or `TRACE` is not recommended as they will only affect
      the launcher program `Z390RMTE`.
    - The number of terminals actually started is subject to both the
      `REMOTE_TERMINALS=` and the `MAXTHREADS=` parameters.

## Test transactions

When the terminal is opened, the termid is in the title.

From the 'initial screen' you can perform the following tests. Where two transids are
shown, e.g. `BED1`/`BEC1`, the second is the identical process but written in zCOBOL.

- `CTRL+C`
  - Clear the screen.
  - Within a transaction that has issued a `RECEIVE`, the `CLEAR AID` is returned.
- `AAAA`
  - test invalid transid message
- `MMM1`
  - test abend `APCT` message
- `MMM2`
  - 'hello world'
- `GUI4`
  - Conversational test 1
- `GUI6`
  - Conversational test 2
- `TST1`
  - Conversational test 3 and LINK test
  - Added test for `CWA`.
  - Added tests for `ASSIGN`.

- `BED1`/`BEC1`
  - Test bed for `LINK`, `XCTL` and `RETURN` with `COMMAREA`.
  - Added test for `CWA`.
  - Added tests for `ASSIGN`.
  - Keep pressing `ENTER` until the 'clear screen' message is displayed.
- `BED2`/`BEC2`
  - `HANDLE AID` testing
  - Follow the on-screen instructions.
  - `DUMP TRANSACTION` testing.
- `BED3`/`BEC3`
  - `HANDLE CONDITION`/`IGNORE CONDITION`/`PUSH`/`POP` testing.
  - `GETMAIN`/`FREEMAIN` testing.
  - Abend handling.
  - Follow the on-screen instructions.
- `BED4`/`BEC4` `HANDLE ABEND` testing. Simple handling.
  - Follow the on-screen instructions.
- `BED5`/`BEC5`
  - `HANDLE ABEND` testing. Complex handling.
  - Follow the on-screen instructions.
- `BED9`/`BEC9`
  - Temporary Storage testing.
  - Also builds an environment for testing `CEBR`.
  - Can be run multiple times to extend the `CEBR` test queues.
- `IC01`/`ICC1` Test Interval Control `ASKTIME` (`ABSTIME`) and `DELAY`.
  - This task may take up to 2 minutes to complete.
- `IC02` Test Interval Control `START`, `RETRIEVE` and `CANCEL`.
  - Watch for `HELLO FROM TEST1C03, THIS IS MESSAGE n` on the bottom line.
  - Press `ENTER` after each change of n, there are 5.
  - After a few seconds this message appears:
    `HELLO FROM TEST1C03, THIS IS SUBVERSION`
  - Press `ENTER` to complete the test, when you see `END`
  - Task end, now press CLEAR the test is complete.
  - This test cannot be repeated as there are residual `TS` records left behind.
- `IC04`
  - Test `START`/`RETRIEVE` with `CHANNEL`.
- `VSM1`/`VSC1`
  - Read and browse of `ESDS` files.
  - Records successfully read are written to the `TS` queue `VSM1`.
- `VSM2`/`VSC2` Read and browse of `RRDS` files.
  - Records successfully read are written to the TS queue `VSM2`.
- `VSM3`/`VSC3`
  - Read and browse of `KSDS` files.
  - Records successfully read are written to the `TS` queue `VSM3`.
- `BMS1`/`BMC1`
  - Test simple functions of mapping support.
- `BMS2`
  - Test complex functions of mapping support.
- `ENQ1`
  - This tests `ENQ`/`DEQ`
  - Ensure `LOCAL_TERMINALS=2` in the `.INI` file.
  - This is time critical.
    - On `DON0`, clear the logo and enter `ENQ1`
    - Switch to `DON1`, clear the logo and enter `ENQ1`
    - Steps a and b must occur within 5 seconds.
    - `DON1` will suspend a number of times.
- `ASGN`
  - Tests `ASSIGN`.
- `NT01`
  - Test for non-terminal tasks.
  - A bubble sort is performed for 20,000 records and timed.
  - The data is split into 2 sets of 10,000 records and 2 non-terminal tasks
    are started to sort each set in parallel.
  - The display shows the timings of the 3 processes.
- `SETF`
  - Tests `SET FILE`.
- `CHN1`
  - Test all Channel/Container functions.

## Shutdown

`CEMT P SHU IMM` may still leave a zCICS window behind.
- We are working to resolve this, just click close (big X) on each stranded window.

`CEMT S TER OUT`
- This closes the terminal.
- Local terminals may not be re-instated.
- Even if all terminals are closed with this method, zCICS will not shut down.
- A remote terminal may still be started.

`CEMT P SHU`
- The terminal issuing this command will be closed.
- No new terminals or tasks may be started.
- If there are no active tasks, then the server is closed and zCICS ends.
- When all active tasks terminate, zCICS will end.

`CEMT P SHU IMM`
- The terminal issuing this command will be closed.
- The server is then closed and zCICS ends.
- Terminals with active tasks may remain stranded.

## Abends

`SNAP` dumps are provided when requested or when circumstances demand them.

A standard abend message (`DFH2206`) is usually displayed.

An `ASRA` abend which is handled by a `HANDLE ABEND` command will always
produce a `SNAP` dump but no message.

If the initial program of a transaction is not available an `APCT` abend will occur.
This won't produce a dump and cannot be `HANDLE`d.
The `ID` of the `SNAP` indicates its origin:
- 999
  - Abend `ASRATEXT='ABEND ASRA'`
- 998
  - Requested dump by `EXEC CICS ABEND TEXT='ABEND abcode'`
- 997
  - Requested dump by `EXEC CICS DUMP`
  - `COMPLETE TEXT='DUMP dumpcode COMPLETE'`
  - Single area `TEXT='DUMP dumpcode AREA'`
  - Segments `TEXT='DUMP dumpcode SEGMENT nnn'`
  - Other values: The `EIBRESP` field, i.e. `27=PGMIDERR` has occurred
    `TEXT='ABEND xxxx'` i.e. `AEI0=PGMIDERR`

## Aborting the environment violently

This is occasionally necessary to preserve traces and dumps.

The z390 GUI can stay up and won't be harmed by this process.
- Right click the taskbar
- Select Task Manager
- Search for `java.exe`
  - There might be more than one, repeat steps below for each one.
  - You can sort the tasks by clicking on 'Image name'
  - Don't confuse this with `javaw.exe`
- Right click on `java.exe`
- Select `End Process` and `Yes`.
  - All the zCICS environment and terminals should go away.

## Change Summary

- February 1, 2012
  - Added test transactions IC04 and CHN1
- June 10, 2010
  - Removed paragraph about limited application resources
- November 1, 2010
  - Added NT01 and SETF to list of test transactions
- August 1, 2009
  - Added zCOBOL to various texts
  - Comment about extra ASSIGN tests for TST1 and BED1.
  - Updated test transaction IC02...reconstructed
  - Added ASGN to list of test transactions
- February 21, 2009
  - Comment about extra CWA tests for TST1 and BED1.
  - Added 'copy' transids for zCOBOL.
- November 24, 2008
  - Added IC01, IC02, BMS2, ENQ1 to list of test transactions
- June 27, 2008
  - Added BMS1 to list of test transactions
