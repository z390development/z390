# zCICS Supplied Transactions

## CEBR

Browse Temporary Storage Queues.

A new implementation of this transaction combines the old `CEBR` with `CEMT ITSQ`.

The Temporary Storage queues are continuously updated/deleted, and
therefore show only the state at the moment of request.

A fixed state environment for testing `CEBR` may be obtained by running the test
transaction `BED9`.

Input formats:

- `CEBR` Invokes the queue names display.
- `CEBR qname` qname is case sensitive.
  - If found, goes to the data display.
- `CEBR X'..'` The hex characters are not case sensitive.
  - A maximum of 16 hex characters are processed.
  - If found, goes to the data display.
  - If the qname is not found, goes to the queue names display.

There are two displays:
- The queue names display
- The data display

### The queue names display

All current queue names are displayed with the number of items in
each queue at that moment.

There is only room for the 1st 60 queue names.

PF2 switches the queue name format cyclically, EBCDIC/ASCII/HEX.
In hex mode, only the first eight bytes are shown.
The current mode is shown top right.

CLEAR will terminate `CEBR`

Select a queue name with the cursor and press ENTER to go to the
data display.

Undisplayable characters are shown by an @-sign.

### The data display

The 1st entry will show data items 1-16 or fewer starting from the 1st
byte.

Undisplayable characters are shown by an @-sign.

Paging functions are usually by the PF keys shown at the bottom of the
screen. These keys are dynamic, only those shown are active.

#### PF keys

- PF1
  - Displays available commands, these are explained later. Any AID key returns to the data display.
- PF2
  - PF2 switches the data format cyclically, EBCDIC/ASCII/HEX
  - The current mode is shown top right.
  - The queue name is also converted and in hex mode only the 1st eight bytes are shown.
- PF3
  - Either terminates CEBR or returns to the queue names display.
- PF4
  - 1st data item after `** TOP **`
- PF5
  - Even if the `** BOTTOM **` line is on the screen, PF5 will
    always display that line and as much preceding data as is
    possible.
- PF7/PF8
  - Scroll forward or back 8 items.
- PF9/PF12
  - Scroll right or left 72 bytes if the mode is EBCDIC or ASCII.
  - In hex mode, scrolling is 36 bytes.
- PF10/PF11
  - Scroll forward or back 16 items.

#### Top line

- REC n of n
  - Shows the current item and maximum items.
- COL n of n
  - Shows the current data position and the maximum available on
    the current display.
  - If the data is of variable length, the display may show (e.g.)
    column 37 of 13. This may look odd, but indicates that a preset
    position and scroll forward or backward has displayed data that
    is too short for the preset position. Corrected by PF12 or a
    Column command.

#### Commands

- Typed after `ENTER COMMAND ===>`
- The minimum characters typed are indicated by upper case.
- In general, bad syntax causes the command to be ignored.
- There must be a space between parameters.

| Command     | Effect                                       |
|-------------|----------------------------------------------|
| Top         | same as PF4                                  |
| Bottom      | same as PF5                                  |
| Line n..n   | Start from item number n..n                  |
| Column n..n | Show data from position n..n                 |
| Find        | Not yet implemented                          |
| Queue       | Return to the queue names display            |
| Queue qname | qname is case sensitive                      |
|             | Display data from qname                      |
| Queue X'..' | The hex characters are not case sensitive    |
|             | A maximum of 16 hex characters are processed |
| PURGE       |  Must be upper case, deletes the TS queue.   |

Any syntax error here returns to the queue names display.

## CEDF

Intercept `EXEC CICS` commands and display associated parameters.

Currently only two-terminal working is supported, therefore a minimum of
two terminals must be active before `CEDF` may be invoked.

For all the instructions below, `DON0` is used as the termid receiving the
intercepts and `DON1` as the termid running the user transaction.
Any two termids may be used in your environment.

### EXEC CICS commands and CEDF

- All commands are intercepted by default.
- If a `CEDF` intercept is not wanted for an `EXEC CICS` command then add
  the parameter `NOEDF`.
- If no `CEDF` intercepts are wanted for a program or module then code
  `NOEDF` as a parameter on the `CALL MZ390`.

### Starting and operating CEDF

- On 1DON01, `CTRL+C` (clear), then enter `CEDF DON1`.
- See the message `EDF REQUEST SENT TO DON1`
- Switch to `DON1` (`ALT+TAB`) to see the message `THIS TERMINAL: EDF MODE ON`
- On `DON1`, `CTRL+C` (clear) and type your transid.

There should be an immediate switch to `DON0` to display the program
initiation screen.

### Tracing through the EXEC CICS commands

Just press `ENTER`.
See Special intercepts below.

### Actions available for each intercept

1. For some commands there may be too many parameters to fit on one
   screen. In these cases the paging keys will appear.

2. Where there are parameters that may have valid values in EBCDIC,
   ASCII or HEX then the PF2 key is displayed to switch modes.
   The current mode is displayed on Line 1.

3. For all intercepts PF5 can be used to display the `DSA` (Working Storage).
   Paging is available as required as well as PF2 to switch the
   character display between EBCDIC and ASCII.
   PF2 in this display does not affect the mode of the intercept display.
   Pressing ENTER will return you to the last display.

### Special intercepts

1. Conversational mode
   - `EXEC CICS RECEIVE` will display the message `AFTER ENTER ACTION REQUIRED ON TERMINAL`.
   - Press `ENTER` on `DON0`, then switch to `DON1`, enter some data
     and/or press the appropriate key for the transaction.
   - A switch to `DON0` will automatically occur to intercept the `RECEIVE`.

2. EXEC CICS RETURN TRANSID(....)
   - On `DON0` the message `TASK TERMINATION:TRANSID ... SET`.
   - `CEDF` is still active, switch to `DON1`, enter some data and/or press
     the appropriate key for the transaction.
   - A switch to `DON0` will automatically occur to start the pseudoconversational
     transaction.

3. EXEC CICS RETURN at the lowest level
   - On `DON0` the message `TASK TERMINATION:NO TRANSID SET`
   - The task has terminated but `CEDF` is still active.
   - On `DON1` another transid may be entered to continue the intercepts.

4. EXEC CICS RETURN at a higher level
   - On `DON0` the message `PROGRAM TERMINATION:LEVEL nn`
   - Continue to press ENTER as this is a RETURN from a LINK and
     the transaction will continue.

### Turning CEDF off

1. During normal interception press `PF3` and the transaction on `DON1`
   will complete as normal.

2. At TASK TERMINATION
   - Clear the screen and enter `CEDF DON1,OFF`
   - No message is sent to `DON1` in case it corrupts any current map
     but normal operation can continue on `DON1`.

### Redisplay Mode

There is no 'save screen' facility in zCICS as all intercepts are saved.

#### Redisplay Mode - on and off

`PF12` invokes Redisplay Mode with the display fields appearing at the
top right and the navigation PF keys displayed.

Pressing `ENTER` at any stage will return you to Intercept Mode so that
further use of `ENTER` will step through your programs.

#### Redisplay Mode - Navigation

You can move back and forth using the navigation keys, or type a
display number at the top. The display numbers start at 000 for the
current intercept and increase negatively. The number on the far right is
the number of saved intercepts. eg. 000/009 means you are viewing
the first of nine intercepts numbered -000 through -008.

Typing a high number like 999 will take you to the earliest intercept.

#### Redisplay Mode - Working Storage

During Redisplay Mode you can press `PF5` to display Working Storage.
It's important to note that this is the `DSA` as it existed when the intercept
was done and not the current `DSA`.

#### Redisplay Mode - Paging

Some commands like `EXEC CICS ASSIGN` may have too many
parameters to display on one screen. When this happens in Redisplay
Mode, `PF12:PAGING KEYS` is activated and you can switch the keyset
if you need to page the command up and down, and then `PF12` again to
return to Redisplay navigation.

### Forthcoming features

- Stop conditions.
- Dump display of FROM/INTO/SET etc. data areas.
- Abend intercept and abend task.
- Parameter/DSA modification.

## CEMT

Conversational Mode
- If any syntax error occurs then CEMT is switched into conversational mode
  and the principle parameters can be cursor selected from the screen.
- You cannot retype the command here, cursor selection or `CLEAR` are the
  only options.
- The cursor may be placed anywhere on the selection line.
- Note that `CEMT I FI` when selected defaults to `CEMT I FI ALL` and any
  syntax error on `CEMT S FI` does not enter conversational mode but
  executes `CEMT I FI ALL`.

The following `INQUIRE` commands have a single screen response with no
page forward/back implemented. Only the minimum abbreviations are shown.
The full text may also be typed, e.g. `CEMT INQUIRE TERMINAL`

- `CEMT I ENQ`
  - Displays the `QEA` (`ENQ`) chain
- `CEMT I FI`
  - Displays the files in the `FCT`.
  - Changes may be made to multiple files.
  - Cursor select a file for a full display.
  - See [CEMT INQUIRE FILE detailed description](#cemt-inquire-file-detailed-description)
    for a detailed description.
- `CEMT I TER`
  -  Displays the state of all terminals.
- `CEMT I TRA`
  - Displays the `PCT`.
  - Currently limited to 42 transactions.
- `CEMT I SYS`
  -  Displays the `Z390CICS.INI` file.
  - `JAR_PATH` and `CICS_PATH` are no longer `INI` parameters so
    they are extracted from their environment variables
- `CEMT S FI` Changes the status of one or more files.
  - See [CEMT SET FILE detailed description](#cemt-set-file-detailed-description)
    for a detailed description

#### Operational commands

- `CEMT S TER OUT`
  - Shuts down the terminal (`Z390KCP`).
- `CEMT P SHU`
  - Shut down the server if no active tasks.
  - When all active tasks are closed the server is shut down and zCICS ends.
- `CEMT P SHU IMM`
  - Shut down the server immediately.

### CEMT INQUIRE FILE detailed description

Multiple parameters may be specified.
When conflicts occur the rightmost parameter takes precedence.

The capitalized part of each parameter is the minimum accepted.

If the file specification is omitted then ALl is the default.
The file specification can immediately follow FI or can be a separate parameter.

File specification:
- As a single filename e.g. (`MYFILE`)
- As multiple filenames e.g. (`MYFILE1,MYFILE2`)
- Wild cards can be used e.g. (`+Y++`)
  - This means all 4-character filenames with `Y` as the 2nd
- Ending generic e.g. (`+Y*`)
  - This means all filenames of at least 2 character with `Y` as the 2nd
- Floating generic e.g. (`*AB*`) is not supported.

Additional selection parameters:
- OPen CLosed
- ENabled DIsabled UNenabled
- REad NORead
- UPdate NOUpdate
- ADd NOAdd
- BRowse NOBrowse
- DElete NODelete

Extension:
- FiXed Variable

> [!NOTE]
> the abbreviation for fixed is `FX` to avoid conflict with `FILE`.

When `ENTER` is pressed all files that meet the parameter criteria are displayed.

From the multiple file list screen you can do one of the following:
1. Amend the command line to change the parameters or to go to a different
   `CEMT` function. Any other changes on the screen will be discarded and the
   cursor position ignored.
2. Cursor select a file to go to the detailed display.
3. Overtype any of the green fields to change the file(s) status and press
   `ENTER`. Only the first character of each field is checked.
   - All successful amendments will change to white.
   - If an error was encountered, no more file changes will be processed and an
     error message will be displayed on line 23.

From the detailed display you can overtype any of the green fields to change the
file(s) status and press `ENTER`. Only the first character of each field is checked.
- All successful amendments will change to white.
- If an error was encountered, an error message will be displayed on line 23.
- Press `PF3` to return to the multiple file display.

### CEMT SET FILE detailed description

Multiple parameters may be specified.

When conflicts occur the rightmost parameter takes precedence.

The capitalized part of each parameter is the minimum accepted.

The file specification is mandatory, a `SYNTAX ERROR` message is displayed if missing.

The file specification can immediately follow `FI` or can be a separate parameter.

File specification:
-  As a single filename e.g. (`MYFILE`)
-  As multiple filenames e.g. (`MYFILE1,MYFILE2`)
-  Wild cards can be used e.g. (`+Y++`)
   - This means all 4-character filenames with `Y` as the 2nd
-  Ending generic e.g. (`+Y*`)
  - This means all filenames of at least 2 character with `Y` as the 2nd
- Floating generic e.g. (`*AB*`) is not supported.

Change parameters:
- OPen CLosed
- ENabled DIsabled
- REad NORead
- UPdate NOUpdate
- ADd NOAdd
- BRowse NOBrowse
- DElete NODelete

When `ENTER` is pressed all files that meet the file specification are changed
by the parameters specified.

The SET is now changed to INQUIRE and the command re-issued to display the
multiple file list screen.

- All successful amendments are shown in white.
- If an error was encountered, no more file changes will be processed and an
  error message will be displayed on line 23.
