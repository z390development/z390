# zCICS Diagnosis Reference

## Client Control Blocks

### Dynamic Storage Area (DSA)

| Item        | Details                            |
|-------------|------------------------------------|
| Eye catcher | None                               |
| Acquired    | DFHEIENT                           |
| Released    | RETURN at highest level XCTL       |
| Anchor      | R13                                |
| DSECT       | DFHEISTG (prefix only)             |
| Cleared     | Only the prefix, not the user area |
| Length      | Variable                           |

### EXEC Interface Block (EIB)

| Item        | Details                            |
|-------------|------------------------------------|
| Eye catcher | 'DFHEIBLK'                         |
| Acquired    | Z390KCP                            |
| Released    | Close of thread                    |
| Anchor      | R11 (DFHEIBR)                      |
| DSECT       | DFHEIBLK                           |
| Cleared     | Yes                                |
| Length      | EIBLENG                            |

### HANDLE ABEND Block

| Item        | Details                            |
|-------------|------------------------------------|
| Eye catcher | 'DFHABBLK'                         |
| Acquired    | 1st use                            |
| Released    | Task end                           |
| Anchor      | TCTTEABD (Byte after eye catcher)  |
| DSECT       | DFHABBLK                           |
| Cleared     | Yes                                |
| Length      | ABDLENG (one table entry)          |
| Entries     | 25                                 |

### HANDLE AID block

| Item        | Details                                          |
|-------------|--------------------------------------------------|
| Eye catcher | 'DFHADBLK'                                       |
| Acquired    | HANDLE AID (1st block only)                      |
|             | PUSH HANDLE                                      |
| Released    | RETURN at highest level (all chained AID blocks) |
|             | POP HANDLE (only top-of-chain block)             |
|             | XCTL (all chained AID blocks)                    |
| Anchor      | DFHEIAID (DSA)                                   |
| Chain       | AIDCHAIN                                         |
| DSECT       | DFHADBLK                                         |
| Cleared     | Yes                                              |
| Length      | AIDLENG                                          |

### HANDLE CONDITION block

| Item        | Details                                                       |
|-------------|---------------------------------------------------------------|
| Eye catcher | 'DFHHCBLK'                                                    |
| Acquired    | HANDLE CONDITION (1st block only)                             |
|             | PUSH HANDLE                                                   |
| Released    | RETURN at highest level (all chained HANDLE CONDITION blocks) |
|             | POP HANDLE (only top-of-chain block)                          |
|             | XCTL (all chained HANDLE CONDITION blocks)                    |
| Anchor      | DFHEIHCN (DSA)                                                |
| Chain       | HCNCHAIN                                                      |
| DSECT       | DFHHCBLK                                                      |
| Cleared     | Yes                                                           |
| Length      | HCNLENG                                                       |

### Link-Level Area (LKA)

| Item        | Details                            |
|-------------|------------------------------------|
| Eye catcher | None                               |
| Acquired    | Z390KCP                            |
| Released    | Close of thread                    |
| Anchor      | TCTTELKA                           |
| Cleared     | Yes                                |
| Length      | 4 (R13 value for this link-level)  |
| Entries     | 25                                 |

### Terminal Control Table Terminal Entry (TCTTE)

| Item        | Details                            |
|-------------|------------------------------------|
| Eye catcher | 'DFHTCTTE'                         |
| Acquired    | Z390KCP                            |
| Released    | Close of thread                    |
| Anchor      | R10 (TCTTEAR)                      |
| DSECT       | DFHTCTTE                           |
| Cleared     | Yes                                |
| Length      | TCTTELEN                           |

### Temporary Storage request/reply block (DFHTSBLK).

Data sent/received follows the block.

| Item        | Details                            |
|-------------|------------------------------------|
| Eye catcher | None                               |
| Acquired    | GETMAINd by EXEC CICS TS command   |
| Released    | FREEMAINd by EXEC CICS TS command  |
| Anchor      | None                               |
| DSECT       | DFHTSBLK                           |
| Cleared     | No                                 |
| Length      | TSPREFIX                           |

### File Control request/reply block (DFHFCBLK)

Data sent/received follows the block.

| Item        | Details                                        |
|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | GETMAINd by an EXEC CICS file control command  |
| Released    | FREEMAINd by an EXEC CICS file control command |
| Anchor      | None                                           |
| DSECT       | DFHFCBLK                                       |
| Cleared     | Yes                                            |
| Length      | FCPREFIX                                       |

### Interval Control request/reply block (DFHICBLK)

| Item        | Details                                        |
|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | GETMAINd by an EXEC CICS START/CANCEL command  |
| Released    | At task end                                    |
| Anchor      | None                                           |
| DSECT       | DFHICBLK                                       |
| Cleared     | Yes                                            |
| Length      | ICPREFIX                                       |

### Task Control request/reply block (DFHKCBLK)

| Item        | Details                                        |
|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | GETMAINd by an EXEC CICS ENQ/DEQ command       |
| Released    | At task end                                    |
| Anchor      | None                                           |
| DSECT       | DFHKCBLK                                       |
| Cleared     | Yes                                            |
| Length      | KCPREFIX                                       |

### CWA request/reply block (DFHCWBLK)

| Item        | Details                                        |
|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | Embedded in LCL0202                            |
| Released    | N/A                                            |
| Anchor      | None                                           |
| DSECT       | DFHCWBLK                                       |
| Cleared     | No                                             |
| Length      | CWPREFIX                                       |

### Channel control block (DFHCHAN)

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | EXEC CICS PUT/MOVE (LCL3416/LCL3440)                 |
| Released    | EXEC CICS DELETE (LCL3412), link-level end, task end |
| Anchor      | TCTTECAQ                                             |
| DSECT       | DFHCHAN                                              |
| Cleared     | Yes                                                  |
| Length      | 26                                                   |

### Container control block (DFHCONT)

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | EXEC CICS PUT/MOVE (LCL3416/LCL3440)                 |
| Released    | EXEC CICS DELETE (LCL3412), link-level end, task end |
| Anchor      | CHANCONT (DFHCHAN)                                   |
| DSECT       | DFHCONT (in DFHCHAN)                                 |
| Cleared     | Yes                                                  |
| Length      | 28+Container length                                  |

### STARTBROWSE CONTAINER control block (DFHCHWA)

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | EXEC CICS STARTBROWSE (LCL9626)                      |
| Released    | EXEC CICS ENDBROWSE (LCL962A), task end              |
| Anchor      | TCTTECHW                                             |
| DSECT       | DFHCHWA                                              |
| Cleared     | Yes                                                  |
| Length      | 40                                                   |

## Server Control Blocks

### File Control Table (DFHFCT)

Contains the ACB for each file operation.

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | Z390CICS at start                                    |
| Released    | Never                                                |
| Anchor      | DFHFCTAD                                             |
| DSECT       | DFHFCTDS                                             |
| Length      | FCTABLEN                                             |

### VSAM Work Area (DFHVSWAD)

Also contains the RPL for file operations.

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | Z390CICS at File Control process start               |
| Released    | Z390CICS at File Control process end or task abend   |
| Anchor      | FCTVSWA                                              |
| Chain       | VSWCHAIN                                             |
| DSECT       | DFHVSWAD                                             |
| Cleared     | Yes                                                  |
| Length      | VSWLEN                                               |

### Interval Control Element (DFHICEDS)

Chained in time order

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | GBL1008 when a valid EXEC CICS START is processed    |
| Released    | GBL100C when a valid EXEC CICS CANCEL is processed   |
|             | GBL10FF when an ICE scan can start a task            |
| Anchor      | ICEANCHR                                             |
| Chain       | ICECHAIN                                             |
| DSECT       | DFHICEDS                                             |
| Cleared     | Yes                                                  |
| Length      | ICELEN                                               |

### Temporary Storage Name Table (DFHTSNDS)

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | GBL0A02 when first EXEC CICS WRITEQ TS is processed  |
| Released    | GBL0A06 when EXEC CICS DELETEQ TS is processed       |
| Anchor      | TSNANCHR                                             |
| Chain       | TSNCHAIN                                             |
| DSECT       | DFHTSNDS                                             |
| Cleared     | Yes                                                  |
| Length      | TSNLEN                                               |

### Thread Control (THRDDSCT)

One entry for each terminal to a maximum of 10.
When `SEQ_TERM=YES`, the special 11th terminal is reserved for `SQ01`.

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | Fixed in Z390CICS                                    |
| Released    | Never                                                |
| Anchor      | THRDCNTL                                             |
| DSECT       | THRDDSCT                                             |
| Cleared     | Yes                                                  |
| Length      | THRDLEN                                              |

### Queue Element Area (DFHQEADS)

| Item        | Details                                              |
|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | GBL1204 when EXEC CICS ENQ is processed              |
| Released    | GBL1206 when EXEC CICS DEQ is processed              |
|             | GBL12FC when task end DEQALL is processed            |
| Anchor      | QEAANCHR                                             |
| Chain       | QEACHNF                                              |
| DSECT       | DFHQEADS                                             |
| Cleared     | Yes                                                  |
| Length      | QEALEN                                               |

## Management descriptions

### Abend Management

The following types of abend may occur:
- Program check
- IGNORE CONDITION not permitted (program check)
- Condition raised but not handled or ignored
- EXEC CICS ABEND

Whether any of these result in a dump and/or termination of the task depends on
the HANDLE ABEND status which is discussed later.

All of the above types cause a program check, with the following markers:

| Item                                 | Details             |
|--------------------------------------|---------------------|
| X'000000',C'ABEND',A(IGNORE address) | HANDLE CONDITION    |
| X'0000FE',C'ABEND',C'xxxx'           | ABEND with dump     |
| X'0000FF',C'ABEND',C'????'           | ABEND without dump  |
| Other                                | Program check       |

The program check is trapped by the ESTAE routine `APPABEND` in `Z390KCP`.
This routine determines the cause of the abend and takes the correct action.

#### HANDLE CONDITION marker

- The last `CONDITION` block is located (if any) and the condition slot is tested, followed by the `ERROR` slot.
- If no `HANDLE` or `IGNORE` then the terminate handler is invoked (see later).

ABEND with/without dump and program checks go straight to the terminate handler.

#### Terminate handler

`APPTABDN` in `Z390KCP` tests for the existence of a `HANDLE ABEND`
block (`DFHABBLK`). If none the task is terminated abnormally.

Each entry in the `HANDLE ABEND` block represents a link-level, so the
table is scanned backwards for the highest active entry.
If there are no active entries, the task is terminated abnormally.

If an active entry is found it is immediately inactivated.
- `HANDLE ABEND LABEL` will cause a branch to the label.
- `HANDLE ABEND PROGRAM` will cause an `XCTL` to the program.
- If the abending program has received a `COMMAREA`, then that will be passed to the abend handler.

If `HANDLE ABEND LABEL` is at a higher link-level than the abending
program, then a special `EXEC CICS RETURN CLEANER` is issued (in `Z390KCP`)
to simulate a `RETURN`, as all lower levels are now considered abandoned.

Note that `PUSH`, `POP`, `HANDLE ABEND CANCEL/RESET` only affect the
current link-level.

#### Dumps

- A program check will always produce an `ASRA SNAP` dump.
- `ABEND` without `NODUMP`, will always produce a dump using `ABCODE`.
- No other dumps will be produced if an abend or condition is handled.

## COMMAREA Management

### RETURN COMMAREA

The program issuing the `RETURN` must be at link-level 1 (i.e. about to return to 
`Z390KCP`). If this is not the case then `INVREQ` will be raised. This condition
cannot be `IGNORE`d as it is assumed that no valid code follows a `RETURN`.

The `RETURN` macro sets `TCTTECA` (address) and `TCTTECAL` (length).
When the next task is invoked `TCTTECAL` is used to refresh `EIBCALEN`.

When `Z390KCP` regains control after `RETURN`, the `COMMAREA` address and
length are compared with the last `RETURN COMMAREA` (holding areas
`COMMADDR` and `COMMLEN`):
- If both are the same, they are passed to the next transid.
- If either is different, then a new area is `GETMAIN`ed, the `COMMAREA` is
  copied, and the old one `FREEMAIN`ed.
- Temporary holding areas `COMMSAVA` and `COMMSAVL` are used during the `FREEMAIN` process.

### LINK COMMAREA

The address is stored in the linkers `DSA` at `DFHEICAP` and the length in
`EIBCALEN`. `DFHEICAP` is passed as a parameter.

> [!NOTE]
> A `LINK COMMAREA` is never specifically `FREEMAIN`ed; it is always part
> of another storage area (`DSA`, Program, `GETMAIN`).

### XCTL COMMAREA

The current `COMMAREA` address in the `DSA` (`DFHEICAP`) and length in
`EIBCALEN` are compared with the `XCTL COMMAREA`.
- If both are the same, the address is passed to the next program.
- If either is different, then a new area is `GETMAIN`ed and the `COMMAREA` is copied.<br />
  The new `COMMAREA` address is held in the callers `DSA` (`DFHEICAP`) and
  its existence flagged by `TCTTECND=X'FF'`. This `COMMAREA` copy will be
  `FREEMAIN`ed by the next `RETURN` without the `COMMAREA` parameter.

See [Interval Control Management](#interval-control-management) for further documentation which may affect
`COMMAREA` processing.

## GETMAIN/FREEMAIN Management

A chain of storage areas is anchored from `TCTTESCC`.
`TCTTESCC` has the address of the first `GETMAIN`ed area.

Eight bytes are added to each request, and they serve as a prefix:
- 4-byte address of next `GETMAIN` or 0
- 4-byte total length

The user is passed the address after the prefix.

`FREEMAIN` must have the same address as the `GETMAIN` passed otherwise an
`INVREQ` condition is raised. `FREEMAIN`s may occur in any order, the chain is just
'repaired' at that point.

At task end or task abend all remaining `GETMAIN`s are `FREEMAIN`ed.

## HANDLE AID Management

A `HANDLE AID` is owned by a program and is never passed to another program.
The `AID` block (`DFHADBLK`) is acquired on first use. `PUSH` and `POP` will
acquire/release additional `AID` blocks.

At task end, task abend or an `XCTL`, all `AID` blocks are `FREEMAIN`ed.

`HANDLE AID` only works for conversational tasks.

## HANDLE/IGNORE CONDITION Management

A `HANDLE CONDITION` is owned by a program and is never passed to another
program. The `CONDITION` block (`DFHHCBLK`) is acquired on first use. `PUSH` and
`POP` will acquire/release additional `CONDITION` blocks.

Each 4-byte entry represents a condition, this may contain:
|---------|----------------------------------------|
| 4X'00'  | The condition is not handled (default) |
| A(label)| The condition will be handled at label |
| 4X'FF'  | The condition should be ignored        |

The `ERROR` condition can be handled as a 'catch-all' for any type of condition that
doesn't have a specific `HANDLE CONDITION`.
When both a condition and `ERROR` are set, only the condition label is used, not both.

At task end, task abend or an `XCTL`, all `CONDITION` blocks are `FREEMAIN`ed.

## Temporary Storage Management

The queues are owned by `Z390CICS`, so all requests for TS services are sent by
zCICS tasks to the server.

There are two structures in the server:
- The queue name chain
  - A chain of all queue names.
  - The anchor of the chain is internal (`TSNANCHR`).
  - The TS name block is `DFHTSNDS`.
  - A queue name is created by the first `WRITEQ TS` for the name and is chained on the end.
- The TS data chain
  - A chain of all items added to the queue.
  - The anchor of the chain is in the queue name table (`TSNITEM1`).
  - The `DSECT` for the TS data chain prefix is internal (`TSDPREFX`), the data follows the prefix.

> [!NOTE]
> - `WRITEQ TS` will add a new item to the chain end.
> - `WRITEQ TS REWRITE` will free the old item, create a new one, and repair the chain.
> - `DELETEQ TS` will delete all the data items and then delete the queue name and repair the chain.

## File Control Management

The files are owned by `Z390CICS`, so all requests for FC services are sent by
zCICS tasks to the server.

The `FCT` (`DFHFCTDS`) defines the status of each file.
Each `FCTTE` contains the `ACB` for that file.
See the [zCICS VSAM Guide](zcics_vsam_guide.md) to see how files are created and defined to zCICS.

### File opening

When `Z390CICS` starts, all files defined as `FILSTAT=OPENED` are opened.
Failure results in the status `(CLOSED,DISABLED)`.

Files defined as `(CLOSED,ENABLED)` are opened when the first request is received.
Failure results in the status `(CLOSED,DISABLED)`.

### Request processing

No error conditions are explained here, they are listed in
the [zCICS Application Programming Guide](zcics_app_prog_guide.md), and in the IBM Manuals.

Any VSAM feedback codes and errors are converted to `RESP`/`RESP2` values
and sent back to the Client.

When a task ends or abends all `VSWA`s owned by the task are released. An
exception to this occurs when a condition is raised after a browse command is
issued and there is a `HANDLE CONDITION`. In this case, the `VSWA` is not
released unless the transaction is abended and will require an `ENDBR`.

A `READ` will always release the `VSWA` regardless of `HANDLE CONDITION`.

#### READ (ESDS) logic

- A VSWA is acquired.
- RPL OPTCD is set to (ADR) or (ADR,XRBA).
- RPLARG is set to the address of FCP(X)RBA.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- GET issued and the data is sent to the program.
- The VSWA is released.

#### READ (RRDS) logic

- A VSWA is acquired.
- RPL OPTCD is set to (KEY).
- RPLARG is set to the address of FCPRRN.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- GET issued and the data is sent to the program.
- The VSWA is released.

#### READ (KSDS) logic

- A VSWA is acquired.
- RPL OPTCD is set to (KEY,FKS,KEQ).
  - Options KGE and/or GEN are also set if specified.
  - KEYLENGTH(0) is a special case and forces GEN and KGE.
- RPLARG is set to the address of FCPRID.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- If GEN or KGE is specified, then POINT is issued.
- GET issued and the data is sent to the program.
- The VSWA is released.

#### STARTBR (ESDS) logic

- A VSWA is acquired.
- The REQID is set (default is zero).
- RPL OPTCD is set to (ADR,SEQ) or (ADR,SEQ,XRBA).
- RPLARG is set to the address of FCP(X)RBA.
- A POINT is issued.
- The current XRBA is saved in the VSWA.

#### STARTBR (RRDS) logic

- A VSWA is acquired.
- The REQID is set (default is zero).
- RPL OPTCD is set to (KEY,SEQ).
- RPLARG is set to the address of FCPRRN.
- A POINT is issued.
- The current RRN is saved in the VSWA.

#### STARTBR (KSDS) logic

- A VSWA is acquired.
- The REQID is set (default is zero).
- RPL OPTCD is set to (KEY,SEQ,FKS,KEQ).
  - Options KGE and/or GEN are also set if specified.
  - KEYLENGTH(0) is a special case and forces GEN and KGE.
- RPLARG is set to the address of FCPRID.
- A POINT is issued.
- The current (generic) FCPRID is saved in the VSWA.
- The current KEYLENGTH is saved in the VSWA.

#### READNEXT (ESDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (ADR,SEQ,FWD) or (ADR,SEQ,FWD,XRBA).
- RPLARG is set to the address of FCP(X)RBA.
- A check is made to see if the XRBA supplied differs from the current XRBA,
  if it does then a POINT is issued. This allows skip-sequential processing to occur.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- The current XRBA is saved in the VSWA, a GET is issued, and the current
  (X)RBA and the data are sent to the program.

#### READNEXT (RRDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (KEY,SEQ,FWD).
- RPLARG is set to the address of FCPRRN.
- A check is made to see if the RRN supplied differs from the current RRN, if
  it does then a POINT is issued. This allows skip-sequential processing to occur.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- The current RRN is saved in the VSWA, a GET is issued, and the current
  RRN and the data are sent to the program.

#### READNEXT (KSDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (KEY,SEQ,FKS,KEQ,FWD).
  - Options KGE and/or GEN are also set if specified in the STARTBR.
- RPLARG is set to the address of FCPRID.
- POINT is issued for skip-sequential:
  - If the keylength has changed and/or the (generic) key in FCPRID has changed.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- The current (generic) FCPRID is saved in the VSWA, a GET is issued, and
  the current full key and the data are sent to the program.

#### READPREV (ESDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (ADR,SEQ,BWD) or (ADR,SEQ,BWD,XRBA).
- RPLARG is set to the address of FCP(X)RBA.
- A check is made to see if the XRBA supplied differs from the current XRBA,
  if it does then a POINT is issued. This allows skip-sequential processing to occur.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- The current XRBA is saved in the VSWA, a GET is issued, and the current
  (X)RBA and the data are sent to the program.

#### READPREV (RRDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (KEY,SEQ,BWD).
- RPLARG is set to the address of FCPRRN.
- A check is made to see if the RRN supplied differs from the current RRN, if
  it does then a POINT is issued. This allows skip-sequential processing to occur.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- The current RRN is saved in the VSWA, a GET is issued, and the current
  RRN and the data are sent to the program.

#### READPREV (KSDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (KEY,SEQ,FKS,KEQ,BWD).
  - Note: GEN is invalid and KGE is ignored.
- RPLARG is set to the address of FCPRID.
- POINT is issued for skip-sequential if the keylength has changed.
- Area of the maximum or fixed length is GETMAINd and RPLAREA is set.
- The current FCPRID is saved in the VSWA, a GET is issued, and the
  current full key and the data are sent to the program.

#### RESETBR (ESDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (ADR,SEQ) or (ADR,SEQ,XRBA).
- RPLARG is set to the address of FCP(X)RBA.
- A POINT is issued.
- The current XRBA is saved in the VSWA.

#### RESETBR (RRDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (KEY,SEQ).
- RPLARG is set to the address of FCPRRN.
- A POINT is issued.
- The current RRN is saved in the VSWA.

#### RESETBR (KSDS) logic

- The VSWA created by the STARTBR is located.
- RPL OPTCD is set to (KEY,SEQ,FKS,KEQ).
  - Options KGE and/or GEN are also set if specified.
  - KEYLENGTH(0) is a special case and forces GEN and KGE.
- RPLARG is set to the address of FCPRID.
- A POINT is issued.
- The current (generic) FCPRID is saved in the VSWA.
- The current KEYLENGTH is saved in the VSWA.

#### ENDBR (ESDS, RRDS, KSDS) logic

- The VSWA created by the STARTBR is located.
- The VSWA is released.

## Interval Control Management

### DELAY

`DELAY` is handled by the Client.

### ASKTIME

`ASKTIME` is handled by the Client.

### START

The module `LCL1008` validates the time parameters and also the `TRANSID`.

However the time is specified, it is converted to a `STCK` time that has been
reduced to units of 0.01 seconds.

If `REQID` has not been specified, one is created by multiplying `TRANSID` and
`TERMID` and then overlaying the first two bytes with `C'DF'`. If `TERMID` is
omitted then blanks are assumed as zeros would produce a zero result. The
`REQID` is then stored in `EIBREQID`.

If the `START` command contains any of the parameters `FROM`, `QUEUE`,
`RTRANSID` or `RTERMID` then a `TS Q` will be built.

The `TS Q` record will have a 16-byte prefix to hold any of the parameters
`QUEUE`, `RTRANSID` or `RTERMID` followed by the `FROM` data (if any). In the
prefix, any parameters not specified will be `X'00'`.

If the `CHANNEL` parameter is specified the channel and its containers are
written to a `TS Q`. The queue name is created by multiplying `TRANSID` and
`TERMID` and then overlaying the first two bytes with `C'CH'`. If `TERMID` is
omitted then blanks are assumed as zeros would produce a zero result.
As containers can be up to 2GB, the `FLENGTH` extension to `WRITEQ TS` is
used.

The `WRITEQ TS` has two special parameters (internal use only), `ICTRAN` and
`ICTERM`. These are the `TRANSID` and `TERMID` parameters from the `START`
command. When the `WRITEQ TS` is shipped to the Server for processing,
these parameters are stored in the `TS` name table. The `REQID` (`TS Q` name)
must be owned by the `START TRANSID`/`TERMID` combination. Any violation of
that rule will raise the `IOERR` condition. Although the `WRITEQ TS` detects the
`IOERR` condition, it is passed on to the `START` command.

The `ICTRAN`/`ICTERM` parameters also serve another purpose, in that
specifying them allows the writing of a DF-prefix `TS` record which would
normally raise the `INVREQ` condition.

The `START` command parameters are then shipped to the Server.

#### Server processing (`GBL1008`)

The `TERMID` is validated; if invalid the `TERMIDERR` condition is returned to the Client.

An Interval Control Element (`ICE`) is then created and chained in expiration time order.

### Invocation

This mechanism is unique to each terminal.

The `IC` scan mechanism is in `Z390KCP`.
On every attempt to receive data from a terminal a request is sent to the
Server to do an `ICE` scan for an expired one.

#### Server processing (`GBL10FF`)

Assuming an expired `ICE` is found for our terminal, checks are made to see if the terminal is available.

- Terminal not available
  - If the `ICE REQID` and `TRANSID` are the same then this is a repeated
    `START` request which has written more records to the `TS Q` to be read
    by the initiated task. The `ICE` is deleted.<br />
    This situation will occur if multiple tasks issue `START` requests all with
    a very close expiry time.
- Terminal available
  - The Client is sent a zero return and will initiate the transaction and set
    `EIBREQID`. The `ICE` is then deleted.
  - An `ICE` rescan is then done, and if any expired `ICE`s match the `REQID`
    and `TRANSID`, they are deleted.<br />
    This situation will occur if one task issues multiple `START` requests all
    with a very close expiry time.
  - `Z390KCP` will issue a `READQ TS` for the `'CH...'` queue to see if
    channels and containers have been passed. If the read succeeds then
    the channel/container structure is built for the task and the `'CH...'`
    queue is deleted.

### RETRIEVE

An `IC` invoked transaction is passed the `REQID` in `EIBREQID`.
The module `LCL100A` issues a `READQ TS QUEUE(EIBREQID)`

If conditions `ITEMERR` or `QIDERR` occur these are converted to condition
`ENDDATA` and this condition is raised.

Each `TS Q` record has a 16-byte prefix and the fields `QUEUE`, `RTRANSID` and
`RTERMID` are moved into the labels provided.

If a field is requested but is `X'00'`, then the `ENVDEFERR` condition is raised.

> [!NOTE]
> If one of the three fields is supplied but not requested, then no error is raised.

The relationship between `SET`/`INTO` and `LENGTH` is complex, rather than
repeat the logic here, I refer the reader to the text following label `PFXDUN` in `LCL100A`.

`INTO`/`SET` will receive the data after the prefix.

Deletion of the `TS Q`
- The condition `ENDDATA` will attempt a `DELETEQ TS`, no error occurs if this fails.
- A successful `READQ TS` will do a `DELETEQ TS` if it returns `NUMITEMS=1`.
- The condition `LENGERR` will attempt a `DELETEQ TS` if `NUMITEMS=1`.
  - No error occurs if this fails.
- If `EIBREQID` is not null at task end or abend, a `DELETEQ TS` is done.
  - No error occurs if this fails.

Mysteries:
- It is not known how the condition `NOTFND` can be raised.
  IBM have now acknowledged that this is an error in the Manual and will be
  corrected.

### CANCEL

The module `LCL100C` sends the `REQID` to the server.

The server module `GBL100C` scans the `ICE` chain for a matching `REQID`. If
found the `ICE` is deleted and a rescan is done.

If no `ICE`s are deleted then the `NOTFND` condition is raised.
Any `TS Q` records associated with the deleted `ICE`s are not deleted.

### IC tasks

`IC` tasks initiated while a pseudo-conversational task is in progress.

> [!NOTE]
> It is unwise to change the screen in a way that would affect a
> mapping operation. e.g. don't add any unprotected fields or overlay
> any existing attributes.

#### IC tasks that end with EXEC CICS RETURN

e.g. message broadcasters

The pseudo-conversational task should continue to operate correctly.

#### IC tasks that end with EXEC CICS RETURN TRANSID()

The original pseudo-conversational task will no longer operate. Any
`COMMAREA` built by that task will be freed.

The next task initiated will be the `TRANSID` specified.

#### IC tasks that end with EXEC CICS RETURN TRANSID() COMMAREA()

The original pseudo-conversational task will no longer operate. Any
`COMMAREA` built by that task will be freed.

The next task initiated will be the `TRANSID` specified and will be passed
the `COMMAREA` specified.

## ENQ/DEQ Management

### ENQ

The module `LCL1204` validates the length.
Although an omitted `LENGTH` is accepted, it may not work in zCICS.

The ENQ command parameters are then shipped to the Server.

Server processing `GBL1204`

- The `QEA` chain is scanned.
- If a `QEA` is found with the same resource, length and originating termid,
  then the use count (`QEAUCT`) is incremented.
- If no `QEA` is found with the same resource and length, then a new `QEA` is
  built and chained.
- If a QEA is found with the same resource and length but a different
  originating termid, then a potential suspend has occurred.
  - Please refer to the matrix in `GBL1204` at label `KCENQBSY` for the
    complete logic of the `ENQBUSY` condition.
  - The Manual omits one description, when `HANDLE CONDITION ENQBUSY()`
    is not specified, `NOSUSPEND` is specified and there is no
    `NOHANDLE`, then `NOHANDLE` must be forced as the task can never
    abend `ENQBUSY` (there is no abend code).
- When a task is suspended the `THRDCNTL` entry for this terminal is marked
  `SUS-ENQ` and the `QEA` is flagged with the termid suffix (`QEAWAIT`).
- A task is suspended because we don't respond to the shipped ENQ
  request; this is done by the DEQ process.

### DEQ

The module `LCL1206` validates the length.
The DEQ command parameters are then shipped to the Server.

Server processing `GBL1206`

- The `QEA` chain is scanned.
- If a QEA is found with the same resource, length and originating termid
  - The use count is decremented if not zero. Ok response is returned.
  - If the use count is zero, and no tasks are waiting for this resource, then
    the `QEA` is released and the chain repaired.
    Ok response is returned.
  - If the use count is zero, and tasks are waiting for this resource, then ok
    response is sent to each waiting task.<br />
    The terminal status is reset to `RUNNING`.<br />
    Then the `QEA` is released and the chain repaired.
    Ok response is returned.

### Task end or abend

`Z390KCP` will invoke a special `DEQALL` server request.

Server processing `GBL12FC`

The `QEA` chain is scanned.

If a `QEA` is found with the same originating termid
- If no tasks are waiting for this resource, then the `QEA` is released and
  the chain repaired. Ok response is returned.
- If tasks are waiting for this resource, then ok response is sent to each
  waiting task.<br />
  The terminal status is reset to `RUNNING`.<br />
  Then the `QEA` is released and the chain repaired. Ok response is returned.

## CWA Management

The `CWA` is considered a Global Resource, and is therefore owned and handled by
the Global Manager, `Z390CICS`.

`CWA` size is determined by the `Z390CICS.INI` parameter `CWASIZE=` and the
original 3.5K limit has been extended to 9,999,999 bytes.

The only access to the `CWA` is via `EXEC CICS ADDRESS CWA()`.
This obtains a copy of the `CWA` and ENQ is used to lock any further `CWA` access
for the duration of the task. See later for exceptions.

When the task ends or abends the `CWA` is sent back to the Global Manager for a
refresh, and the natural clean-up process will ensure that the local copy is freed
and DEQ takes place.

There is no programmable access to the refresh/unlock mechanism.

### Exceptions

`EXEC CICS RECEIVE` (conversational) and `EXEC CICS DELAY` are
considered to be long running and therefore each of those processes have a
refresh/unlock and re-acquire mechanism wrapped around them.

## Macros, LCLhhhh modules and Z390LCL - the linkage mechanisms

Before the terminal is created, the module `Z390LCL` is loaded and the address
placed in `TCTTELCL`.

I am going to use `EXEC CICS READ` as an example.

When this macro is issued the parameter list is set up and these instructions are
issued:
```hlasm
         L     R15,TCTTELCL R15=LCL    MODULE INDEXER
         LARL  R1,=A(P0602) R1=LCL     MODULE PARAMETER LIST
         BAKR  0,R15                   STACK REGS AND GO
```

`Z390LCL` has almost all (see later for exceptions) the macro processors linked in.
The processor is located and a direct branch, `BR R15` is used to invoke it.

An `LCLhhhh` module can itself issue an `EXEC CICS` command, even though it's
not really a CICS program. This will just create another stack entry.

When a processor completes, the `PR` will unstack (only `GR2`-`GR14`) and return to
the invoking macro for error processing or return to the invoking application.

### Exceptions

There are three processes that don't return immediately to the invoking
application after completion: `LINK`, `XCTL` and `RETURN`.

These LCL modules are not part of `Z390LCL` and are linked in to each
application that uses them.

When these macros are issued the parameter list is set up and these
instructions are issued:
```hlasm
         LARL  R1,P0E08             R1=PARAMETER LIST
         LARL  R14,P0E08RTN_&SYSNDX RETURN ADDRESS
         LRL   R15,=V(LCL0E08)      R15=EXTERNAL RETURN MODULE ADDRESS
         BR    R15                  GO TO IT
P0E08RTN_&SYSNDX EQU *
```

The processor decides where to go next.

## Non-terminal Support

Tne `INI` parameter `MAX_NONTERMS=n` limits the number of non-terminal tasks
that may be running at the same time.

Each non-terminal in zCICS is really a special type of terminal with a terminal id.
of !!0n. Tests have been placed in various macro processors (LCL modules) to
raise conditions for commands like `SEND` and `RECEIVE` which cannot be
executed in a non-terminal environment.

When a non-terminal task ends or abends, the terminal is shut down.

`CEMT I TER` will display non-terminals with a type of `NONTERM`.

## Sequential Terminal support

Please read the Doc for this feature in zCICS Sequential Terminal Support. This
section does not cover the batch programs `Z390SEQ` or `Z390COMP`.

When the `INI` parameter `SEQ_TERM=YES` is specified then `Z390CICS` will start a
special `CMDPROC` terminal with a terminal id of `SQ01`.
This will reduce the total terminals that may be started to nine.

When `Z390KCP` is invoked for `SQ01`, two QSAM files are opened, one to process
the input streams, and the other to write the output streams to `SEQO0001`. The
`TCTTE` contains the `DCB` addresses and other supporting fields.

The internal `EXEC CICS RECEIVE` in `Z390KCP` and any in user programs read
the next data stream from the input QSAM file.

The internal `EXEC CICS SEND` in `Z390KCP` and any in user programs writes all
data streams to the output QSAM file and displays them on the `SQ01` terminal.

When an input file reaches the end, the file number is incremented and the file is
closed and re-opened.

When all of the input streams are exhausted, the terminal is closed via an
emulated `CEMT S TER OUT` unless the last input data stream was `CEMT P SHU`
(recommended), in which case zCICS is shut down.

## Channel/Container support

This is an extended form of IBM's badly defined facility.

I have ignored the term 'scope' and have allowed both `CHANNEL` and
`COMMAREA` to co-exist. In addition, `START` may have other parameters as well as
`CHANNEL`. In these cases a warning `MNOTE` is issued as the program won't
assemble in the mainframe environment.

When a `PUT` or `MOVE` is issued a `DFHCHAN` block is built. The first one sets the
anchor at `TCTTECAQ`, subsequent channels are chained via `CHANADDR`.

The first container is chained from `CHANCONT` and each has a prefix
(`DFHCONT`). Subsequent containers are chained from `CONTADDR`.

For `GET` with `SET`, a copy of the container is made and chained from `CONTSET`,
if any copies have been made before (`CONTSET` has an address), then it is freed
before a new copy is made.

For `DELETE`, the container and any `SET` are freed and the chain is repaired.

A `PUT` to an existing container will do a `DELETE` first and then a `PUT`.
Container data is never overlaid (this is not well described).

In zCICS a channel belongs to the link-level that created it (`CHANLINK`), therefore
when a `RETURN` is issued all channels/containers belonging to that link-level are
deleted. A special `DELETE` is used internally to delete a channel and all of its
containers: `EXEC CICS DELETE CONTAINER('*') CHANNEL(name)`

If a `RETURN CHANNEL(name)` returns to zCICS, all channels are deleted except
the one named in the `RETURN`.

When a channel is passed via `LINK`, `XCTL`, `RETURN` or `START` the name is stored
in `TCTTECHN`. The program receiving the passed channel can access it via
`ASSIGN CHANNEL(name)` and then `GET` etc. The passed channel name is also
stored in that program's `DSA` (`DFHEICHN`) so that it can be restored if control is
passed back (another thing not well defined).

For further information about the mechanism used to pass a channel on the `START`
command see [Interval Control Management](#interval-control-management).

## Browse operation

When a `STARTBROWSE` is issued a `DFHCHWA` block is built. The first one sets
the anchor at `TCTTECHW`, subsequent blocks are chained via `CHWAADDR`.

A fullword counter at `TCTTETKN` is incremented and this is used as the
`BROWSETOKEN`, stored at `CHWATOKN` and returned to the requestor.

Because the creation and deletion of containers is dynamic, browsing operations
may lead to container names being returned that no longer exist when a `GETNEXT`
is issued. The last container name retrieved is stored at `CHWACONT`, if this is not
found or if the channel no longer exists then the `GETNEXT` will raise the `END`
condition.

`ENDBROWSE` will delete the `DFHCHWA` and repair the chain.

## CEDF operation

In this chapter `DON0` is used as the terminal to receive the intercepts and `DON1`
as the terminal running the transaction being monitored. Any two terminals can be
used.

### Starting and Stopping CEDF

The processing for start and stop is in `Z390CEDF`.

#### Starting

On `DON0` the transaction `CEDF DON1,ON` is entered.

This causes an `EXEC CICS START TRANSID('CEDZ')` with data to be sent to
`DON1`. The data contains the invoking termid (`DON0`) and the parameter `ON`.

A confirmation message is sent to `DON0`.

When `CEDZ` is invoked on `DON1`, the invoking termid is saved in `TCTTEEDT`
and the `CEDF` flag is set (`TCTTEEDF=X'FF'`). A confirmation message is
displayed.

#### Stopping

During a `CEDF` session, PF3 can be pressed.

See the [intercept operation](#interception-operation).

At task termination, clear the screen, enter `CEDF DON1,OFF`
in a similar manner to starting, `EXEC CICS START TRANSID('CEDZ')` with
data is sent to `DON1`. The data contains the invoking termid (`DON0`) and the
parameter `OFF`.

When `CEDZ` is invoked on `DON1`, `TCTTEEDT` and `TCTTEEDF` are cleared.
No confirmation message is displayed as it may interfere with the display on `DON1`.

The redisplay `TS` queue `--CEDF--` is deleted.

#### Command Interception

The `CEDF` interception module `LCLCEDF` is loaded when the terminal
environment is created and the address is stored in `TCTTEEDA`.
Each `EXEC CICS` command is associated with an LCL module.
See [LCL Submodules for EXEC CICS processing](#lcl-submodules-for-exec-cics-processing) for a full list.
Within the module there is an interception point both before and after the command processing.

`LCLCEDF` is not invoked if `EDF` is off, or if the command has specified `NOEDF`.
`TCTTEEBA` is set to `X'00'` (before) or `X'FF'` (after).

The indicator in `TCTTEEDL` specifies the type of linkage and parameter list
location:
- `X'00'` Conventional invocation with GR3 pointing to the parameter list
- `X'FE'` Entry from `DFHEIENT` for the `PROGRAM INITIATION` intercept.
  The parameter list comes from `LINK`/`XCTL` and is stored inside `DFHEIENT`
  it is then extracted using the `ESTA` instruction and moved to `GR3`.
- `X'FF'` Direct Linkage from `LINK`, `XCTL` or `RETURN`.
  The parameter list pointer is passed in `GR1` and moved to `GR3`

In `LCLCEDF` a table (`EDFTABL`) is scanned and the routine associated with the
command is invoked.

The `EDF` mapset `MAPEDF` contains all the maps used in the interception.
`GR4` is used as the map structure base.
The intercept map is built and a `GETMAIN` is done to hold:
- The map structure
- `DSA` address and lengths
- The current `DSA`
- Additional data for pagable intercepts

For intercepts that may require paging, the data is not set into the map in
`LCLCEDF` but sent to `Z390CEDF` and built there.

An `EXEC CICS START TRANSID('CEDZ')` then passes this data to `DON0` with the
`SEND` parameter and the function code of the command.

The task is then suspended using `EXEC CICS DELAY HOURS(1)` with a `REQID` of
`!!CEDF!!`. The time period should be large enough for normal operation.

This `DELAY` is canceled using the `REQID` when the user wishes to continue the
task.

#### Interception operation

`CEDZ` is started on `DON0` and the parameters and data are retrieved.

On first entry, PF2 is preset as the first input key and is used as a reset function.

After the transaction id. has been entered on `DON1`, the `PROGRAM INITIATION`
intercept is invoked and the switch (focus) to `DON0` occurs.

A table (`KEYTAB`) containing all valid keypresses and function codes is scanned
and the processing routine invoked.

Subsequent keypresses cause a rescan of `KEYTAB`.

An exception to this is PF3 which has special handling:

- A special `EXEC CICS CANCEL` is issued using an internal parameter `EDFOFF`.
- This not only cancels the `DELAY` that has suspended the task, but also turns off
  `CEDF` on `DON1` thus allowing the task to continue normally.
- A message is displayed on `DON0` confirming `CEDF` has ended.

Most commands just display the interception screen as it was sent, exceptions are
discussed later. If further modification of the display screen is possible then the key
operation lines at the bottom are set up. Only those keys which can be seen are
active.

PF2: Mode change

- If shown, then there are fields displayed which can have valid EBCDIC, ASCII or
  hex content. Pressing PF2 will cycle through the modes and is controlled by the
  field `CURRMODE`. `R` is used as the reset, then `E`,`A`,`H` accordingly.

PF5: Working Storage

- Except at task termination, it is always available.
- A dump display is shown in EBCDIC and hex, paging keys are activated if
  appropriate and PF2 can be used to switch between EBCDIC and ASCII.
- The Working Storage mode is independent of the intercept mode.
- Pressing ENTER will return to the intercept display.

PF7,8,10,11: Paging keys

- For some commands, there are too many parameters to display on one screen,
- Paging keys are displayed as appropriate.

PF12: Redisplay Mode and Redisplay Paging

- There is no 'screen save' function, all command environments are saved to the
  `TS` queue `--CEDF--`.
- When the initialization process is done the `TS` record is written and a switch
  (`EDFRDYWQ`) is set to prevent multiple writes. This switch is reset when the
  user presses ENTER to continue the task.
- The screen is modified for Redisplay Mode by showing navigation keys which
  allow +/-1 and +/-5 commands. Also an input field appears top right to go directly
  to a specific screen - typing a large number like 999 will go to the earliest
  intercept.
- Pressing PF5 while in Redisplay Mode will display the Working Storage as it was
  when the command was intercepted.
- Pressing ENTER will return to Redisplay Mode with the Redisplay keyset.
- If PF2 is displayed then a mode change is possible and works identically to
  intercept mode described above. The control switch (`REDPMODE`) is different.

Redisplay Mode and Paging

As described above, some commands have too many parameters to display on
one screen. When this happens in Redisplay Mode, `PF12:PAGING KEYS`
appears and is used to switch between the Redisplay keyset and the Paging
keyset.

## Event Tracing

Tracing is currently limited to those events that the server knows about. These
events appear on the log as `WTO` messages.

The level of tracing is controlled by the `INI` parm `TRACE_Z390CICS=`.

There is an intent to provide full application tracing, but each event would have to
be sent to the server and may be too great an overhead in this environment.

## LCL Submodules for EXEC CICS processing

| Module  | Process               | Notes              |
|---------|-----------------------|--------------------|
| LCL0202 | ADDRESS               | Embedded EXEC CICS |
| LCL0204 | HANDLE CONDITION      |                    |
| LCL0206 | HANDLE AID            |                    |
| LCL0208 | ASSIGN                |                    |
| LCL020A | IGNORE CONDITION      |                    |
| LCL020C | PUSH HANDLE           |                    |
| LCL020E | POP HANDLE            |                    |
|         |                       |                    |
| LCL0402 | RECEIVE               | Embedded EXEC CICS |
| LCL0404 | SEND                  |                    |
|         |                       |                    |
| LCL0602 | READ                  | Embedded EXEC CICS |
| LCL060C | STARTBR               | Embedded EXEC CICS |
| LCL060E | READNEXT              | Embedded EXEC CICS |
| LCL0610 | READPREV              | Embedded EXEC CICS |
| LCL0612 | ENDBR                 | Embedded EXEC CICS |
| LCL0614 | RESETBR               | Embedded EXEC CICS |
|         |                       |                    |
| LCL0A02 | WRITEQ TS             | Embedded EXEC CICS |
| LCL0A04 | READQ TS              | Embedded EXEC CICS |
| LCL0A06 | DELETEQ TS            | Embedded EXEC CICS |
|         |                       |                    |
| LCL0C02 | GETMAIN               |                    |
| LCL0C04 | FREEMAIN              |                    |
|         |                       |                    |
| LCL0E02 | LINK                  | Direct linkage     |
| LCL0E04 | XCTL                  | Direct linkage     |
| LCL0E06 | LOAD                  |                    |
| LCL0E08 | RETURN                | Direct linkage     |
| LCL0E0A | RELEASE               |                    |
| LCL0E0C | ABEND                 |                    |
| LCL0E0E | HANDLE ABEND          |                    |
|         |                       |                    |
| LCL1002 | ASKTIME               |                    |
| LCL1004 | DELAY                 |Embedded EXEC CICS  |
| LCL1008 | START                 |Embedded EXEC CICS  |
| LCL100A | RETRIEVE              |Embedded EXEC CICS  |
| LCL100C | CANCEL                |                    |
|         |                       |                    |
| LCL1204 | ENQ                   |                    |
| LCL1206 | DEQ                   |                    |
|         |                       |                    |
| LCL1802 | RECEIVE MAP           |                    |
| LCL1804 | SEND MAP              |                    |
| LCL1812 | SEND CONTROL          | Embedded EXEC CICS |
|         |                       |                    |
| LCL1C02 | DUMP                  |                    |
|         |                       |                    |
| LCL3412 | DELETE CONTAINER      |                    |
| LCL3414 | GET CONTAINER         |                    |
| LCL3416 | PUT CONTAINER         |                    |
| LCL3440 | MOVE CONTAINER        |                    |
| LCL9626 | STARTBROWSE CONTAINER |                    |
| LCL9628 | GETNEXT CONTAINER     |                    |
| LCL962A | ENDBROWSE CONTAINER   |                    |
|         |                       |                    |
| LCL4A02 | ASKTIME ABSTIME       |                    |
| LCL4A04 | FORMATTIME            |                    |
|         |                       |                    |
| LCL4C02 | INQUIRE FILE          |                    |
| LCL4C04 | SET FILE              |                    |

## Z390CEMT submodules

`Z390CEMT` handles all the `CEMT` functions.

Most functions have been split into submodules.

| Module   | Function            |
|----------|---------------------|
| CEMTFILE | INQUIRE/SET FILE    |
| CEMTIENQ | INQUIRE ENQUEUE     |
| CEMTISYS | INQUIRE SYSTEM      |
| CEMTITER | INQUIRE TERMINAL    |
| CEMTITRN | INQUIRE TRANSACTION |

## Z390CICS Operation

- The file `Z390CICS.INI` is opened and the parameters analyzed and used to
  set fixed fields in the program.<br />
- Note: `CEMT I SYS` can be used to display them.
- A Command Prompt is started with the correct directory and parameters for
  each local terminal requested and Z390KCP is invoked in each one.
- A 32K receive area is acquired
- The Server port is opened.
- The `FCT` is loaded and any files eligible to be opened immediately are opened.
- At `READLOOP` a `TCPIO RECEIVE` is done. The process is slightly different
  when in shutdown mode (see later).
  - `Z390CICS` will wait here for a request sent by any client.
  - When a request is received the identity of the Client is return in `GR2`.
  - Occasionally more than one request is received at the same time (batched),
    these are identified and the messages are split and individually processed.
  - `REQTABLE` is then scanned for the requested process and the routine is invoked.
  - Some routines are handled within `Z390CICS` and others will `CALL` a `GBL`
    submodule. For a list, see below.
  - After the process is complete a `TCPIO SEND` is done with return codes and
    data (if requested) to the Client and the program returns to `READLOOP`.
- Shutdown processing
  - After a Client has issued `CEMT P SHU (IMM)` a flag (`SHUTIND`) is set. The
    alternate code at `READLOOP` is then invoked which does a `TCPIO RECEIVE,NOWAIT`
    and checks every second if all Clients have closed.
  - Then all open VSAM files are closed and the Server (`Z390CICS`) shuts down.

## GBL Submodules

| Module  | Description           | Program  |
|---------|-----------------------|----------|
| GBL0602 | READ                  | Any      |
| GBL060C | STARTBR               | Any      |
| GBL060E | READNEXT              | Any      |
| GBL0610 | READPREV              | Any      |
| GBL0612 | ENDBR                 | Any      |
| GBL0614 | RESETBR               | Any      |
|         |                       |          |
| GBL0A02 | WRITEQ TS             | Any      |
| GBL0A04 | READQ TS              | Any      |
| GBL0A06 | DELETEQ TS            | Any      |
| GBL0AFF | CEBR Request Qnames   | Z390CEBR |
|         |                       |          |
| GBL1004 | DELAY                 | Any      |
| GBL1008 | START                 | Any      |
| GBL100C | CANCEL                | Any      |
| GBL10FF | ICE SCAN              | Z390KCP  |
|         |                       |          |
| GBL1204 | ENQ                   | Any      |
| GBL1206 | DEQ                   | Any      |
| GBL12FC | DEQALL                | Z390KCP  |
|         |                       |          |
| GBL4C02 | INQUIRE FILE          | Any      |
| GBL4C04 | SET FILE              | Any      |
|         |                       |          |
| GBLFE00 | CEMT I TERm           | Z390CEMT |
| GBLFE01 | CEMT I SYStem         | Z390CEMT |
| GBLFE06 | CEMT I ENQueue        | Z390CEMT |
| GBLFE07 | CWA GET/PUT           | Any      |

## Copy Books

| Name     | Purpose               | Program  |
|----------|-----------------------|----------|
| FILEERTB | VSAM Error Code Table | Z390CICS |

## Internal Abends

These are mostly caused by programming errors or situations that were not anticipated.
If you encounter any of them, please report them to the z390 support team.

| Abend code | Meaning                                       |
|------------|-----------------------------------------------|
| 444        | Abend code missing from the table in Z390CICS |
| 555        | Unknown request sent to Server                |
| 666        | CMDPROC failed                                |
| 777        | TCPIO OPEN/CLOSE Server failed                |
| 778        | TCPIO RECEIVE failed                          |
| 780        | TCPIO SEND failed                             |
| 790        | VSAM feedback code was not expected           |

## Change Summary

- February 1, 2012
  - Abend code 444 added
  - Seven new LCL Modules added for Channel/Container support
  - Control blocks DFHCHAN, DFHCONT and DFHCHWA added
  - Added management description for Channels/Containers
  - Added description of START with CHANNEL to the IC management section
- June 10, 2011
  - GBLFE05 renamed to GBL4C02
  - GBLFE08 renamed to GBL4C04
  - Removed duplicate paragraph about DELETEQ TS
  - Added section on CEDF
- November 1, 2010
  - Update to LCL1812
  - Section on Non-terminal support added
  - Change to description of GBLFE05
  - Added LCL4C04 and GBLFE08 (SET FILE)
  - Added section on Z390CEMT submodules
  - Added GBL1004 (DELAY)
- August 1, 2009
  - References to DFHEIRET removed
  - References to DFHEIPRM removed, XCTL updated
  - Added LCL0208, LCL 4C02
  - Amended Z390LCL processing
- February 21, 2009
  - Added LCL0202, GBLFE07
  - Added DFHCWBLK and CWA Management
  - Added Macro and LCL processor Management
- November 24, 2008
  - Added section on LCL submodules
  - Added control blocks:
  - DFHICBLK, DFHICEDS, DFHTSNDS, DFHKCBLK, THRDDSCT,
  - DFHQEADS
  - Added Interval Control Management
  - Added ENQ/DEQ Management
  - Completed Z390CICS operation
  - GBLFE06 added (CEMT I ENQ)
- June 27, 2008
  - Z390CICS operation extensively expanded
- January 18, 2008
  - Extensive updates to File Control
