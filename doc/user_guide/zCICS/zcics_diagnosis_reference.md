# zCICS Diagnosis Reference

## Client Control Blocks

### Dynamic Storage Area (DSA)

|-------------|------------------------------------|
| Eye catcher | None                               |
| Acquired    | DFHEIENT                           |
| Released    | RETURN at highest level XCTL       |
| Anchor      | R13                                |
| DSECT       | DFHEISTG (prefix only)             |
| Cleared     | Only the prefix, not the user area |
| Length      | Variable                           |

### EXEC Interface Block (EIB)

|-------------|------------------------------------|
| Eye catcher | 'DFHEIBLK'                         |
| Acquired    | Z390KCP                            |
| Released    | Close of thread                    |
| Anchor      | R11 (DFHEIBR)                      |
| DSECT       | DFHEIBLK                           |
| Cleared     | Yes                                |
| Length      | EIBLENG                            |

### HANDLE ABEND Block

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

|-------------|------------------------------------|
| Eye catcher | None                               |
| Acquired    | Z390KCP                            |
| Released    | Close of thread                    |
| Anchor      | TCTTELKA                           |
| Cleared     | Yes                                |
| Length      | 4 (R13 value for this link-level)  |
| Entries     | 25                                 |

### Terminal Control Table Terminal Entry (TCTTE)

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

|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | GETMAINd by an EXEC CICS file control command  |
| Released    | FREEMAINd by an EXEC CICS file control command |
| Anchor      | None                                           |
| DSECT       | DFHFCBLK                                       |
| Cleared     | Yes                                            |
| Length      | FCPREFIX                                       |

### Interval Control request/reply block (DFHICBLK)

|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | GETMAINd by an EXEC CICS START/CANCEL command  |
| Released    | At task end                                    |
| Anchor      | None                                           |
| DSECT       | DFHICBLK                                       |
| Cleared     | Yes                                            |
| Length      | ICPREFIX                                       |

### Task Control request/reply block (DFHKCBLK)

|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | GETMAINd by an EXEC CICS ENQ/DEQ command       |
| Released    | At task end                                    |
| Anchor      | None                                           |
| DSECT       | DFHKCBLK                                       |
| Cleared     | Yes                                            |
| Length      | KCPREFIX                                       |

### CWA request/reply block (DFHCWBLK)

|-------------|------------------------------------------------|
| Eye catcher | None                                           |
| Acquired    | Embedded in LCL0202                            |
| Released    | N/A                                            |
| Anchor      | None                                           |
| DSECT       | DFHCWBLK                                       |
| Cleared     | No                                             |
| Length      | CWPREFIX                                       |

### Channel control block (DFHCHAN)

|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | EXEC CICS PUT/MOVE (LCL3416/LCL3440)                 |
| Released    | EXEC CICS DELETE (LCL3412), link-level end, task end |
| Anchor      | TCTTECAQ                                             |
| DSECT       | DFHCHAN                                              |
| Cleared     | Yes                                                  |
| Length      | 26                                                   |

### Container control block (DFHCONT)

|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | EXEC CICS PUT/MOVE (LCL3416/LCL3440)                 |
| Released    | EXEC CICS DELETE (LCL3412), link-level end, task end |
| Anchor      | CHANCONT (DFHCHAN)                                   |
| DSECT       | DFHCONT (in DFHCHAN)                                 |
| Cleared     | Yes                                                  |
| Length      | 28+Container length                                  |

### STARTBROWSE CONTAINER control block (DFHCHWA)

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

|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | Z390CICS at start                                    |
| Released    | Never                                                |
| Anchor      | DFHFCTAD                                             |
| DSECT       | DFHFCTDS                                             |
| Length      | FCTABLEN                                             |

### VSAM Work Area (DFHVSWAD)

Also contains the RPL for file operations.

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

|-------------|------------------------------------------------------|
| Eye catcher | None                                                 |
| Acquired    | Fixed in Z390CICS                                    |
| Released    | Never                                                |
| Anchor      | THRDCNTL                                             |
| DSECT       | THRDDSCT                                             |
| Cleared     | Yes                                                  |
| Length      | THRDLEN                                              |

### Queue Element Area (DFHQEADS)

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











