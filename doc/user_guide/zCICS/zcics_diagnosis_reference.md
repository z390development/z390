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

















