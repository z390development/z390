# zVSAM V2 - Logical processes for ACB-based requests

This document describes the logical processes for ACB-based requests to be implemented in zVSAM V2.

## Open

Open logic has two major components: the open macro and the actual run-time logic to execute a request to open a file or a number of files.

Open parameter list entries have two different formats depending on the MODE parameter.
- When MODE=24 then each entry is one fullword
- When MODE=31 then each entry is two fullwords

Only one SVC 19 is generated for each OPEN macro (MF=I or E)

A header precedes the list as follows:
- DC C'zOPC'
- DC AL2(no. of entries) The senior bit is off for MODE=24 and on for MODE=31

The list format and input to OPEN depend on MODE=

R1 points to the header

- MODE=24 AL1(option),AL3(DCB/ACB address)
- MODE=31 AL1(option),XL3'00',AL4(DCB/ACB address)

- option=X'40' INPUT
- option=X'20' OUTPUT
- option=X'60' UPDATE

The last entry has the X'80' bit on in option.
The option is ignored when opening an ACB.

### Open macro logic

The open macro generates an open/close parameter list and/or an SVC 19 instruction to invoke the open routine.
The syntax of the open macro is given in [OPEN macro](zVSAM_V2_Design_Interfaces.md#open-macro).

The macro generates the following code:

| MF variant | Generated Code                                                                                                                          |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| MF=L       | Open/close parameter list data only                                                                                                     |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| MF=E       | 1) Code to modify/populate the open/close parameter list at the indicated address, which may be a relocatable constant or a (register). |
|            | 2) Code to invoke the open routine with R1 pointing to the open/close parameter list.                                                   |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| MF omitted | 1) Open/close parameter list data (inline)                                                                                              |
|            | 2) Code to invoke the open routine with R1 pointing to the open/close parameter list.                                                   |

When constructing the open/close parameter list, the end-of-list indicator of the last entry is set.

Open/close parameter list entries have two different formats. The one being used depends on the current addressing mode as indicated in the &MODE parameter.

- When &MODE=24 then each entry is one fullword on a fullword boundary.
- When &MODE=31 then each entry is two fullwords on a fullword boundary.

| Label    | Offset | Field type | Function                        |
|----------|--------|------------|---------------------------------|
| OCPL     |        | DSECT      | Open/Close Parameter List       |
| OC24     | X'000' | XL4        | Entry for &MODE=24              |
| OC24OPT  | X'000' | XL1        | Option byte (ignored for ACB)   |
| OC24_EOL |        | =X'80'     | End-of-list indicator           |
| OC24DCB  | X'001' | AL3        | Pointer to DCB or ACB           |
|          |        |            |                                 |
| OC31     | X'000' | XL8        | Entry for &MODE=31              |
| OC31OPT  | X'000' | XL1        | Option byte (ignored for ACB)   |
| OC31_EOL |        | =X'80'     | End-of-list indicator           |
| OC31NULL | X'001' | XL3        | Reserved, should contain zeroes |
| OC31DCB  | X'004' | AL4        | Pointer to DCB or ACB           |

To invoke the open execution logic SVC 19 (X'13') is used.
The type of parameter list is indicated as follows:
- if R1 is zero, then R0 contains a pointer to a list of OC31 entries
- otherwise R1 contains a pointer to a list of OC24 entries.

### Open execution logic

Open execution logic is implemented as a Java routine.

This logic consists of the following elements:

| Action                                      | Details                                                                |
|---------------------------------------------|------------------------------------------------------------------------|
| determine type of parameter list            | The list consists solely of OC31 entries, addressed by R0, if R1 = 0.  |
|                                             | The list consists solely of OC24 entries, addressed by R1, if R1 <> 0. |
|---------------------------------------------|------------------------------------------------------------------------|
| loop over all entries in the parameter list | End-of-list is indicated in the option byte of the entry               |
|---------------------------------------------|------------------------------------------------------------------------|
| - check pointer: ACB or DCB                 | First byte = X'A0' => ACB V1                                           |
|                                             | First four bytes = C'zACB' => ACB V2                                   |
|                                             | Otherwise => Assume DCB                                                |
|---------------------------------------------|------------------------------------------------------------------------|
| - if DCB invoke DCB open routine            | OPEN logic for DCB is beyond the scope of this document                |
|---------------------------------------------|------------------------------------------------------------------------|
| - if ACB validate ACB                       | `ACBID` <> X'A0' => Error                                              |
|                                             | `ACBSTYP` <> X'10' => Error                                            |
|                                             | `ACBVER` <> X'02' => Error                                             |
|---------------------------------------------|------------------------------------------------------------------------|
| - if ACB valid invoke VSAM open routine     |                                                                        |
|---------------------------------------------|------------------------------------------------------------------------|
| - next entry or end-of-loop                 | If bit 0 of OCPL entry is on, terminate loop                           |
|---------------------------------------------|------------------------------------------------------------------------|

> [!NOTE]
> Melvyn suggested to check pointers for a value of 'DCBV' in the first four bytes. Any other value to cause an error.
> Since that would impact the non-VSAM macros for DCB handling, I doubt this is feasible.
> To be investigated and evaluated.

Open logic for ACB handles a single ACB and proceeds as follows:

| Action                            | Details                                                                              |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Check ACB status                  | If ACB already open, issue error and fail open                                       |
                                    | Set ACBIOSFG                                                                         |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Copy ACB to newly created FCB     | FCB is the java-equivalent of the ACB                                                |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Extract DDNAME                    | Copy `ACBDDNAM` field from ACB/FCB                                                   |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Find actual file name             | Retrieve host variable with name matching `ACBDDNAM` content                         |
|                                   | if not available: issue error and fail open                                          |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Validate against catalog          | Find the file name in the catalog                                                    |
|                                   | if missing: issue error                                                              |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Issue OS open against file        | Read-only if ACB specifies `MACRF=IN`                                                |
|                                   | for input/update/extend otherwise.                                                   |
|                                   | If unsuccessful issue error and fail open                                            |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Allocate buffer for prefix block  | Save buffer address in FCB                                                           |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Read first 4096 bytes into buffer | If read fails, issue error                                                           |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Validate block header and footer  | if `BHDREYE` <> C'HDR' issue error                                                   |
|                                   | if `BFTREYE` <> C'FTR' issue error                                                   |
|                                   | if `BHDRSEQ#` <> `BFTRSEQ#` issue error                                              |
|                                   | if `BHDRVER` <> X'02' issue error                                                    |
|                                   | if `BHDRSELF` <> foxes issue error                                                   |
|                                   | if `BHDRPREV` <> foxes issue error                                                   |
|                                   | if `BHDRNEXT` <> foxes issue error                                                   |
|                                   | if `BHDRFLGS` <> X'80' issue error                                                   |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Validate prefix area              | if `PFXEYE` <> C'zPFX' issue error                                                   |
|                                   | if filename <> `PFXDNAM` issue error                                                 |
|                                   | if file's path <> `PFXDPAT` issue error                                              |
|                                   | if `PFX_INDX` is on issue error                                                      |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Validate counters area            | if `CTREYE` <> C'zCTR' issue error                                                   |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Validate prefix against catalog   | Only if no errors detected thus far:                                                 |
|                                   | compare cluster type                                                                 |
|                                   | compare lrecl                                                                        |
|                                   | compare blocksize                                                                    |
|                                   | compare key offset                                                                   |
|                                   | compare key length                                                                   |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Fail open on error                | If any error was detected:                                                           |
|                                   | - request OS to close the file                                                       |
|                                   | - free the prefix buffer                                                             |
|                                   | - set buffer pointer in FCB to zeros                                                 |
|                                   | - fail the open request                                                              |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Issue OS open against index file  | If `PFXXNAM@` is non-zero then open the indicated index file                         |
|                                   | readonly if ACB `MACRF=IN` for input/update/extend otherwise                         |
|                                   | for input/update/extend otherwise.                                                   |
|                                   | Read index header block and repeat all validations with the following modifications: |
|                                   | - if `PFX_INDX` is off rather than on issue an error                                 |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Fail open on error                | If any error was detected:                                                           |
|                                   | - request OS to close the files                                                      |
|                                   | - free the prefix buffers                                                            |
|                                   | - set buffer pointer in FCB to zeroes                                                |
|                                   | - fail the open request                                                              |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Create data buffers               | Based on `ACBBUFND`                                                                  |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Create index buffers              | Only if index is present                                                             |
|                                   | At least `ACBBUFNI` in total                                                         |
|                                   | Exactly one for the root block                                                       |
|                                   | At least 4 for each other index level                                                |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Open base cluster                 | If DDNAME refers to a path, repeat the open process for the base cluster.            |
|                                   | File names and other info to be gathered from the path's catalog entry.              |
|                                   | This basically repeats the steps above.                                              |
|                                   | Create an FCB – even though there is no ACB – to represent the cluster.              |
|-----------------------------------|--------------------------------------------------------------------------------------|
| Update ACB on OPEN completion     | Set ACBIOSFG off                                                                     |
|                                   | Set ACBOPEN on                                                                       |
|                                   | Set addresses for ACBPFX, ACBXPFX, ACBBUFD, ACBBUFI                                  |
|                                   | Set ACBDTYPE:                                                                        |
|                                   | - If MACRF=AIX is specified, then ACB_AIX and ACB_BASE are set on                    |


> [!NOTE]
> The environment variables take the following form (in a Windows environment)
> SET ddname=drive:\path\catalog.filename \
> SET catalog=drive:\path \
> The ddname variable may only contain one dot

### Implied OPEN table

This table has the permutations of component types, indented entries are implied processing.

    | Open component          | MACRF=IN                                       | MACRF=OUT                                      |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | Base                    | Opened for input                               | Opened for in/out                              |
    | - AIXs (UPGRADE=NO)     | Not opened                                     | Not opened                                     |
    | - AIXs (UPGRADE=YES)    | Not opened                                     | Opened for in/out                              |
    |                         |                                                | See note 3                                     |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (NOUPDATE) to Base | Opened for input; No error if already open     | Opened for in/out; No error if already open    |
    | - AIXs (UPGRADE=NO)     | Not opened                                     | Not opened                                     |
    | - AIXs (UPGRADE=YES)    | Not opened                                     | Not opened                                     |
    |                         |                                                | See note 1                                     |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (UPDATE) to Base   | Opened for input; No error if already open     | Opened for in/out; No error if already open    |
    | - AIXs (UPGRADE=NO)     | Not opened                                     | Not opened                                     |
    | - AIXs (UPGRADE=YES)    | Not opened                                     | Opened for in/out                              |
    |                         |                                                | See note 3                                     |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (NOUPDATE) to AIX  | Implied open of Base; No error if already open | Implied open of Base; No error if already open |
    | See Note 4              | See Note 2                                     | See Note 2                                     |
    | - AIXs (UPGRADE=NO)     | AIX opened for input                           | AIX opened for input                           |
    | - AIXs (UPGRADE=YES)    | Not opened; See Note 1                         | Not opened; See Note 1                         |
    |-------------------------|------------------------------------------------|------------------------------------------------|
    | PATH (UPDATE) to AIX    | Implied open of Base; No error if already open | Implied open of Base; No error if already open |
    | See Note 4              | See Note 2                                     | See Note 2                                     |
    | - AIXs (UPGRADE=NO)     | AIX opened for input                           | AIX opened for input                           |
    | - AIXs (UPGRADE=YES)    | Opened for in/out; See Note 3                  | Opened for in/out; See Note 3                  |
    |-------------------------|------------------------------------------------|------------------------------------------------|

*Notes:*
1. A `NOUPDATE PATH` means that the structures for AIXs on the upgrade set are not created
2. The Base is opened by zVSAM for input but has no associated ACB as it hasn't been opened by the application
3. All AIXs on the upgrade set are opened for in/out by zVSAM and may be updated
4. A PATH to an AIX ignores MACRF=IN/OUT

### Exit logic

This logic is only entered if any of the following conditions are raised:
- End-of-data (EODAD)
- Logical error (LERAD)
- Physical error (SYNAD)

| Action                             | Details               |
|------------------------------------|-----------------------|
| ACBEXLST has an address            | No action if zero     |
| Check that the exit is active      | No action if inactive |
| Check that the address is not zero | No action if zero     |
| Branch to the exit address         |                       |

## Close

Upon close the prefix counter field `CTRSTMST` must be updated.

Do we need to include a last-updated timestamp to validate that last update of AIX is not behind the base cluster?

Close is a lot more complex than you realise, you have to check that there are no outstanding block/record locks.

include a last-updated timestamp to validate that last update of AIX is not behind the base cluster?

I'm not sure about the usefulness of timestamps anyway, if you want to maintain them...ok

An update of a base record can occur without affecting any AIX on the upgrade set, so only the timestamp of the base is affected.
When AIX keys are changed then the base timestamp will precede the AIX timestamp(s). Looks like a lot of work for no good purpose.

### CLOSE macro logic

The close macro generates a close parameter list and/or an SVC 20 instruction to invoke the close routine.

The macro generates the following code:

| MF variant      | Generated Code                                                                                                                     |
|-----------------|------------------------------------------------------------------------------------------------------------------------------------|
| MF=L            | Close parameter list data only                                                                                                     |
| MF=(E,address)  | 1) Code to modify/populate the close parameter list at the indicated address, which may be a relocatable constant or a (register). |
|                 | 2) Code to invoke the close routine                                                                                                |
| MF=I or omitted | 1) Close parameter list data (inline)                                                                                              |
|                 | 2) Code to invoke the close routine                                                                                                |

Close parameter list entries have two different formats depending on the MODE parameter.
- When MODE=24 then each entry is one fullword
- When MODE=31 then each entry is two fullwords

Only one SVC 20 is generated for each CLOSE macro (MF=I or E)

A header precedes the list as follows:
- DC C'zOPC'
- DC AL2(no. of entries) The senior bit is off for MODE=24 and on for MODE=31

The list format and input to CLOSE depend on MODE=

R1 points to the header
- MODE=24 AL1(option),AL3(DCB/ACB address)
- MODE=31 AL1(option),XL3'00',AL4(DCB/ACB address)
option=0 except for the last entry when option=X'80'

### CLOSE execution logic

Close involves lock and buffer management and may involve the closure of associated AIXs.

CLOSE execution logic is implemented as a Java routine.
This logic consists of the following elements:

| Action                                      | Details                                                  |
|---------------------------------------------|----------------------------------------------------------|
| Determine type of parameter list            | no.of entries senior bit off, MODE=24                    |
|                                             | no.of entries senior bit on, MODE=31                     |
| Determine if zVSAM V1 or V2                 | 1st 4 bytes <> C'zOPC'=> ZVSAM V1                        |
|                                             | 1st 4 bytes = C'zOPC'=> ZVSAM V2                         |
| loop over all entries in the parameter list | End-of-list is indicated in the option byte of the entry |
| - check : ACB or DCB                        | First byte = X'A0' => ACB V1                             |
|                                             | First four bytes = C'zACB' => ACB V2                     |
|                                             | First four bytes = C'DCBV' => DCB                        |
|                                             | Otherwise => Error                                       |
| - if DCB invoke DCB close routine           | CLOSE logic for DCB is beyond the scope of this document |
| - if ACB valid invoke VSAM close routine    |                                                          |
| - next entry or end-of-loop                 | If bit 0 of an entry is on, terminate loop               |

CLOSE logic for ACB handles a single ACB and proceeds as follows:

| Action                                      | Details                                                                                            |
|---------------------------------------------|----------------------------------------------------------------------------------------------------|
| Check ACB status                            | If ACB already closed, issue error and fail close                                                  |
| Check lock status                           | If any blocks in this dataset or any associated AIX are locked then wait until the locks are freed |
|                                             | ???may need a timeout mechanism                                                                    |
| Check buffer status                         | Free any read buffers                                                                              |
|                                             | Write any buffers marked as 'pending write' and then free them                                     |
| Issue OS close against file                 | If unsuccessful issue error and fail close                                                         |

