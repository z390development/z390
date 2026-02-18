# zVSAM V2 - Logical processes for ACB-based requests

This document describes the file structure for implementing zVSAM V2 data sets.
It contains the following major chapters:

1. [ACB](#ACB)
2. [GENCB ACB](#GENCB-ACB)
3. [MODCB ACB](#MODCB-ACB)
4. [SHOWCB ACB](#SHOWCB-ACB)
5. [TESTCB ACB](#TESTCB-ACB)
6. [EXLST](#EXLST)
7. [GENCB EXLST](#GENCB-EXLST)
8. [MODCB EXLST](#MODCB-EXLST)
9. [SHOWCB EXLST](#SHOWCB-EXLST)
10. [TESTCB EXLST](#TESTCB-EXLST)
11. [Open](#Open)

## ACB

## GENCB ACB

## MODCB ACB

## SHOWCB ACB

## TESTCB ACB

## EXLST

## GENCB EXLST

## MODCB EXLST

## SHOWCB EXLST

## TESTCB EXLST

## Open

Open logic has two major components: the open macro and the actual run-time logic to execute a request to open a file or a number of files.

### Open macro logic

The open macro generates an open/close parameter list and/or an SVC 19 instruction to invoke the open routine.
The syntax of the open macro is given in [OPEN macro](zVSAM_V2_Design_Interfaces.md#OPEN-macro)

The macro generates the following code:

| MF variant | Generated Code                                                                                                                          |
|------------|-----------------------------------------------------------------------------------------------------------------------------------------|
| MF=L       | Open/close parameter list data only                                                                                                     |
| MF=E       | 1) Code to modify/populate the open/close parameter list at the indicated address, which may be a relocatable constant or a (register). |
|            | 2) Code to invoke the open routine with R1 pointing to the open/close parameter list.                                                   |
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
| loop over all entries in the parameter list | End-of-list is indicated in the option byte of the entry               |
| - check pointer: ACB or DCB                 | First byte = X'A0' => ACB V1                                           |
|                                             | First four bytes = C'zACB' => assume ACB                               |
|                                             | Otherwise => Assume DCB                                                |
| - if DCB invoke DCB open routine            |                                                                        |
| - if ACB validate ACB                       | `ACBID` <> X'A0' => Error                                              |
|                                             | `ACBSTYP` <> X'10' => Error                                            |
|                                             | `ACBVER` <> X'02' => Error                                             |
| - if ACB valid invoke VSAM open routine     |                                                                        |
| - next entry or end-of-loop                 | If bit 0 of OCPL entry is on, terminate loop                           |


























