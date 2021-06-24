
# zCICS Application Programming Guide

## Introduction

The EXEC CICS commands and parameters listed here are the only ones currently
supported in the zCICS environment.

Differences between mainframe operation and zCICS are explained.

For the operation of each command and parameter please refer to the Manuals
listed in the Reference section at the end of this document.

If you create your own zCICS applications, it would be wise to create a .BAT file that
re-assembles them all in one go. The internal interfaces are volatile at present and
this will be a frequent instruction.

The current zCICS environment and all test programs can be re-assembled using
DFHALL.BAT. The test VSAM catalog and files can be rebuilt using DFHALLV.BAT.
Assembly notes

The `CICS` option must be used when executing `mz390` command.

PROLOG and EPILOG are defaults.

NOEPILOG is supported but not fully tested, testing and correct usage is scheduled
for a future release of zCICS.

PROLOG inserts the following:

DFHEISTG
:  Define the prefix areas of the Dynamic Storage Area (DSA).

DFHEIEND
:  Replaces the END statement and defines the end of the DSA.

DFHEIENT
:  Replaces the first CSECT statement
:  Establish linkage and base registers
:  GETMAIN the DSA
:  Establish addressability to the EIB and TCTTE
:  Some COMMAREA management

## Other macros

* DFHEIBLK - EIB DSECT
* DFHPCT - Transaction definition
* DFHFCT – File definitions and options
* EXEC - Converts EXEC CICS statements into a unique macro call with a parameter list
* DFHREGS – A synonym for EQUREGS

## Copy books

* DFHAID - Standard CICS equates for AID keys
* DFHBMSCA - Mapping support equates
* DFHPCTUS - User transaction codes
* DFHFCTUS - User file definitions

Inclusion of the macro DFHREGS/EQUREGS is mandatory.

## Register Usage

* R0 - Reserved for internal use
* R1 - Reserved for internal use
* R10 - TCTTE address, must not be modified
* R11 - EIB address, must not be modified
* R12 - Default base register
* R13 - DSA address, must not be modified
* R14 - Reserved for internal use
* R15 - Reserved for internal use

### Multiple base registers (assembler only)

The standard entry for a CICS program is as follows:
e.g. DFHEISTG DSECT

``` hlasm
MYFIELD  DS    CL100 demo user field
......
MYPROG   CSECT
```

This standard method with the PROLOG option (default) will generate a single code
base of R12 and a single DSA base of R13.

If you want to extend the code base and/or the DSA base registers, convert your
code in line with the sample given and include the `NOPROLOG` option in mz390 command.

e.g. DFHEISTG

```hlasm
MYFIELD  DS    CL100 demo user field
......
MYPROG   DFHEIENT CODEREG=(R8,R5),DATAREG=(R13,R6,R7)
```

!!! Note
    You cannot override the first DATAREG value, it will always be R13.
    i.e. if you code `DATAREG=(R6,R7)` you will get `DATAREG=(R13,R7)`.

There is no cross-checking for register conflicts.

## Assembler EXEC CICS command syntax

There is no formal definition of an EXEC CICS command in any IBM Manual.

These assembler syntaxes are currently supported.

### In-line space separated
```hlasm
name     EXEC  CICS function subfunction parm parm()
```

### In-line comma separated
```hlasm
name     EXEC  CICS function,subfunction,parm,parm(), parm ()
```

### Split, space separated

Non-blank in column 72 is assumed for all lines except the last.

Comments are only allowed following a dot or comma delimiter on the last line.
```hlasm
name     EXEC  CICS function                                          X
         subfunction                                                  X
         parm                                                         X
         parm() parm (). a nice comment
```

### Split, comma separated

Comments are allowed on all lines that end with comma+space.

```hlasm
name     EXEC  CICS function, some                                    X
         subfunction, very                                            X
         parm, nice                                                   X
         parm(),parm (), comments
```

_name_ is supported and optional.

`EXEC CICS` is expected, `EXECUTE CICS` is not currently supported.

`subfunction` is optional and depends on the function but must follow function, 
e.g. `EXEC CICS WRITEQ TS`

`parm()` without spacing and `parm ()` with spacing are allowed.

## VSAM support

See the [zCICS VSAM Guide]() for guidance in the setup of a VSAM environment.

This document also contains extensions to the VSAM facilities currently available.
## Basic Mapping Support
The `EXEC CICS` commands `RECEIVE MAP`, `SEND MAP` and `SEND CONTROL` are documented
here.

For general BMS documentation and the mapping macros DFHMSD, DFHMDI and
DFHMDF see the [zCICS BMS Guide]().

## zCOBOL support

EXEC CICS command format follows the rules for COBOL.

Each command must end END-EXEC (a following dot may affect the logic).

Parameters like `SET()` which address imported structures may use the ADDRESS
OF special register.

`LENGTH`, `FLENGTH` and `KEYLENGTH` which would normally allow a numeric option
may use the `LENGTH OF` special register.

### Extra Parameters

You can add `NOEDF` to any `EXEC CICS` command if you wish that command to be
excluded from a CEDF session.

You can add `NOEDF` to the mz390 command if you wish all CEDF intercepts in that
program excluded.

## Command reference - General Commands

### HANDLE AID

```hlasm
name     EXEC  CICS HANDLE AID key(label) key
```

!!! Warning
    The following parameters are not supported.

    * CLRPARTN
    * LIGHTPEN
    * OPERID
    * TRIGGER

!!! Note
    ANYKEY (no label) clears all settings for CLEAR, PA and PF keys.

#### Parameters

##### key

The key

##### label

_label_ may take three forms:

* Direct reference
* Indirect reference
* Adcon literal

```hlasm
         EXEC  CICS HANDLE AID PA1(GOPA1) PA2(INDGOPA1) PA3(=A(GOPA1))
......
GOPA1    DS    0H
......
INDGOPA1 DC    A(GOPA1)
```

!!! Info
    There is a current limit of 30 AIDs.

#### Errors

* AN INTERNAL ERROR HAS OCCURRED
* NO PARAMETERS SPECIFIED
* NUMBER OF AIDS EXCEEDS 30
* BAD PARM
* HANDLE TYPE NOT RECOGNISED

### HANDLE CONDITION

```hlasm
name     EXEC  CICS HANDLE CONDITION condition(label) condition
```

#### Parameters

##### condition

Name of the condition to handle

##### label

Location in program to branch to when condition occurs.

_label_ may take three forms:

* Direct reference
* Indirect reference
* Adcon literal

```hlasm
         EXEC  CICS HANDLE CONDITION EOF(ISEOF)
......
ISEOF    DS    0H
```

!!! Note
    There is a current limit of 30 conditions.
    `DSIDERR` is supported as a synonym to `FILENOTFOUND`.

#### Errors

* AN INTERNAL ERROR HAS OCCURRED
* NUMBER OF CONDITIONS EXCEEDS 30
* BAD PARM
* HANDLE TYPE NOT RECOGNISED

### IGNORE CONDITION

```hlasm
name     EXEC  CICS IGNORE CONDITION condition
```

!!! Warning
    Ignoring an error may lead to unpredictable abends.
    There is a current limit of 30 conditions.

#### Parameters

INVREQ, PGMIDERR or ERROR by default.

The EXEC CICS command treated as never existed.
INVREQ on EXEC CICS RETURN will abend the task ASRA as z390 cannot ignore a RETURN.

LENGERR or ERROR by default...
NOHANDLE and any outstanding HANDLE AID will not invoke this condition.
DSIDERR is supported as a synonym to FILENOTFOUND.

#### Errors

* BAD PARM
* IGNORE TYPE NOT RECOGNISED
* AN INTERNAL ERROR HAS OCCURRED
* NO PARAMETERS SPECIFIED
* NUMBER OF CONDITIONS EXCEEDS 30

### POP HANDLE

```hlasm
name     EXEC  CICS POP HANDLE
```

For the HANDLE ABEND, a POP is the equivalent of a HANDLE ABEND RESET.

#### Errors

* BAD PARM
* POP TYPE NOT RECOGNISED

#### Conditions (RESP/RESP2)

* INVREQ/0

### PUSH HANDLE

```hlasm
name     EXEC  CICS PUSH HANDLE
```

For the HANDLE ABEND, a PUSH is the equivalent of a `HANDLE ABEND CANCEL`.

#### Errors

* BAD PARM
* PUSH TYPE NOT RECOGNISED

### ADDRESS

```hlasm
name     EXEC  CICS ADDRESS                                           X
                    COMMAREA(label)                                   X
                    CWA(label)                                        X
                    EIB(label)
```

CWA has a different implementation in zCICS.

See [CWA Management in zCICS Diagnosis Reference]() for more information.

#### Errors

* BAD PARM

### ASSIGN

```hlasm
name     EXEC  CICS ASSIGN
```

The following parameters are not supported:

ACTIVITY,ACTIVITYID,ALTSCRNHT,ALTSCRNWD,APPLID,ASRAINTRPT,ASRAKEY,ASRASPC,ASRASTG,BRIDGE,
DELIMITER,DESTCOUNT,DESTID,DESTIDLENG,GCHARS,GCODES,INITPARM,INITPARMLEN,INPARTN,
LDCMNEM,LDCNUM,NETNAME,NUMTAB,OPCLASS,OPERKEYS,OPID,OPSECURITY,ORGABCODE,PAGENUM,
PARTNPAGE,PARTNPAGE,PRINSYSID,PROCESS,PROCESSTYPE,QNAME,SIGDATA,STATIONID,SYSID,
TELLERID,USERID,USERNAME

!!! Note 
    zCICS allows a CWA size greater than 32K. If the CWA
    does exceed 32K, then ASSIGN CWALENG() will return an
    incorrect value.

#### Conditions (RESP/RESP2)

* INVREQ/2
* INVREQ/5 

#### Errors

* BAD PARM

## Command reference - Terminal Control

### RECEIVE 

```hlasm
name     EXEC  CICS RECEIVE                                           X
                    INTO(label)                                       X
                    LENGTH(label)                                     X
                    NOHANDLE
```

#### Parameters

* INTO(label) and LENGTH(label) are mandatory.
* LENGTH must point to a 2-byte field.
* Although MAXLENGTH is not implemented yet, there is an internal 
  maximum length set to the implied length of the INTO label.
* NOHANDLE is optional.

#### Conditions (RESP/RESP2)

* NOTALLOC/0
* LENGERR/0

#### Errors

* BAD PARM
* BOTH INTO AND LENGTH ARE REQUIRED
* LENGTH ERROR

### SEND

```hlasm
name     EXEC  CICS SEND
                    FROM(label)
                    LENGTH()
```

#### Parameters

FROM(label) is mandatory.

_label_ must point to a 2-byte hex value.

_label_ may take three forms:

  * Direct reference
  * Indirect reference
  * Adcon literal

LENGTH can be specified as LENGTH(value) or LENGTH(label) 

LENGTH(value) supports the use of the length attribute.

The parameters TERMINAL, WAIT, DEFAULT and TEXT are discarded.

#### Conditions (RESP/RESP2)

* INVREQ/0 - Attempt to execute this in a non-terminal attached task.
  This is not documented in the IBM CICS Manuals.
* LENGERR/E1

#### Errors

* BAD PARM
* FROM IS MANDATORY
* LENGTH IS MANDATORY

### SEND CONTROL

```hlasm
name     EXEC  CICS SEND CONTROL
                    CURSOR/CURSOR()
                    ERASE/ERASEAUP
                    ALARM
                    FREEKB
                    FRSET
```
#### Parameters

* TERMINAL and WAIT are accepted and discarded.
* ACCUM and SET are not supported.
* CURSOR() refers to the 24x80 screen.
* CURSOR is not documented.
* SEND CONTROL CURSOR ERASEAUP means erase all input fields
  and don't move the cursor.

#### Conditions (RESP/RESP2)

* INVREQ/0 - Attempt to execute this in a non-terminal attached task.
  This is not documented in the IBM CICS Manual.

#### Errors

* BAD PARM
* CURSOR POSITION AND SYMBOLIC CURSOR SPECIFIED
* ERASE AND ERASEAUP SPECIFIED

## Command reference - File control

### READ 

```hlasm
name     EXEC  CICS READ
                    FILE()/DATASET()
                    INTO()/SET()
                    LENGTH()/FLENGTH()
                    RIDFLD()
                    RBA/XRBA/RRN/GENERIC
                    GTEQ/EQUAL
                    KEYLENGTH()
```

#### Parameters

##### LENGTH

* Can be specified as a constant, literal or label.
    * constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.

##### FLENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 2G-1.
    * A literal or label must be 4 bytes and must not exceed 2G-1.

!!! Info
    **LENGTH/FLENGTH**:

    * If SET is specified, LENGTH/FLENGTH are ignored and LENGERR
      cannot occur.
    * If INTO is specified and LENGTH/FLENGTH are not, then the implied
      length of INTO is used. This may raise the LENGERR condition if the
      data length is larger.
    * If LENGTH/FLENGTH is numeric then it specifies the maximum data
      length that can be received. LENGERR can be raised if the data length
      is larger.
    * If LENGTH/FLENGTH is a label then it specifies the maximum data
      length that can be received. LENGERR can be raised if the data length
      is larger. The true data length is returned in label.

##### KEYLENGTH

* Can be specified as a constant or label.
    * A constant must not exceed 32767.
    * A label must be 2 bytes and must not exceed 32767.
* Keylengths greater than 128 are ignored.
* The parameter is ignored for ESDS and RRDS.
* KEYLENGTH and GENERIC must be paired.
* If KEYLENGTH is zero by constant or label then parameters are
  changed internally:
    ```
    GENERIC/EQUAL or GENERIC/GTEQ
    Becomes 
    KEYLENGTH(1) Key=X'00' GENERIC GTEQ
    ```
##### RBA 

RIDFLD has a 4-byte RBA

##### XRBA 

RIDFLD has an 8-byte RBA

##### RRN 

RIDFLD has a 4-byte relative record number

##### GTEQ/EQUAL

The parameter is ignored for ESDS and RRDS.

##### GENERIC

KEYLENGTH must be specified.
The parameter is ignored for ESDS and RRDS.

#### Errors

* BAD PARM
* BOTH FILE AND DATASET ARE SPECIFIED
* BOTH GTEQ AND EQUAL ARE SPECIFIED
* BOTH INTO AND SET ARE SPECIFIED
* BOTH LENGTH AND FLENGTH ARE SPECIFIED
* BOTH RBA AND XRBA ARE SPECIFIED
* BOTH RRN AND (X)RBA ARE SPECIFIED
* FILE OR DATASET MUST BE SPECIFIED
* GENERIC CANNOT BE SPECIFIED WITH RRN OR (X)RBA
* GENERIC REQUIRES KEYLENGTH
* INTO OR SET MUST BE SPECIFIED
* INVALID FILE OR DATASET
* KEYLENGTH REQUIRES GENERIC
* RIDFLD IS MANDATORY

#### Conditions (RESP/RESP2)

* FILENOTFOUND/1
* DISABLED/50
* ILLOGIC/110
* INVREQ/20
* INVREQ/25
* INVREQ/42
* LENGERR/E1
* NOTFND/80
* NOTOPEN/60

!!! Note
    FLENGTH and XRBA are extensions; do not use these parameters if the
    source code is likely to be ported back to a mainframe environment.
    RBA access to a KSDS is not supported.
    
    DATASET is supported for legacy applications. It is noted that this parameter
    no longer appears in the IBM(r) CICS Manuals.
    
    When conditions are raised as a result of a VSAM error, the RPL feedback codes
    (2nd and 4th bytes) are placed in EIBRCODE +1 and +2.

### STARTBR 

```hlasm
name     EXEC  CICS STARTBR
                    FILE()/DATASET()
                    RIDFLD()
                    REQID()
                    RBA/XRBA/RRN/GENERIC
                    GTEQ/EQUAL
                    KEYLENGTH()
```

#### Parameters

##### REQID

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.
* If omitted, zero is assumed.

##### RBA 

RIDFLD has a 4-byte RBA

##### XRBA 

RIDFLD has an 8-byte RBA

##### RRN 

RIDFLD has a 4-byte relative record number

##### KEYLENGTH

* Can be specified as a constant or label.
    * A constant must not exceed 32767.
    * A label must be 2 bytes and must not exceed 32767.
    * Keylengths greater than 128 are ignored.
* The parameter is ignored for ESDS and RRDS.
* KEYLENGTH and GENERIC must be paired.
* If KEYLENGTH is zero by constant or label then parameters are
  changed internally:
    ```
      GENERIC/EQUAL or GENERIC/GTEQ
      Becomes 
      KEYLENGTH(1) Key=X'00' GENERIC GTEQ
    ```
##### GTEQ/EQUAL

The parameter is ignored for ESDS and RRDS.

##### GENERIC

KEYLENGTH must be specified.

The parameter is ignored for ESDS and RRDS.

#### Conditions (RESP/RESP2)

* DISABLED/50
* FILENOTFOUND/1
* ILLOGIC/110
* INVREQ/20
* INVREQ/25
* INVREQ/33
* INVREQ/42
* NOTFND/80
* NOTOPEN/60

!!! Note 
    NOTFND cannot occur for an ESDS or RRDS

#### Errors

* BAD PARM
* BOTH FILE AND DATASET ARE SPECIFIED
* BOTH GTEQ AND EQUAL ARE SPECIFIED
* BOTH RBA AND XRBA ARE SPECIFIED
* BOTH RRN AND (X)RBA ARE SPECIFIED
* FILE OR DATASET MUST BE SPECIFIED
* GENERIC CANNOT BE SPECIFIED WITH RRN OR (X)RBA
* GENERIC REQUIRES KEYLENGTH
* INVALID FILE OR DATASET
* KEYLENGTH REQUIRES GENERIC
* RIDFLD IS MANDATORY

### READNEXT 

```
name     EXEC  CICS READNEXT
                    FILE()/DATASET()
                    INTO()/SET()
                    LENGTH()/FLENGTH()
                    RIDFLD()
                    REQID()
                    RBA/XRBA/RRN
                    KEYLENGTH()
```

#### Parameters

##### LENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.

##### FLENGTH
* Can be specified as a constant, literal or label.
    * A constant must not exceed 2G-1.
    * A literal or label must be 4 bytes and must not exceed 2G-1.


!!! Info
    **LENGTH/FLENGTH**

    If either is not a label then:

    * If INTO is specified, then the length received is the implied length of
      INTO. This may raise the LENGERR condition if the data length is
      larger.
    * If SET is specified, the complete record is returned and LENGERR
      cannot occur.

    If either is a label then:
    
    * If INTO or SET is specified, then it specifies the maximum data
      length that can be received. LENGERR can be raised if the data
      length is larger. The true data length is returned in label.

##### REQID

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.
* If omitted, zero is assumed.

##### RBA 

RIDFLD has a 4-byte RBA

##### XRBA 

RIDFLD has an 8-byte RBA

##### RRN 

RIDFLD has a 4-byte relative record number

##### KEYLENGTH

* Can be specified as a constant or label.
    * A constant must not exceed 32767.
    * A label must be 2 bytes and must not exceed 32767.
* Keylengths greater than 128 are ignored.
* The parameter is ignored for ESDS and RRDS.
* If KEYLENGTH is zero by constant or label then parameters are
  changed internally:
    ```
    GENERIC/EQUAL or GENERIC/GTEQ
    Becomes 
    KEYLENGTH(1) Key=X'00' GENERIC GTEQ
    ```

#### Errors

* BAD PARM
* BOTH FILE AND DATASET ARE SPECIFIED
* BOTH INTO AND SET ARE SPECIFIED
* BOTH LENGTH AND FLENGTH ARE SPECIFIED
* BOTH RBA AND XRBA ARE SPECIFIED
* BOTH RRN AND (X)RBA ARE SPECIFIED
* FILE OR DATASET MUST BE SPECIFIED
* INTO OR SET MUST BE SPECIFIED
* INVALID FILE OR DATASET
* RIDFLD IS MANDATORY
* THIS TYPE OF INTO REQUIRES LENGTH/FLENGTH

#### Conditions (RESP/RESP2)

* DISABLED/50
* ENDFILE/90
* FILENOTFOUND/1
* ILLOGIC/110
* INVREQ/20
* INVREQ/25
* INVREQ/26
* INVREQ/34
* INVREQ/42
* LENGERR/E1
* NOTFND/80
* NOTOPEN/60

### READPREV 
```hlasm
name     EXEC  CICS READPREV
                    FILE()/DATASET()
                    INTO()/SET()
                    LENGTH()/FLENGTH()
                    RIDFLD()
                    REQID()
                    RBA/XRBA/RRN
                    KEYLENGTH()
```
#### Parameters

##### LENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.

##### FLENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 2G-1.
    * A literal or label must be 4 bytes and must not exceed 2G-1.

!!! Info
    **LENGTH/FLENGTH**

    If either is not a label then:

    * If INTO is specified, then the length received is the implied length of
      INTO. This may raise the LENGERR condition if the data length is
      larger.
    * If SET is specified, the complete record is returned and LENGERR
      cannot occur.
    
    If either is a label then:
    
    * If INTO or SET is specified, then it specifies the maximum data
      length that can be received. LENGERR can be raised if the data
      length is larger. The true data length is returned in label.

##### REQID

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.
* If omitted, zero is assumed.

##### RBA 

RIDFLD has a 4-byte RBA

##### XRBA 

RIDFLD has an 8-byte RBA

##### RRN 

RIDFLD has a 4-byte relative record number

##### KEYLENGTH

* Can be specified as a constant or label.
    * A constant must not exceed 32767.
    * A label must be 2 bytes and must not exceed 32767.
* Keylengths greater than 128 are ignored.
* The parameter is ignored for ESDS and RRDS.
* If KEYLENGTH is specified, the value must be equal to the keylength
  defined for the file.

#### Errors

* BAD PARM
* BOTH FILE AND DATASET ARE SPECIFIED
* BOTH INTO AND SET ARE SPECIFIED
* BOTH LENGTH AND FLENGTH ARE SPECIFIED
* BOTH RBA AND XRBA ARE SPECIFIED
* BOTH RRN AND (X)RBA ARE SPECIFIED
* FILE OR DATASET MUST BE SPECIFIED
* INTO OR SET MUST BE SPECIFIED
* INVALID FILE OR DATASET
* RIDFLD IS MANDATORY
* THIS TYPE OF INTO REQUIRES LENGTH/FLENGTH

#### Conditions (RESP/RESP2)

* DISABLED/50
* ENDFILE/90
* FILENOTFOUND/1
* ILLOGIC/110
* INVREQ/20
* INVREQ/24
* INVREQ/26
* INVREQ/41
* LENGERR/E1
* NOTFND/80
* NOTOPEN/60

!!! Note
    ENDFILE can occur when a READPREV attempts to read past
    the beginning of the file.

### ENDBR

```hlasm
name     EXEC  CICS ENDBR                                             X
                    FILE()/DATASET()                                  X
                    REQID()
```

#### Parameters

##### REQID

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.
* If omitted, zero is assumed.

!!! Note
    In real CICS, ENDBR cannot cause a file to open, but it will in zCICS.

    The ENDBR command will be invalid, and may result in a transaction
    abend.

#### Errors

* BAD PARM
* BOTH FILE AND DATASET ARE SPECIFIED
* FILE OR DATASET MUST BE SPECIFIED
* INVALID FILE OR DATASET

#### Conditions (RESP/RESP2)

* DISABLED/50
* FILENOTFOUND/1
* ILLOGIC/110
* INVREQ/20
* INVREQ/35
* NOTOPEN/60


### RESETBR 
```hlasm
name     EXEC  CICS RESETBR
                    FILE()/DATASET()
                    RIDFLD()
                    REQID()
                    RBA/XRBA/RRN/GENERIC
                    GTEQ/EQUAL
                    KEYLENGTH()
```
#### Parameters

##### REQID

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.
* If omitted, zero is assumed.

##### RBA 

RIDFLD has a 4-byte RBA

##### XRBA 

RIDFLD has an 8-byte RBA

##### RRN 

RIDFLD has a 4-byte relative record number

##### KEYLENGTH

* Can be specified as a constant or label.
    * A constant must not exceed 32767.
    * A label must be 2 bytes and must not exceed 32767.
* Keylengths greater than 128 are ignored.
* The parameter is ignored for ESDS and RRDS.
* KEYLENGTH and GENERIC must be paired.
* If KEYLENGTH is zero by constant or label then parameters are
  changed internally:
    ```
    GENERIC/EQUAL or GENERIC/GTEQ
    Becomes 
    KEYLENGTH(1) Key=X'00' GENERIC GTEQ
    ```

##### GTEQ/EQUAL

The parameter is ignored for ESDS and RRDS.

##### GENERIC

KEYLENGTH must be specified.

The parameter is ignored for ESDS and RRDS.

!!! Note 
    In real CICS, RESETBR cannot cause a file to open, but it will in
    zCICS. The RESETBR command will be invalid, and may result in a
    transaction abend.

#### Errors

* BAD PARM
* BOTH FILE AND DATASET ARE SPECIFIED
* BOTH GTEQ AND EQUAL ARE SPECIFIED
* BOTH RBA AND XRBA ARE SPECIFIED
* BOTH RRN AND (X)RBA ARE SPECIFIED
* FILE OR DATASET MUST BE SPECIFIED
* GENERIC CANNOT BE SPECIFIED WITH RRN OR (X)RBA
* GENERIC REQUIRES KEYLENGTH
* INVALID FILE OR DATASET
* KEYLENGTH REQUIRES GENERIC
* RIDFLD IS MANDATORY

#### Conditions (RESP/RESP2)

* DISABLED/50
* FILENOTFOUND/1
* ILLOGIC/110
* INVREQ/20
* INVREQ/25
* INVREQ/36
* INVREQ/42
* NOTFND/80
* NOTOPEN/60

!!! Note
    NOTFND cannot occur for an ESDS or RRDS

## Command reference - Storage Control

### FREEMAIN 
```hlasm
name     EXEC  CICS FREEMAIN                                          X
                    DATA()/DATAPOINTER()                              X
                    DATA(label)
```
#### Parameters

_label_ may only be an indirect reference to the address.

##### DATAPOINTER

Must be specified as a permitted general register value.

#### Errors

* BAD PARM
* BOTH DATA AND DATAPOINTER ARE SPECIFIED
* DATA OR DATAPOINTER MUST BE SPECIFIED

#### Conditions (RESP/RESP2)

* INVREQ/1

### GETMAIN

```hlasm
name     EXEC  CICS GETMAIN                                           X
                    SET()                                             X
                    LENGTH()/FLENGTH()                                X
                    INITIMG()
```
#### Parameters

##### SET 

* SET is mandatory
* Must be specified as a permitted general register value.

##### LENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 32767.
    * A literal or label must be 2 bytes and must not exceed 32767.

##### FLENGTH

* Can be specified as a constant, literal or label.
* A constant must not exceed 2G-1.
* A literal or label must be 4 bytes and must not exceed 2G-1.

##### INITIMG 

* Is optional
* If omitted, the storage contents are not predictable.
* Can be specified as a constant, literal or label.

!!! Info
    zCOBOL supports all 3 data types, but for mainframe COBOL programs
    only label is supported.

    Only the first byte generated by the parameter is used.

#### Errors

* BAD PARM
* BOTH LENGTH AND FLENGTH ARE SPECIFIED
* LENGTH OR FLENGTH MUST BE SPECIFIED
* SET IS MANDATORY

## Command reference - Temporary Storage Control

!!! Note
    FLENGTH is an extension; do not use this parameter if the source code is
    likely to be ported back to a mainframe environment.

### DELETEQ 
```hlasm
name     EXEC  CICS DELETEQ TS
                    QUEUE()/QNAME()
```

#### Parameters

The parameters MAIN and AUXILIARY are accepted and discarded.

##### QUEUE 

`QUEUE` may be specified as:

* A quoted string which must not exceed 8 bytes.
* A label which points to an 8-byte field.
* A literal not exceeding 8 bytes.

Only label or literal may be used to specify a QUEUE with hex characters.

##### QNAME

`QNAME` may be specified as:

* A quoted string which must not exceed 16 bytes.
* A label which points to a 16-byte field.
* A literal not exceeding 16 bytes.

Only label or literal may be used to specify a QNAME with hex characters.

#### Errors

* BAD PARM
* BOTH QUEUE AND QNAME ARE SPECIFIED
* DELETEQ TYPE NOT RECOGNIZED
* INVALID QUEUE OR QNAME
* QUEUE OR QNAME MUST BE SPECIFIED

#### Conditions (RESP/RESP2)

* INVREQ/0
* QIDERR/0

### READQ

```hlasm
name     EXEC  CICS READQ TS
                    QUEUE()/QNAME()
                    INTO()/SET()
                    LENGTH()/FLENGTH()
                    NUMITEMS()
                    ITEM()/NEXT
```

#### Parameters

The parameters MAIN and AUXILIARY are accepted and discarded.

##### QUEUE

`QUEUE` may be specified as:

* A quoted string which must not exceed 8 bytes.
* A label which points to an 8-byte field.
* A literal not exceeding 8 bytes.

Only label or literal may be used to specify a QUEUE with hex characters.

##### QNAME

QNAME may be specified as:

* A quoted string which must not exceed 16 bytes.
* A label which points to a 16-byte field.
* A literal not exceeding 16 bytes.

Only label or literal may be used to specify a QNAME with hex characters.

##### LENGTH

* May be specified as LENGTH(value) or LENGTH(label)
    * LENGTH(value) supports the use of the length attribute.
    * label must point to a 2-byte hex value.

##### FLENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 2G-1.
    * A literal or label must be 4 bytes and must not exceed 2G-1.
* LENGTH/FLENGTH can be omitted. When they are, the implied length
  of INTO is used. LENGTH/FLENGTH is mandatory when SET is used.
  ITEM
* May be specified as ITEM(value) or ITEM(label)
    * _label_ must point to a 2-byte hex value.

#### Errors

* BAD PARM
* BOTH INTO AND SET ARE SPECIFIED
* BOTH ITEM AND NEXT ARE SPECIFIED
* BOTH LENGTH AND FLENGTH ARE SPECIFIED
* BOTH QUEUE AND QNAME ARE SPECIFIED
* INTO OR SET MUST BE SPECIFIED
* INVALID QUEUE OR QNAME
* ITEM OR NEXT MUST BE SPECIFIED
* QUEUE OR QNAME MUST BE SPECIFIED
* READQ TYPE NOT RECOGNIZED
* SET REQUIRES LENGTH

#### Conditions (RESP/RESP2)

* INVREQ/0
* LENGERR/0
* ITEMERR/0
* QIDERR/0

### WRITEQ
```hlasm
name     EXEC  CICS WRITEQ TS
                    QUEUE()/QNAME()
                    FROM()
                    LENGTH()/FLENGTH()
                    NUMITEMS()
                    ITEM()
                    REWRITE
```

#### Parameters

The parameters MAIN and AUXILIARY are accepted and discarded.

##### QUEUE

QUEUE may be specified as:

* A quoted string which must not exceed 8 bytes.
* A label which points to an 8-byte field.
* A literal not exceeding 8 bytes.

Only label or literal may be used to specify a QUEUE with hex characters.

##### QNAME

QNAME may be specified as:

* A quoted string which must not exceed 16 bytes.
* A label which points to a 16-byte field.
* A literal not exceeding 16 bytes.

Only label or literal may be used to specify a QNAME with hex characters.

##### FROM 

`FROM(label)` is mandatory.

_label_ may take three forms:

* Direct reference
* Indirect reference
* Adcon literal

##### LENGTH

* May be specified as LENGTH(value) or LENGTH(label)
    * LENGTH(value) supports the use of the length attribute.
    * label must point to a 2-byte hex value.
* LENGTH can be omitted. When they are, the implied length of FROM is used. 
* LENGTH is mandatory when FROM is an indirect reference.

##### FLENGTH

* Can be specified as a constant, literal or label.
    * A constant must not exceed 2G-1.
    * A literal or label must be 4 bytes and must not exceed 2G-1.
* FLENGTH can be omitted. When they are, the implied length of FROM is used. 
* FLENGTH is mandatory when FROM is an indirect reference.

##### ITEM

* May be specified as ITEM(value) or ITEM(label)
* label must point to a 2-byte hex value.

!!! Info
    For compatibility with old releases of CICS, ITEM is accepted
    without REWRITE and becomes NUMITEMS. ITEM must be a
    label in this case.

#### Errors

* BAD PARM
* BOTH LENGTH AND FLENGTH ARE SPECIFIED
* BOTH QUEUE AND QNAME ARE SPECIFIED
* FROM IS MANDATORY
* IF NUMITEMS IS SPECIFIED, ITEM AND REWRITE ARE INVALID
* INVALID QUEUE OR QNAME
* LENGTH/FLENGTH IS MANDATORY FOR INDIRECT FROM
* LENGTH WITHOUT FROM
* NUMITEMS MUST BE A LABEL (MAY HAVE ORIGINATED AS ITEM)
* QUEUE OR QNAME MUST BE SPECIFIED
* REWRITE REQUIRES ITEM
* WRITEQ TYPE NOT RECOGNIZED

#### Conditions (RESP/RESP2)

* INVREQ/0
* LENGERR/0
* ITEMERR/0
* QIDERR/0

## Command reference - Program Control

### ABEND 
```hlasm
name     EXEC  CICS ABEND
                    ABCODE()
                    CANCEL
                    NODUMP
```
#### Parameters

##### ABCODE

* ABCODE can be specified as ABCODE('xxxx') or ABCODE(label)
* label must point to a 4-byte field.

#### Errors

* ABCODE MUST NOT BEGIN WITH 'A'
* ABCODE IS INVALID
* BAD PARM


### HANDLE ABEND 
```hlasm
name     EXEC  CICS HANDLE ABEND CANCEL
name     EXEC  CICS HANDLE ABEND RESET
name     EXEC  CICS HANDLE ABEND LABEL(label)
name     EXEC  CICS HANDLE ABEND PROGRAM()
```

#### Parameters

##### LABEL

_label_ may take three forms:

* Direct reference
* Indirect reference
* Adcon literal

##### PROGRAM 

* Can be specified as PROGRAM('xxxxxxxx') or PROGRAM(label)
    * _label_ must point to an 8-byte field.
* Any received COMMAREA when the EXEC CICS HANDLE ABEND is
  issued is passed to the handling program when an abend occurs.

!!! Note 
    When an XCTL is executed, any HANDLE ABEND LABEL at the
    current logical level is cleared as the current program is no longer in
    use. HANDLE ABEND PROGRAMs are not cleared.

#### Errors

* BAD PARM
* HANDLE TYPE NOT RECOGNISED
* INVALID PROGRAM
* NO PARAMETERS SPECIFIED 
* PARMS MISSING OR TOO MANY PARMS


### LINK 
```hlasm
name     EXEC  CICS LINK
                    PROGRAM()
                    COMMAREA(label)
                    LENGTH()
                    CHANNEL()
```

Executes another CICS program.

* Return is to the linker.

In zCICS both CHANNEL and COMMAREA may be specified.
A warning MNOTE is issued.

#### Parameters


##### PROGRAM

* PROGRAM is mandatory
* Can be specified as PROGRAM('xxxxxxxx') or PROGRAM(label)
    * label must point to an 8-byte field.

##### COMMAREA
* COMMAREA(label) is optional
* If COMMAREA is present, the address/length are passed.
* label may take three forms:
    * Direct reference
    * Indirect reference
    * Adcon literal

##### LENGTH

* Can be specified as LENGTH(value) or LENGTH(label)
    * LENGTH(value) supports the use of the length attribute.
    * label must point to a 2-byte hex value.
* LENGTH can be omitted. When it is, the implied length of the
  COMMAREA is used. LENGTH is mandatory when COMMAREA is an
  indirect reference.

#### Errors

* BAD PARM
* INVALID CHANNEL
* INVALID PROGRAM
* PROGRAM IS MISSING
* LENGTH IS MANDATORY FOR INDIRECT COMMAREA
* LENGTH WITHOUT COMMAREA

#### Warning

* CHANNEL and COMMAREA specified

#### Conditions (RESP/RESP2)

* CHANNELERR/1
* PGMIDERR/3

### LOAD
```hlasm
name     EXEC  CICS LOAD
                    PROGRAM()
                    ENTRY()/SET()
                    LENGTH(label)
                    FLENGTH(label)
```

Loads a module.

The intention in the zCICS environment is to load a table or some other data,
not an executable program.

#### Parameters

##### PROGRAM

* PROGRAM is mandatory
* Can be specified as PROGRAM('xxxxxxxx') or PROGRAM(label)
    * label must point to an 8-byte field.
* At present, only modules with a suffix of .390 may be LOADed.

##### ENTRY/SET

* ENTRY and SET are optional
* Must be specified as a permitted general register value.
    * Both are equivalent in zCICS.

##### LENGTH

* LENGTH is optional
* LENGTH(label) is the only format.
* label must point to a 2-byte field.

##### FLENGTH

* FLENGTH is optional
* FLENGTH(label) is the only format.
* label must point to a 4-byte field.

!!! Note 
    At task end the LOADed module is not RELEASEd.

#### Errors

* BAD PARM
* INVALID PROGRAM
* LENGTH AND FLENGTH SPECIFIED
* PROGRAM IS MISSING

#### Conditions (RESP/RESP2)

* PGMIDERR/3

### RELEASE 
```hlasm
name     EXEC  CICS RELEASE
                    PROGRAM()
```
Releases a previously loaded module.

#### Parameters

##### PROGRAM

* PROGRAM is mandatory
* Can be specified as PROGRAM('xxxxxxxx') or PROGRAM(label)
    * label must point to an 8-byte field.

#### Errors

* BAD PARM
* INVALID PROGRAM
* PROGRAM IS MISSING

#### Conditions (RESP/RESP2)

* INVREQ/5
* INVREQ/6


### RETURN
```hlasm
name     EXEC  CICS RETURN
                    TRANSID()
                    COMMAREA(label)
                    LENGTH()
                    CHANNEL()
                    IMMEDIATE
```
Returns to the last caller.

In zCICS both CHANNEL and COMMAREA may be specified. A warning MNOTE is issued.

#### Parameters

##### TRANSID

* Optional, but when COMMAREA is specified, TRANSID is mandatory.
* Can be specified as TRANSID('xxxx') or TRANSID(label)
    * _label_ must point to a 4-byte field.

##### COMMAREA

COMMAREA(label) is optional

_label_ may take three forms:

    * Direct reference
    * Indirect reference
    * Adcon literal

##### LENGTH

* Can be specified as LENGTH(value) or LENGTH(label)
* LENGTH(value) supports the use of the length attribute.
    * _label_ must point to a 2-byte hex value.
* LENGTH can be omitted. When it is, the implied length of the
  COMMAREA is used. LENGTH is mandatory when COMMAREA is an 
  indirect reference.

#### Errors

* BAD PARM
* IMMEDIATE REQUIRES TRANSID
* INVALID CHANNEL
* INVALID TRANSID
* TRANSID IS MISSING
* LENGTH IS MANDATORY FOR INDIRECT COMMAREA
* LENGTH WITHOUT COMMAREA

#### Warning

* CHANNEL and COMMAREA specified

#### Conditions (RESP/RESP2)

See the section on [IGNORE CONDITION](#ignore-condition) for these conditions.

* CHANNELERR/1
* INVREQ/1 
* INVREQ/2 

### XCTL 
```hlasm
name     EXEC  CICS XCTL
                    PROGRAM()
                    COMMAREA(label)
                    LENGTH()
                    CHANNEL()
```

Executes another CICS program.

In zCICS both CHANNEL and COMMAREA may be specified. A warning MNOTE is issued.

If COMMAREA is present and both the address and length are the same as
passed to the current program, then address/length are passed to the new
program.

If the address or length differs, then a copy of the COMMAREA is taken and
the new address/length are passed to the new program.

Return is to the last linker.

#### Parameters

##### PROGRAM

* PROGRAM is mandatory
* Can be specified as PROGRAM('xxxxxxxx') or PROGRAM(label)
    * label must point to an 8-byte field.

##### COMMAREA

* COMMAREA(label) is optional
* label may take three forms:
    * Direct reference
    * Indirect reference
    * Adcon literal

##### LENGTH

* Can be specified as LENGTH(value) or LENGTH(label)
    * LENGTH(value) supports the use of the length attribute.
    * label must point to a 2-byte hex value.
* LENGTH can be omitted. When it is, the implied length of the
  COMMAREA is used. LENGTH is mandatory when COMMAREA is an
  indirect reference.

#### Errors

* BAD PARM
* INVALID CHANNEL
* INVALID PROGRAM
* PROGRAM IS MISSING
* LENGTH IS MANDATORY FOR INDIRECT COMMAREA
* LENGTH WITHOUT COMMAREA

#### Warning

* CHANNEL and COMMAREA specified

#### Conditions (RESP/RESP2)

* CHANNELERR/1 
* PGMIDERR/3

## Command reference - Interval control

!!! Note
    FLENGTH is an extension; do not use this parameter if the source code is 
    likely to be ported back to a mainframe environment.

### ASKTIME 
ABSTIME()

a) ASKTIME
Name Operation Operands
name EXEC CICS ASKTIME
ABSTIME()
Errors
BAD PARM


### DELAY 
REQID()/INTERVAL()/TIME() FOR/UNTIL HOURS() MINUTES() SECONDS()

b) DELAY
Name Operation Operands
name EXEC CICS DELAY
REQID()
INTERVAL()/TIME()
FOR/UNTIL
HOURS()
MINUTES()
SECONDS()
INTERVAL
Can be specified as INTERVAL(s) through to INTERVAL(hhmmss).
i.e. INTERVAL(234) means wait for 2 minutes 34 seconds.
INTERVAL(label) is also permitted (extension).
label must point to a 6-byte character field with leading character zeros
as needed.
e.g. INTERVAL(MYTIME)
...
MYTIME DC C'000234'
TIME
Can be specified as TIME(s) through to TIME(hhmmss).
i.e. TIME(234) means resume the task at 2 minutes 34 seconds after
midnight. Expiration time rules apply; see the IBM Application
Programming Guide.
TIME(label) is also permitted (extension).
label must point to a 6-byte character field with leading character zeros
as needed.
e.g. TIME(MYTIME)
...
MYTIME DC C'000234'
FOR HOURS() MINUTES() SECONDS()
FOR is an alternative to INTERVAL.
HOURS/MINUTES/SECONDS must be numeric values.
UNTIL HOURS() MINUTES() SECONDS()
UNTIL is an alternative to TIME.
HOURS/MINUTES/SECONDS must be numeric values.
The result from the parameters is a time-of-day.
e.g. UNTIL SECONDS(10000) means resume the task at 02:46:40.
Expiration time rules apply; see the IBM Application Programming Guide.
If no parameters are specified, then DELAY INTERVAL(0) is assumed.
Errors
BAD PARM
BOTH FOR AND UNTIL ARE SPECIFIED
BOTH INTERVAL AND TIME ARE SPECIFIED
FOR/UNTIL SPECIFIED, BUT NO TIME PARMS
HOURS/MINUTES/SECONDS ARE INVALID WITH INTERVAL OR TIME
HOURS/MINUTES/SECONDS ARE SPECIFIED WITHOUT FOR/UNTIL
INTERVAL/TIME CANNOT BE SPECIFIED WITH FOR/UNTIL
INTERVAL/TIME MUST BE 1 TO 6 BYTES
INVALID REQID
Conditions (RESP/RESP2)
INVREQ/4
INVREQ/5
INVREQ/6


### FORMATTIME 
all parameters

c) FORMATTIME
Name Operation Operands
name EXEC CICS FORMATTIME
all parameters
Notes:
STRINGFORMAT is discarded as there is only one option.
DATESEP(label) and TIMESEP(label) are added as extensions.
Only the first byte is used.
DATESTRING returns the following 25-byte string.
The time zone (e.g. GMT) is not returned.
e.g. "Mon, 17 Dec 2007 10:20:30"
Errors
ABSTIME IS MANDATORY
BAD PARM
Conditions (RESP/RESP2)
INVREQ/1


### START 
TRANSID() INTERVAL()/TIME() TERMID() CHANNEL() REQID() FROM() LENGTH() QUEUE() RTRANSID() RTERMID() AFTER/AT HOURS() MINUTES() SECONDS()

d) START
Name Operation Operands
name EXEC CICS START
TRANSID()
INTERVAL()/TIME()
TERMID()
REQID()
FROM()
LENGTH()/FLENGTH()
CHANNEL()
QUEUE()
RTRANSID()
RTERMID()
AFTER/AT
HOURS()
MINUTES()
SECONDS()
USERID is not supported.
In zCICS both CHANNEL and other parms may be specified.
A warning MNOTE is issued.
INTERVAL and TIME follow the same syntax and rules as for DELAY.
AFTER and AT follow the same syntax and rules as FOR and UNTIL in
DELAY above.
Errors
AFTER/AT SPECIFIED, BUT NO TIME PARAMETERS
BAD PARM
BOTH AFTER AND AT ARE SPECIFIED
BOTH INTERVAL AND TIME ARE SPECIFIED
BOTH LENGTH AND FLENGTH ARE SPECIFIED
FROM/(F)LENGTH MUST BOTH BE MISSING OR BOTH SPECIFIED
HOURS/MINUTES/SECONDS ARE INVALID WITH INTERVAL OR TIME
HOURS/MINUTES/SECONDS ARE SPECIFIED WITHOUT AFTER/AT
INTERVAL/TIME CANNOT BE SPECIFIED WITH AFTER/AT
INTERVAL/TIME MUST BE 1 TO 6 BYTES
INVALID CHANNEL
INVALID QUEUE
INVALID REQID
INVALID RTERMID
INVALID RTRANSID
INVALID TERMID
INVALID TRANSID
TRANSID IS MANDATORY
Warning
 CHANNEL AND OTHER PARMS SPECIFIED BUT ALLOWED
Conditions (RESP/RESP2)
 CHANNELERR/1
INVREQ/0
INVREQ/4
INVREQ/5
INVREQ/6
IOERR
LENGERR
TERMIDERR
TRANSIDERR


### RETRIEVE 
INTO()/SET() LENGTH() RTRANSID() RTERMID() QUEUE()

e) RETRIEVE
Name Operation Operands
name EXEC CICS RETRIEVE
INTO()/SET()
LENGTH()/FLENGTH()
RTRANSID()
RTERMID()
QUEUE()
WAIT is not supported.
Errors
BAD PARM
BOTH INTO AND SET ARE SPECIFIED
BOTH LENGTH AND FLENGTH ARE SPECIFIED
INTO OR SET MUST BE SPECIFIED
INVALID QUEUE
INVALID RTERMID
INVALID RTRANSID
LENGTH OR FLENGTH MUST BE A LABEL
SET REQUIRES LENGTH OR FLENGTH
Conditions (RESP/RESP2)
INVREQ/0
ENDDATA
ENVDEFERR
LENGERR


### CANCEL 
REQID()

f) CANCEL
Name Operation Operands
name EXEC CICS CANCEL
REQID()
TRANSID is not supported.
Errors
BAD PARM
INVALID REQID
REQID IS MANDATORY
Conditions (RESP/RESP2)
INVREQ
NOTFND


## Command reference - Task Control

### ENQ 
RESOURCE() LENGTH() NOSUSPEND

a) ENQ
Name Operation Operands
name EXEC CICS ENQ
RESOURCE()
LENGTH()
NOSUSPEND
LENGTH
 Can be specified as LENGTH(value) or LENGTH(label)
LENGTH(value) supports the use of the length attribute.
Errors
BAD PARM
RESOURCE IS MANDATORY
*** ENQ ON ADDRESS MAY NOT WORK IN zCICS
*** BUT THE COMMAND WILL BE PROCESSED
*** PLEASE CONTACT THE AUTHOR FOR ADVICE
Conditions (RESP/RESP2)
ENQBUSY
LENGERR/1


### DEQ 
RESOURCE() LENGTH()

b) DEQ
Name Operation Operands
name EXEC CICS DEQ
RESOURCE()
LENGTH()
LENGTH
 Can be specified as LENGTH(value) or LENGTH(label)
LENGTH(value) supports the use of the length attribute.
Errors
BAD PARM
RESOURCE IS MANDATORY
Conditions (RESP/RESP2)
LENGERR/1

## Command reference - BMS

### RECEIVE 
MAP(name) MAPSET(name) INTO()

a) RECEIVE MAP
Name Operation Operands
name EXEC CICS RECEIVE MAP()
MAPSET()
INTO()
TERMINAL and ASIS are accepted and discarded.
SET, FROM and LENGTH are not supported.
MAP can be a quoted string, maximum 7 characters or a label pointing to a
7-byte field.
If MAP is a label, then INTO is mandatory.
Note: The map structure will not be cleared before the mapping takes
place.
If MAP is a string, then INTO is optional...
If INTO is omitted, the default is map.I
Note: The map structure will be cleared before the mapping takes place.
MAPSET can be a quoted string, maximum 7 characters or an address
pointing to an 8-byte field containing no more than 7 characters.
If MAPSET is omitted, then the MAPname is used.
Errors
BAD PARM
FROM NOT SUPPORTED
INTO IS REQUIRED WHEN MAP IS A LABEL
INVALID MAP NAME
INVALID MAPSET NAME
LENGTH NOT SUPPORTED
MAP IS MANDATORY
Conditions (RESP/RESP2)
Note: EIBRESP2 is an extension for MAPFAIL; please see the zCICS BMS
Guide for more information.
Many of the conditions can arise through a mismatch of map and
structure. Typically a map is re-assembled but the programs using it
are not.
INVMPSZ/0
MAPFAIL/1 The map cannot be found in the mapset.
MAPFAIL/2 A short read key (CLEAR or PA) has been pressed or there
are no modified fields.
MAPFAIL/3 Data has been received, but there are no named fields in the
map.
MAPFAIL/4 An SBA has been located, but there is no field in the map
that matches.
MAPFAIL/5 An SBA has been located, but it matches an unnamed field.
MAPFAIL/6 The data received for this field is longer than the DFHMDF
LENGTH= parameter.
MAPFAIL/7 There has been a mismatch between the physical map and
the DSECT.
MAPFAIL/8 Data to be processed by PICIN is over 31 digits.
Data is not numeric after being PACKed.
Data length is greater than the edit pattern allows.
INVREQ/0

### SEND 
MAP(name) MAPSET(name) CURSOR/CURSOR() DATAONLY/MAPONLY ERASE/ERASEAUP FROM() LENGTH() ALARM FREEKB FRSET SET()

b) SEND MAP
Name Operation Operands
name EXEC CICS SEND MAP()
MAPSET()
CURSOR/CURSOR()
DATAONLY/MAPONLY
ERASE/ERASEAUP
FROM()
LENGTH()
ALARM
FREEKB
FRSET
SET()
TERMINAL and WAIT are accepted and discarded.
ACCUM is not supported.
MAP can be a quoted string, maximum 7 characters or a label pointing to a
7-byte field.
If MAP is a label, then FROM is mandatory.
If MAP is a string, then FROM and/or LENGTH are optional...
If FROM is omitted, the default is map.O
If LENGTH is omitted, the default is map.L
LENGTH
 LENGTH is supported but the value used is always that of the structure
 length. 
Can be specified as LENGTH(value) or LENGTH(label)
LENGTH(value) supports the use of the length attribute.
label must point to a 2-byte hex value.
MAPSET can be a quoted string, maximum 7 characters or a label pointing
to an 8-byte field containing no more than 7 characters.
If MAPSET is omitted, then MAP is used.
SET() is currently intended to be an internal parameter.
It does not conform to the standard used for BMS PAGING.
Errors
BAD PARM
CONTROL IS NOT COMPATABLE WITH SEND MAP
CURSOR POSITION AND SYMBOLIC CURSOR SPECIFIED
DATAONLY AND MAPONLY SPECIFIED
ERASE AND ERASEAUP SPECIFIED
FROM IS REQUIRED WHEN MAP IS A LABEL
INVALID MAP NAME
INVALID MAPSET NAME
MAP IS MANDATORY
Conditions (RESP/RESP2)
Note: EIBRESP2 is an extension for MAPFAIL, please see the zCICS BMS
Guide for more information.
INVMPSZ/0
MAPFAIL/1 The map cannot be found in the mapset.
MAPFAIL/8 Data to be processed by PICOUT is over 31 digits.
Data is not numeric after being PACKed.
 Data length is greater than the edit pattern allows.
MAPFAIL/9 Override field or colour attribute is invalid
INVREQ/0
 Note: Attempt to execute this in a non-terminal attached task.
 This is not documented in the Manual.

### SEND CONTROL CURSOR/CURSOR() ERASE/ERASEAUP ALARM FREEKB FRSET

## Command reference - Dump Control

### DUMP 
TRANSACTION DUMPCODE() COMPLETE FROM() LENGTH()/FLENGTH() SEGMENTLIST() LENGTHLIST() NUMSEGMENTS()

a) DUMP
Name Operation Operands
name EXEC CICS DUMP
TRANSACTION
DUMPCODE()
COMPLETE
FROM()
LENGTH()/FLENGTH()
SEGMENTLIST()
LENGTHLIST()
NUMSEGMENTS()
TRANSACTION is mandatory.
DUMPCODE is mandatory and can be a constant or label.
label must point to a 4-byte field.
No syntax checking is done.
COMPLETE
If there are no storage area parameters then COMPLETE is the default.
Produces a SNAP dump ID=997,TEXT='DUMP dddd COMPLETE'
If there are storage area parameters and COMPLETE is not specified,
only the storage areas are dumped.
FROM() LENGTH()/FLENGTH()
Produces a SNAP dump ID=997,TEXT='DUMP dddd AREA'
LENGTH
Can be specified as a constant or label.
A constant must not exceed 32767.
A label must be 2 bytes and must not exceed 32767.
FLENGTH
Can be specified as a constant or label.
A constant must not exceed 2G-1.
A label must be 4 bytes and must not exceed 2G-1.
SEGMENTLIST/LENGTHLIST/NUMSEGMENTS
Produces multiple SNAP dumps ID=997,TEXT='DUMP dddd SEGMENT
nnn'
NUMSEGMENTS
Can be specified as a constant or label.
A constant must not exceed 2G-1.
A label must be 4 bytes and must not exceed 2G-1.
Errors
BAD PARM
BOTH LENGTH AND FLENGTH ARE SPECIFIED
DUMPCODE IS GREATER THAN 4 BYTES
DUMPCODE MUST BE SPECIFIED
LENGTH OR FLENGTH REQUIRES FROM
LENGTH OR FLENGTH MUST BE SPECIFIED
SEGMENTLIST, LENGTHLIST AND NUMSEGMENTS MUST
 ALL BE SPECIFIED OR ALL ABSENT
TRANSACTION MUST BE SPECIFIED 

## Command reference - Inquire

### INQUIRE FILE

a) INQUIRE FILE 
 The following formats are supported:
Name Operation Operands
name EXEC CICS INQUIRE FILE START
name EXEC CICS INQUIRE FILE START AT()
name EXEC CICS INQUIRE FILE END
name EXEC CICS INQUIRE FILE() NEXT ...
See parameter list below
name EXEC CICS INQUIRE FILE() ...
See parameter list below
 The following parameters are supported:
ACCESSMETHOD, ADD, BASEDSNAME, BROWSE, DELETE,
DSNAME, ENABLESTATUS, KEYLENGTH ,KEYPOSITION, OBJECT,
OPENSTATUS, READ, RECORDFORMAT,
 RECORDSIZE, TYPE, UPDATE
 BASEDSNAME() and DSNAME() parameters:
 The length of data returned is the implied length of the data
 area to a maximum of 128 bytes. This can be extended on
 request.
 Errors
 AT() IS SPECIFIED WITHOUT START
 BAD PARM
 FILE DOES NOT PRECEDE END
 FILE DOES NOT PRECEDE START
 FILE() CANNOT BE A STRING WHEN NEXT IS SPECIFIED
 FILE() IS MISSING
 INQUIRE TYPE NOT RECOGNIZED
 INVALID AT()
 INVALID FILE()
 NEXT AND END ARE SPECIFIED
 START AND END ARE SPECIFIED

### SET FILE

b) SET FILE 
 The following formats are supported:
Name Operation Operands
name EXEC CICS SET FILE()/DATASET() ...
See parameter list below
 The following parameters are supported:
ADD(), ADDABLE, NOTADDABLE, BROWSE(), BROWSABLE,
NOTBROWSABLE, DELETE(), DELETABLE, NOTDELETABLE,
ENABLESTATUS(), ENABLED, DISABLED, OPENSTATUS(), OPEN,
CLOSED, READ(), READABLE, NOTREADABLE, UPDATE(),
UPDATABLE, NOTUPDATABLE
 Errors
BAD PARM
BOTH FILE AND DATASET SPECIFIED
 FILE OR DATASET MUST BE SPECIFIED
 INVALID FILE OR DATASET
MORE THAN ONE ADD PARAMETER
MORE THAN ONE BROWSE PARAMETER
MORE THAN ONE DELETE PARAMETER
MORE THAN ONE ENABLESTATUS PARAMETER
MORE THAN ONE OPEN STATUS PARAMETER
MORE THAN ONE READ PARAMETER
MORE THAN ONE UPDATE PARAMETER
Conditions (RESP/RESP2)
 FILENOTFOUND
INVREQ/2
INVREQ/3
INVREQ/4
INVREQ/5
INVREQ/7
INVREQ/12
INVREQ/14
INVREQ/16
INVREQ/17
IOERR

## Command reference - Channel and containers

### GET 
CONTAINER() CHANNEL() INTO()/SET()/NODATA FLENGTH()

 a) GET
Name Operation Operands
name EXEC CICS GET
CONTAINER()
CHANNEL()
INTO()/FLENGTH()
SET()/FLENGTH()
NODATA/FLENGTH()
Errors
 BAD PARM
 BOTH INTO AND SET SPECIFIED
 CONTAINER IS MANDATORY 
 INTO AND NODATA SPECIFIED
 INTO OR SET OR NODATA IS REQUIRED
 INVALID CHANNEL
 INVALID CONTAINER 
 NODATA REQUIRES FLENGTH
 SET AND NODATA SPECIFIED
 SET OR NODATA REQUIRES FLENGTH AS LABEL
 SET REQUIRES FLENGTH
Conditions (RESP/RESP2)
 CHANNELERR/2
 INVREQ/4
 LENGERR/11 

### PUT 
CONTAINER() CHANNEL() FROM() FLENGTH()

 b) PUT 
Name Operation Operands
name EXEC CICS PUT
CONTAINER()
CHANNEL()
FROM()/FLENGTH()
Errors
 BAD PARM
 CONTAINER IS MANDATORY 
 FLENGTH IS MANDATORY FOR INDIRECT FROM
 FLENGTH WITHOUT FROM
 FROM IS MANDATORY
 INVALID CHANNEL
 INVALID CONTAINER 
 
Conditions (RESP/RESP2)
 CHANNELERR/1
 CONTAINERERR/18
 INVREQ/4
 LENGERR/1 


### DELETE 
CONTAINER() CHANNEL()

 c) DELETE 
Name Operation Operands
name EXEC CICS DELETE
CONTAINER()
CHANNEL()
Errors
 BAD PARM
 CONTAINER IS MANDATORY 
 INVALID CHANNEL
 INVALID CONTAINER 
 
Conditions (RESP/RESP2)
 CHANNELERR/2
 CONTAINERERR/10

### MOVE 
CONTAINER() AS() CHANNEL() TOCHANNEL()

 d) MOVE 
Name Operation Operands
name EXEC CICS MOVE
CONTAINER()
AS()
CHANNEL()
TOCHANNEL()
Errors
 BAD PARM
 CONTAINER AND/OR AS ARE MISSING 
 INVALID AS
 INVALID CHANNEL
 INVALID CONTAINER 
 INVALID TOCHANNEL
 
Conditions (RESP/RESP2)
 CHANNELERR/1
 CHANNELERR/2 
 CONTAINERERR/10
 CONTAINERERR/18 
 INVREQ/4


### STARTBROWSE 
CONTAINER CHANNEL() BROWSETOKEN() 

e) STARTBROWSE
Name Operation Operands
name EXEC CICS STARTBROWSE
CONTAINER
CHANNEL()
BROWSETOKEN()
Errors
 BAD PARM
 INVALID CHANNEL 
 BROWSETOKEN IS MANDATORY
 STARTBROWSE TYPE NOT RECOGNISED 
 
Conditions (RESP/RESP2)
 ACTIVITYERR/2 
 CHANNELERR/2 

### GETNEXT 
CONTAINER() BROWSETOKEN() 

 f) GETNEXT
Name Operation Operands
name EXEC CICS GETNEXT
CONTAINER()
BROWSETOKEN()
Errors
 BAD PARM
 BROWSETOKEN IS MANDATORY
 CONTAINER IS MANDATORY 
 
Conditions (RESP/RESP2)
 END/2 
 TOKENERR/3 


### ENDBROWSE 
CONTAINER BROWSETOKEN()

 g) ENDBROWSE
Name Operation Operands
name EXEC CICS ENDBROWSE
CONTAINER
BROWSETOKEN()
Errors
 BAD PARM
 BROWSETOKEN IS MANDATORY
 ENDBROWSE TYPE NOT RECOGNISED 
Condition (RESP/RESP2)
 TOKENERR/3 