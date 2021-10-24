# Program Services

## Save area and linkage conventions

Standard save area is defined as follows:

``` hlasm
         DS   0CL72
         DS   F         +0 reserved
         DS   F         +4 address of calling savearea
         DS   F         +8 address of called savearea
         DS   15F       +12 callers GR14 through GR12
```

There are normally two save areas involved when a program is called by another program:

* The save area for the calling program -- the caller
* The save area for the called program -- the callee

Upon entry to the called program, GR13 contains the address of the caller's save area. 

The callee:

1. Saves GR14 to GR12 (registers at entry to the called program) at +12 of the caller's save area
2. Obtains its own save area (the called program's save area)
3. Stores the address of its save area at +8 of the caller's save area (caller's forward chain)
4. Stores the address of the caller's save area (GR13 at entry) at +4 of its (the called program's) save area (called program's back chain)
5. Loads GR13 with the address of its (the called program's) save area

At this point the called program begins its processing. 

If it calls another program steps 1--5 are repeated; this time the "caller" is the current program and there is a new called program.

When the called program finishes processing, it returns to the caller as follows:

6. The called program uses its back chain (+4 in its save area) to restore GR13 to caller's save area
7. The called program restores GR14 to GR12 from that save area (restores registers at entry); there are cases when GR15, GR0, and/or GR1 are not restored -- depends on the program. 
8. The called program returns to the caller by doing an unconditional branch to the address in GR14.

This calling convention is explained in detail in the [IBM z/OS Version 2 Release 4 MVS Programming: Assembler Services
Guide (SA23-1368-40)](https://www-01.ibm.com/servers/resourcelink/svc00100.nsf/pages/zOSV2R4sa231368/$file/ieaa600_v2r4.pdf). Refer to Chapter 2 - Linkage conventions.

GR15 by convention may contain a return code upon exit.

## Program parameters

There are two methods of passing parameters to a program.

* At assembly time using SYSPARM
* At run time using PARM

In either case enclose the entire parm to be passed in double quotes.

The double quotes are required to handle commas and spaces that otherwise cause command processors to split the parm.

The double quotes are not required if there are no commas or spaces in the text. 

### Assembly time - SYSPARM

If single quotes are included in text, they are passed on to &SYSPARM.

There is a limit of 32767 bytes for the text. 

SYSPARM will be transferred to the macro variable &SYSPARM.

``` dos
mz390 ... "SYSPARM(HELLO WORLD)"
```
Access the text by coding:

``` hlasm
label    DC    C'&SYSPARM'
...... will translate to 
label    DC    C'HELLO WORLD'
```

### Run time - PARM

PARM can be accessed via GR1 at program entry.

GR1 will contain an address that points to a fullword in storage which points to a storage location with the parm length in a halfword followed by the parm value.

``` dos
ez390 ... "PARM(HELLO WORLD)"
```

The fullword pointed to by GR1 points to:

``` hlasm
         DC   H'11',C'HELLO WORLD'`
```

If single quotes are included around text in PARM they are removed.

``` dos
ez390 ... "PARM('HELLO WORLD')" 
```

Will also result in the fullword pointed to by GR1 pointing to:

``` hlasm
         DC   H'11',C'HELLO WORLD'`
```

#### Usage

The program is run with the parameter passed

``` dos
ez390 myprog "PARM('HELLO WORLD')"
```

The program code:

``` hlasm
         L     R2,0(,R1)       R2=address of parm length(HW)+value
         LH    R3,0(,R2)       R3=length of parm value
         BCTR  R3,0            Subtract 1 from R3 - length for use in machine code 
         EX    R3,MOVE         Execute MOVE instruction using length code from R3
......
MOVE     MVC   PARM(0),2(R2)   Move instruction used by EX
PARM     DC    CL80' '         Work area for parm value
```

* GR1 is populated by ez390 with the parm address pointer.
* GR2 is loaded with the address of the parm length and address - fullword pointed to by GR1.
* GR3 is loaded with the parm length - halfword value pointed to by GR2.
* GR3 would contain decimal 11 which is the length of the parm "HELLO WORLD".
* GR3 is decremented by 1, making it a length code.
* The actual parm value of "HELLO WORLD" begins 2 bytes past the address in GR2.
* The `EX` instruction executes the instruction at label MOVE which will move the parm to the storage at label PARM using the length code in R3.

## Macro reference

### SVC functions

The following is a list of the z390 SVC services that support the macros.

DEC | HEX | Service
----|-----|--------
3   | 03  | [EXIT](#exit)
3   | 03  | [EXIT (VSE)](#exit)
6   | 06  | [LINK](#link)
7   | 07  | [XCTL](#xctl)
8   | 08  | [LOAD](#load)
8   | 08  | [CDLOAD (VSE)](#cdload)
9   | 09  | [DELETE](#delete)
9   | 09  | [CDDELETE (VSE)](#cddelete)
13  | 0D  | [ABEND](#abend)

### BLDL - Build a directory list {: #bldl}

``` hlasm
name     BLDL  0,list
```

Build a directory list for use with LOAD, DELETE, LINK and XCTL. 

After a BLDL, an individual table entry may be used in these macros via the DE= parameter.

#### Parameters

##### list

May be a label or (reg) and points to a storage area in
the following format:

format         | description
---------------|------------
H'count'       | The number of entries in the table. 

The following data structure represents an entry.

Data Storage    | description
----------------|------------
H'entry length' | The length of the following entry
CL8'name'       | The name of the program
XL2'00'         | TT (unused)
X'00'           | R (1=found)
X'00'           | K (unused)
X'00'           | Z (1=program found in storage)
X'00'           | C (unused)

* Entry length must be a minimum of 12, which would omit the Z and C fields.
* Names must be in alphameric order, a suffix of .390 is assumed.

#### Register Usage

* R1 = BLDL list
* R15= Return code

#### Return

Return code is passed in GR15:

* 0 - All programs found
* 4 - Some programs not found
* 8 - Invalid count or invalid entry length

#### Usage

``` hlasm
         BLDL  0,LIST1
         LOAD  DE=BLDL2
......
LIST1    DC    H'2'
BLDL1    DC    H'14',CL8'MYPROG1',XL6'00'
BLDL2    DC    H'14',CL8'MYPROG2',XL6'00'
```

!!! Note 
    In z390, there is no performance benefit in issuing a BLDL before a LOAD, DELETE, LINK or XCTL.

### LOAD - Load a program or module {: #load}

``` hlasm
name     LOAD  EP=,EPLOC=,DDNAME=,DSNAME=,DE=,LOADPT=
``` 
Load a program or module.

#### Parameters

See [Common Program load parameters](#common-program-load-parameters).

#### Register usage

* R0 = Pointer to program name or BLDL entry, returned address
* R1 = Returned length
* R15= Path pointer and return code

#### Return

GR0 returns the address of the loaded module.

If LOADPT is used, then GR0 may be stored at a label, or the address contained in a general register.

GR1 returns the length as follows:

1. For a program, the number of doublewords (8-byte units).
2. For other modules, the length rounded up to the next doubleword boundary.

GR15 has a return code:

* 0 - Load ok
* 4 - Module not found

#### Abends

* S80A - Out of memory

### CDLOAD - Load a program or module (VSE) {: #cdload}

``` hlasm
name     CDLOAD phasename  Maps to LOAD EP=phasename
name     CDLOAD (reg)      Maps to LOAD EPLOC=(reg)
```

Load a program or module (VSE only).

#### Parameters

See [Common Program load parameters](#common-program-load-parameters).

#### Register Usage

* R0 = Pointer to program name, returned address
* R1 = Returned address
* R15= Return code

#### Return

GR0 and GR1 return the address of the loaded module. Length is not returned.

GR15 has a return code:

* 0 - Load ok
* 4 - Module not found

#### Abends

* S80A - Out of memory

### DELETE - Delete a program or module {: #delete}

``` hlasm
name     DELETE EP=,EPLOC=,DDNAME=,DSNAME=,DE=
```

Delete a program or module.

#### Parameters

See [Common Program load parameters](#common-program-load-parameters).

#### Register Usage

* R0 = Pointer to program name or BLDL entry
* R15= Path pointer

#### Return

GR15 has a return code:

* 0 - Load ok
* 4 - Module not found

### CDDELETE - Delete a program or module (VSE) {: #cddelete}

``` hlasm
name     CDDELETE phasename       Maps to DELETE EP=phasename
name     CDDELETE (reg)           Maps to DELETE EPLOC=(reg)
```

Delete a program or module. (VSE only)

#### Parameters

See [Common Program load parameters](#common-program-load-parameters).

#### Register Usage

* R0 = Pointer to program name

#### Return

GR15 has a return code:

* 0 - Load ok
* 4 - Module not found

### LINK - Load and pass control {: #link}

``` hlasm
name     LINK  EP=,EPLOC=,DDNAME=,DSNAME=,DE=,PARAM=,VL=
```

Load and pass control to another program. Return to 'linker'.

#### Parameters

See [Common Program load parameters](#common-program-load-parameters).

#### Register Usage

* R0 = Pointer to program name or BLDL entry
* R1 = Parameter list
* R15= Path pointer

#### Abends

* S806 - Module not found
* S80A - Out of memory

### XCTL - Load and pass control {: #xctl}

``` hlasm
name     XCTL  (fromreg,toreg),EP=,EPLOC=,DDNAME=,DSNAME=,DE=,PARAM=,VL=
```

Load and pass control to another program. Return to last 'linker' or terminate.

#### Parameters

See [Common Program load parameters](#common-program-load-parameters).

**(fromreg,toreg)**

Optional - Restores the specified register range from the savearea
pointed to by GR13. The registers are restored from their
conventional positions. The range must not specify or
include the following general registers: 0, 1, 13, 15

#### Register Usage

* R0 = Pointer to program name or BLDL entry
* R1 = Parameter list
* R15= Path pointer
* All registers in the range fromreg-toreg

#### Abends

* S806 - Module not found
* S80A - Out of memory

### RESTORE - Restores registers {: #restore}

``` hlasm
name     RESTORE (fromreg,toreg)
```

Restores the specified register range from the save area pointed
to by GR13. The registers are restored from their conventional
positions.

#### Register Usage

All registers in the range fromreg-toreg

### SNAP - Produces a component dump {: #snap}

``` hlasm
name     SNAP  STORAGE=(from,to),PDATA=(options, ... ),ID=,TEXT=
```

Produces a component dump on the z390 console without terminating the program.

#### Parameters

**STORAGE=(from,to) or STORAGE=((reg1),(reg2))**

Optional parameter to dump some storage.

* Either 'from' or 'to' can be labels or register pointers.
* The first byte displayed is 'from' and the last is 'to'-1.

**PDATA=(options ... )**

Optional parameter to display registers and/or control blocks.
Default is `PDATA=ALL`.

option  | description
--------|------------
ALL     | Display all registers, control blocks and storage.  When the STORAGE parameter is present only that area of storage is displayed.
REGS    | Display all general and floating point registers.
GPR     | Display general registers.
FPR     | Display floating point registers.
CDE     | Display information related to loaded programs or modules.
DCB     | Display information related to opened and closed files.

**ID=nnnnn or ID=(reg)**

Numeric identifier, either numeric value or general register containing the identifier.

Specify values 0-32767, higher values are negative.

**TEXT=string or TEXT='a string' or TEXT=(reg)**

Character identifier.

Specify either a string without blanks, a string constant  enclosed by single quotes or a general register pointing to
a string terminated by X'00'.
The string in all cases is limited to 60 bytes.
 
#### Register Usage

* R0 = ID and flags
* R1 = TEXT pointer
* R14= STORAGE from
* R15= STORAGE to

### ABEND - Terminate program {: #abend}

``` hlasm
name ABEND id,DUMP
```

Terminate the program.

#### Parameters

**id**

* Optional numeric identifier.
* Values from 0 to 4095.
* Displayed as abend `Unnnn`.

**DUMP**

A dump is always produced, overrides the NODUMP parm on ez390.

All storage areas are dumped.

#### Register Usage

* R1 = id and flags

### ESTAE, ESTAEX - Define Abend exit processing {: #estae}

!!! Info
    ESTAEX is provided for compatibility, only ESTAE is described here.

``` hlasm
name     ESTAE  label,type,PARAM=
name     ESTAE  (reg),type,PARAM=
name     ESTAE  0
```

When a program abends, control is given to the label or address specified. 

#### Parameters

**0**

ESTAE 0 is used to cancel any previously established ESTAE routine.

**type**

Type is optional with default value of CT

* CT - adds a new exit
* OV - replaces an existing exit

**PARAM**

PARAM=label is optional
PARAM=(reg) is optional

When specified, the address of the label or the contents of the register are made available in the ESTAE control block at ESTAPARM.

#### Exit invocation

* GR15 will contain the entry point, it is recommended that GR15 not be used as the base for the ESTAE routine.
* GR1 contains the address of the SDWA control block.
* The DSECT for the SDWA is generated by macro IHASDWA.
* This area may also be addressed by using the ZCVT.

After processing the abend, several options are available:

1.  Cancel the exit and retry the failing instruction. Issue an ESTAE 0.
    Load GR0 with the address of the failing instruction, which is in the rightmost four bytes of the SDWA field SDWAEC1 and ensure that GR15=4, then return via BR R14. This will cancel the ESTAE and re-execute the instruction that caused the abend. If the instruction abends again, it will terminate the program. 
    
    Sample code snippet:

                 USING SDWA,R1
                 L     R0,SDWAEC1+4
                 LA    R15,4
                 BR    R14

2.  Enter a retry or cleanup routine.
    Place the retry address in GR0 and ensure GR15=4.
    If ESTAE 0 has not been issued, then the ESTAE routine
    remains active.
3.  Percolate through other recovery (ESTAE) routines.
    The current ESTAE routine is automatically cancelled.
    Set GR15=0 and return via BR R14.
    This will invoke previous recovery routines or abend the
    program.
    When percolate happens all LINK stack entries at a lower
    level than the latest ESTAE will be purged.
 
!!! Note
    * In the z390 environment the abend code 0C5 may be caused by an internal error as well as a genuine addressing exception.
    * If an abend occurs after the ESTAE exit is invoked and before ESTAE 0 or BR R14 are issued, then the program will be terminated.
#### Register Usage

* R0 = exit address and flags
* R1 = parameter list
* R15= return code

#### Return

GR15 has a return code:

* 0 - ESTAE ok

#### Abends

* SFFF - ESTAE stack exceeded

### ESPIE - Interrupt exit processing {: #espie}

``` hlasm
name     ESPIE  SET,addr,list,PARAM=
name     ESPIE  SET,(reg),list,PARAM=
name     ESPIE  RESET
```

When a program interruption occurs eg. fixed point overflow, control is given to the label or address specified. 

#### Parameters

**RESET** 

Will reset any previous ESPIE settings.

**list**

Set interupts that should be processed.

If any of the codes 8, 10, 13 or 14 are specified, then the
appropriate bit is set on in the PSW using the SPM instruction.

The following interruption codes can appear in the list.

* 1 - operation
* 2 - privilege
* 3 - execute
* 4 - protection
* 5 - addressing
* 6 - specification
* 7 - data exception
* 8 - fixed point overflow (SPM mask bit X'8')
* 9 - fixed point divide
* 10 - decimal overflow (SPM mask bit X'4')
* 11 - decimal divide
* 12 - HFP exponent overflow
* 13 - HFP exponent underflow (SPM mask bit X'2')
* 14 - HFP significance (SPM mask bit X'1')
* 15 - HFP divide

``` hlasm
         ESPIE SET,label,8              Single code
         ESPIE SET,label,(1,4,6)        Multiple codes
         ESPIE SET,label,((2,6))        Range of codes: 2 through 6
         ESPIE SET,label,(3,5,(7,9),14) Mixed codes: 3,5,7,8,9,14
```

**PARAM** (optional)

``` hlasm
PARAM=label
PARAM=(reg)
```

When specified the address of the label or the contents of the register are made available in the ESPIE control block.

When the exit is invoked, GR1 contains the address of the EPIE control block. The macro IHAEPIE generates the DSECT for the EPIE.

The EPIE control block is located in the ZCVT and may also be addressed by the ZCVT and EPIED macros.

!!! Note
    In the z390 environment, interruption code 5 may be caused
    by an internal error as well as a genuine addressing exception.

#### Register Usage

* R0 = program mask
* R1 = exit address
* R15= parameter list

### SUBENTRY - Program entry {: #subentry}

``` hlasm
name     SUBENTRY CSECT=,BASES=,RENT=,RWA=,RWALNG=,STACK=,PSTACK=,PCHECK=
```

Provides a standard entry for programs.

#### Parameters

**name** (optional)

Although name is optional, care needs to be taken if it is omitted. 
A CSECT or sub-program should be named.

**CSECT=**

This is useful for setting up sub-programs within the main program that are invoked by the CALL macro.

* CSECT=YES (default) -  Generates: name CSECT Standard entry.
* CSECT=NO - Generates: name DS 0D

**BASES=**

Override and extend the base registers for this program.
Default BASES=(13)

**RENT**

Defines a re-entrant program.

_RENT=NO (default)_

A standard savearea is built, and GR13 is set as the default
base register. This also serves as the pointer to the program's
savearea to facilitate further linkage.

The default base register GR13 may be overridden by the BASES=
parameter (qv).

It is recommended that the first register specified is GR13,
if it isn't then a program is generated with non-standard
linkage and may cause problems.

_RENT=YES_

The GETMAINed area described below is defined in the SUBENTRY macro.

BASES= must be specified and the first register must not be GR13.

Storage is GETMAINed and GR13 is set to the savearea within this storage. 

The STACK= parameter can generate multiple saveareas.

The first register specified must NOT be 13.

Each additional register generates the code and USING at the
standard 4K intervals. eg. BASES=(13,7,8) will cover 12K of
code.

**STACK=n**

Requires RENT=YES, default 0.

Generates an addition to the GETMAINed area acquired allowing for multiple saveareas, each of these may have an additional read-only work area defined by RWALNG.

**RWA=dsectname**   (requires RENT=YES)

RWA= defines the DSECT associated with this work area.

**RWALNG=n**        (requires RENT=YES, default 0)

RWALNG defines the length of an additional work area to each savearea defined by STACK=.

**PSAVE**

Causes extra instructions to save and restore GR14 and GR15 registers.

* PSAVE=YES (default) - Save and restore GR14 and GR15
* PSAVE=No - No save performed

**PSTACK=reg** (requires RENT=YES, default=0)

If PSTACK=0, then the user area address of each stack entry is stored at offset +80.

Otherwise the user area address is not stored at offset +80, but loaded into the register specified.

**PCHECK** (requires RENT=YES, default=YES)

* PCHECK=YES clears the stack area and sets the senior bit of the front and end pointers to 1.

#### Register Usage

* R0,1,2,13,14,15 have multiple uses
 
### SUBEXIT - Program exit {: #subexit}

``` hlasm
name     SUBEXIT RC=returncode
name     SUBEXIT RC=(reg)
```
 
Provides a standard exit for programs.
If SUBENTRY used the parameter RENT=YES then the whole stack area will be FREEMAINed before GR15 is set.

#### Parameters

**name** (optional)

RC will return the value in GR15, zero is the default. 

#### Register Usage

All registers may be affected
 
### PERFORM or PM - Branch to local procedure {: #perform}

``` hlasm
name     PERFORM procedure
name     PM    procedure
```

Generate a branch to a local procedure with base addressability.

PERFORM and PM are identical macros.
 
Uses MVC and B if [SUBENTRY](#subentry-program-entry) RENT=NO or push/pop stack if RENT=YES.

#### Register Usage

* R14=Return address
* R15=Linkage register

### PENTRY - Define local procedure {: #pentry}

``` hlasm
name     PENTRY
```

Define local procedure using name.

Generates an entry-point for a local procedure preceded with a branch instruction if [SUBENTRY](#subentry-program-entry) RENT=NO.

### PEXIT - Exit local procedure {: #pexit}

``` hlasm
name     PEXIT
```

Branch to last caller of local procedure.

Generate branch to last [PENTRY](#pentry-define-local-procedure) name address - 4.
If SUBENTRY RENT=NO or generate decrement stack pointer, load, and branch if RENT=YES.

#### Register Usage

* R14=Stack address
* R15=Saved linkage register

### EXIT - Return to last caller {: #exit}

``` hlasm
name     EXIT
```

Returns immediately to the last caller.

* No registers are restored.
* Use of [SUBEXIT](#subexit-program-exit) is preferred.

#### Register Usage

No registers affected

### EOJ (VSE only) {: #eoj}

``` hlasm
name     EOJ  RC=returncode
name     EOJ  RC=(reg)
```

Returns immediately to the last caller.

#### Parameters

name is optional.
RC will return the value in GR15, zero is the default.

#### Register Usage

* R15= Return code

### CALL (list form) - Internal/external subroutine call {: #call-list}

``` hlasm
name     CALL  ,(parm1,parm2,...),VL,MF=L
```

Generates a parameter list for use with the execute form of CALL. `name DC A(parm1,parm2...)`

#### Parameters

**(parm1,parm2,...)**

The parameters can be anything that is permitted in an A-type constant. 
Note that register forms like (R5) are not  interpreted as general registers, 
but as constants.

**VL** (optional)

If the called program can accept a variable parameter list, then VL will turn on the senior bit (bit 0) of the last parameter.

### CALL (execute form) - Internal/external subroutine call {: #call-exec}

``` hlasm
name     CALL  routine,(parm1,parm2,...),VL,LINKINST=,MF=(E,parms)
name     CALL  (reg),(parm1,parm2,...),VL,MF=(E,parms)
```

Provides a standard internal or external subroutine call.

#### Parameters

Parameters are addressed by GR1 and linkage by GR14 routine.
If a label, it can be internal (resolved at assembly time) or external (loaded and resolved by the linkage editor).
If the routine is in register notation, it can be internal or separately loaded.

**(parm1,parm2,...)**

Modify a fixed or variable parameter list to be accessed by the called program.
The parameter list must have initially been defined using the list form of the CALL. 
The parameters specified here will overlay that parameter list. 

It is important that the number of parameters specified here does not exceed those specified in the list form of the CALL.

The parameters can be anything that is permitted in an A-type constant. 
Any parameters bounded by parentheses, eg. (R5) are  assumed to be registers or register equates. 
The content of each register (fullword only) is stored at the parameter location.

**VL** (optional)

If the called program can accept a variable parameter list, then VL will turn on the senior bit (bit 0) of the last
parameter. 

!!! Note
    This is the last parameter in the modified parameter list.

**LINKINST=** (optional)

Determines the calling instruction.

Choose BALR (default) or BASR.

**MF=(E,label) or  MF=(E,(reg))**

The label or register points to a parameter list previously defined with the list form of the CALL.

#### Usage

Call subroutine MYSUBR, replace the two parameters and mark the last parameter.

``` hlasm
MYCALL   CALL  MYSUBR,(8,MYDATA),VL,MF=(E,PARMS)
......
PARMS    CALL  ,(7,OLDDATA),VL,MF=L
```

### CALL (standard form) - Internal/external subroutine call {: #call-std}

``` hlasm
name     CALL  routine,(parm1,parm2,...),VL,LINKINST=,MF=I
name     CALL  (reg),(parm1,parm2,...),VL,LINKINST=,MF=I
```

Provides a standard internal or external subroutine call.

#### Parameters

Parameters are addressed by GR1 and linkage by GR14 routine.

If a label, it can be internal (resolved at assembly time) or external (loaded and resolved by the linkage editor).

If the routine is in register notation, it can be internal or separately loaded.

**(parm1,parm2,...)** (optional)

Pass a fixed or variable parameter list to the called program.
The parameters can be anything that is permitted in an A-type constant. 
Any parameters bounded by parentheses, eg. (R5) are assumed to be registers or register equates. 
The content of each register (fullword only) is stored at the parameter location.
 
**VL** (optional)

If the called program can accept a variable parameter list, then VL will turn on the senior bit (bit 0) of the last
parameter.

**LINKINST=** (optional)

Determines the calling instruction. Choose BALR (default) or BASR.

#### Usage

Call subroutine MYSUBR, pass two parameters and mark the last parameter.

``` hlasm
MYCALL   CALL  MYSUBR,(8,MYDATA),VL
```

#### Register Usage

* R0 = indirect parameter list
* R1 = parameter list
* R14= linkage
* R15= program location
 
### SAVE - Save registers {: #save}

``` hlasm
name     SAVE  (fromreg,toreg)
```

Saves the specified register range in the save area pointed to by GR13. 
The registers are saved in their conventional positions.

### RETURN - Restore registers {: #return}

``` hlasm
name    RETURN (fromreg,toreg),flag,RC=
```

Restores the specified register range from the save area pointed to by GR13.
The registers are restored from their conventional positions.

Return is by the restored GR14.

#### Parameters

**flag** (optional)

`T` specifies that the byte at savearea+15 has the junior bit (bit 7) 
turned on to indicate a return to a called program. 

This bit (rightmost bit of saved GR14) is set after GR14 has been loaded with the return address.

**RC=nn or RC=(reg)**

If RC is omitted, GR15 is assumed to contain the return code.

GR15 is loaded with this return code before returning via GR14.
RC may have a numeric value or the value may be in GRreg.

#### Register Usage

* R15= Return code
* All registers in the range fromreg-toreg

#### Usage

``` hlasm
MYRET    RETURN (14,12),T,RC=12
```

Restore registers 14 through 12. After the register restore, flag the savearea to indicate return 
to caller and set return code to 12.

### PSAD - PSA structure {: #psad}

Provides a DSECT for the limited fields available in the first 8K of memory (PSA). The CVT may be addressed from here.

### ZCVTD - ZCVT structure {: #zcvt}

Provides a DSECT for the limited fields available in the ZCVT. This follows the PSA and may be addressed as follows:

``` hlasm
         L     reg,ZCVT
         USING IHAZCVT,reg
......
         ZCVTD
```

### CVTD - CVT structure {: #cvt}

Provides a DSECT for the limited fields available in the Communications Vector Table. This may be addressed as follows:

``` hlasm
         L     reg,X'10'
         USING IHACVT,reg
......
         CVTD
```

### EQUREGS - Register equates {: #equregs}

``` hlasm
         EQUREGS REGS=option,TYPE=option
```

Generates standard equates for the general or floating point registers.

No parameters passed:
``` hlasm
         EQUREGS    (defaults to REGS=GPR,TYPE=DEC)
R0       EQU   0
......
R15      EQU   15
```
TYPE=HEX:
``` hlasm
EQUREGS  TYPE=HEX
R0       EQU   0
......
RF       EQU   15
```

REGS=FPR:
``` hlasm
EQUREGS  REGS=FPR
F0       EQU   0
......
F15      EQU   15
```

REGS=FPR,TYPE=HEX
``` hlasm
EQUREGS  REGS=FPR,TYPE=HEX
F0       EQU   0
......
FF       EQU   15
```

### YREGS - General register equates {: #yregs}

YREGS is identical to [EQUREGS](#equregs-register-equates) with default parameters which will generate general register equates.


### Additional information

#### Use counts and parameter passing

On the first invocation and after a [LOAD](#load-load-a-program-or-module), [LINK](#link-load-and-pass-control) or [XCTL](#xctl-load-and-pass-control), the program receiving control has its use 
count incremented.

When a program is DELETEd, it terminates or loses control via an XCTL, then the use count is 
decremented.  When the use count is zero, the storage for that program is freed. 
When passing parameters it is important to consider whether those parameters are in a program whose 
storage may be reused. If in doubt, place parameters for passing on, in a separate GETMAINed area.

#### Common program load parameters

The following parameter descriptions apply to the [LOAD](#load-load-a-program-or-module), [CDLOAD](#cdload-load-a-program-or-module-vse), [DELETE](#delete-delete-a-program-or-module), [CDDELETE](#cddelete-delete-a-program-or-module-vse), 
[LINK](#link-load-and-pass-control), [XCTL](#xctl-load-and-pass-control) and [RESTORE](#restore-restores-registers) macros.

When a program is loaded (with suffix .390) then relocation takes place.

* EP, EPLOC, DDNAME and DSNAME are used to locate the program or module.
    * EP, EPLOC and DE are mutually exclusive.
    * DDNAME and DSNAME cannot both be present.

##### EP=program

Specify the program name, maximum 8 bytes.

##### EPLOC=label or EPLOC=(reg)

The label or the register must point to an 8-byte field containing the program name.

##### DE=label or DE=(reg)

The label or the register must point to a BLDL entry.
In all the above cases as neither DDNAME nor DSNAME are specified,
then the z390 search path is used. This may be overridden by the ez390 parameter SYS390.

##### EP/EPLOC/DE and DDNAME=name or EP/EPLOC/DE and DDNAME=(reg)

DDNAME has or points to the name of an environment variable.

This variable may contain:

* The complete path and filename. In this case EP, EPLOC or DE are ignored.
* A single path. Only this path will be searched.
* Multiple paths separated by plus signs.

All paths will be searched in the order specified.

``` hlasm
         LINK  EP=MYLINK,DDNAME=MYPATH ...
```

For execution, define a script file setting the environment variable `MYPATH`

Program specified with EP ignored

``` dos
SET MYPATH=c:\path\to\390\file.390 
```

Single search path

``` dos
SET MYPATH=c:\path\to\folder
```

Multiple search paths

``` dos
SET MYPATH=c:\path\to\dir1+c:\path2 
```

##### EP/EPLOC/DE and DSNAME=name or EP/EPLOC/DE and DSNAME=(reg)

DSNAME is or points to a label defined in the program which has the file spec.

The file spec must terminate with X'00' or be defined as a double-quoted string within the standard C-type constant.

Either constant may contain:

* The complete path and filename. In this case EP, EPLOC or DE are ignored.
* A single path. Only this path will be searched.
* Multiple paths separated by semicolons or plus signs. All paths will be searched in the order specified.
 
``` hlasm
         LINK EP=MYLINK,DSNAME=MYPATH ...
......
MYPATH   DC C'c:\path\to\file',X'00'
MYPATH   DC C'"c:\path1;c:\path2"'
```

!!! Note
    In the above cases where the filename is specified in the environment variable or 
    the DC constant, then the .390 suffix should be omitted.
    The exceptions to this are [LOAD](#load-load-a-program-or-module) and [DELETE](#delete-delete-a-program-or-module), which may be used to
    load or delete a non-program module and may have any suffix appended.

##### PARAM= and VL=

Only available on [LINK](#link-load-and-pass-control) and [XCTL](#xctl-load-and-pass-control).

Used for passing a fixed or variable parameter list to a program.

``` hlasm
PARAM=(parm1,parm2,...)
```

The parameters can be anything that is permitted in an A-type constant. 
When the program is invoked GR1 points to the parameter list.

See [Use counts and parameter passing](#use-counts-and-parameter-passing) below for special considerations.

##### VL=0 or VL=1

* Default - VL=1

If the called program can accept a variable parameter list, then VL=1 will turn on the senior bit (bit 0) of the last
parameter.
