# Storage services

## Macro reference

### SVC functions

The following is a list of the z390 SVC services that support the macros.

DEC | HEX | Service
----|-----|--------
4   | 04  | [GETMAIN](#getmain)
4   | 04  | [GETVIS (VSE)](#getvis)
5   | 05  | [FREEMAIN](#freemain)
5   | 05  | [FREEVIS (VSE)](#freevis)

### Supporting macros

* [STORAGE](#storage) - Obtain/release storage
* [CPOOL](#cpool) - Obtain a storage area

### STORAGE - Obtain/release storage {: #storage}

``` hlasm
name     STORAGE OBTAIN,LENGTH=,LOC=,COND=  Acquire storage
name     STORAGE RELEASE,ADDR=,LENGTH=      Free storage
```

An alternative to [GETMAIN](#getmain) and [FREEMAIN](#freemain).

* LENGTH= becomes the GETMAIN/FREEMAIN parameter LV=
* LOC= is the same
* COND=NO (default) is the GETMAIN type R
* COND=YES is the GETMAIN type RC
* ADDR= becomes the GETMAIN/FREEMAIN parameter [A](#a-optional)

### GETMAIN - Acquire storage {: #getmain}

```
name     GETMAIN type,LV=,LOC=,A=
```

#### Parameters

##### type

Value | Description
------|------------
*R*   | Obtain storage unconditionally. The default location is below 16M.
*RC*  | Obtain storage conditionally. The default location is above 16M. The return code indicates whether the acquisition was successful.
*RU*  | Obtain storage unconditionally. The default location is above 16M.

##### LV

Value | Description
------|------------
LV=n  | Obtain n bytes (maximum value of n is 2G-1)
LV=nK | Obtain nK bytes (maximum value of n is 2097151)
LV=nM | Obtain nM bytes (maximum value of n is 2047)
LV=(reg) | Length required is in GR reg

!!! Note 
    All storage requests will be rounded up to the next 8-byte boundary
    and is not initialized.

##### LOC (optional)

If omitted:

* type=R default to LOC=ABOVE
* type=RC default to LOC=BELOW 
* type=RU default to LOC=BELOW

Value     | Description
----------|------------
LOC=BELOW<br/>LOC=RES<br/>LOC=24<br/>LOC=(24) | Try to acquire storage below 16M.
LOC=ABOVE<br/>LOC=ANY<br/>LOC=31<br/>LOC=(31)<br/>LOC=(24,31)<br/>| Try to acquire storage above 16M.

##### A (optional)

After successful completion GR1 will contain the address of the 
acquired storage. This 4-byte address may be placed at label,
or at the address in GRreg.

The length will be returned in GR0, rounded as necessary.

#### Usage

GETMAIN 1024 bytes below 16M, unconditionally.

``` hlasm 
         GETMAIN R,LV=1024
```

GETMAIN number of bytes in GR3 below 16M, conditionally,

``` hlasm
         GETMAIN RC,LV=(R3),LOC=BELOW
``` 

#### Memory allocation

Storage is limited by the `MEM(nnn)` parameter on *ez390* with nnn in megabytes. 
The default is `MEM(1)`.

When the value is 16 or less then all GETMAINs will allocate storage LOC=BELOW. 

When the value is above 16, then 16M bytes is available LOC=BELOW and the rest 
LOC=ABOVE.

There is a preset maximum of `MEM(50)` set by Java. If this is insufficient, 
then code `-Xmx nnnnnnnnn` after the `-Xrs` option on *ez390* to extend the MEM 
limit.

!!! Warning
    Over-extending memory this way may degrade the performance of your
    operating system.

#### Register Usage

* R0 = Input flags, output length
* R1 = Input length, output address
* R15= Return code

#### Return

GR15 has a return code:

* 0 - GETMAIN ok
* 4 - Conditional request unsuccessful

#### Abends

* S804 Invalid request - Can occur if LRECL/BLKSIZE on a DCB are both zero
* S80A Unconditional out of memory

### GETVIS - Acquire storage (VSE) {: #getvis}

``` hlasm
name     GETVIS LENGTH=,ADDRESS=,LOC=
```

All forms map to GETMAIN R

LENGTH and ADDRESS are mandatory.

Parameters map to GETMAIN as follows:

* LENGTH=n Maps to LV=n
* LENGTH=(reg) Maps to LV=(reg)
* ADDRESS=label Maps to A=label
* ADDRESS=(reg) GR reg is not a pointer, the GETMAINd area
* address is placed in GR reg.
* LOC= Maps the same.

#### Register Usage

* R0 = Input flags, output length
* R1 = Input length, output address
* R15= Return code

#### Return

GR15 has a return code:

* 0 - GETVIS ok

#### Abends

* S804 Invalid request - Can occur if LRECL/BLKSIZE on a DCB are both zero
* S80A Unconditional out of memory

### FREEMAIN - Free storage {: #freemain}

``` hlasm
name     FREEMAIN LV=,LA=,A=
```

Specify either LV= or LA=, if both are present LV= will be ignored.

#### Parameters

##### LV

Value | Description
------|------------
LV=n  | Free n bytes (maximum value of n is 2G-1)
LV=nK | Free nK bytes (maximum value of n is 2097151)
LV=nM | Free nM bytes (maximum value of n is 2047)
LV=label | label must be an equated value
LV=(reg) | Length to be freed is in GRreg


##### LA

Value    | Description
---------|------------
LA=label | The location of a 4-byte length
LA=(reg) | GR reg must point to a 4-byte length

##### A (optional)

If A= is omitted then GR1 must contain the address of the storage to be freed.

Value   | Description
--------|------------
A=label | The location of the 4-byte address of the storage to be freed.
A=(reg) | The 4-byte address of the storage to be freed is in GR reg.

!!! Note
    The storage address specified must be on a doubleword boundary.
    A section of a previous GETMAIN may be freed.
    It is the programmer's responsibility to manage the resulting fragmentation.

#### Register Usage

* R0 = length
* R1 = address
* R15= return code

#### Return

GR15 has a return code:

* 0 - FREEMAIN ok

#### Abends

* S804 Invalid request
* S90A Attempt to FREEMAIN an area which is not on a doubleword boundary
* SA0A Attempt to FREEMAIN an area already free

### FREEVIS - Free storage (VSE) {: #freevis}

``` hlasm
name     FREEVIS LENGTH=,ADDRESS=
```

All forms map to FREEMAIN R

Defaults are LENGTH=(0) and ADDRESS=(1).

Parameters map to FREEMAIN as follows:

* LENGTH=n Maps to LV=n
* LENGTH=(reg) Maps to LV=(reg)
* ADDRESS=label Maps to A=label
* ADDRESS=(reg) Maps to A=(reg)

#### Register Usage

* R0 = length
* R1 = address
* R15= return code

#### Return

GR15 has a return code:

* 0 - FREEVIS ok

#### Abends
 
* S804 Invalid request

### CPOOL - Obtain a storage area {: #cpool}

``` hlasm
         CPOOL BUILD,CPID=label,PCELLCT=nnn, SCELLCT=nnn,CSIZE=nnn,HDR='...'
```

BUILD obtains a storage area divided into cells.
Each subsequent GET retrieves the next cell. 

!!! Warning
    FREE and DELETE are not yet implemented.

* CPID will contain the address of the GET routine for this cell
* PCELLCT contains the primary number of cells
* SCELLCT contains the secondary number of cells 
* CSIZE is the size of each cell
* HDR is an optional string to identify the pool

The GETMAIN size will be (PCELLCT+SCELLCT)*CSIZE

``` hlasm
         CPOOL GET,CPID=label
```

Get the next cell in this pool, GR1 contains the address or zero if all cells 
have been read. 