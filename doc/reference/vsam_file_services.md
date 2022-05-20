# zVSAM services

With the exception of the DDNAME parameter explained below, all supported 
parameters are implemented compatibly with IBM's VSAM implementation. For 
details, please refer to [_z/OS DFSMS Macro Instructions for Data Sets_ (SC23-6852-02)](https://www.ibm.com/docs/en/zos/2.1.0?topic=instructions-vsam-macro-descriptions-examples).

## Macro reference

### ACB - Access control block

``` hlasm
label    ACB   AM=VSAM,                                      X
               DDNAME=ddname,                                X
               PASSWD=ptr,                                   X
               EXLST=ptr,                                    X
               MACRF=(keyword list),                         X
               BUFSP=nr,                                     X
               BUFND=nr,                                     X
               BUFNI=nr,                                     X
               RMODE31=keyword,                              X
               STRNO=1,                                      X
               BSTRNO=nr,                                    X
               MAREA=ptr,                                    X
               MLEN=nr,                                      X
               RLSREAD=keyword,                              X
               SHRPOOL=nr
```


The ACB macro will generate an ACB and initialize it according to the parameters 
specified on the macro invocation.

Direct access to subfields in the ACB is discouraged. Use SHOWCB ACB=, TESTCB 
ACB= and/or MODCB ACB= to inspect, test, and/or modify the ACB's content.

All keywords on the ACB macro are optional. Before the cluster is opened, all 
ACB values can be modified using MODCB ACB=, or by changing the ACB directly. 
The latter is not recommended, as it is not guaranteed to be portable or 
compatible with future versions of zVSAM.

#### Parameters

##### AM - Access method

Designates this ACB as a zVSAM ACB.

##### DDNAME - Data name

DDNAME refers to the name of an environment variable in the host OS. 

This variable in turn should contain the path and qualified filename of the 
cluster to be opened. The qualifier is the name of an environment variable in 
the host OS and is the path to the assembled catalog.

```dos
SET ddname=drive:\path\catalog.filename
SET catalog=drive:\path 
```

!!! Warning
    The ddname variable may only contain one dot

##### PASSWD - Password pointer

Pointer to password for the cluster.
Points to a single byte length followed by the password. 

``` hlasm
X'05',C'ABCDE'
```

##### EXLST - Exit list pointer

Pointer to an exit list.  See [EXLST macro]() for more details.

##### MACRF - Processing options

A list of keywords is provided.

``` hlasm
MACRF=(KEY,DIR)
```

Keyword group   | Keyword | Description
----------------|---------|------------
[ADR, KEY, CNV] | ADR     | Addressed access to ESDS by (X)RBA. Using (X)RBA to access a KSDS is not supported.
                | KEY     | Keyed access to a KSDS. RRN access to an RRDS.
                | CNV     | Not supported. Keyword is flagged with a warning message. 
[DFR \| NDF]    | DFR     | Allow writes to be deferred
                | NDF     | Do not defer writes
[DIR, SEQ, SKP] | DIR     | Direct access to ESDS, KSDS or RRDS
                | SEQ     | Sequential access to ESDS, KSDS or RRDS
                | SKP     | Skip sequential access to KSDS or RRDS. Only for keyed access. Allows the use of POINT.
[IN, OUT]       | IN      | Read only access for ESDS, KSDS or RRDS
                | OUT     | Both read and write access for ESDS, KSDS or RRDS
[NIS \| SIS]    | NIS     | Normal Insert Strategy for KSDS
                | SIS     | Sequential Insert Strategy for KSDS
[NRM \| AIX]    | NRM     | DDNAME indicates cluster to be processed
                | AIX     | DDNAME of a path to access an AIX directly, rather than using it to access records in the underlying base cluster.
[NRS \| RST]    |         | Not supported. Keyword is flagged with a warning message 
[LSR \| GSR \| NSR \| RLS] || Local, Global or no Shared Buffers. RLS is not supported
[NUB \| UBF]    |         | Not supported. Keyword is flagged with a warning message 
[CFX \| NFX]    |         | Not supported. Keyword is flagged with a warning message 
[DDN \| DSN]    |         | Not supported. Keyword is flagged with a warning message 
[ICI \| NCI]    |         | Not supported. Keyword is flagged with a warning message 
[LEW \| NLW]    |         | Not supported. Keyword is flagged with a warning message

##### BUFSP - Buffer storage

Max amount of storage (in bytes) to use for buffers. 

Maximum buffer space in virtual storage for this cluster.

This is the combined size in bytes of all buffers allocated for this cluster. 
If (BUFND + BUFNI) * Block_size exceeds the value specified for BUFSP, then 
BUFND and BUFNI will be reduced proportionally to keep the total allocation 
below the limit specified in the BUFSP parameter.

##### BUFND - Data buffers

Number of data buffers to allocate for this ACB.

Specify a number between 1 and 65535.

##### BUFNI - Index buffers

Number of index buffers to allocate for this ACB.

Specify a number between 1 and 65535.

##### RMODE31 - Above the line storage usage

The default value for RMODE31 is NONE.

Specifies whether buffers and/or control blocks should be allocated below or 
above the 16M line:

option | effect
-------|-------
NONE   | Control Blocks and buffers below 16M
CB     | Control Blocks above or below 16M, buffers below 16M
BUFF   | Control Blocks below 16M, buffers above or below 16M
ALL    | Control Blocks and buffers above 16M or below 16M

##### STRNO - Concurrent requests

Number of concurrent requests allowable for this ACB. 

Specify a number between 1 and 255.

##### BSTRNO - Initial allocated requests

Beginning number of concurrent requests allocated to this ACB when a path is 
opened. Only applies if MACRF=NSR. 

Specify a number between 0 and 255.

##### Unsupported options

The following options are currently not supported.
If used, the keyword is flagged as ignored with a warning message.

* MAREA
* MLEN
* RLSREAD
* SHRPOOL


### OPEN - Open cluster

``` hlasm
label    OPEN  (entry[,entry]...),
         MODE=,
         MF=,
```

The open macro is used to open one or more clusters and/or one or more 
sequential files in a single call.

A cluster needs to be opened before it can be processed. 

#### Parameters

##### entry - ACB/DCB address

``` hlasm
addr[,options],[...]
```

The OPEN macro accepts a list of entries. Each entry consists of two consecutive 
parameters, address and options.

**addr - Address of ACB or DCB**

The address can be specified as an A-type address or as a register. If a register 
is coded the register number or name must be enclosed in parentheses. The address 
can be either the address of a DCB or the address of an ACB

**options**

* For DCB, options may be encoded according to the [z390 File Services](file_services.md).
* For ACB, options list is ignored and should be coded as an omitted parameter.

For ACB's, all options are taken from the ACB, not the open parm list.

Open parameter list entries have two different formats depending on the 
[MODE](#mode-residence-mode) parameter.

* MODE=24 `AL1(option),AL3(DCB/ACB address)` R1 points to the list.
* MODE=31 `AL1(option),XL3'00',AL4(DCB/ACB address)` R0 points to the list and R1=0.

Available option values:

* INPUT - X'40'
* OUTPUT - X'20'
* UPDATE - X'60'

The last entry has the X'80' bit on in option.

##### MF - Parm 

Available options:

option          | effect
----------------|-------
MF=I or omitted | An open parmlist is generated inline, plus a call to the OPEN SVC using the parmlist.
MF=L            | An open parmlist is generated inline.
MF=(E,addr)     | Code to modify/populate the open parameter list at the indicated address, which may be a relocatable constant or a (register), plus a call to the OPEN SVC using the parmlist.

##### MODE - Residence mode

Residency mode of all control blocks involved. 

* MODE=24 - Use below the line only 
* MODE=31 (Default) - if any resides above the line.

### EXLST - Exit list control block

``` hlasm
label    EXLST AM=VSAM, 
         EODAD=(addr[,mod]),
         LERAD=(addr[,mod]),
         SYNAD=(addr[,mod]),
         JRNAD=(addr[,mod]),
         UPAD=(addr[,mod]),
         RLSWAIT=(addr[,mod])
```

The EXLST macro will generate an exit list control block and initialize it 
according to the parameters specified on the macro invocation.

!!! Info
    The structure and layout of the generated EXLST are not part of the 
    interface and are therefore not shown. Direct access to subfields in the 
    EXLST is discouraged. Use SHOWCB EXLST=, TESTCB EXLST= and/or MODCB EXLST= 
    to inspect, test, and/or modify the EXLST's content.

All keywords on the EXLST macro are optional. Before the cluster is opened, all 
EXLST values can be modified using [MODCB EXLST=](), or by changing the EXLST 
directly. The latter is not recommended, as it is not guaranteed to be portable 
or compatible with future versions of zVSAM.

The AMODE for the exit routines is encoded in the address using the common 
convention.

For exit modifiers, if a routine is not active it will not be called by zVSAM.

The secondary modifier of `L` (for Load from linklib) is not currently supported.


#### Parameters

##### AM - Access method

AM=VSAM - Designates this EXLST as a zVSAM EXLST.

##### EODAD - End of data exit

``` hlasm
EODAD=(addr[,mod])
```

**addr - entry address** 

Optional parameter to specify the entry address of an exit that handles an 
end-of-data condition during sequential access.

**mod - modifier**

* A - Active 
* N - Not active

As long as the routine is not active it will not be called by zVSAM.

The secondary modifier of `L` (for Load from linklib) is not currently supported.

##### LERAD - Logical error analysis exit

``` hlasm
LERAD=(addr[,mod])
```

**addr - entry address** 

Entry address of an exit routine that handles logic errors.

The AMODE for the routine is encoded in the address using the common convention.

**mod - modifier**

* A - Active 
* N - Not active

As long as the routine is not active it will not be called by zVSAM.

The secondary modifier of `L` (for Load from linklib) is not currently supported.

##### SYNAD - Physical error analysis exit

``` hlasm
SYNAD=(addr[,mod])
```

**addr - entry address** 

Optional parameter to specify the entry address of an exit that handles physical 
errors.

**mod - modifier**

* A - Active 
* N - Not-active

### CLOSE - Close cluster

### RPL - Request parameter list

### POINT

### GET

### PUT

### ERASE

### CHECK

### ENDREQ

### VERIFY

### GENCB - Generate control block
 
### MODCB - Modify control block

### TESTCB - Test control block

### SHOWCB - Show control block





