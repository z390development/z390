# SVC services

## Macro reference

### TIME(SVC) - Obtain the time and date

``` hlasm
name     TIME  type,addr,LINKAGE=SVC,DATETYPE=,CLOCKTYPE=
```

Obtain the time and/or date in various formats.

#### Parameters

##### Type

**DEC - Decimal**

``` hlasm
name     TIME
name     TIME  DEC
```

Returns: Time in GR0 as HHMMSSTH

Hours, mins and secs to 2 decimal places.

The values are unsigned packed decimal:

    X'21420654' = 21:42:06.54

The MVO instruction can be used after storing the register to convert it to standard packed decimal format.

Date in GR1 as CCYYDDDF - Century, year, day number and sign. 

The values are signed decimal:

* CC is (almost) the century number.
* YY the year number.
* DDD the day number within the year.
* F the positive sign.

    X'0106003F' = 3rd January 2006

After storing, `AP DATE,=P'1900000'` can be used to convert to a 4-digit year.

**BIN - Binary**

``` hlasm
name     TIME  BIN
```
Returns: Time in GR0 in hundredths of a second since midnight in binary.

Date in GR1 as CCYYDDDF - Century, year, day number and sign. 

**TU - Time units**

``` hlasm
name     TIME  TU
```
Returns: Time in GR0 in timer units of 26.04166&micro;s since midnight in binary.
 
Date in GR1 as CCYYDDDF - Century, year, day number and sign. 

**INS - Instruction count**

``` hlasm
name     TIME  INS
``` 
Returns: Instruction count in GR1 (64 bit value).

**MIC - Microseconds**

``` hlasm
name     TIME  MIC,label
name     TIME  MIC,(reg)
```
Returns: Time in units of 1&micro;s in binary since midnight.

The time is stored at the 8 bytes specified.
Date in GR1 as CCYYDDDF - Century, year, day number and sign. 

**NS - Nanoseconds**

``` hlasm
name     TIME  NS,label
name     TIME  NS,(reg)
```
Returns: Time in units of 1ns in binary since midnight.

The time is stored at the 8 bytes specified.

**STCK - Microseconds bits 0-51**

``` hlasm
name     TIME  STCK,label
name     TIME  STCK,(reg)
```
Returns: Time in units of 1&micro;s in binary since midnight.

The time is stored at the 8 bytes specified and uses only bits 0-51 of the 8-byte field.
Date in GR1 as above.

**TS - Timestamp**

``` hlasm
name     TIME  TS,label
name     TIME  TS,(reg)
```
Returns: A string of 29 bytes at the label or pointed to by reg.

The format is "YYYY-MM-DD HH:MM:SS.NNNNNNNNN".

**CLOCK - Microseconds since 1/1/1900**

*CLOCKTYPE=STCK*

``` hlasm
name     TIME  CLOCK,label,CLOCKTYPE=STCK
name     TIME  CLOCK,(reg),CLOCKTYPE=STCK
```
Returns: Time in units of 1&micro;s in binary since 1st January 1900.

The time is stored at the 8 bytes specified and uses only bits 0-51 of the 8-byte field.

*CLOCKTYPE=STCKE*

``` hlasm
name     TIME  CLOCK,label,CLOCKTYPE=STCKE
name     TIME  CLOCK,(reg),CLOCKTYPE=STCKE
```
Returns: Time in units of 1&micro;s in binary since 1st January 1900.

The time is stored at the 16 bytes specified:

* Byte 0 : Zero
* Bytes 1-13 : The time
* Bytes 14-15 : Programmable field set by the SCKPF instruction and not currently implemented.

The time uses only bits 8-111 of the 16-byte field with bits 8-59 being the value in microseconds.

*CLOCKTYPE=JAVA*

``` hlasm
name     TIME  CLOCK,label,CLOCKTYPE=JAVA
name     TIME  CLOCK,(reg),CLOCKTYPE=JAVA
```
Returns: Time in units of 1ms in binary since 1st January 1970.

The time is stored at the 8 bytes specified.

#### Register Usage

* R0 = Code for units and date type
* R1 = Result area

#### Return

GR15 has a return code:

* 0 - TIME ok
* 4 - Invalid request

### TIME(SYSTEM) - Obtain the time and date

``` hlasm
name     TIME  type,addr,LINKAGE=SYSTEM,DATETYPE=,CLOCKTYPE=
```

#### Parameters

##### DATETYPE

Specify the date format returned.

All the formats are 4 bytes and the values are unsigned packed decimal.

The MVO instruction can be used to convert it to standard packed decimal format.

* YYYY the year number.
* DDD the day number within the year.
* DD the day number within the month.
* MM the month number.

``` text
YYYYDDD (default) stored as 0YYYYDDD
MMDDYYYY
DDMMYYYY
YYYYMMDD
```

##### Type

**DEC - Decimal**

``` hlasm
name     TIME  ,label,LINKAGE=SYSTEM
name     TIME  ,(reg),LINKAGE=SYSTEM
name     TIME  DEC,label,LINKAGE=SYSTEM
name     TIME  DEC,(reg),LINKAGE=SYSTEM
```

Returns: Time as HHMMSSTH

The time is stored at the 4 bytes specified. 
Hours, mins and secs to 2 decimal places.

The values are unsigned packed decimal:

    X'21420654' = 21:42:06.54

The MVO instruction can be used to convert it to standard packed decimal format.

The date is stored at label+8 or 8(reg).

**BIN - Binary**

``` hlasm
name     TIME  BIN,label,LINKAGE=SYSTEM
name     TIME  BIN,(reg),LINKAGE=SYSTEM
```
Returns: The time is stored at the 4 bytes specified in hundredths of a second since midnight

The date is stored at label+8 or 8(reg).

**MIC - Microseconds**

``` hlasm
name     TIME  MIC,label,LINKAGE=SYSTEM
name     TIME  MIC,(reg),LINKAGE=SYSTEM
```
Returns: Time in units of 1&micro;s in binary since midnight.

* The time is stored at the 8 bytes specified.
* The date is stored at label+8 or 8(reg).

**STCK - Microseconds 0-51 bits only**

``` hlasm
name     TIME  STCK,label.LINKAGE=SYSTEM
name     TIME  STCK,(reg),LINKAGE=SYSTEM
```
Returns: Time in units of 1&micro;s in binary since midnight.

The time is stored at the 8 bytes specified and uses only bits 0-51 of the 8-byte field.
The date is stored at label+8 or 8(reg).

**STCKE - Microseconds 16 byte**

``` hlasm
name     TIME  STCKE,label.LINKAGE=SYSTEM
name     TIME  STCKE,(reg),LINKAGE=SYSTEM
```

Returns: Time in units of 1&micro;s in binary since midnight.

The time is stored at the 16 bytes specified:

* Byte 0 : Zero
* Bytes 1-13 : The time
* Bytes 14-15 : Programmable field set by the SCKPF instruction and not currently implemented.

The time uses only bits 8-111 of the 16-byte field with bits 8-59 being the value in microseconds.

!!! Note
    The DATETYPE parameter is ignored.

#### Register Usage

* R0 = Code for units and date type
* R1 = Result area

#### Return

GR15 has a return code:

* 0 - TIME ok
* 4 - Invalid request
 
### GETIME - Obtain time and date (VSE)

``` hlasm
         GETIME type
```

Obtain the time in various formats (VSE only). 
The time is stored in the GR0/GR1 register pair.

#### Parameters

##### type - date type

**STANDARD (default)**

Time in GR1 as `0HHMMSSc`. Hours, mins and secs in packed format.

**BIN - Binary**

Time in GR1 in seconds since midnight in binary.

**TU - Time unit**

Time in GR1 in timer units of 26.04166&micro;s since midnight in binary.

**MIC - Microseconds**

Time in units of 1&micro;s in binary since midnight.


#### Return

GR15 has a return code:

* 0 - GETIME ok
* 4 - Invalid request
 
### STIMER - Wait for an interval of time

Wait for an interval of time.

``` hlasm
name     STIMER WAIT,BINTVL=label
name     STIMER WAIT,DINTVL=label
name     STIMER WAIT,MICVL=label
name     STIMER WAIT,TUINTVL=label
```

Start a timer and continue. When the time expires the exit routine is invoked.

``` hlasm
name     STIMER REAL,exit,BINTVL=label
name     STIMER REAL,exit,DINTVL=label
name     STIMER REAL,exit,MICVL=label
name     STIMER REAL,exit,TUINTVL=label
```
Only one STIMER can be waiting for expiry at any moment.

In each case the label points to a number of timer units.

value   | effect | Maximum value
--------|--------|--------------
BINTVL  | Fullword with 100th of a second units. | X'7FFFFFFF' is approx. 249 days.
DINTVL  | Doubleword PL8'HHMMSSth', where th is 2 decimal positions of seconds. | 99595999, approximately 4 days.
MICVL   | Doubleword with microsecond units | X'7FFFFFFFFFFFFFFF', or nearly 300000 years 
TUINTVL | Fullword with 26.04166&micro;s units | X'7FFFFFFF', approximately 16 hours.

#### Parameters

##### exit

Can be label or (reg).

* When the time expires, the exit routine is invoked.
* GR15 has the address of the exit routine.
* Other registers must be assumed to be destroyed.

!!! Note
    STIMER REAL is measuring clock time, and not the time that the Z390 program is executing.

#### Register Usage

* R0 = Code for timer units and exit address
* R1 = Address of the timer units
* R15 = By implication, exit routine address

### TTIMER - Test or cancel STIMER REAL

Test or cancel a previously set STIMER REAL

``` hlasm
name     TTIMER CANCEL,type,addr
```

#### Parameters

##### CANCEL

CANCEL means that the STIMER timing is terminated.

##### type

**TU (default)**

Returns the remaining time in GR0 as 4 bytes in timer units of 26.04166&micro;s. addr is ignored.

**MIC,addr**

Using MIC requires addr which may be specified as label or (reg). The remaining time is returned at the doubleword address in microseconds.
 
#### Usage

Cancel the current STIMER REAL, return the remaining time in GR0 in timer units.

``` hlasm
         TTIMER CANCEL
```

Return the remaining time in microseconds at REMAIN.

``` hlasm
         TTIMER ,MIC,REMAIN
......
REMAIN   DS     D
```

#### Register Usage

* R0 = Code for timer units, returned value
* R1 = Address of returned timer units

#### Return
GR15 has a return code:
* 0 TTIMER ok
* 4 TU units remaining exceed 31 bits


### CMDPROC - execute host OS commands

Open, close, read and write from the host command processor or shell.

With the CMDPROC macro, you can issue OS shell commands, receive the replies from those commands line by line and start
other programs.

There is a limit of 10 command processors that can be open at any time. The limit is only to protect the operating system
from storage depletion. In all cases below, ID may be defined as a numeric value or in a general register. For example, ID=2 or ID=(R5).

#### Parameters

##### ID

* ID may range from 0 (default) to 9.
* If the ID exceeds 9 then an abend SFFF will occur

#### Subcommands

##### START

``` hlasm
name      CMDPROC START,ID=,CMDLOG=
```

Start a command processor and assign an identifier.

If the memory queue exceeds the MAXQUE value (default 1000) then the memory queue is written to the log and CMDPROC=YES is assumed. 
An error message is generated.

**CMDLOG**

* CMDLOG=YES (Default) -  All output from the command processor is written to the log.
* CMDLOG=NO -  All output is saved in a memory queue. Use this option if you intend to use CMDPROC READ to retrieve command processor messages.

##### STOP

``` hlasm
name     CMDPROC STOP,ID=
```

Close a previously opened command processor.

##### WRITE

``` hlasm
name     CMDPROC WRITE,label,ID=
name     CMDPROC WRITE,literal,ID=
name     CMDPROC WRITE,(reg),ID=
```
Send a command to a previously opened command processor.

**label or (reg)**

Points to a constant which terminates with X'00' or is defined as a double-quoted string within a standard C-type constant.

**literal**

Double-quoted string within a standard C-type constant preceded by an equals sign.

``` hlasm
name     CMDPROC WRITE,CMD1,ID=5
......
CMD1     DC    C'DIR /X',X'00'
```

``` hlasm
name     CMDPROC WRITE,CMD1,ID=5
......
CMD1     DC    C'"DIR /X"'
```

##### READ

``` hlasm
name     CMDPROC READ,label,len,ID=,WAIT=
```

Obtain the output, a line at a time, from the result of a command issued by CMDPROC WRITE from a previously
opened command processor. 
 
_label_ is the receiving area and may be specified as (reg).

**len**

Maximum length that is passed to your program. 

* The default is the implied length of the receiving field. Maximum value is 4095 bytes.
* _len_ may be specified as _(reg)_.
* Maximum register value is 2G - 1 bytes.
* If _label_ is specified as _(reg)_, then _len_ is mandatory.

**WAIT=**

Time in milliseconds before the READ will terminate if no output from the command processor is available to be read.

* Default is 500 milliseconds.
* Maximum value is 4095 (4 seconds).
* WAIT may be specified as (reg).  Maximum value is X'7FFFFFFF' (about 24 days).

#### Register Usage

* R0 = Operation code and ID
* R1 = Command area
* R2 = Length
* R3 = Wait value
* R15 = Formation of ID and return code

#### Return 

GR15 has a return code:

* 0 - CMDPROC ok
* 4 - READ terminated as WAIT time has expired
* 8 - READ terminated because the command processor has ended
* 16 - Command Processor abnormally ended (see log message)

### WTO - Write to operator

Display a message on the GUI console.

The record descriptor word (RDW) defines the variable length text message generated by the WTO macro.

``` hlasm
         DC    AL2(len,0),C'text'
```

len includes the 4 bytes for the RDW.

#### Formats

##### Text

``` hlasm
name     WTO   'text'
```

The RDW that describes the message is generated internally.

##### List

``` hlasm
name     WTO   'text',MF=L
```

No text is written to the console; only the RDW and text is generated.

This allows a 'collection' of messages to be constructed which can be used by the execute form.

##### Execute 1

``` hlasm
name     WTO   MF=E
```

GR1 must be preloaded with the address of an RDW previously generated with the list form of WTO.

##### Execute 2

``` hlasm
name     WTO   MF=(E,label)
name     WTO   MF=(E,(reg))
```

*label* or *(reg)* points to an RDW previously generated with the list form of the WTO.

#### Register Usage

* R1 = Branch around RDW or parm pointer

### XLATE - EBCDIC to ASCII

``` hlasm
name     XLATE area,len,TO=
```

Translates data to EBCDIC or ASCII.

#### Parameters

##### area

_area_ may be specified as _label_ or _(reg)_.

##### len

_len_ may be specified as a number or (reg).

* Maximum numeric value is 4095 bytes.
* Maximum register value is 2G - 1 bytes.

##### TO

Type of conversion to perform:

* TO=A - convert area to ASCII.
* TO=E - convert area to EBCDIC.

#### Register Usage

* R0 = Area address and codes
* R1 = Length

### WTOR - Write to operator

Display a message on the GUI console and receive a response.

``` hlasm
name     WTOR  'text',reply,len,ecb
name     WTOR  "text",reply,len,ecb
```

The RDW (see WTO) that describes the message is generated internally. The text appears on the console.

#### Parameters

##### reply

Specified as label or (reg), is the field into which the reply is put. The reply appears on the console.

##### len

Maximum length of reply.

* If reply is specified as (reg) then len is mandatory.
* If len is omitted, then the implied length of reply is used. 

##### ecb

Specified as label or (reg), by convention defined as DC F'0'.

After the WTOR macro, instruction execution can proceed until the reply is completed by the user (commonly the Return key).

Usage: Implied length, named ECB, wait for reply immediately.

``` hlasm
         WTOR  'Enter your name',NAME,,MYECB
         WAIT  ECB=MYECB
......
NAME     DC    CL40' '
MYECB    DC    F'0'
```

Usage: Register notation, maximum length, no wait for reply.

``` hlasm
         LA    R5,NAME
         LA    R6,MYECB
         WTOR  'Enter your name',(R5),40,(R6)
......
         TM    MYECB,X'40'
         BO    GOTREPLY
......
NAME     DC    CL40' '
MYECB    DC    F'0'
```

#### Register Usage

* R0 = Reply address
* R1 = Branch around RDW
* R14 = Reply length
* R15 = ECB address

### WAIT - Wait for ECB completion

``` hlasm
name WAIT num,ECB=
name WAIT num,ECBLIST=
```

ECB or ECBLIST must be specified.

#### Parameters

##### num

_num_ is optional and defaults to 1.

For ECB= _num_ must be 1 or omitted.

For ECBLIST= _num_ is the minimum number of ECBs that must be posted before the WAIT is complete. This value must, of 
course, be less or equal to the number of ECBs in the list. An abend SF05 will occur if this is not the case.


##### ECB=

Specified as label or (reg).

The location of a single 4-byte ECB.

##### ECBLIST=

Specified as label or (reg).
The location of a sequence of 4-byte addresses, each of which points to a 4-byte ECB. The last 4-byte address must have 
bit 0 set to 1.

!!! Note
    For DECBs, use the CHECK macro rather than WAIT, otherwise error routines may not be correctly invoked.

#### Usage

Wait for 2 out of 3 ECBs.

``` hlasm
         WAIT  2,ECBLIST
......
ECBLIST  DC    A(ECB1)
         DC    A(ECB2)
         DC    A(X'80000000'+ECB3)
ECB1     DC    F'0'
ECB2     DC    F'0'
ECB3     DC    F'0'
```

#### Register Usage

* R0 = Number of ECBs
* R1 = ECB address

### POST - Signal ECB completion

Signal the completion of one ECB.

``` hlasm
name     POST  ecb,code
```

#### Parameters

##### ecb

_ecb_ is required. Specified as label or (reg). The location of a single 4-byte ECB.

##### code

_code_ is optional and defaults to zero. Specified as a value (eg. 14 or X'123') or as (reg).

#### Return

The completion code is placed in bits 2-31 of the ECB.

#### Register Usage

* R0 = Event completion code
* R1 = ECB address

### CTD - Convert binary or FP value

``` hlasm
name     CTD   type,IN=input,OUT=output,LINKAGE=
```

Convert a binary or floating point value to a printable format.

#### Parameters

##### type

This is a numeric value which determines the operation to be carried out. 
Equates are automatically generated. The value of type also determines the length of the input field.

_type_ may be specified in a register eg. (R5). 

Value | Equate        | Length | Description
------|---------------|--------|------------
1     | CTD_INT128    | 16     | binary
2     | CTD_EH        |  4     | short HFP
3     | CTD_EB        |  4     | short BFP
4     | CTD_DH        |  8     | long HFP
5     | CTD_DB        |  8     | long BFP
6     | CTD_LH        | 16     | extended HFP
7     | CTD_LB        | 16     | extended BFP
8     | CTD_DD        |  8     | long DFP
9     | CTD_ED        |  4     | short DFP
10    | CTD_LD        | 16     | extended DFP

##### IN=

The input field may be specified as a literal eg. `IN==DH'3.8'`, a label, a register pointer eg. `IN=(R4)` 
or a register eg. `IN=R4`.

For some types, input from a register implies the use of a register pair as follows:

Value | Equate     | Register specified
------|------------|-------------------
1     | CTD_INT128 | Any even general register, input is from the even/odd pair.
2     | CTD_EH     | Any floating point register.
3     | CTD_EB     | Any floating point register. 
4     | CTD_DH     | Any floating point register.
5     | CTD_DB     | Any floating point register.
8     | CTD_DD     | Any floating point register.
9     | CTD_ED     | Any floating point register.
6     | CTD_LH     | The first floating point register of a valid pair, input is from the the register pair.
7     | CTD_LB     | The first floating point register of a valid pair, input is from the the register pair.
10    | CTD_LD     | The first floating point register of a valid pair, input is from the the register pair.


##### OUT=

The output field may be specified as a label or a register pointer eg. `OUT=(R4)`.

The output field is always 45 bytes, and is initialized to blanks. Not all 45 bytes may be used.

The output field will be ASCII if the ASCII option is used, otherwise EBCDIC will be used.

The output field has the following format in this sequence: 

Output field | Condition
-------------|----------
-            | If the value is negative
n...n        | Digits preceding the decimal point. If the value is less than 1 and there is no exponent, then 0 is output. eg. 0.04
.            | Decimal point if there are decimal positions
n...n        | Digits following the decimal point if the value is not a whole number
E            | Indicates an exponent follows
-            | Indicates a negative exponent
nnnn         | The exponent value, 1-4 digits

Examples:

Value       | Output
------------|-------
zero        | 0
root2       | 1.4142...
-root2      | -1.4142...
50!         | 3.0414...E64
2 power -50 | 8.8817...E-16

##### LINKAGE=

* SVC (default) invokes SVC 170
* CALL generates a CALL to module FPCONMFC

#### Register Usage

* R0 = Parameter formation
* R1 = Parameter list
* R14 = Subroutine call
* R15 = Subroutine address and return code

#### Return

GR15 has a return code:

* 0 - CTD ok
* 8 - Invalid data address

### CFD - Convert to binary or FP value

Convert a printable format number to a binary or floating point value.

``` hlasm
name     CFD   type,IN=input,OUT=output,LINKAGE=
```

#### Parameters

##### type

This is a numeric value which determines the operation to be carried out. Equates are automatically generated. The value of
type also determines the length of the output field. 

_type_ may be specified in a register eg. (R5).

Value | Equate     | Length | Description
------|------------|--------|------------
21    | CFD_INT128 | 16     | binary
22    | CFD_EH     |  4     | short HFP
23    | CFD_EB     |  4     | short BFP
24    | CFD_DH     |  8     | long HFP
25    | CFD_DB     |  8     | long BFP
26    | CFD_LH     | 16     | extended HFP
27    | CFD_LB     | 16     | extended BFP
28    | CFD_DD     |  8     | long DFP
29    | CFD_ED     |  4     | short DFP
30    | CFD_LD     | 16     | extended DFP

##### IN=

The input field may be specified as a label or a register pointer eg. (R4).

The input field must be in ASCII if the ASCII option is used, otherwise EBCDIC.

The input field is always 45 bytes, and has the following format in this sequence:

output field | condition
-------------|----------
             | Optional preceding blanks
-            | If the value is negative
n...n        | Digits preceding the decimal point.
.            | Decimal point if there are decimal positions
n...n        | Digits following the decimal point if the value is not a whole number
E            | Indicates an exponent follows
-            | Indicates a negative exponent
nnnn         | The exponent value, 1-4 digits

For CFD_INT128, all correct forms are accepted and any decimal places are discarded:

    129E-1 = 12

##### OUT=

The output field may be specified as a label, a register pointer eg. `OUT=(R4)` or a register eg. `OUT=R4`

For some types, output to a register implies the use of a register pair as follows:

Value  | Equate     | Register specified
-------|------------|-------------------
21     | CTD_INT128 | Any even general register, input is from the even/odd pair.
22     | CTD_EH     | Any floating point register.
23     | CTD_EB     | Any floating point register. 
24     | CTD_DH     | Any floating point register.
25     | CTD_DB     | Any floating point register.
28     | CTD_DD     | Any floating point register.
29     | CTD_ED     | Any floating point register.
26     | CTD_LH     | The first floating point register of a valid pair, input is from the the register pair.
27     | CTD_LB     | The first floating point register of a valid pair, input is from the the register pair.
30     | CTD_LD     | The first floating point register of a valid pair, input is from the the register pair.

##### LINKAGE=

* SVC (default) invokes SVC 171
* CALL generates a CALL to module FPCONMFC

#### Register Usage

* R1 = Parameter list
* R14 = Subroutine call
* R15 = Subroutine address and return code

#### Return

GR15 has a return code:

* 0 - CFD ok
* 8 - Invalid data address
* 12 - invalid input data or number too large for format type

### GETENV - Get environment variable

Get an environment variable from the OS command environment.

GETENV extracts the string in a program.

``` hlasm
name     GETENV setname
name     GETENV (reg)
```
#### Parameters

##### setname

_setname_ is the label of a null terminated string or the string can be pointed to by reg.

``` hlasm
SETNAME  DC    C'MYDATA',X'00'
```

GETENV acquires a storage area for the variable and sets the address in GR2. The string is terminated with X'00'.

#### Register Usage

* R0 = Function code
* R1 = setname pointer
* R2 = Address of variable
* R15 = Return code

#### Return

GR15 has a return code:

* 0 - GETENV ok
* 4 - setname is null
* 8 - variable is null

### COMRG - Comm region addressability

!!! Note
    VSE only

Establish addressability to the Communications region in the ZCVT.

``` hlasm
         COMRG REG=(reg)
```

* If REG is omitted it defaults to GR1.
* It is the users responsibility to provide a DSECT to map the COMRG.

#### Register Usage

* R1 = Address the ZCVT

_reg_ used in REG parm.

## Supporting Macros

COMRG Address Communications region (VSE)

## Time periods

* ms - milliseconds 0.001 seconds (one thousandth)
* &micro;s - microseconds 0.000001 seconds (one millionth)
* ns - nanoseconds 0.000000001 seconds (one billionth)

## Event Control Block

### Bits 0-1

* 00 - The initial state. WAIT requires both these bits to be zero.
* 10 - When the WAIT macro is issued for the ECB, this wait bit is set and the program enters the wait state.
* 01 - Set to this state internally or by the POST macro indicates that the event is complete or that the task in a wait state is to be resumed. It is valid to test for this state using a bit test instruction like TM.  
* 11 Invalid.

### Bits 2-31

Completion code, set internally or by the POST macro.

## SVC functions

DEC | HEX | Service
----|-----|--------
  1 | 01  | WAIT
  2 | 02  | POST
 11 | 0B  | TIME (and date)
 11 | 0B  | GETIME (VSE)
 40 | 28  | GETENV
 46 | 2E  | TTIMER
 47 | 2F  | STIMER
 52 | 34  | CMDPROC
 53 | 35  | WTO
103 | 67  | XLATE
160 | A0  | WTOR
170 | AA  | CTD
171 | AB  | CFD
