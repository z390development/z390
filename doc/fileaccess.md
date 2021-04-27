# Non-VSAM file access

z390 supports sequential and random access to files through the provision of 
SVC functions and assembler macros.

The macros described here seek to emulate those provided by IBM&reg; as part of their
operating system macro services where it makes sense in the context of z390 runtime environments. 

* [z/OS: DFSMS Macro Instructions for Data Sets](https://www-01.ibm.com/servers/resourcelink/svc00100.nsf/pages/zOSV2R4sc236852/$file/idad500_v2r4.pdf) - Part 2 Non-VSAM macro instructions.

!!! Note "VSAM support"
    The macros GET, PUT and POINT can be used for VSAM access with the
    parameter RPL=rpladdress. VSAM usage is not documented here. Please refer
    to [Z390 VSAM User Guide]().

## Quick starts

To access a file, a data control block (DCB) must be defined.

Programs that use the File I/O macros must include the DCBD DSECT, so a typical program 
structure would be:

    DCBD
    EQUREGS
    END

Include the DECBD DSECT if you are using READ or WRITE.

### Read a file

### Write to a file

## Macro reference

### DCB

Used to create the Data Control Block

Some of the parameters may be set before and/or after OPEN.

The individual descriptions indicate this.

             1         2         3         4         5         6         7
    123456789!123456789!123456789!123456789!123456789!123456789!123456789!1
    label   DCB  DSORG=PS,                                                X
                 EODAD=0,                                                 X
                 RECFM=FB,                                                X
                 DDNAME=,                                                 X
                 MACRF=GM,                                                X
                 SYNAD=0,                                                 X
                 BLKSIZE=0,                                               X
                 LRECL=0,                                                 X
                 DCBE=,                                                   X
                 RECORD=0,                                                X
                 DSNAME=0

EXLST is currently unsupported.

'label' is the file identifier, the DCB name, which is used in all
the file I/O macros.

Term | Meaning                    | Description
-----|----------------------------|------------
BDW  | Block Descriptor Word      | Only used for RECFM=VB files. <br/>A 4-byte field of the form  'blocklength',H'0'<br/>The blocklength includes the length of the BDW.
RDW  | Record Descriptor Word     |  Used for all variable files.<br/> A 4-byte field of the form H'recordlength',H'0'<br/>The recordlength includes the length of the RDW.<br/> The program must set this field correctly for output, and expect it on input.
CRLF | Carriage Return, Line Feed | A term for the hex string X'0D0A' (ASCII) or X'0D25' (EBCDIC).<br/>Now commonly used to terminate a text record.


#### DDNAME and DSNAME - DD statement/Dataset name

DDNAME DSECT=DCBDDNAM Type=CL8 Default=CL8'label'

DSNAME DSECT=DCBDSNAM Type=A No default

* Can these be set in the DCB prior to OPEN: Yes
* Can these be set in the DCB after OPEN : No

Only one of these parameters may be set in the DCB.

If both parameters are set in the program prior to open, the DSNAME will take precedence.

When DDNAME is specified, an environment variable will point to the path and file that 
is to be opened.

Within a program the GETENV macro can be used to extract the environment
variable. See the [Macro and Services Guide]() for full details.

    MYDCB   DCB   DDNAME=MYDATA, ...
 
For execution in a Windows environment:

    SET MYDATA=c:\path\file
    CALL EZ390 c:\path\program parms

For execution in a \*nix environment

    MYDATA=/path/file ez390 program parms

DSNAME is a label defined in the program which has the file spec.

The file spec must terminate with X'00' or be defined as a double-quoted string within a standard 
C-type constant.

    MYDCB DCB DSNAME=MYDATA, ...
    ...
    MYDATA DC C'drive:\path\file',X'00'

    MYDATA DC C'"drive:\path\file"'

#### MACRF - Macro type

DSECT=DCBMACRF, Coded Default=GM

* Can this be set in the DCB prior to OPEN: Yes, but not recommended
* Can this be set in the DCB after OPEN : No

Option    | Effect
----------|-------
 MACRF=GM | Get Move, use the GET macro to read a record<br/>Use for standard (QSAM) file read
 MACRF=PM | Put Move, use the PUT macro to write a record<br/>Use for standard (QSAM) file write
 MACRF=R  | Read Mode.<br/> Use READ/CHECK to read a record.<br/> Use POINT for positioning.
 MACRF=W  | Write Mode.<br/> Use WRITE/CHECK to write or update a record.<br/> Use POINT for positioning.
 MACRF=RW | Update Mode.<br/> Use READ/WRITE/CHECK to read, write or update a record.<br/> Use POINT for positioning.

#### DSORG - Dataset organisation

DSORG DSECT=DCBDSORG Coded Default=PS

* Can this be set in the DCB prior to OPEN: Yes, but not recommended
* Can this be set in the DCB after OPEN : No

##### DSORG=PS Physical Sequential

The only option at present, can be omitted.

#### RECFM - Record format

RECFM DSECT=DCBRECFM Coded Default=FB

* Can this be set in the DCB prior to OPEN: Yes, but not recommended
* Can this be set in the DCB after OPEN : No

##### RECFM=F Fixed

Translation: None

Input: Records are read LRECL at a time.

Output: Records are written LRECL at a time.

##### RECFM=FB Fixed Blocked

Translation: None

Input:

* MACRF=GM
    * Records are read LRECL at a time out of BLKSIZE.
* MACRF=R or RW
    * The whole block is read.

Output:

* MACRF=PM
    * Records are written LRECL at a time into BLKSIZE.<br/>CLOSE may write a short block.
* MACRF=W or RW
    * The whole block is written.

##### RECFM=V Variable

Translation: None

Input:

* Each record is prefixed by the RDW.
* The receiving area must be big enough for the largest RDW+record.
* If the RDW indicates a record size larger than LRECL, abend S013 will occur.

Output:

* Each record must be prefixed by the RDW.

##### RECFM=VB Variable Blocked

Translation: None

Input:

* MACRF=GM
    * Each record is prefixed by the RDW.
    * The receiving area must be big enough for the largest RDW+record.
    * If the RDW indicates a record size larger than LRECL, abend S013 will occur.
* MACRF=R or RW
    * The whole block is read, prefixed by the BDW.
    * The receiving area must be big enough for the largest BDW+block.
    * If the BDW indicates a block size larger than BLKSIZE, abend S013 will occur.

Output:

* MACRF=PM
    * Each record must be prefixed by the RDW.
    * CLOSE will write the last block.
    * Each block written will have the BDW automatically inserted at the start.
* MACRF=W or RW
    * Each block must be prefixed by the BDW.

##### RECFM=FT Fixed ASCII text

Translation:

* ASCII mode:
    * None
* Non-ASCII mode input
    * ASCII chars are translated to EBCDIC after being read.
* Non-ASCII mode output
    * EBCDIC chars are translated to ASCII before being written, this is an 
      internal function and does not affect the record in storage. Non-EBCDIC chars may be translated to X'00'.

Input:

* The file is assumed to be in conventional ASCII format, with each record ending in CRLF.
* CRLF is never read as part of the record.
* If the record is shorter than LRECL, then trailing blanks are inserted.
* EODAD is invoked when all bytes have been read. 

Output:

* CRLF is inserted at the end of each record after trailing blanks are stripped.


##### RECFM=VT Variable ASCII text

Translation: 

* ASCII mode
    * None
* Non-ASCII mode input
    * ASCII chars are translated to EBCDIC after being read.
* Non-ASCII mode output
    * EBCDIC chars are translated to ASCII before  being written, this is an internal function  and does not affect the record in storage.  Non-EBCDIC chars may be translated to X'00'.
 
Input:

* The file is assumed to be in conventional ASCII format, with each record ending in CRLF.
* CRLF is never read as part of the record.
* Each record is prefixed with the RDW.
* EODAD is invoked when all bytes have been read.
* The receiving area must be big enough for the largest RDW+record.
* If the RDW indicates a record size larger than LRECL, abend S013 will occur.
 
Output:

* Each record must be prefixed with the RDW.
* CRLF is inserted at the end of each record after trailing blanks are stripped.

!!! Warning
    The RDW is not written.

#### RECORD - Record address

RECORD DSECT=DCBREC Type=A Default=0 (undefined)

Default I/O area which can be overridden on the GET/PUT/READ/WRITE Macros.

* Can this be set in the DCB prior to OPEN: Yes
* Can this be set in the DCB after OPEN : Yes

If omitted, then the I/O area must be specified on the GET/PUT/READ/WRITE Macros.

#### LRECL - Record length

LRECL DSECT=DCBLRECLF Type=F Default=0

* Can this be set in the DCB prior to OPEN: Yes
* Can this be set in the DCB after OPEN : Yes
* Maximum value is 2G-1.
* For RECFM=F or FB, sets the record size.
* For all other RECFM, sets the maximum record size.
* For RECFM=F, LRECL=0 is valid, provided that BLKSIZE is set.
* For RECFM=FB, LRECL must be a multiple of BLKSIZE.
* For variable records, include 4 for the RDW.
* The field DCBLRECL (Type=H) is retained for compatability.

#### BLKSIZE - Block size

BLKSIZE DSECT=DCBBLKSIF Type=F Default=0

* Can this be set in the DCB prior to OPEN: Yes
* Can this be set in the DCB after OPEN : Yes
* Maximum value is 2G-1.
* LRECL and BLKSIZE cannot both be zero.
* For RECFM=F sets the record size if LRECL=0.
* For RECFM=FB, sets the block size and LRECL must be a multiple of BLKSIZE.
* For RECFM=VB, BLKSIZE must be at least 4 greater than LRECL.
* For all other RECFM, sets the maximum block size.
* The field DCBBLKSI (Type=H) is retained for compatability.

#### DCBE - Data control block extension

DCBE DSECT=DCBDCBE Type=A Default=0 (undefined)

The DCBE is a control block defined by the DCBE macro.

* Can this be set in the DCB prior to OPEN: Yes
* Can this be set in the DCB after OPEN : Yes
* When defined, the addresses of the end-of-file (EODAD) and the I/O error routine (SYNAD) 
  may be defined.  When set, these addresses override the DCB EODAD and SYNAD parameters.
* The DSECT IHADCBE maps the DCBE control block.
* The DCBE macro is DCBE EODAD=,SYNAD= with both parameters defaulting to 0 (undefined).

#### EODAD - End of file routine

EODAD DSECT=DCBEODAD Type=A Default=0 (undefined)

The address of the end-of-file routine.

* Can this be set in the DCB prior to OPEN: Yes
* Can this be set in the DCB after OPEN : Yes
* This may be overidden if DCBE is coded.
* If a further GET is done after end-of-file, then the program is terminated.

#### SYNAD - Error analysis routine

SYNAD DSECT=DCBSYNAD Type=A Default=0 (undefined)

The address of the uncorrectable I/O error routine.

* Can this be set in the DCB prior to OPEN: Yes
* Can this be set in the DCB after OPEN : Yes
* This may be overidden if DCBE is coded.

### DTFSD

!!! Info "VSE only"

DTFSD maps to the standard DCB as follows:

             1         2         3         4         5         6         7
    123456789!123456789!123456789!123456789!123456789!123456789!123456789!1
    label  DTFSD BLKSIZE=n,                                               X
                 EOFADDR=
    label  DCB   DSORG=PS,                                                X
                 EODAD=,   maps from EOFADDR                              X
                 RECFM=F,                                                 X
                 DDNAME=,  label if it exists, otherwise blank            X
                 MACRF=RW,                                                X
                 SYNAD=0,                                                 X
                 BLKSIZE=, maps from BLKSIZE                              X
                 LRECL=,   maps from BLKSIZE                              X
                 DCBE=0,                                                  X
                 RECORD=0,                                                X
                 DSNAME=0


### DTFPR

!!! Info "VSE only"

DTFPR maps to the standard DCB as follows:

             1         2         3         4         5         6         7
    123456789!123456789!123456789!123456789!123456789!123456789!123456789!1
    label  DTFPR BLKSIZE=n
    label  DCB   DSORG=PS,                                                X
                 EODAD=,   maps from EOFADDR                              X
                 RECFM=F,                                                 X
                 DDNAME=,  label if it exists, otherwise blank            X
                 MACRF=RW,                                                X
                 SYNAD=0,                                                 X
                 BLKSIZE=, maps from BLKSIZE                              X
                 LRECL=,   maps from BLKSIZE                              X
                 DCBE=0,                                                  X
                 RECORD=0,                                                X
                 DSNAME=0


### OPEN

Open one or more files.

#### Usage

Open one file for INPUT

    OPEN dcbname

Open one file for non-INPUT

    OPEN (dcbname,type)

Open multiple files

    OPEN (dcbname1,type,dcbname2,type)

The same, using register notation

    OPEN ((reg),type)
    OPEN ((reg1),type,(reg2),type)

#### Registers

* R0 = Flags
* R1 = DCB addresses

#### Abends

* S013 OPEN failed and no SYNAD exit provided

### OPEN (VSE)

Open one or more files for UPDATE.

#### Usage

    OPEN dtfname
    OPEN (dtfname1,dtfname2,...)

#### Registers

* R0 = Flags
* R1 = DTF addresses

#### Abends

* S013 OPEN failed and no SYNAD exit provided

### CLOSE

Close one or more files.

#### Usage

Close one file

    CLOSE dcbname

Close multiple files

    CLOSE (dcbname1,,dcbname2)

The same, using register notation

    CLOSE ((reg1),,(reg2))

#### Registers

* R1 = DCB addresses

##### Abends

* S013 CLOSE failed and no SYNAD exit provided

### CLOSE (VSE)

Close one or more files.

#### Usage

    CLOSE (dtfname1,dtfname2,...)

#### Registers

* R1 = DTF addresses

#### Abends

* S013 CLOSE failed and no SYNAD exit provided

### GET

Read a sequential record

#### Usage

Read a record into an I/O area supplied on DCB RECORD=ioarea
    
    GET dcbname

Read a record into an I/O area

    GET dcbname,ioarea

The same, using register notation

    GET (reg)
    GET (reg1),(reg2)

#### Registers

* R0 = ioarea
* R1 = DCB address

### PUT

Write a sequential record

#### Usage

Write a record from an I/O area supplied on the DCB RECORD=ioarea

    PUT dcbname

Write a record from an I/O area

    PUT dcbname,ioarea

The same, using register notation

    PUT (reg)
    PUT (reg1),(reg2)

#### Registers

* R0 = ioarea
* R1 = DCB address

### READ

 Read a block from a file.

 
1. If EODAD or SYNAD result from the READ, they will only be
   processed by the CHECK macro.
2. If the decbname parameter is specified in register notation,
   no internal DECB is generated. It is the programmer's
   responsibilty to create and address the 16-byte DECB.
 
#### Usage
 
Read a block using an internal DECB, into an I/O area
supplied on the DCB RECORD=ioarea

    READ decbname,,dcbname

Read a block using an internal DECB into an I/O area.
The I/O area must be at least as large as DCB BLKSIZE

    READ decbname,,dcbname,ioarea

The same, using register notation

    READ (reg1),,(reg2)
    READ (reg1),,(reg2),(reg3)

#### Registers

* R0 = DCB and ioarea addresses...saved
* R1 = DECB address
* R15= DCB address

### WRITE

Write a block to a file.

1. If SYNAD results from the WRITE, it will only be processed
   by the CHECK macro.
2. If the decbname parameter is specified in register notation,
   no internal DECB is generated. It is the programmer's
   responsibilty to create and address the 16-byte DECB.

#### Usage

Write a block using an internal DECB, from an I/O area supplied on the DCB RECORD=ioarea

    WRITE decbname,,dcbname

Write a block using an internal DECB from an I/O area

    WRITE decbname,,dcbname,ioarea

The same, using register notation

    WRITE (reg1),,(reg2)
    WRITE (reg1),,(reg2),(reg3)

#### Registers

* R0 = DCB and ioarea addresses...saved
* R1 = DECB address
* R15= DCB address

### CHECK

* Process EODAD or SYNAD on READ.
* Process SYNAD on WRITE.

#### Usage

    CHECK decbname
 
The same, using register notation

    CHECK (reg)

#### Registers

* R1 = DECB address

#### Abends

* S013 READ/WRITE failed and no SYNAD exit provided

### POINT

 Position pointer for next READ or WRITE

 1. When register notation is used for rba or rel, the register points to a field containing the value.
 2. rel is a fullword, maximum value 2,147,483,647 (2G)<br/>
    rel is multiplied by BLKSIZE to get the rba.
 3. rba is signed 64-bit, maximum value...very big.
 
#### Usage
 
Point to a record using relative record number

    POINT dcbname,rel
 
Point to a record using relative byte address
 
    POINT dcbname,,RBA=rba
 
The same, using register notation
 
    POINT (reg1),(reg2)
    POINT (reg1),,RBA=(reg2)
 
#### Registers

* R0 = rel or RBA
* R1 = DCB address
* R15= Blocksize

## Terminology

Term     | Definition
---------|-----------
dcbname  | the label on the DCB macro.
reg      | general register, avoid 0, 1, 14 or 15.
type     | INPUT, OUTPUT or UPDATE (default is INPUT).
ioarea   | label of the I/O area for the record to be read/written.
decbname | internal or external DECB for controlling READ/WRITE.
rel      | relative record number.
rba      | relative byte address.

## Supporting Macros

Macro   | Description
--------|------------------------
DCBD    | DCB structure (DSECT)
DCBE    | DCBE fields
DECBD   | DECB structure (DSECT)
IHADCBE | DCBE structure (DSECT)

## SVC functions

DEC | HEX | Service
----|-----|--
19  | 13  | OPEN
19  | 13  | OPEN (VSE)
20  | 14  | CLOSE
20  | 14  | CLOSE (VSE)
151 | 97  | GET
152 | 98  | PUT
153 | 99  | READ
154 | 9A  | WRITE
155 | 9B  | CHECK
156 | 9C  | POINT
