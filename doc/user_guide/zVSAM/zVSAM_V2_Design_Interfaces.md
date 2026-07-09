### EXLST macro

The EXLST macro will generate an Exit_List control block and initialize it according to the parameters specified
on the macro invocation.

The structure and layout of the generated EXLST are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST, TESTCB EXLST and/or MODCB EXLST
to inspect, test, and/or modify the EXLST's content.

All keywords on the EXLST macro are optional. Before the cluster is opened,
all EXLST values can be modified using MODCB EXLST, or by changing the EXLST directly.
The latter is not recommended, as it is not guaranteed to be portable or compatible with future versions of zVSAM.

The table below shows how the EXLST macro can be coded.

| Opcode | Operand                | Remarks                                                                                             |
|--------|------------------------|-----------------------------------------------------------------------------------------------------|
| EXLST  | [AM=VSAM]              | Designates this EXLST as a zVSAM EXLST; VSAM is the default                                         |
|        | [EODAD=addr[,mod]]     | End-of-data exit routine                                                                            |
|        | [LERAD=addr[,mod]]     | Logical error analysis routine                                                                      |
|        | [SYNAD=addr[,mod]]     | Physical error analysis routine                                                                     |
|        | [_JRNAD_=addr[,mod]]   | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|        | [_UPAD_=addr[,mod]]    | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|        | [_RLSWAIT_=addr[,mod]] | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### GENCB EXLST macro

The GENCB macro with BLK=EXLST will generate or manipulate Exit Lists for use with ACBs and initialize or change them
according to the parameters specified on the macro invocation.
It is for this reason that all supported parameters and keywords of the EXLST macro (as described above)
are supported on the GENCB macro when BLK=EXLST is specified.

The structure and layout of the generated EXLST are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST, TESTCB EXLST and/or MODCB EXLST
to inspect, test, and/or modify the EXLST's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the GENCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The GENCB EXLST macro can be coded as follows:

| Opcode | Operand                   | Remarks                                                             |
|--------|---------------------------|-------------------------------------------------------------------- |
| GENCB  | BLK=EXLST                 | Instructs GENCB to generate 1 or more EXLSTs                        |
|        | [AM=VSAM]                 | Optional, no other values allowed; VSAM is the default              |
|        | [COPIES=nr]               | The number of identical EXLSTs to generate                          |
|        | [WAREA=addr]              | The work area where the EXLSTs are to be constructed                |
|        | [LENGTH=nr]               | Length of the work area in bytes                                    |
|        | [LOC=keyword]             | Where GENCB is to allocate dynamically acquired storage - if needed |
|        | **[other]**               | **Any parameter supported on the EXLST macro**                      |
|        | [MF=]                     | Use standard form of GENCB EXLST; this is the default               |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of GENCB EXLST                                        |
|        | [MF=(E,addr)]             | Use execute form of GENCB EXLST                                     |
|        | [MF=(G,addr,[label])]     | Use generate form of GENCB EXLST                                    |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### MODCB EXLST macro

The MODCB macro with EXLST=addr will modify an EXLST according to the parameters specified on the macro invocation.
It is for this reason that all parameters and keywords of the EXLST macro (as described above)
are supported on the MODCB macro when EXLST=addr is specified.

The structure and layout of the affected EXLST are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST, TESTCB EXLST and/or MODCB EXLST
to inspect, test, and/or modify the EXLST's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the MODCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The MODCB EXLST macro can be coded as follows:

| Opcode | Operand                   | Remarks                                               |
|--------|---------------------------|-------------------------------------------------------|
| MODCB  | EXLST=address             | Points MODCB to the EXLST to be modified              |
|        | **[other]**               | **Any parameter supported on the EXLST macro**        |
|        | [MF=]                     | Use standard form of MODCB EXLST; this is the default |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of MODCB EXLST                          |
|        | [MF=(E,addr)]             | Use execute form of MODCB EXLST                       |
|        | [MF=(G,addr,[label])]     | Use generate form of MODCB EXLST                      |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### SHOWCB EXLST macro

The SHOWCB macro with EXLST=addr will return ACB-related fields according to the parameters specified
on the macro invocation in the order they are specified.

The structure and layout of the affected EXLST are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST, TESTCB EXLST and/or MODCB EXLST
to inspect, test, and/or modify the EXLST's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the SHOWCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The SHOWCB EXLST macro can be coded as follows:

| Opcode | Operand                   | Remarks                                                       |
|--------|---------------------------|---------------------------------------------------------------|
| SHOWCB | EXLST=address             | Points MODCB to the EXLST to be queried                       |
|        | AREA=addr                 | Address of return area                                        |
|        | LENGTH=nr                 | Size of return area in bytes                                  |
|        | [OBJECT=DATA/INDEX]       | For KSDS: select data or index component; DATA is the default |
|        | FIELDS=(keywd_list)       | List of keywords indicating which fields to return            |
|        | [MF=]                     | Use standard form of SHOWCB EXLST; this is the default        |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of SHOWCB EXLST                                 |
|        | [MF=(E,addr)]             | Use execute form of SHOWCB EXLST                              |
|        | [MF=(G,addr,[label])]     | Use generate form of SHOWCB EXLST                             |

Defined options for the FIELDS parameter are listed below:

| Keyword | Length | Remarks                                                                                             |
|---------|--------|-----------------------------------------------------------------------------------------------------|
| EODAD   | 4      | End-of-data exit routine address                                                                    |
| EXLLEN  | 4      | Size of EXLST in bytes                                                                              |
| _JRNAD_ | 4      | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| LERAD   | 4      | Logical error analysis routine address                                                              |
| SYNAD   | 4      | Physical error analysis routine address                                                             |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### TESTCB EXLST macro

The TESTCB macro with EXLST=addr will test EXLST-related fields according to the parameters specified on the macro invocation.
Only a single test can be specified on each TESTCB invocation. TESTCB returns a PSW condition code.

The structure and layout of the affected EXLST are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the EXLST is discouraged. Use SHOWCB EXLST, TESTCB EXLST and/or MODCB EXLST
to inspect, test, and/or modify the EXLST's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the TESTCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The TESTCB ACB macro can be coded as follows:

| Opcode | Operand                    | Remarks                                                                                             |
|--------|----------------------------|-----------------------------------------------------------------------------------------------------|
| TESTCB | EXLST=address              | Points TESTCB to the EXLST to be tested                                                             |
|        | ERET=addr                  | Address of error handling routine                                                                   |
|        | EODAD=0 / EODAD=addr[,mod] | End-of-data exit routine address                                                                    |
|        | JRNAD=0 / JRNAD=addr[,mod] | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|        | LERAD=0 / LERAD=addr[,mod] | Logical error analysis routine address                                                              |
|        | SYNAD=0 / SYNAD=addr[,mod] | Physical error analysis routine address                                                             |
|        | EXLLEN=nr                  | Size of EXLST in bytes                                                                              |
|        | [MF=]                      | Use standard form of SHOWCB EXLST; this is the default                                              |
|        | [MF=L/MF=(L,addr,[label]]  | Use list form of SHOWCB EXLST                                                                       |
|        | [MF=(E,addr)]              | Use execute form of SHOWCB EXLST                                                                    |
|        | [MF=(G,addr,[label])]      | Use generate form of SHOWCB EXLST                                                                   |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### OPEN macro

A cluster needs to be opened before it can be processed. The open macro is used to open 1 or more cluster(s)
and/or 1 or more sequential file(s) in a single call.

| Opcode        | Operand            | Remarks                                                                                                   |
|---------------|--------------------|-----------------------------------------------------------------------------------------------------------|
| OPEN          | (entry[,entry]...) | Each cluster or file requires an entry of two parameters                                                  |
| Entry format: | address,(options)  | Address of ACB or DCB, followed by a list of options (for DCB only). For ACB omit the list of options     |
|               | [MODE=24/31]       | Residency mode of all control blocks involved. Specify 31 if any reside above the line; 24 is the default |
|               | [MF=]              | Use standard form of OPEN; this is the default                                                            |
|               | [MF=L/MF=(L,addr)] | Use list form of OPEN                                                                                     |
|               | [MF=(E,addr)]      | Use execute form of OPEN                                                                                  |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### CLOSE macro

A cluster needs to be closed after it has been processed. The close macro is used to close 1 or more cluster(s)
and/or 1 or more sequential file(s) in a single call.

| Opcode        | Operand            | Remarks                                                                                                    |
|---------------|--------------------|------------------------------------------------------------------------------------------------------------|
| CLOSE         | (entry[,entry]...) | Each cluster or file requires an entry of two parameters                                                   |
| Entry format: | address,(options)  | Address of ACB or DCB, followed by a list of options (for DCB only). For ACB omit the list of options.     |
|               | [MODE=24/31]       | Residency mode of all control blocks involved. Specify 31 if any resides above the line; 24 is the default |
|               | [_TYPE=T_]         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote)        |
|               | [MF=]              | Use standard form of CLOSE; this is the default                                                            |
|               | [MF=L/MF=(L,addr)] | Use list form of CLOSE                                                                                     |
|               | [MF=(E,addr)]      | Use execute form of CLOSE                                                                                  |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

## RPL-based interfaces

The RPL is the primary interface for operations at the record level.
A program can use multiple RPLs. An RPL must always point to an open ACB in order to specify a valid operation.
- RPL.Get
- RPL.Put
- RPL.Erase
- RPL.Point
- RPL.EndReq
- Support for other RPL-based requests is reserved for a future release

### RPL macro

The RPL macro will generate an RPL and initialize it according to the parameters specified on the macro invocation.

The structure and layout of the generated RPL are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL, TESTCB RPL and/or MODCB RPL
to inspect, test, and/or modify the RPL's content.

All keywords on the RPL macro are optional. Before a request is issued, all RPL values can be modified
using MODCB RPL, or by changing the RPL directly.
The latter is not recommended, as it is not guaranteed to be portable or compatible with future versions of zVSAM.

The table below shows how the RPL macro can be coded.

| Opcode | Operand              | Remarks                                                                                             |
|--------|----------------------|-----------------------------------------------------------------------------------------------------|
| RPL    | [AM=VSAM]            | Designates this ACB as a zVSAM ACB; VSAM is the default                                             |
|        | [ACB=ptr]            | Pointer to ACB                                                                                      |
|        | [AREA=ptr]           | Pointer to record area or record pointer                                                            |
|        | [AREALEN=nr]         | Length of record area or record pointer                                                             |
|        | [ARG=ptr]            | Pointer to search argument                                                                          |
|        | [KEYLEN=nr]          | Length of search argument                                                                           |
|        | [ECB=]               | Pointer to ECB                                                                                      |
|        | [MSGAREA=addr]       | Pointer to message area                                                                             |
|        | [MSGLEN=nr]          | Length of message area                                                                              |
|        | [NXTRPL=ptr]         | Pointer to next RPL when chaining requests                                                          |
|        | [OPTCD=(keywd_list)] | List of keywords specifying processing options. See table below for valid keywords                  |
|        | [RECLEN=nr]          | Max amount of storage (in bytes) to use for buffers                                                 |
|        | [_TIMEOUT_=nr]       | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
|        | [_TRANSID_=nr]       | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |

Defined options for the OPTCD parameter are listed below:

| Keyword subset   | Keyword | Remarks                                                                                             |
|------------------|---------|-----------------------------------------------------------------------------------------------------|
| [ADR/KEY]        |         | KEY is the default                                                                                  |
|                  | ADR     | Addressed access to ESDS                                                                            |
|                  | KEY     | Keyed access to KSDS or RRDS                                                                        |
|                  | _CNV_   | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [DIR/SEQ/SKP]    |         | SEQ is the default                                                                                  |
|                  | DIR     | Direct access to RRDS                                                                               |
|                  | SEQ     | Sequential access to ESDS, KSDS or RRDS                                                             |
|                  | SKP     | Skip sequential access to KSDS or RRDS                                                              |
| [ARD/LRD]        |         | ARD is the default                                                                                  |
|                  | ARD     | Access user-defined record location                                                                 |
|                  | LRD     | Access last record in the cluster                                                                   |
| [FWD/BWD]        |         | FWD is the default                                                                                  |
|                  | FWD     | Forward processing                                                                                  |
|                  | BWD     | Backward processing                                                                                 |
| [SYN/ASY]        |         | SYN is the default                                                                                  |
|                  | SYN     | Synchronous request                                                                                 |
|                  | ASY     | Asynchronous request                                                                                |
| [NUP/UPD/NSP]    |         | NUP is the default                                                                                  |
|                  | NUP     | Not for update                                                                                      |
|                  | UPD     | For update                                                                                          |
|                  | NSP     | Retain positioning for next sequential access                                                       |
| [KEQ/KGE]        |         | KEQ is the default                                                                                  |
|                  | KEQ     | Locate record with exact key match                                                                  |
|                  | KGE     | Locate record with exact key match, or next higher valeu                                            |
| [FKS/GEN]        |         | FKS is the default                                                                                  |
|                  | FKS     | Full key search                                                                                     |
|                  | GEN     | Generic key search. KEYLEN required                                                                 |
| [MVE/LOC]        | MVE     | MVE is the default                                                                                  |
|                  | MVE     | Move mode                                                                                           |
|                  | LOC     | Locate mode                                                                                         |
| [RBA/XRBA]       |         | RBA is the default                                                                                  |
|                  | RBA     | 4-byte RBA values                                                                                   |
|                  | XRBA    | 8-byte extended RBA values                                                                          |
| [_NWAITX/WAITX_] |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |
| [_CR/NRI_]       |         | Not supported – future option. Keyword is flagged as ignored with a warning message (Level 4 Mnote) |       

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### GENCB RPL macro

The GENCB macro with BLK=RPL will generate or manipulate RPLs and initialize or change them according to
the parameters specified on the macro invocation. It is for this reason that all supported parameters
and keywords of the RPL macro (as described above) are supported on the GENCB macro when BLK=RPL is specified.

The structure and layout of the generated RPL are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL, TESTCB RPL and/or MODCB RPL
to inspect, test, and/or modify the RPL's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the GENCB request to the CBMR handler
are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The GENCB RPL macro can be coded as follows:

| Opcode | Operand                   | Remarks                                                             |
|--------|---------------------------|---------------------------------------------------------------------|
| GENCB  | BLK=RPL                   | Instructs GENCB to generate 1 or more RPLs                          |
|        | [AM=VSAM]                 | Optional, no other values allowed; VSAM is the default              |
|        | [COPIES=nr]               | The number of identical RPLs to generate                            |
|        | [WAREA=addr]              | The work area where the RPLs are to be constructed                  |
|        | [LENGTH=nr]               | Length of the work area in bytes                                    |
|        | [LOC=keyword]             | Where GENCB is to allocate dynamically acquired storage - if needed |
|        | **[other]**               | **Any parameter supported on the RPL macro**                        |
|        | [MF=]                     | Use standard form of GENCB RPL; this is the default                 |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of GENCB RPL                                          |
|        | [MF=(E,addr)]             | Use execute form of GENCB RPL                                       |
|        | [MF=(G,addr,[label])]     | Use generate form of GENCB RPL                                      |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### MODCB RPL macro

The MODCB macro with RPL=addr will modify an RPL according to the parameters specified on the macro invocation.
It is for this reason that all parameters and keywords of the RPL macro (as described above) are supported
on the MODCB macro when RPL=addr is specified.

The structure and layout of the affected RPL are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL, TESTCB RPL and/or MODCB RPL
to inspect, test, and/or modify the RPL's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the MODCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The MODCB RPL macro can be coded as follows:

| Opcode | Operand                   | Remarks                                             |
|--------|---------------------------|-----------------------------------------------------|
| MODCB  | RPL=address               | Points MODCB to the RPL to be modified              |
|        | [other]                   | Any parameter supported on the RPL macro            |
|        | [MF=]                     | Use standard form of MODCB RPL; this is the default |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of MODCB RPL                          |
|        | [MF=(E,addr)]             | Use execute form of MODCB RPL                       |
|        | [MF=(G,addr,[label])]     | Use generate form of MODCB RPL                      |

All supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

### SHOWCB RPL macro

The SHOWCB macro with RPL=addr will return RPL-related fields according to the parameters specified
on the macro invocation in the order they are specified.

The structure and layout of the affected RPL are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL, TESTCB RPL and/or MODCB RPL
to inspect, test, and/or modify the RPL's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the SHOWCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The SHOWCB RPL macro can be coded as follows:

| Opcode | Operand                   | Remarks                                              |
|--------|---------------------------|------------------------------------------------------|
| SHOWCB | RPL=address               | Points MODCB to the RPL to be queried                |
|        | AREA=addr                 | Address of return area                               |
|        | LENGTH=nr                 | Size of return area in bytes                         |
|        | FIELDS=(keywd_list)       | List of keywords indicating which fields to return   |
|        | [MF=]                     | Use standard form of SHOWCB RPL; this is the default |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of SHOWCB RPL                          |
|        | [MF=(E,addr)]             | Use execute form of SHOWCB RPL                       |
|        | [MF=(G,addr,[label])]     | Use generate form of SHOWCB RPL                      |

Defined options for the FIELDS parameter are listed below:

| Keyword | Length | Remarks                                        |
|---------|--------|------------------------------------------------|
| ACB     | 4      | Pointer to ACB                                 |
| AIXPC   | 4      | Alternate index pointer count                  |
| AREA    | 4      | Pointer to record buffer                       |
| AREALEN | 4      | Size of record buffer in bytes                 |
| ARG     | 4      | Pointer to last used search argument field     |
| ECB     | 4      | Pointer to user-supplied ECB                   |
| FDBK    | 4      | Feedback code for the last request             |
| FTNCD   | 4      | Function code                                  |
| KEYLEN  | 4      | Length of key, for use with OPTCD=GEN          |
| MSGAREA | 4      | Pointer to message area, foxes if not relevant |
| MSGLEN  | 4      | Length of message area, foxes if not relevant  |
| NXTRPL  | 4      | Pointer to next RPL, if any                    |
| RBA     | 4      | 4-byte RBA of last record processed            |
| RECLEN  | 4      | Length of current record                       |
| RPLLEN  | 4      | Length of RPL                                  |
| TRANSID | 4      | Transaction_id; always foxes                   |
| XRBA    | 8      | 8-byte RBA of last record processed            |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

Overview of differences with IBM VSAM:

FIELDS=RBA/XRBA – zVSAM supports these keywords only for ESDS. For any other type of
cluster a value of foxes will be returned by default.

### TESTCB RPL macro

The TESTCB macro with RPL=addr will test RPL-related fields according to the parameters specified on the macro invocation.
Only a single test can be specified on each TESTCB invocation.
TESTCB returns a PSW condition code of 8=Equal when the specified test is met, 7=NotEqual otherwise.

The structure and layout of the affected RPL are not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the RPL is discouraged. Use SHOWCB RPL, TESTCB RPL and/or MODCB RPL
to inspect, test, and/or modify the RPL's content.

Likewise, the structure and layout of the CBMR that zVSAM uses to transfer the TESTCB request to the CBMR handler
are  not part of the interface and are therefore not shown in this chapter.
Direct access to subfields in the CBMR is strongly discouraged.

The TESTCB RPL macro can be coded as follows:

| Opcode | Operand                   | Remarks                                              |
|--------|---------------------------|------------------------------------------------------|
| TESTCB | RPL=address               | Points TESTCB to the RPL to be tested                |
|        | ERET=addr                 | Address of error handling routine                    |
|        | OPTCD=(keywd_list)        | List of keywords indicating attributes to test       |
|        | AIXFLAG=AIXPKP            | Using primary keys?                                  |
|        | AIXPC=nr                  | Nr of index pointers in use                          |
|        | FTNCD=nr                  | Reflects the condition of the upgrade set            |
|        | IO=COMPLETE               |                                                      |
|        | ACB=addr                  |                                                      |
|        | AREA=addr                 |                                                      |
|        | AREALEN=addr              |                                                      |
|        | ARG=addr                  |                                                      |
|        | ECB=addr                  |                                                      |
|        | FDBK=nr                   |                                                      |
|        | KEYLEN=nr                 | Length of key field                                  |
|        | RECLEN=nr                 | Logical Record Length                                |
|        | MSGAREA=adr               | Message area address                                 |
|        | MSGLEN=nr                 | Length of message area in bytes                      |
|        | NXTRPL=addr               |                                                      |
|        | RBA=nr                    |                                                      |
|        | RPLLEN=nr                 |                                                      |
|        | TRANSID=nr                |                                                      |
|        | [MF=]                     | Use standard form of SHOWCB ACB; this is the default |
|        | [MF=L/MF=(L,addr,[label]] | Use list form of SHOWCB ACB                          |
|        | [MF=(E,addr)]             | Use execute form of SHOWCB ACB                       |
|        | [MF=(G,addr,[label])]     | Use generate form of SHOWCB ACB                      |

Supported options for the OPTCD parameter are the same as those available on the RPL macro.

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary can be found in the addenda.

Overview of differences with IBM VSAM:

RBA=nr – zVSAM supports this keyword only for ESDS. For any other type of
cluster a value of foxes will be assumed by default.

### POINT macro

### GET macro

### PUT macro

### ERASE macro

### CHECK macro

### ENDREQ macro

### VERIFY macro

## Catalog management

This is where all meta-data about the zVSAM components are kept and where the relations between zVSAM components are defined.
Catalogs are currently created as static assembled modules. Dynamic catalogs contained in datasets will be considered
in a future release.

The catalog will hold at least:
- file name
- pointer to index file
- pointers to all related AIX clusters
- LRECL
- record type (F, V, FS, VS)
- type of component (ESDS, KSDS, RRDS, LDS, AIX)
- freeblocks (during load, between blocks)
- freespace (during load, within blocks)
- Physical Block size (aka CI-size, 512 bytes to 16MB)
