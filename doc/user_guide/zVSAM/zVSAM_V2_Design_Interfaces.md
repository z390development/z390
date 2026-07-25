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
