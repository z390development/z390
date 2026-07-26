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
