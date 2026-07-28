## POINT macro

## GET macro

## PUT macro

## ERASE macro

## CHECK macro

## ENDREQ macro

## VERIFY macro

## GENCB, MODCB, TESTCB and SHOWCB macros

### GENCB, MODCB, TESTCB and SHOWCB use of the CBMR

### GENCB, MODCB, TESTCB and SHOWCB use of MF=<a id="MFdetails" />

All forms except MF=L generate executable code.

| Parameter            | Explanation                                        |
|----------------------|----------------------------------------------------|
| MF=I or omitted      | Generates CBMR and invokes ZVSAM19C to process it  |
| MF=L                 | Generates CBMR inline                              |
| MF=(L,address)       | Generates CBMR inline and then moves it to address |
| MF=(L,address,label) | as above and generates label equ size              |
| MF=(E,address)       | Modifies the CBMR at address                       |
|                      | Invokes ZVSAM19C to process the CBMR               |
| MF=(G,address)       | Generates CBMR inline and then moves it to address |
|                      | Invokes ZVSAM19C to process the CBMR               |
| MF=(G,address,label) | as above and generates label equ size              |

address can be label or reg, reg cannot be 0, 1, 14 or 15. reg is not permitted for MF=L

### GENCB, MODCB, TESTCB and SHOWCB parameter types

For abs expression (called value in the macro definitions)

| Parameter type        | For MF=I/G/L                   | For MF=E                       |
|-----------------------|--------------------------------|--------------------------------|
| n                     | Permitted                      | Permitted                      |
| EQUated numeric value | Permitted, but not for LENGTH= | Permitted, but not for LENGTH= |

For address

| Parameter type               | For MF=I/G/L                        | For MF=E                                |
|------------------------------|-------------------------------------|-----------------------------------------|
| n                            | Permitted. See note 1               | Permitted, but not for ERET= See note 1 |
|                              |                                     | When n=0 see note 2                     |
| EQUated numeric value        | Permitted, but not for LENGTH=      | Permitted, but not for LENGTH=          |
|                              | See note 1                          | See note 1 here                         |
| ADCON-type address           | Permitted                           | Permitted                               |
| Register form (reg)          | Permitted, but not regs 0,1,14,15   | Permitted, but not regs 0,1,14,15       |
| Indirect form with ADCON     | Permitted for certain 8-byte fields | Permitted for certain 8-byte fields     |
| (\*,address)                 | See Note 3                          | See Note 3                              |
| Indirect form with disp(reg) | Permitted for certain 8-byte fields | Permitted for certain 8-byte fields     |
| (\*,n(reg))                  | reg cannot be 0,1,14,15. See Note 3 | reg cannot be 0,1,14,15. See Note 3     |

*Note 1:* The use of numeric values instead of an address may lead to accessing low storage and
should be avoided

*Note 2:* An exception is TESTCB EODAD, JRNAD, LERAD and SYNAD where zero instead of an
address means 'don't test the address'

*Note 3:* The following fields only support the indirect form in TESTCB:
`SDTASZ`, `STMST` and all `X*` fields.
The lack of proper syntax checking in the IBM macro can cause access to low storage or
environmental destruction, so the following syntaxes are not allowed: `(*,*)` and `(*,n)`.

## SHOWCB with no specified block type macro

The SHOWCB macro without a block will return length fields according to the parameters specified on the
macro invocation in the order they are specified. Duplicates are permitted.

| Opcode         | Operand               | Remarks                                                  |
|----------------|-----------------------|----------------------------------------------------------|
| [label] SHOWCB | [AM=VSAM]             | Optional, no other values allowed                        |
|                | AREA=address          | Address of return area                                   |
|                | LENGTH=value          | Size of return area in bytes                             |
|                | FIELDS=(keyword list) | List of keywords indicating which fields to return       |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |

Supported options for the FIELDS parameter are listed below:

| Keyword | Length | Remarks                    |
|---------|--------|----------------------------|
| ACBLEN  | 4      | Length of ACB in bytes     |
| EXLLEN  | 4      | Length of EXLST in bytes   |
| RPLLEN  | 4      | Length of RPL in bytes     |

All supported parameters and keywords are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                  |
|-------------|-----------------|--------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                               |
| R15=4       | Reason Code=4   | Invalid control block                                                    |
| R15=4       | Reason Code=9   | Length too small                                                         |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created |

## TESTCB Introduction

Only a single test can be specified on each TESTCB invocation.
In all cases the value or address supplied in the macro is compared to a constant or a value in a control block.
Many parameters have been added in zVSAM V2 to bring it in line with SHOWCB, these are in **bold**.

An extra column is provided in the charts below to indicate the type of condition code that could be returned,
the notation NE=LO (etc) is used to indicate that although LO may be returned the preferred test is for EQ/NE.

Where a parm may have subparameters, then all must be true for an EQ to be returned, if unsupported parms
or subparameters are specified then NE=LO is returned, there are more details against the parm itself.

It is highly recommended that a branch table is placed after the TESTCB to capture any error conditions, the
condition code is unpredictable if an error does occur.

Example:
```
         TESTCB ACB=MYACB,NCIS=20,MF=I
         B     *+4(R15)
         J     OK
         J     ERR04
         J     ERR08
OK       DS    0H
```

IBMs TESTCB is very badly syntax checked, zVSAM V2 has tightened the rules.
It's very unlikely that imported code will result in unexpected errors, the z390 team would like to know of any.

### More details against specific parms.

### TESTCB with no specified block type macro

The TESTCB without a block macro can be coded as follows:

| Opcode         | Operand               | Remarks                                                  | Conditions returned |
|----------------|-----------------------|----------------------------------------------------------|---------------------|
| [label] TESTCB | [AM=VSAM]             | Optional, no other values allowed                        |                     |
|                | [ERET=address]        | Address of error handling routine                        |                     |
|                | ACBLEN=value          | ACB length                                               | EQ LO HI            |
|                | RPLLEN=value          | RPL length                                               | EQ LO HI            |
|                | EXLLEN=value          | EXLST length                                             | EQ LO HI            |
|                | [MF=]                 | See the [description of MF=](#MFdetails)                 |                     |

### Return (R15) and Reason (R0) Codes

| Return Code | Reason Code     | Meaning                                                                          |
|-------------|-----------------|----------------------------------------------------------------------------------|
| R15=0       | Reason Code=n/a | Successful                                                                       |
| R15=4       | Reason Code=4   | Invalid control block                                                            |
| R15=8       | Reason Code=n/a | An attempt was made to update a CBMR with a field not previously created         |

## Catalog management

This is where all meta-data about the zVSAM components are kept and where the relations between zVSAM
components are defined. Catalogs are currently created as static assembled modules.
Extended catalogs contained in datasets will be considered in a future release.

The catalog will hold at least:
- file name
- pointer to index file
- pointers to all related AIX clusters
- LRECL
- record type (F, V, FS, VS)
- type of component (ESDS, KSDS, RRDS, AIX)
- freeblocks (during load, between blocks)
- freespace (during load, within blocks)
- Physical Block size (aka CI-size, 512 bytes to 16MB)

For a complete list of catalog components please see the
z390_zVSAM_Catalog_User_Guide.

