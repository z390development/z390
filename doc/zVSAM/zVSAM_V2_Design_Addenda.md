# zVSAM V2 - Addenda

This document describes Macro parameters and control blocks used in the zVSAM V2 implementation.
It contains the following major chapters:

1. [API for ACB-based interfaces](#API-for-ACB-based-interfaces)
    1. [ACB macro parameters](#ACB-macro-parameters)

## API for ACB-based interfaces

### ACB macro parameters

With the exception of the DDNAME parameter, all supported parameters are implemented compatibly with IBM's VSAM implementation.
For details, please refer to the relevant IBM manual.

For ease of access a short summary follows here:

AM=
:   Optional parameter. AM=VSAM is the default. No other values are supported.

DDNAME=
:   DDname is required before open is executed. If DDname is not supplied on the ACB
macro, the label used on the ACB macro is used as DDname. If neither is specified,
a proper value must be supplied by using MODCB ACB.<br />
In zVSAM the DDname is to hold the name of an environment variable in the host
OS. This variable in turn should contain the path and qualified filename of the cluster
to be opened. The qualifier is the name of an environment variable in the host OS
and is the path to the assembled catalog. For more information on zVSAM catalogs,
please refer to the "z390_zVSAM_Catalog_User_Guide".

`PASSWD`=
:    Supply the address of the password, consisting of a single byte with the password's
length (1-8 characters) followed by the password value.

`EXLST`=
:   Pointer to an exit list. Please see the EXLST macro description for details.

MACRF=
:   List of keywords specifying how the cluster will be processed after open.
Please see below for the list of supported keywords and their meaning.

BUFSP=
:   Maximum buffer space in virtual storage for this cluster. This is the combined size in
bytes of all buffers allocated for this cluster. If (`BUFND` + `BUFNI`) \* Block_size
exceeds the value specified for `BUFSP`, then `BUFND` and `BUFNI` will be reduced
proportionally to keep total allocation below the limit specified in the `BUFSP` parameter.






