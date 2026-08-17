# zVSAM V2 Catalog

z390 does not support catalogs that actively keep track of what goes on in the file system.

In stead z390 uses loadmodules that describe files of interest to z390.
These catalog loadmodules are assembled from source.

For a description of the current catalog implementation, please see the
[z390_zVSAM_Catalog_User_Guide](../../user_guide/zVSAM/zVSAM_V1_Catalog_User_Guide.md).

## Catalog management

This is where all meta-data about the zVSAM components are kept and where the relations between zVSAM
components are defined. Catalogs are currently created as static assembled modules.
Dynamic catalogs contained in datasets will be considered in a future release.

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

> [!NOTE]
> 1. This document needs to be updated. Melvyn already extended the static catalog stuff
>    whith what he needed for his initial zVSAM V2 implementation. He omitted LDS,
>    which we'll have to add back in.
> 2. The catalog support logic in z390 simply loads the catalog loadmod, then uses
>    the data as they appear in z390 memory. For V2 we'll need to complement with java logic
>    to instantiate a catalog object from either a V1 or a V2 catalog.
