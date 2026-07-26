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
