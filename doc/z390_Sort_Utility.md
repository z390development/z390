# z390 ZSORT

The z390 ZSORT internal sort facility is a component of z390 which supports sorting of any number of fixed or variable length records of any size up to the limits of memory and 64 bit file system.

ZSORT is implemented with the intent of being compatible with IBM&reg; DFSORT.
## SORT utility program

SORT.MLC utility assembler program included in the sort and linklib folders performs a sort with the following input files:

* SORTIN – unsorted input file with DCB options 
* SORTOUT – sorted output file with DCB options
* SYSIN – sort field definitions
* SORTWK01/SORTWK02 - sort work files

## Macro reference

The ZSORT utility function can be called from z390 assembler using standard call interface with full work parameters in list pointed to by register 1.

The first positional macro parameter contains the operation code and options:  

The operations available:

* ISORT - initialize for internal sort using PUT to insert unsorted records and GET to retrieve sorted records
* PUT - insert unsorted record following ISORT
* GET - return sorted record following last PUT, returns RC=4 at end of sorted records

### Example usage

Here is an example usage of ZSORT macro interface:

``` hlasm
ZSORT    ISORT,LRECL=80,MEMORY=10000000,FIELDS=(1,80,CH,A)
.......
ZSORT    PUT,REC=(R2)
.......
ZSORT    GET,REC=(R2)
         CHI   R15,4
         BE    END_OF_FILE
```

### ISORT parameters

!!! Note
    The PUT and GET operations only require the keyword REC= defining address of record area.

#### FIELDS
Any number of key fields defined as 

``` hlasm
FIELDS=(offset,length,type,order,{.repeat.})
```

**Offset** to start of key starts at 1 and cannot be greater than LRECL.  
For variable length records the offset does not include the 4 byte prefix.

**Length** of key plus offset must not exceed LRECL

**Type** of sort key field:

* AC - ASCII characters (same as CH)
* BI - unsigned binary (same as CH)
* CH - EBCDIC characters (same as CH)
* FI - signed binary such as half word, full word, or quad word integers
* FL - floating point HFP, BFP, or DFP short, long, or extended
* PD - packed decimal
* ZD - zoned decimal

**Order**

* A - Ascending
* D - Descending

#### LRECL

Length of record (may be maximum length of variable length records)

#### MEMORY

Amount of memory available for sort table.
If no value is specified, the maximum available contiguous memory block within the memory allocated to step by MEM option will be used.

### Execute format

Alternatively the execute form `MF=(E,addr)` can be used.  See linklib\SORT.MLC for example.

## Technical details

ZSORT is implemented via SVC x'A1' which has 3 function calls:

* initialize internal sort request
* submit unsorted record
* retrieve sorted records.

Unsorted records are loaded into dynamically allocated table in memory and sorted.

If the unsorted records exceed size of table, then multiple blocks of sorted records are written to a work file and then merged.

If all the records fit in table, then they are sorted and returned without requiring use of sort work files.

When required, the merging is performed using two dynamically allocated sort work files with DDNAME's SORTWK01 and SORTWK02. 

The sorted strings are merged from one work file to another doubling the size of the sorted strings on each pass until all the records are sorted on last merge pass.

All file I/O is blocked to minimize disk seeking on single disk systems.  

User can define location of SORTWK01 and SORTWK02 if multiple physical disk drives are available.

A set of regression tests are executed via rt\RTSORT.BAT including test of all 7 sort key types.

The utilities sort\TESTSRT3.MLC and TESTSRT4.MLC can be used to generate, sort, and verify any number of records.  

A million records can be sorted in 28 seconds.  Statistics on each sort execution are recorded on the statistics file if option STATS is specified.
