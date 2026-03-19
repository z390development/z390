# zVSAM V2 - Logical processes for RPL-based requests

## POINT function

## GET function

Prefix counter field `CTRNEXCP` needs to be incremented whenever a block is needed that does not yet
reside in a buffer. If buffers need to be written out to make room for a block that needs to be read, then the
`CTRNEXCP` counter needs to be incremented as well.

Prefix counter field `CTRNRETR` needs to be incremented for every block accessed, whether it needs to be
read, or already resides in a buffer does not matter for this count field.

If buffers need to be written out in order to allocate a buffer for a block that needs to be read, then prefix
counter `CTRNNUIW` needs to be incremented.

## PUT function

Prefix counter field `CTRAVSPAC` must be updated. When consuming free space to allocate a new record,
reduce `CTRAVSPAC` with space consumed. When allocating a new block (or splitting an existing block) add
the blocksize – (block header, block footer, RPTR area) and reduce with amount used up (record length,
including RDW/SPX and RPTR). When lengthening a record, subtract the difference; when shortening a
record, add the difference.

Prefix counter fields `CTRHALCRBA` and `CTRENDRBA` should be updated whenever a record is added
beyond the current value of these fields. `CTRENDRBA` also should be updated in case the last record in the
component is lengthened.

Prefix counter field `CTRNCIS` should be incremented whenever a block needs to be split.
Prefix counter field `CTRNEXCP` needs to be incremented whenever a block needs to be written out. This
occurs when the cluster was opened with `MACRF=NDF`. With `MACRF=DFR` no writes are forced and no
EXCP needs to be counted.

Prefix counter fields `CTRNINSR` and `CTRNLOGR` need to be incremented whenever a new record is added
to the component.

Prefix counter field `CTRNRETR` needs to be incremented for every block accessed, whether it needs to be
read, or already resides in a buffer does not matter for this count field. For basic put operation this is
irrelevant, but for updating an index or an AIX reads may be needed and should be counted for the
component being read.

When an existing record is updated, then `CTRNUPDR` needs to be incremented.

When adding a record the record length (including SPX/RDW, but excluding RPTR) needs to be added to
the prefix counter field `CTRSDTASZ`. When a record is updated to a different length, then the difference in
length needs to accounted into the `CTRSDTASZ` field.

When a user write is forced then the prefix counter field `CTRNUIW` needs to be incremented. This happens
when a put is issued to a cluster that was opened with `MACRF=NDF`. For clusters opened with
`MACRF=DFR` writing is done by zVSAM when the buffer is needed for a different block. These writes are
counted in the `CTRNNUIW` field.

The prefix counter field `CTRAVGRL` contains the value of `CTRSDTASZ` / `CTRNLOGR` rounded up to the
nearest integer. The field should be updated whenever either or both of the input values are changed.

The prefix counter field `CTRLOKEY@` is the offset to a length and a value of `PFXKYLEN` bytes
The value should be updated whenever a record is inserted that has a lower key than the current lowest key.

## ERASE function

Prefix counter field `CTRNDELR` should be incremented for every successful erase operation.

Prefix counter field `CTRNEXCP` needs to be incremented whenever a block is needed that does not yet
reside in a buffer. If buffers need to be written out to make room for a block that needs to be read, then the
`CTRNEXCP` counter needs to be incremented as well.

Prefix counter field `CTRNLOGR` needs to be decremented for every successful erase operation.

Prefix counter field `CTRNRETR` needs to be incremented for every block accessed, whether it needs to be
read, or already resides in a buffer does not matter for this count field. For basic erase operation this is
irrelevant, but for updating an index or an AIX reads may be needed and should be counted for the
component being read.

When erasing a record the record length (including SPX/RDW, but excluding RPTR) needs to be subtracted
from the prefix counter field `CTRSDTASZ`.

When a user write is forced then the prefix counter field `CTRNUIW` needs to be incremented. This happens
when an erase is issued to a cluster that was opened with `MACRF=NDF`. For clusters opened with
`MACRF=DFR` writing is done by zVSAM when the buffer is needed for a different block. These writes are
counted in the `CTRNNUIW` field.

The prefix counter field `CTRAVGRL` contains the value of `CTRSDTASZ` / `CTRNLOGR` rounded up to
nearest integer. The field should be updated after every successful erase operation.

The prefix counter field `CTRLOKEY@` is the offset to a length and value of `PFXKEYLN` bytes.
The value should be updated whenever the record is erased with the current lowest key

Prefix counter field `CTRNEXCP` needs to be incremented whenever a block is needed that does not yet
reside in a buffer.

## CHECK function

## ENDREQ function

## VERIFY function

## Locking
