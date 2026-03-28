.*********************************************************************
.* z390 - Mainframe assembler emulator and run-time engine
.* Copyright (C) 2026 z390 Assembler LLC
.*
.* This file is part of z390.
.*
.* z390 is free software; you can redistribute it and/or modify
.* it under the terms of the GNU General Public License as published by
.* the Free Software Foundation; either version 2 of the License, or
.* (at your option) any later version.
.* z390 is distributed in the hope that it will be useful,
.* but WITHOUT ANY WARRANTY; without even the implied warranty of
.* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
.* GNU General Public License for more details.
.*
.* You should have received a copy of the GNU General Public License
.* along with this program; if not, see https://www.gnu.org/licenses.
.*********************************************************************
.* Author - ZH
.* Date   - 29/03/26
.*********************************************************************
.* DSECT for UNSTRING statement parameter block at ZCVT_WORKAREA
.*
.* Built by UNSTRING.MAC, consumed by ZCVT_UNSTRING runtime routine.
.* Overlays ZCVT_WORKAREA (256 bytes available, variable length).
.* Target fields are appended as address/length pairs starting at
.* offset 32, up to the ZCVT_WORKAREA limit.
.*********************************************************************
ZCUNSPL  DSECT
ZCUNS_SRC_ADDR  DS A       Address of source field
ZCUNS_SRC_LEN   DS F       Length of source field
ZCUNS_DLM_ADDR  DS A       Address of delimiter (0 = none)
ZCUNS_DLM_LEN   DS F       Length of delimiter
ZCUNS_DLM_FLAGS DS F       Delimiter flags (bit 0 = ALL)
ZCUNS_TGT_COUNT DS F       Number of target fields
ZCUNS_PTR_ADDR  DS A       Address of pointer field (0 = none)
ZCUNS_TAL_ADDR  DS A       Address of tallying field (0 = none)
ZCUNS_TGT1_ADDR DS A       Address of first target
ZCUNS_TGT1_LEN  DS F       Length of first target
ZCUNSPL_LEN     EQU *-ZCUNSPL
