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
.* DSECT for STRING statement parameter block at ZCVT_WORKAREA
.*
.* Built by STRING.MAC, consumed by ZCVT_STRING runtime routine.
.* Overlays ZCVT_WORKAREA (256 bytes available, 32 used).
.*********************************************************************
ZCSTRPL  DSECT
ZCSTR_SRC_ADDR  DS A       Address of source field
ZCSTR_SRC_LEN   DS F       Length of source field
ZCSTR_DLM_ADDR  DS A       Address of delimiter (0 = SIZE)
ZCSTR_DLM_LEN   DS F       Length of delimiter (0 = SIZE)
ZCSTR_TGT_ADDR  DS A       Address of target field
ZCSTR_TGT_LEN   DS F       Length of target field
ZCSTR_PTR_ADDR  DS A       Address of pointer field (0 = none)
ZCSTR_FLAGS     DS F       Reserved
ZCSTRPL_LEN     EQU *-ZCSTRPL
