/*
z390 - Mainframe assembler emulator and run-time engine
Copyright (C) 2021 z390 Assembler LLC

This file is part of z390.
z390 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

z390 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <https://www.gnu.org/licenses/>.
*/
   /* ***************************************************
    * Maintenance
    * ***************************************************
    * 2025-10-28 #656 AFK Created new



/**
 * enum ArchLevel defines all architecture levels supported by z390
 * This includes all IBM-defined architecture levels, with the
 * addition of two z390-specific architecture levels.
 * ASSIST opcodes are supported as well, but there is no specific
 * architecture level defined for ASSIST. When the ASSIST option
 * is enabled, the ASSIST mnemonics are added to the opcode table
 * in effect.
 */
public enum ArchLevel {
    /** 360-20                             */ ARCH_360_20 (0),
    /** DOS                                */ ARCH_DOS    (1),
    /** 370                                */ ARCH_370    (2),
    /** 370-XA                             */ ARCH_XA     (3),
    /** 370-ESA                            */ ARCH_ESA    (4),
    /** ZOP                                */ ARCH_ZOP    (5),
    /** YOP                                */ ARCH_YOP    (6),
    /**                                    */ // "07:reserved" 07 skipped to assign 09 to z9
    /**                                    */ // "08:reserved" 08 skipped to assign 09 to z9
    /** z9                                 */ ARCH_Z9     (9),
    /** z10                                */ ARCH_Z10    (10),
    /** z11                                */ ARCH_Z11    (11),
    /** z12                                */ ARCH_Z12    (12),
    /** z13                                */ ARCH_Z13    (13),
    /** z14                                */ ARCH_Z14    (14),
    /** z15                                */ ARCH_Z15    (15),
    /** z16                                */ ARCH_Z16    (16),
    /** z17                                */ ARCH_Z17    (17),
    /** UNI     (360-20 thru z17 combined) */ ARCH_UNI    (80),
    /** z390    (UNI plus z390 additions)  */ ARCH_Z390   (90),
    /** default (z390 plus ACALLPRM        */ ARCH_DFLT   (91);
    /** enum value instantiation           */ private final int value;



/**
  * Constructor
  *
  * @param value assigned sequence number for architecture level
  */
    ArchLevel(int value) {
        this.value = value;
    }



/**
 * convert enum reference to an integer
 * This is required to be able to check architecture ranges
 * Most instructions are valid from a starting architecture level
 * Some are defined from/to certain levels.
 *
 * @return assigned sequence number of architecture level
 */
    public int getValue() {
        return value;
    }
}
