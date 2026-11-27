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
 *****************************************************
 * 2026-10-27 initial coding, view on the VPTH mapped by VCDTD2
 *****************************************************/



/**
 * The zCatVPTH object is the Java-level representation of a Catalog definition
 * for a zVSAM path. V1 and V2 share the layout.
 */
public class zCatVPTH extends zControlBlock {
    /** VPTHLEN                               */ static final int LEN = 28;
    /** translation tables                    */ private final tz390 tz;
    /** entry when it is an AIX               */ private zCatVAIX aix;
    /** entry when it is a cluster alias      */ private zCatVCLR alias;



    /**
     * Map a VPTH in z390 memory
     *
     * @param tz   translation tables
     * @param mem  z390 memory
     * @param addr address of the VPTH
     */
    public zCatVPTH(tz390 tz, byte[] mem, int addr) {
        super(mem, addr, LEN);
        this.tz = tz;
        if (!"PATH".equals(get_string(0, 4, tz))) {
            throw new zException("zCatVPTH: no PATH eyecatcher at " + Integer.toHexString(addr), 8, 8);
        }
    }



    /**
     * @return path name
     */
    public String get_name() {
        return get_string(4, 8, tz);
    }



    /**
     * @return name of the AIX or cluster this path is for
     */
    public String get_entry_name() {
        return get_string(12, 8, tz);
    }



    /**
     * @return true when the entry is an AIX, false for an alias of a cluster
     */
    public boolean is_aix_path() {
        return get_boolean(20, (byte) 0x80);
    }



    /**
     * @return true for UPDATE=YES
     */
    public boolean is_update() {
        return get_boolean(20, (byte) 0x40);
    }



    /**
     * @return the AIX for an AIX path, null for an alias
     */
    public zCatVAIX get_aix() {
        return aix;
    }



    /**
     * The base cluster, which for an AIX path is reached via VAIXRELA
     *
     * @return base cluster
     */
    public zCatVCLR get_cluster() {
        return aix != null ? aix.get_cluster() : alias;
    }



    /**
     * @return address of the VAIX or VCLR
     */
    int get_entry_addr() {
        return get_int(24) & 0x7fffffff;
    }



    /**
     * Called by zCatVCDT while it links the tables
     *
     * @param aix_entry   the AIX, or null
     * @param alias_entry the cluster, or null
     */
    void set_entry(zCatVAIX aix_entry, zCatVCLR alias_entry) {
        aix   = aix_entry;
        alias = alias_entry;
    }
}
