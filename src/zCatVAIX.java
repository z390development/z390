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
 * 2016-12-28 initial coding
 * 2026-03-23 AFK Fix/Add javadoc comments
 * 2026-08-07 #807 Fix issues flagged by linter
 * 2026-10-27 View on the VAIX in z390 memory, mapped by VCDTD2 (V1 by VCDTD1)
 *****************************************************/



/**
 * The zCatVAIX object is the Java-level representation of a Catalog definition
 * for a zVSAM Alternate IndeX
 */
public class zCatVAIX extends zControlBlock {
    /** VAIXLEN for a V2 catalog              */ static final int LEN_V2 = 68;
    /** VAIXLEN for a V1 catalog              */ static final int LEN_V1 = 40;
    /** translation tables                    */ private final tz390 tz;
    /** true for a V1 (C'VCDT') catalog       */ private final boolean v1;
    /** related base cluster, set by zCatVCDT */ private zCatVCLR cluster;



    /**
     * Map a VAIX in z390 memory
     *
     * @param tz   translation tables
     * @param mem  z390 memory
     * @param addr address of the VAIX
     * @param v1   true when the catalog is V1
     */
    public zCatVAIX(tz390 tz, byte[] mem, int addr, boolean v1) {
        super(mem, addr, v1 ? LEN_V1 : LEN_V2);
        this.tz = tz;
        this.v1 = v1;
        if (!"VAIX".equals(get_string(0, 4, tz))) {
            throw new zException("zCatVAIX: no VAIX eyecatcher at " + Integer.toHexString(addr), 8, 8);
        }
    }



    /**
     * @return AIX name
     */
    public String get_name() {
        return get_string(4, 8, tz);
    }



    /**
     * @return name of the related base cluster
     */
    public String get_relate_name() {
        return get_string(12, 8, tz);
    }



    /**
     * @return the related base cluster
     */
    public zCatVCLR get_cluster() {
        return cluster;
    }



    /**
     * @return true for UNIQUEKEY=YES
     */
    public boolean is_unique() {
        return get_boolean(20, (byte) 0x40);
    }



    /**
     * @return true when in the base cluster's upgrade set
     */
    public boolean is_upgrade() {
        return get_boolean(20, (byte) 0x20);
    }



    /**
     * @return true for VS records, which is what a non-unique AIX gets
     */
    public boolean is_variable() {
        return v1 ? !is_unique() : get_boolean(20, (byte) 0x80);
    }



    /**
     * @return true for spanned records, which is what a non-unique AIX gets
     */
    public boolean is_spanned() {
        return v1 ? !is_unique() : get_boolean(20, (byte) 0x10);
    }



    /**
     * @return true for REUSE=YES, which V1 kept in byte 1
     */
    public boolean is_reuse() {
        return v1 ? get_boolean(20, (byte) 0x80) : get_boolean(21, (byte) 0x20);
    }



    /**
     * @return true for DATAADJUST=YES
     */
    public boolean is_data_adjust() {
        return !v1 && get_boolean(21, (byte) 0x80);
    }



    /**
     * @return true for INDEXADJUST=YES
     */
    public boolean is_index_adjust() {
        return !v1 && get_boolean(21, (byte) 0x40);
    }



    /**
     * @return alternate key length in the base record
     */
    public int get_key_length() {
        return get_int(24);
    }



    /**
     * @return alternate key offset in the base record
     */
    public int get_key_offset() {
        return get_int(28);
    }



    /**
     * @return DTADSN override (V1 VXN name), empty when there is none
     */
    public String get_dta_dsn() {
        return dsn(32);
    }



    /**
     * @return IDXDSN override, empty when there is none
     */
    public String get_idx_dsn() {
        return v1 ? "" : dsn(36);
    }



    /**
     * @return data block size
     */
    public int get_data_block_size() {
        return v1 ? zCatVCLR.DFT_BLKSZ : get_int(44);
    }



    /**
     * @return index block size
     */
    public int get_index_block_size() {
        return v1 ? zCatVCLR.DFT_BLKSZ : get_int(48);
    }



    /**
     * @return DATAFREESPACE percentage
     */
    public int get_data_freespace() {
        return v1 ? 0 : get_byte(52) & 0xff;
    }



    /**
     * @return INDEXFREESPACE percentage
     */
    public int get_index_freespace() {
        return v1 ? 0 : get_byte(53) & 0xff;
    }



    /**
     * @return DATAFREEBLOCKS x, the number of free blocks
     */
    public int get_data_free_blocks() {
        return v1 ? 0 : get_short(54) & 0xffff;
    }



    /**
     * @return DATAFREEBLOCKS y, the interval
     */
    public int get_data_free_interval() {
        return v1 ? 0 : get_short(56) & 0xffff;
    }



    /**
     * @return INDEXFREEBLOCKS x, the number of free blocks
     */
    public int get_index_free_blocks() {
        return v1 ? 0 : get_short(58) & 0xffff;
    }



    /**
     * @return INDEXFREEBLOCKS y, the interval
     */
    public int get_index_free_interval() {
        return v1 ? 0 : get_short(60) & 0xffff;
    }



    /**
     * @return MAXELEMENTS, zero when not given
     */
    public int get_max_elements() {
        return v1 ? 0 : get_int(64);
    }



    /**
     * Derive the first data file, override or {dir}{name}.DTA
     *
     * @param dir catalog directory, ending in a separator
     * @return host file name
     */
    public String get_dta_file(String dir) {
        String dsn = get_dta_dsn();
        return dsn.isEmpty() ? dir + get_name() + ".DTA" : dsn;
    }



    /**
     * Derive the first index file, override or {dir}{name}.IDX
     *
     * @param dir catalog directory, ending in a separator
     * @return host file name
     */
    public String get_idx_file(String dir) {
        String dsn = get_idx_dsn();
        return dsn.isEmpty() ? dir + get_name() + ".IDX" : dsn;
    }



    /**
     * @return address of the related VCLR
     */
    int get_relate_addr() {
        return get_int(v1 ? 36 : 40) & 0x7fffffff;
    }



    /**
     * Called by zCatVCDT while it links the tables
     *
     * @param clr the related base cluster
     */
    void set_cluster(zCatVCLR clr) {
        cluster = clr;
    }



    /**
     * @param off offset of the DSN pointer
     * @return the string it points at, empty when there is none
     */
    private String dsn(int off) {
        int addr = get_int(off) & 0x7fffffff;
        return addr == 0 ? "" : get_cstring(addr, zCatVCLR.MAX_DSN, tz);
    }
}
