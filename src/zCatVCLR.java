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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;



/* ***************************************************
 * Maintenance
 *****************************************************
 * 2016-12-28 initial coding
 * 2026-03-23 AFK Fix/Add javadoc comments
 * 2026-08-07 #807 Fix issues flagged by linter
 * 2026-10-27 View on the VCLR in z390 memory, mapped by VCDTD2 (V1 by VCDTD1)
 *****************************************************/



/**
 * The zCatVCLR object is the Java-level representation of a Catalog definition
 *              for a zVSAM base cluster
 */
public class zCatVCLR extends zControlBlock {
    /** VCLRLEN for a V2 catalog              */ static final int LEN_V2 = 68;
    /** VCLRLEN for a V1 catalog              */ static final int LEN_V1 = 56;
    /** block size when the catalog has none  */ static final int DFT_BLKSZ = 4096;
    /** longest DSN accepted                  */ static final int MAX_DSN = 1024;
    /** translation tables                    */ private final tz390 tz;
    /** true for a V1 (C'VCDT') catalog       */ private final boolean v1;
    /** the upgrade set, linked by zCatVCDT   */ private final List<zCatVAIX> upgrades = new ArrayList<zCatVAIX>();



    /**
     * Map a VCLR in z390 memory
     *
     * @param tz   translation tables
     * @param mem  z390 memory
     * @param addr address of the VCLR
     * @param v1   true when the catalog is V1
     */
    public zCatVCLR(tz390 tz, byte[] mem, int addr, boolean v1) {
        super(mem, addr, v1 ? LEN_V1 : LEN_V2);
        this.tz = tz;
        this.v1 = v1;
        if (!"VCLR".equals(get_string(0, 4, tz))) {
            throw new zException("zCatVCLR: no VCLR eyecatcher at " + Integer.toHexString(addr), 8, 8);
        }
    }



    /**
     * @return cluster name
     */
    public String get_name() {
        return get_string(4, 8, tz);
    }



    /**
     * @return ESDS, KSDS, RRDS or LDS
     */
    public String get_type() {
        return get_string(12, 4, tz);
    }



    /**
     * @return true for variable length records
     */
    public boolean is_variable() {
        return get_boolean(16, (byte) 0x80);
    }



    /**
     * @return true for spanned records, never for V1
     */
    public boolean is_spanned() {
        return !v1 && get_boolean(16, (byte) 0x10);
    }



    /**
     * @return true for a KSDS
     */
    public boolean is_ksds() {
        return get_boolean(16, (byte) 0x08);
    }



    /**
     * @return true for an RRDS
     */
    public boolean is_rrds() {
        return get_boolean(16, (byte) 0x04);
    }



    /**
     * @return true for an ESDS
     */
    public boolean is_esds() {
        return get_boolean(16, (byte) 0x02);
    }



    /**
     * @return true for an LDS
     */
    public boolean is_lds() {
        return get_boolean(16, (byte) 0x01);
    }



    /**
     * @return true for REUSE=YES, which V1 kept in byte 1
     */
    public boolean is_reuse() {
        return v1 ? get_boolean(16, (byte) 0x40) : get_boolean(17, (byte) 0x20);
    }



    /**
     * @return true for DATAADJUST=YES
     */
    public boolean is_data_adjust() {
        return !v1 && get_boolean(17, (byte) 0x80);
    }



    /**
     * @return true for INDEXADJUST=YES
     */
    public boolean is_index_adjust() {
        return !v1 && get_boolean(17, (byte) 0x40);
    }



    /**
     * @return average record length
     */
    public int get_lavg() {
        return get_int(20);
    }



    /**
     * @return maximum (or fixed) record length
     */
    public int get_lmax() {
        return get_int(24);
    }



    /**
     * @return primary key length
     */
    public int get_key_length() {
        return get_int(28);
    }



    /**
     * @return primary key offset
     */
    public int get_key_offset() {
        return get_int(32);
    }



    /**
     * @return DTADSN override, empty when there is none
     */
    public String get_dta_dsn() {
        return dsn(36);
    }



    /**
     * @return IDXDSN override, empty when there is none
     */
    public String get_idx_dsn() {
        return dsn(40);
    }



    /**
     * @return data block size; V1 has only the CI size, if that
     */
    public int get_data_block_size() {
        int size = get_int(44);
        return v1 && size == 0 ? DFT_BLKSZ : size;
    }



    /**
     * @return index block size
     */
    public int get_index_block_size() {
        return v1 ? DFT_BLKSZ : get_int(48);
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
     * @return the AIXs in this cluster's upgrade set
     */
    public List<zCatVAIX> get_upgrades() {
        return Collections.unmodifiableList(upgrades);
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
     * @return number of upgrade AIXs, AL1 in V2 and F in V1
     */
    int get_upgrade_count() {
        return v1 ? get_int(48) : get_byte(62) & 0xff;
    }



    /**
     * @return address of the upgrade address list
     */
    int get_upgrade_list() {
        return (v1 ? get_int(52) : get_int(63)) & 0x7fffffff;
    }



    /**
     * Called by zCatVCDT while it links the tables
     *
     * @param aix an AIX in the upgrade set
     */
    void add_upgrade(zCatVAIX aix) {
        upgrades.add(aix);
    }



    /**
     * @param off offset of the DSN pointer
     * @return the string it points at, empty when there is none
     */
    private String dsn(int off) {
        int addr = get_int(off) & 0x7fffffff;
        return addr == 0 ? "" : get_cstring(addr, MAX_DSN, tz);
    }
}
