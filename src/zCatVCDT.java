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
import java.util.HashMap;
import java.util.List;
import java.util.Map;



/* ***************************************************
 * Maintenance
 *****************************************************
 * 2026-10-27 initial coding, view on the VCDT mapped by VCDTD2 (V1 by VCDTD1)
 *****************************************************/



/**
 * The zCatVCDT object is the Java-level representation of a loaded catalog.
 * "The fourth byte of VCDTID is the discriminator for a runtime that must
 * accept both layouts (T = V1, 2 = V2)."
 */
public class zCatVCDT extends zControlBlock {
    /** IHAVCDT length, V2 (V1 adds VCDTDCBA) */ static final int LEN = 36;
    /** translation tables                    */ private final tz390 tz;
    /** true for a V1 (C'VCDT') catalog       */ private final boolean v1;
    /** base clusters in table order          */ private final List<zCatVCLR> clusters = new ArrayList<zCatVCLR>();
    /** AIXs in table order                   */ private final List<zCatVAIX> aixs = new ArrayList<zCatVAIX>();
    /** paths in table order                  */ private final List<zCatVPTH> paths = new ArrayList<zCatVPTH>();



    /**
     * Map a loaded catalog and link its entries to one another
     *
     * @param tz   translation tables
     * @param mem  z390 memory
     * @param addr load address of the VCDT
     */
    public zCatVCDT(tz390 tz, byte[] mem, int addr) {
        super(mem, addr, LEN);
        this.tz = tz;
        String id = get_string(0, 4, tz);
        if ("VCDT".equals(id)) {
            v1 = true;
        } else if ("VCD2".equals(id)) {
            v1 = false;
        } else {
            throw new zException("zCatVCDT: not a V1 or V2 catalog, VCDTID=" + id, 8, 8);
        }
        Map<Integer, zCatVCLR> clr_at = new HashMap<Integer, zCatVCLR>();
        Map<Integer, zCatVAIX> aix_at = new HashMap<Integer, zCatVAIX>();
        int len  = v1 ? zCatVCLR.LEN_V1 : zCatVCLR.LEN_V2;
        int base = table(12, len);
        for (int i = 0; i < get_int(12); i++) {
            zCatVCLR clr = new zCatVCLR(tz, mem, base + i * len, v1);
            clusters.add(clr);
            clr_at.put(clr.get_offset(), clr);
        }
        len  = v1 ? zCatVAIX.LEN_V1 : zCatVAIX.LEN_V2;
        base = table(20, len);
        for (int i = 0; i < get_int(20); i++) {
            zCatVAIX aix = new zCatVAIX(tz, mem, base + i * len, v1);
            aixs.add(aix);
            aix_at.put(aix.get_offset(), aix);
        }
        base = table(28, zCatVPTH.LEN);
        for (int i = 0; i < get_int(28); i++) {
            paths.add(new zCatVPTH(tz, mem, base + i * zCatVPTH.LEN));
        }
        for (zCatVAIX aix : aixs) {
            zCatVCLR clr = clr_at.get(aix.get_relate_addr());
            if (clr == null) {
                throw new zException("zCatVCDT: AIX " + aix.get_name() + " relates to no cluster", 8, 8);
            }
            aix.set_cluster(clr);
        }
        for (zCatVCLR clr : clusters) {
            int count = clr.get_upgrade_count();
            if (count == 0) {
                continue;
            }
            zControlBlock list = new zControlBlock(mem, clr.get_upgrade_list(), Math.max(4, 4 * count));
            for (int i = 0; i < count; i++) {
                zCatVAIX aix = aix_at.get(list.get_int(4 * i) & 0x7fffffff);
                if (aix == null) {
                    throw new zException("zCatVCDT: upgrade set of " + clr.get_name() + " names no AIX", 8, 8);
                }
                clr.add_upgrade(aix);
            }
        }
        for (zCatVPTH pth : paths) {
            int entry = pth.get_entry_addr();
            if (pth.is_aix_path()) {
                pth.set_entry(aix_at.get(entry), null);
            } else {
                pth.set_entry(null, clr_at.get(entry));
            }
            if (pth.get_cluster() == null) {
                throw new zException("zCatVCDT: path " + pth.get_name() + " leads nowhere", 8, 8);
            }
        }
    }



    /**
     * @return catalog name
     */
    public String get_name() {
        return get_string(4, 8, tz);
    }



    /**
     * @return true for a V1 catalog
     */
    public boolean is_v1() {
        return v1;
    }



    /**
     * @return base clusters in table order
     */
    public List<zCatVCLR> get_clusters() {
        return Collections.unmodifiableList(clusters);
    }



    /**
     * @return AIXs in table order
     */
    public List<zCatVAIX> get_aixs() {
        return Collections.unmodifiableList(aixs);
    }



    /**
     * @return paths in table order
     */
    public List<zCatVPTH> get_paths() {
        return Collections.unmodifiableList(paths);
    }



    /**
     * @param name cluster name
     * @return the cluster, or null
     */
    public zCatVCLR find_cluster(String name) {
        for (zCatVCLR clr : clusters) {
            if (clr.get_name().equals(name)) {
                return clr;
            }
        }
        return null;
    }



    /**
     * @param name AIX name
     * @return the AIX, or null
     */
    public zCatVAIX find_aix(String name) {
        for (zCatVAIX aix : aixs) {
            if (aix.get_name().equals(name)) {
                return aix;
            }
        }
        return null;
    }



    /**
     * @param name path name
     * @return the path, or null
     */
    public zCatVPTH find_path(String name) {
        for (zCatVPTH pth : paths) {
            if (pth.get_name().equals(name)) {
                return pth;
            }
        }
        return null;
    }



    /**
     * Check a table's count and adress fit in memory before walking it
     *
     * @param off offset of the count; the address follows it
     * @param len entry length
     * @return address of the first entry
     */
    private int table(int off, int len) {
        int count = get_int(off);
        int addr  = get_int(off + 4) & 0x7fffffff;
        if (count < 0 || (count > 0 && (addr <= 0 || (long) addr + (long) count * len > array().length))) {
            throw new zException("zCatVCDT: table at offset " + off + " runs outside memory", 8, 8);
        }
        return addr;
    }
}
