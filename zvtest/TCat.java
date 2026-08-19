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
import java.io.File;
import java.util.Map;
import java.util.TreeMap;

/**
 * Catalog navigation test, ported from tcat.js.
 *
 * The original ran under jjs, which went out of the JDK at 15, so on any
 * current runtime it is unrunnable rather than merely stale. Same steps,
 * same order, plus exit codes so it can be driven from a script.
 *
 * Walks what open() does outside Java: given a ddname, resolve the
 * environment variable, split it into catalogue and cluster, find both,
 * then confirm the file is really on disk.
 */
public class TCat {

    private static int failures = 0;

    private static void check(String what, boolean ok) {
        System.out.println((ok ? "  ok   " : "  FAIL ") + what);
        if (!ok) failures++;
    }

    public static void main(String[] args) {
        String ddname = args.length > 0 ? args[0] : "ESF1";

        Democat.main(new String[]{""});
        InlineCatalog cat = Democat.objCat;

        check("catalogue built", cat != null);
        check("catalogue name is DEMOCAT", "DEMOCAT".equals(cat.Catname));
        check("implemented inline",
              cat.implementedAs == CatalogImplementationType.INLINE);

        // Ten more clusters, so the search has something to search through
        // rather than finding the answer first every time.
        for (int i = 0; i <= 9; i++) {
            CatEntry_Cluster c = new CatEntry_Cluster(
                "TC" + i, "N", 80, new int[]{5, 10}, false, "", "", 0);
            cat.CatalogEntries.put(c.formKey(), c);
        }
        check("12 entries after load", cat.CatalogEntries.size() == 12);

        System.out.println("catalogue contents:");
        Map<String, CatalogEntry> sorted = new TreeMap<>(cat.CatalogEntries);
        for (String k : sorted.keySet()) {
            System.out.println("    " + k);
        }

        // From here on this is vz390.V2.vsam_op_open() with the ACB left out.
        System.out.println("ddname: " + ddname);
        String qualified = System.getenv(ddname);
        if (qualified == null) {
            System.out.println("  FAIL " + ddname + " is not set in the environment");
            System.exit(8);
        }
        System.out.println("path-qualified filename: " + qualified);

        int dot = qualified.lastIndexOf('.');
        int sep = Math.max(qualified.lastIndexOf('/'), qualified.lastIndexOf(0x5c));
        check("filename splits into catalogue and cluster", dot > sep && dot >= 0);
        if (failures > 0) System.exit(8);

        String entryName = qualified.substring(dot + 1);
        String catName   = qualified.substring(sep + 1, dot);
        String path      = qualified.substring(0, sep + 1) + entryName + ".dta";
        System.out.println("cluster: " + entryName + "  catalogue: " + catName);

        check("catalogue " + catName + " found", cat.FindCatalog(catName) != null);
        check("cluster " + entryName + " found",
              cat.FindCatalogEntry("CL", entryName) != null);
        check("a miss returns null rather than throwing",
              cat.FindCatalogEntry("CL", "NOSUCH") == null);
        check("file " + path + " exists", new File(path).exists());

        System.out.println(failures == 0
            ? "TCAT SUCCESSFUL"
            : "TCAT FAILED, " + failures + " check(s)");
        System.exit(failures == 0 ? 0 : 15);
    }
}
