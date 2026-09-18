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
//import java.util.Map;
//import java.util.HashMap;


public class Democat {
  public static InlineCatalog objCat = new InlineCatalog();     
  public static CatalogEntry x;
  public static int[] k = {10, 5};

  public static void main(String[] args) {
    x = new CatEntry_Cluster("ESF1", "N", 80, k, false, "", "", 0);
    objCat.CatalogEntries.put(x.formKey(), x);

    x = new CatEntry_Cluster("ESF2", "N", 80, k, false, "", "", 0);
    objCat.CatalogEntries.put(x.formKey(), x);

  }
}
