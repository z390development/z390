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
/** Cluster */
public class CatEntry_Cluster extends CatalogEntry {

  public CatEntry_Cluster(String aName, String aIndx, int aRecsz, int[] aKy, boolean aReuse, String aVesdsn, String aVx0dsn, int aCISZ) {
    entryName = aName;
    entryType = "CL";
    // further initialisations ...
  }
  // further constructors ...

}
