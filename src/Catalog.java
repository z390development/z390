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
import java.util.Map;
import java.util.HashMap;

/**
 * VSAM Catalog
 * This abstract class defines all Catalog services.
 */
public abstract class Catalog {
   
  private final CatalogImplementationType implementedAs = null;

  private final Map<String, CatalogEntry> CatalogEntries = null;

  abstract Catalog FindCatalog(String catName);

  public CatalogEntry FindCatalogEntry(String entryType, String entryName) {
    return null;
  }

}




