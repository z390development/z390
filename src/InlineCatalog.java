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
  // !! I added 'public' access to some entities for Nashorn use; review before deployment. HS

import java.util.Map;
import java.util.HashMap;

public class InlineCatalog extends Catalog {

  public final CatalogImplementationType implementedAs = CatalogImplementationType.INLINE;

  public final String Catname = "DEMOCAT";

  public final Map<String, CatalogEntry> CatalogEntries = new HashMap<String, CatalogEntry>();

  /**
   * We can't currently have more than one InlineCatalog defined, so searching for it is easy.
   */
  public Catalog FindCatalog(String aCat) {
    if (aCat.equals(Catname)) return this;
    return null;
  }

  /**
   * Find a named entry of a particular type.
   */
  public CatalogEntry FindCatalogEntry(String entryType, String entryName) {
    Object entry = CatalogEntries.get(entryType + entryName);  
    if (entry == null) return null;
    return (CatalogEntry) entry;          // !! needs to be type-appropriate
  }

}
