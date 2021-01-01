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
    if (entry.equals(null)) return null;
    return (CatalogEntry) entry;          // !! needs to be type-appropriate
  }

}
