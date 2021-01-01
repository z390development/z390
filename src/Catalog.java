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




