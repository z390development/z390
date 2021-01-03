/** AlternateIndex */
public class AixEntry implements CatalogEntry {
  public String entryType;
  public String entryName;
  AixEntry(String aName, String aRelat, int[] aKy, boolean aUniqk, boolean aUpgr, String aVxndsn) {
    entryType = "AX";
    entryName = aName;
  }

  public String formKey() {
    return (entryType + entryName);
  }
}


