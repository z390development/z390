/** Cluster */
public class ClusterEntry implements CatalogEntry {
  public String entryType;
  public String entryName;

  public ClusterEntry(String aName, String aIndx, int aRecsz) {
    entryType = "CL";
    entryName = aName;
  }

  public ClusterEntry(String aName, String aIndx, int aRecsz, int[] aKy, boolean aReuse, String aVesdsn, String aVx0dsn, int aCISZ) {
    this(aName, aIndx, aRecsz);
    // further initialisations ...
  }

  public String formKey() {
    return (entryType + entryName);
  }
}


