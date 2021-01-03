/** Path */
public class PathEntry implements CatalogEntry {
  public String entryType;
  public String entryName;
  PathEntry(String aName, String aEntry, boolean aUpd) {
    entryType = "PA";
    entryName = aName;
  }

  public String formKey() {
    return (entryType + entryName);
  }
}



