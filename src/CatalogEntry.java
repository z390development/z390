/**
 * Abstract class for the commonality among all types of catalog entry.
 */
public abstract class CatalogEntry {
  public String entryType;
  public String entryName;

  public String formKey() {
    return (entryType + entryName);
  }
}
