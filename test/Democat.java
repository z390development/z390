import java.util.Map;
import java.util.HashMap;


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
