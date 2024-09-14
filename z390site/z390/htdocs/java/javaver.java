 import java.applet.*;
 import java.awt.*;
 public class javaver extends Applet
 { private Label VersionVendor; 
   public javaver()
   { VersionVendor = new Label (" Java Version: " +
                                    System.getProperty("java.version")+
                           " from "+System.getProperty("java.vendor"));
     this.add(VersionVendor);
   }
 }