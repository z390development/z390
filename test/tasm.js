print("Dir: "+ __DIR__, __FILE__, __LINE__, " starting");
// A Nashorn script to assemble and link from-to handover/N/test. Based on ASML.BAT.

//   Housekeeping: adopt path characters specific to this OS:
var psep = java.io.File.pathSeparator;
var dsep = java.io.File.separator;
var base = "?Dev?zVSAM".replace(/\?/g, dsep);     // Where I keep pre-existing z390 classes.

{ // Housekeeping: enforce correct working dir:
let p = java.lang.System.getProperty("user.dir");
if  (p.substr(-10, 8) == "handover"       &&
     p.substr( -2, 1) == dsep             &&
     "23456789".indexOf(p.substr(-1, 1)) > -1     // Presumed not compatible with handover/1/
    ) {}
else throw new Error(__FILE__ +" requires current dir to be ... handover"+dsep+"N (N=2,3); found "+ p);
}
/*  ------------  End of housekeeping. Actual functionality starts here.  ------------  */

var strCp = [   // classpath                                                    
  new (Java.type("java.io.File"))(java.nio.file.Paths.get( base, "..",         "z390b12.jar" )).getCanonicalPath(), // Beta 12      
  new (Java.type("java.io.File"))(java.nio.file.Paths.get( base, "..", "zB11", "z390.jar"    )).getCanonicalPath(), // Beta 11
  new (Java.type("java.io.File"))(java.nio.file.Paths.get( base, "..", "z",    "z390.jar"    )).getCanonicalPath()  // 1.5.06       
  ].join(psep);

// Macro libraries:
var rel = new (Java.type("java.io.File"))(java.nio.file.Paths.get( base, "..", "z", "mac"    )).getCanonicalPath();   
var b13 = new (Java.type("java.io.File"))(java.nio.file.Paths.get( base,       "MACVSAM2_MM" )).getCanonicalPath();   

let cmd = "java -classpath "+ strCp             +   
                " -Xrs -Xms150000K -Xmx150000K" +                               // As set in mz390.bat
                " mz390 "+ arguments[0]         +                               // Only 1 argument
                " sysmac("+ b13 +"+.)"          + 
                " sysmac(+"+ rel +")"           +
                " syscpy("+ rel +"+.)";
                              
print('Issuing: "'+ cmd +'"');
$ENV.PWD = "test";                  // Change to directory for source and object.
$EXEC( cmd );
print("$OUT\n"+ $OUT +"$ERR"+ $ERR);

quit($EXIT);
