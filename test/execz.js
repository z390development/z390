/* A Nashorn script to test zVSAM V2 on my USB thumb drive, label CRUZER3, on both Windows & Linux. *\
\* Also on my HP Stream 7, without USB, Win 8.1 only. This script runs a .390 load module.          */

//   Housekeeping: adopt path charactors specific to this OS:
var psep = java.io.File.pathSeparator;
var dsep = java.io.File.separator;
var base = "?Dev?zVSAM".replace(/\?/g, dsep);     // Where I keep pre-existing z390 classes.

{ // Housekeeping: enforce correct working dir:
let p = java.lang.System.getProperty("user.dir");
if  (p.substr(-12, 8) == "handover"       &&
     p.substr( -4, 1) == dsep             &&
     "23456789".indexOf(p.substr(-3, 1)) > -1     // Presumed not compatible with handover/1/
    ) {}
else throw new Error(__FILE__ +" requires current dir to be ... handover"+dsep+"N (N=1,2,3); found "+ p);
}
/*  ------------  End of housekeeping. Actual functionality starts here.  ------------  */

// Figure out paths to all class files:
var strCp = [   // classpath                                                    
  "test",       // Non-product class(es) that I have written                    relative to working dir
  "classes",    // Product classes that I have written or modified              relative to working dir
  java.nio.file.Paths.get( base, "..",         "z390b12.jar" ), // Beta 12      absolute
  java.nio.file.Paths.get( base, "..", "zB11", "z390.jar"    ), // Beta 11      absolute
  java.nio.file.Paths.get( base, "..", "z",    "z390.jar"    ), // 1.5.06       absolute
  ].join(psep);

// Build the whole comamnd line to run this .390 prog:
cmd = 'java -cp '+ strCp +' ez390 test'+ dsep + arguments.join(" ");
print('Issuing: "'+ cmd +'".');                   // and log it.
$EXEC( cmd );                                     // Run the .390 prog.
java.lang.System.out.print($OUT);                 // Show the captured STDOUT data.
java.lang.System.err.print($ERR);                 // Show the captured STDERR data.
var rc = $EXIT;
if (rc != 0) print( 'RC: '+ rc);
quit(rc);                                         // Return the exit code. (%errorlevel% in Windows; $? in Linux)
