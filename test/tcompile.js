// Compile from-to ./test
// Env: Nashorn; cwd: handover/2
// Args: list of 1 or more files or dirs, presumed in the 'test' directory.

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

// Figure out paths to all class files:
var strCp = [   // classpath                                                    
  "test",       // Non-product class(es) that I have written                    relative to working dir
  "classes",    // Product classes that I have written or modified              relative to working dir
  java.nio.file.Paths.get( base, "..",         "z390b12.jar" ), // Beta 12      absolute
  java.nio.file.Paths.get( base, "..", "zB11", "z390.jar"    ), // Beta 11      absolute
  java.nio.file.Paths.get( base, "..", "z",    "z390.jar"    ), // 1.5.06       absolute
  ].join(psep);

var cmd = "javac -encoding UTF-8 "             +
                "-source 8 -target 8 "         +
                "-classpath " + strCp +" "     +
                "-d test "                     +
                arguments.map(function(f){return " test"+dsep+f;}).join("");

print('Issuing: "'+ cmd +'".');                   // and log it.
$EXEC( cmd );                                     // Run the .390 prog.
java.lang.System.out.print($OUT);                 // Show the captured STDOUT data.
java.lang.System.err.print($ERR);                 // Show the captured STDERR data.
var rc = $EXIT;
if (rc != 0) print( 'RC: '+ rc);
quit(rc);                                         // Return the exit code. (%errorlevel% in Windows; $? in Linux)
