/* A Nashorn script to test zVSAM V2 on my USB thumb drive, label CRUZER3, on both Windows & Linux. */

var psep, dsep, base;
// Windows or Linux? Basic z390 class libraries are at different locations;
// also need different path separator in -classpath option.
var os = java.lang.System.getProperty("os.name");	
switch (os.toUpperCase().slice(0,3)) {
  case "WIN" : psep = ";"; dsep = "\\"; base = "\\Dev\\zVSAM"; break;
  case "LIN" : psep = ":"; dsep = "/"; base = "/media/CRUZER3/Dev/zVSAM"; break;
  default    : throw ("Unknown OS: "+ os);
  }

{ // Enforce correct current dir
let p = java.lang.System.getProperty("user.dir");
if  (p.substr(-10, 8) == "handover"       &&
     p.substr( -2, 1) == dsep             &&
     "123".indexOf(p.substr(-1, 1)) > -1
    ) {}
else throw new Error(__FILE__ +" requires current dir to be ... handover"+dsep+"N (N=1,2,3); found "+ p);
}

// Figure out paths to all class files:
myJar = new java.io.File(base + dsep + '..'+ dsep +'z390b12.jar').getCanonicalPath();          /* Prior beta   */
prodn = new java.io.File(base + dsep + '..'+ dsep +'z'+ dsep +'z390.jar').getCanonicalPath();  /* 1.5.06       */

// Build the whole comamnd line to run this .390 prog:
cmd = 'java -cp test'+ psep +'classes'+ psep + myJar + psep + prodn +' ez390 test/'+ arguments.join(" ");
print('Issuing: "'+ cmd +'".');                   // and log it.
$EXEC( cmd );                                     // Run the .390 prog.
java.lang.System.out.print($OUT);                 // Show the captured STDOUT data.
java.lang.System.err.print($ERR);                 // Show the captured STDERR data.
var rc = $EXIT;
if (rc != 0) print( 'RC: '+ rc);
quit(rc);                                         // Return the exit code. (%errorlevel% in Windows; $? in Linux)
