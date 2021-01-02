print(__FILE__ +", line "+ __LINE__ +": Starting.");
// Environment: Nashorn shell.  cd handover/N

/**                                                                                                       **\

  Pre-diff processing: given a set of source directories, produce a table of the union of their
  contents, showing for each file its last-modified date in each directory in which it occurs.

  Then use it to drive the diff program to compare the latest version of each file with its next oldest.

  @author:  Hugh Sweeney
  @version: (z390) 1.5.06.b13

\**                                                                                                       **/


//   Housekeeping: adopt path charactors specific to this OS:
var psep = java.io.File.pathSeparator;
var dsep = java.io.File.separator;
var base = "?Dev?zVSAM".replace(/\?/g, dsep);

{ // Housekeeping: enforce correct working dir
let p = java.lang.System.getProperty("user.dir");
if  (p.substr(-10, 8) == "handover"       &&
     p.substr( -2, 1) == dsep             &&
     "23456789".indexOf(p.substr(-1, 1)) > -1     // Presume no compatibility with handover/1/.
    ) {}
else throw new Error(__FILE__ +" requires current dir to be ... handover"+dsep+"N (N=1,2,3); found "+ p);
}

// Housekeeping: short JS names for Java types:
var juTM  = Java.type("java.util.TreeMap");
var jiF   = Java.type("java.io.File");
var jiFnF = Java.type("java.io.FilenameFilter");
var jlS   = Java.type("java.lang.String");
var jlSy  = Java.type("java.lang.System");
var juC   = Java.type("java.util.Comparator");
var juFmt = Java.type("java.util.Formatter");
var JiFW  = Java.type("java.io.FileWriter");
/*  ------------  End of housekeeping. Actual functionality starts here.  ------------  */

// All source file names & timestamps are recorded here:
var allFiles = new juTM(
  new juC (    // Comparator, required by iterator.
    { compare: function( k1, k2 ) 
                { 
                  if (k1 < k2) return -1;
                  if (k1 > k2) return +1;
                               return  0;
                },
      equals:  function( k )      
                { return (compare(this, k) == 0); } 
    }
  )
);

// Test-data, relative to handover/N
var args = [  ".java",                                                           // Extension of interest.
              "./src",                                                           // Directory of interest.
              java.nio.file.Paths.get(base, "../zB12/test/java").toString(),     // Older directories ...
              java.nio.file.Paths.get(base, "../zB11/src").toString(),           // ... in reverse date ...
              java.nio.file.Paths.get(base, "../z/src").toString()               // ... order.
           ];
for (let i=1; i<args.length; i++) args[i] = new jiF(args[i]).getCanonicalPath(); 
var filter        = args[0];                        // file extension
var dirOfInterest = args[1];
var dirDiff = new jiF(dirOfInterest).getParent() + dsep +"diff";      // where the diffs go

for (var i=1; i<args.length; i++) { addSourceFileNamesToList(i); }    // from all dirs

// Create the lastModified table
print("Generating lastModified table of all "+ allFiles.size() +" source files.");
var pD = new JiFW(dirDiff + dsep +"preDiff.txt");
var fmtr = new juFmt();
var longType = Java.type("java.lang.Long"); var longVar = new longType( (new Date()).getTime() );
fmtr.format("All source file last-modified timestamps in all directories.\n");
fmtr.format("This analysis was run on %1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS.\n", longVar );
fmtr.format("\n\u2193File                       Dir\u2192 %1$21s %2$21s %3$21s %4$21s\n", abbr(args[1]), abbr(args[2]), abbr(args[3]), abbr(args[4]));
fmtr.format("==============================   --------------------- --------------------- --------------------- ---------------------\n");
for (var srcSorted = allFiles.keySet().iterator(); srcSorted.hasNext(); )  { 
  var name1 =    srcSorted.next();
  var arr   = allFiles.get(name1);    // Array of timestamps
  fmtr.format( "%1$-32s %2$21s %3$21s %4$21s %5$21s\n", name1, d(arr[0]), d(arr[1]), d(arr[2]), d(arr[3]) )
  }
fWrt( fmtr.out().toString() );
pD.close();

// Create diffs (for directory of interest only)
print("Running diffs for all source files in "+ args[1] );
for (var srcSorted = allFiles.keySet().iterator(); srcSorted.hasNext(); )  {
  // Work our way down the directory of interest, looking for the same file in an older directory.
  var key     =  srcSorted.next();
  var tstamps = allFiles.get(key);
  if (tstamps[0] == -1) continue;     // This file not in the interesting directory
  for (var k = 1; k<tstamps.length; k++) {
    if (tstamps[k] == -1) continue;   // This file not in directory args[k+1]
    print( key );
    $EXEC("diff -abBE "+ args[k+1]+ "/"+ key +" "+ args[1]);
    var fw = new JiFW(dirDiff + dsep + key +".txt");
    fw.write("# "+ args[k+1]  +" : "+ args[1] +"\n\n");
    fw.write($OUT, 0, $OUT.length);
    fw.close();
    break;
    }
  }
print(__FILE__ +", line "+ __LINE__ +": All done.");
exit(0);


function addSourceFileNamesToList( argNum ) {
  print("Reading "+ args[argNum] );
  var f = new jiF(args[argNum]);
  if (! f.isDirectory()) throw args[argNum] +" is not a directory.";

  // Create a file list of this directory, filtered on file extension:
  var fileList = f.listFiles(new jiFnF( 
        { accept: function(f, name) 
                  { // match the file extension
                    var x = name.lastIndexOf(".");
                    if (x < 0) return false;
                    if (name.substring(x) !== filter) return false;
                    return true; 
                  } 
        }
        ));

  // Update the allFiles list from the filelist's names and timestamps
  for (var i=0; i<fileList.length; i++) {
    var n = fileList[i].getName();        // sourcefile name      (key)
    var t = fileList[i].lastModified();   // sourcefile timestamp (value)
    var arrTstamp = new Array(args.length-1); for (var j=0; j<arrTstamp.length; j++) arrTstamp[j]=-1;
    arrTstamp[ argNum-1 ] = t;            // print(__LINE__, n, arrTstamp);
    if (allFiles.containsKey(n)) {        // handle multiple-occurrence case ...
      var o = allFiles.get(n);            // get the value
      o[ argNum-1 ] = t;                  // update with the file's timestamp from this dir
    }
    else {                                // handle first-occurrence case ..
      allFiles.put( n, arrTstamp );
    }
  }
}


function d(msec) {    // Format a (possible) date.
  return (msec < 0)? "-" : new juFmt().format("%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS", msec).toString();
}

function fWrt( text ) {     // Write to the diff FileWriter.
  pD.write(text, 0, text.length );
}

function abbr(path) { // abbreviate a path name 
if (path.length <= 21) return path;
return ( path.substr(0,3) + "..." + path.substr(-15, 15) );
}
