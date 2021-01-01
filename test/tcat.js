var jlS = Java.type("java.lang.System");
var jioF = Java.type("java.io.File");

// Access the Democat class:
var dcType = Java.type("Democat");  print("assigned dcType "+ dcType);
dcType.main([""]);                  print("ran dcType.main");   // creates some entries

// Show some members of the Democat:
print("["+ dcType.objCat +"]");
print("["+ dcType.objCat.Catname +"]");
print("["+ dcType.objCat.implementedAs +"]");

// Local names for the collection of entries and the Cluster type:
var entries = dcType.objCat.CatalogEntries;
var ceType = Java.type("CatEntry_Cluster");

// Add some more cluster entries to the catalog:
for (var i=0; i<=9; i++) {
  var x = new ceType("JJS"+i , "N", 80, [5, 10], false, "", "", 0);
  entries.put(x.formKey(), x);
}
// Catalog is now ready for use.


// Enumerate all the entries in the catalog:
for each (var k in entries.keySet()) print(k +": "+ entries.get(k));



/* ---------------------------------------------------------- *\
   Simulate opening a file.
\* ---------------------------------------------------------- */

// Logic here is as if vz390.V2.vsam_op_open() were invoked.
// In z390 Java, we would have a(userprog's ACB). We want its ddname.
var ddname = "ESF1"
print("ddname: ["+ ddname +"]");

var pathQualFile = jlS.getenv(ddname);
print("path-qualified_filename: ["+ pathQualFile +"]");

var xdot = pathQualFile.lastIndexOf(".");
var entryName = pathQualFile.slice(xdot+1);
print("entryName: ["+ entryName +"]");

var xsl = pathQualFile.lastIndexOf("/");
var catName = pathQualFile.slice(xsl+1, xdot)
print("catName: ["+ catName +"]");

var theCat = dcType.objCat.FindCatalog(catName);
if (theCat.equals(null)) throw ("No such catalog: "+ catName);
print("Found catalog "+ catName +": "+ theCat);



var path =  pathQualFile.slice(0, xsl+1)  /* "/media/CRUZER2/Dev/zVSAM/" */ /* "\\users\\sean\\Desktop\\" */ + entryName + ".dta";
print("path: ["+ path +"]");


var catEntry = dcType.objCat.FindCatalogEntry("CL", entryName);
if (catEntry == null) throw ("!! No catalog entry found for "+ entryName);
print("Found catalog entry: "+ entryName);


var fileEnt = new jioF(path);
if (! fileEnt.exists()) throw ("File '"+ path +"' not found, so it wasn't.");
print("Found file '"+ path +"', so I did!");
