Contents of the _handover/2/test_ Directory
===========================================

This directory contains files that are not part of the product, nor even part of the developer's deliverables.
They include test data and test drivers (procedures/scripts). There may be half-baked READMEs, like this one.
But in general they exist to allow me or possibly A.N. Other to test my code.

As of today, they are:
acb2.*      a user assembly prog that opens a zzVSAM v2 ACB to verify that vz390 recognises it and proceeds to
            opening the corresponding file.
DEMOCAT.*   a replica written in Java of (some of) the contents of the DEMOCAT in 1.5.06. It allows vz390.v2 to
            call upon catalog services without knowing anything about v1 catalog loadmod layout.
ESF1.dta    a little scrap of a file matching the ESF1 entry in DEMOCAT so that vz390 will find a file to
            open as an ESDS for reading.
*.js        scripts to do the above tests. Specifically:
execz.js    functionally similar to EXEC.BAT in z390 as shipped. It executes the specified .390 program.
tcat.js     a test of catalog processing.
preDiff.js  a script to work out the most recent version of each source file (in Beta 12, Beta 11, 1.5.06) that
            I have updated, and create a diff file for each, in handover/N/diff. (N=1, 2 or 3)


Test results
------------
1. execz.js
Commands:
  cd handover/2
  export ESF1=test/DEMOCAT.ESF1                                                       # Linux
  jjs -cp test:classes -scripting --language=es6 test/execz.js -- acb2 zVSAM\(2\)     # Linux
  set ESF1=test\DEMOCAT.ESF1                                                          # Windows
  jjs -cp test:classes -scripting --language=es6 test\execz.js -- acb2 zVSAM(2)       # Windows
This results in vz390 passing control to the_VSAM_Handler, which passes control to vz390.v2's handle_open_req method,
validating the correct version of ACB, creating a zACB, adding it to the ACB Hashmap, extracting the DDNAME, and 
retrieving and logging the value from the environment.

2. tcat.js
Commands:
  cd handover/2
  export ESF1=test/DEMOCAT.ESF1
  jjs -cp test:classes -scripting --language=es6 test/tcat.js
This exercises the catalog access classes DEMOCAT, Catalog, InlineCatalog, CatalogEntry, CatalogImplementationType
and CatEntry_Cluster. It locates the catalog, adds some extra cluster entries to it in order to stress the subsequent
search method a little, and retrieves all the cluster entries from the catalog and lists them on the console. Then,
assuming a given ddname ESF1, it retrieves the environment variable value and parses it to extract the catalog and
cluster names. Then it finds the catalog and the cluster entry within it, builds the path to the fully qualified
filename, creates a File object, and verifies the existence of the file on disk.
(That is all that Open consists of, external to Java.)

<pre>
On Mon, 23 Mar 2020 at 11:39, Hugh Sweeney <hsweeney@pobox.com> wrote:
|  On Sun, 22 Mar 2020 at 17:09, MELVYN MALTZ <zarf99999@blueyonder.co.uk> wrote:
|  |
|  |  Did you get as far as opening a zVSAM V2 dataset ?
|  |
|  I can answer that one from memory. Yes. In so far as a flat Windows file can be treated as an ESDS. 
|  I think I used the ESF1 from V1 demo.
</pre>

I believed that was true at that time. It's more accurate to say that it's 50% true in two different ways: the test with
acb2 drives the code up to entering the VSAM open() routine, extracting the ddname and fetching the corresponding env
value; and tcat.js establishes that, given a ddname, my Java code successfully navigates the Catalog, derives the DSN
and determines that that file exists on disk. There's a slight overlap between these two tests, so perhaps 98% true.
The Java catalog-manipulation code used in tcat.js is not yet incorporated into vz390, but is glued together with JS.





## Notes

I don't place any copy of Beta 12, Beta 11, or 1.5.06 code in the handover/N/ hierarchy. Those files are not my
work and I am not distributing my copies of them. If A.N. Other wishes to run my scripts they will need to edit
them to point to their own copies of those distributions.


## Other tools
preDiff.js    Not part of functionality testing. For each source file in handover/N/src, it figures out the most
              recent equivalent in older releases and creates a diff (delta) file that shows all source changes
              required to transform the latter into the former. Thus every single line changed, added, or deleted
              can be identified. This tool should be run only on Linux, as _diff_ is a Linux command.
