# Java V8 compatibility issues

The following issues arise when compiling z390 java programs with OpenJDL 21.0.6"

```
javac -Xlint:unchecked -Xlint:deprecation ..\src\*.java
..\src\az390.java:1425: warning: [unchecked] unchecked cast
    sym_xref = (TreeSet<Integer>[])Array.newInstance(TreeSet.class,tz390.opt_maxsym);
                                                    ^
  required: TreeSet<Integer>[]
  found:    Object
..\src\az390.java:1439: warning: [unchecked] unchecked cast
    lit_xref = (TreeSet<Integer>[])Array.newInstance(TreeSet.class,tz390.opt_maxsym);
                                                    ^
  required: TreeSet<Integer>[]
  found:    Object
..\src\tz390.java:6460: warning: [deprecation] exec(String) in Runtime has been deprecated
                   Runtime.getRuntime().exec(cmd);
                                       ^
..\src\sz390.java:397: warning: [unchecked] unchecked cast
    /** variable      */ LinkedList<String>[] cmd_output_queue  = (LinkedList<String>[])Array.newInstance(LinkedList.class,max_cmd_proc);
                                                                                                         ^
  required: LinkedList<String>[]
  found:    Object
..\src\gz390.java:610: warning: [removal] SecurityManager in java.lang has been deprecated and marked for removal
            SecurityManager sm = System.getSecurityManager();
            ^
..\src\gz390.java:610: warning: [removal] getSecurityManager() in System has been deprecated and marked for removal
            SecurityManager sm = System.getSecurityManager();
                                       ^
..\src\gz390.java:1589: warning: [deprecation] exec(String) in Runtime has been deprecated
                           Runtime.getRuntime().exec(cmd);
                                               ^
..\src\z390.java:462: warning: [removal] SecurityManager in java.lang has been deprecated and marked for removal
            SecurityManager sm = System.getSecurityManager();
            ^
..\src\z390.java:462: warning: [removal] getSecurityManager() in System has been deprecated and marked for removal
            SecurityManager sm = System.getSecurityManager();
                                       ^
```
