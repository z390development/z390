package org.z390.test

import org.junit.jupiter.api.Test

class RunCmdProc extends z390Test {
    /*
    Because the source contains hard coded file paths, we need to replace these with a path using basePath method.
    We load the source into a variable and the update the file paths using a series of regex replacements
    We then load the source into a temporary file and run it as per inline source process
     */

    var options = ["SYSMAC(${basePath("mac")})"]
    var os = System.getProperty("os.name")

    RunCmdProc() {
        // This is a hack as the Java process does not set COMSPEC in Windows
        if (os.startsWith('Windows')) {
            env.put('COMSPEC', 'Windows')
        }
    }

    @Test
    void test_TESTCMD3() {

        // regex replacement of batch file locations - use basePath
        String WinBatOld = "C'rt\\\\test\\\\bat\\\\TESTCMD3\\.BAT',X'00'   Windows command"
        println(WinBatOld)
        var WinBatNew = "C'${basePathRelative('rt','test','bat','TESTCMD3.BAT')}',X'00'\n".replaceAll("[\\W|_]", /\\$0/)
        println(WinBatNew)

        String BashOld = "C'rt/test/bash/testcmd3',X'00'.*\n"
        var BashNew = "C'${basePathRelative('rt','test','bash','testcmd3')}',X'00'\n"

        // load the original source file
        loadFile(basePath("rt", "test", "TESTCMD3.MLC"), 'source')
        String source = fileData['source'].toString()

        // update the bat/bash file locations in source
        source = source.replaceAll(WinBatOld, WinBatNew)
        source = source.replaceAll(BashOld, BashNew)

        // Create temp source file containing updated source and run
        String sourceFilename = createTempFile('TESTCMD3.MLC', source)
        int rc = this.asmlg(sourceFilename, *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTCMD4() {
        // load the source file
        loadFile(basePath("rt", "test", "TESTCMD4.MLC"), 'source')
        String source = fileData['source'].toString()

        // update the bat/bash file locations in source
        for (let in [[1, 'A'],[2, 'B'] ,[3, 'C']]) {
            source = source.replaceAll(
                    "TstCmdW${let[0]} DC    C'\\\"rt\\\\test\\\\bat\\\\TESTCMD4${let[1]}.BAT\\\"'",
                    "TstCmdW${let[0]} DC    C'\"${basePathRelative('rt', 'test', 'bat', "TESTCMD4${let[1]}").replaceAll("[\\W|_]", /\\$0/)}\"'"
            )
        }

        for (let in [[1, 'a'],[2, 'b'] ,[3, 'c']]) {
            source = source.replaceAll(
                    "TstCmdL${let[0]} DC    C'\"rt/test/bash/testcmd4${let[1]}\"'.*\n",
                    "TstCmdL${let[0]} DC    C'\"${basePathRelative('rt', 'test', 'bash', "testcmd4${let[1]}")}\"'\n"
            )
        }

        // Create temp source file containing updated source and run
        String sourceFilename = createTempFile('TESTCMD4.MLC', source)

        // Run with PARM(1)
        int rc = this.asmlg(sourceFilename, *options, 'PARM(1)')
        this.printOutput()
        assert rc == 0

        // Run with PARM(2)
        rc = this.asmlg(sourceFilename, *options, 'PARM(2)')
        this.printOutput()
        assert rc == 0

        // Run with PARM(3)
        rc = this.asmlg(sourceFilename, *options, 'PARM(3)')
        this.printOutput()
        assert rc == 0

    }
}
