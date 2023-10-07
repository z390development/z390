package org.z390.test

import org.junit.jupiter.api.Test

class RunCmdProc extends z390Test {

    /*
     * This test set tests the use of "command processors" (CP) that process
     * "commands" -- bat files for Windows, bash scripts for Linux/MacOS.
     * Because of this distinction, we must use separate input files that
     * contain commands based on what operating system is active when running
     * the tests.
     */
     
    var options = ["SYSMAC(${basePath("mac")})"]
    var os = System.getProperty("os.name")

    @Test
    void test_TESTCMD3() {

        // Test one command processor
        if (os.startsWith('Windows')) {
            this.env.put('TESTCMDFILE', basePath('rt', 'test', 'bat', 'TESTCMD3.BAT'))
        } 
        else {
            this.env.put('TESTCMDFILE', basePath('rt', 'test', 'bash', 'testcmd3'))
        }
        int rc = this.asmlg(basePath("rt", "test", "TESTCMD3"), *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTCMD4() {

        /* Test two command processors (3 tests)
         * 1. Both have same number of lines of output
         * 2. Second CP has more lines of output
         * 3. First CP has more lines of output
         */

        // test 1
        if (os.startsWith('Windows')) {
            this.env.put('TESTCMDFILE1', basePath('rt', 'test', 'bat', 'TESTCMD4A.BAT'))
            this.env.put('TESTCMDFILE2', basePath('rt', 'test', 'bat', 'TESTCMD4B.BAT'))
        } 
        else {
            this.env.put('TESTCMDFILE1', basePath('rt', 'test', 'bash', 'testcmd4a'))
            this.env.put('TESTCMDFILE2', basePath('rt', 'test', 'bash', 'testcmd4b'))
        }
        int rc = this.asmlg(basePath("rt", "test", "TESTCMD4"), *options)
        this.printOutput()
        assert rc == 0

        // test 2
        if (os.startsWith('Windows')) {
            this.env.put('TESTCMDFILE1', basePath('rt', 'test', 'bat', 'TESTCMD4A.BAT'))
            this.env.put('TESTCMDFILE2', basePath('rt', 'test', 'bat', 'TESTCMD4C.BAT'))
        } 
        else {
            this.env.put('TESTCMDFILE1', basePath('rt', 'test', 'bash', 'testcmd4a'))
            this.env.put('TESTCMDFILE2', basePath('rt', 'test', 'bash', 'testcmd4c'))
        }
        rc = this.asmlg(basePath("rt", "test", "TESTCMD4"), *options)
        this.printOutput()
        assert rc == 0

        // test 3
        if (os.startsWith('Windows')) {
            this.env.put('TESTCMDFILE1', basePath('rt', 'test', 'bat', 'TESTCMD4C.BAT'))
            this.env.put('TESTCMDFILE2', basePath('rt', 'test', 'bat', 'TESTCMD4B.BAT'))
        } 
        else {
            this.env.put('TESTCMDFILE1', basePath('rt', 'test', 'bash', 'testcmd4c'))
            this.env.put('TESTCMDFILE2', basePath('rt', 'test', 'bash', 'testcmd4b'))
        }
        rc = this.asmlg(basePath("rt", "test", "TESTCMD4"), *options)
        this.printOutput()
        assert rc == 0
    }
}
