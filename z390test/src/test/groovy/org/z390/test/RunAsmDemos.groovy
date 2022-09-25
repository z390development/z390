package org.z390.test

import org.junit.jupiter.api.Test

class RunAsmDemos extends z390Test {

    var options = ['trace', 'noloadhigh',
                   "SYSMAC(${basePath("mac")})",
                   "SYSOBJ(${basePath("linklib")})"]

    @Test
    void test_HELLO() {
        int rc = this.asmlg(basePath("demo", "HELLO"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMOM8Q1() {
        int rc = this.asm(basePath("demo", "DEMOM8Q1"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMODCB1() {
        // Set input files
        env.put('SYSUT1', basePath('demo', 'TESTDCB1.TF1'))
        env.put('SYSUT2', basePath('demo', 'TESTDCB1.TF2'))
        env.put('SYSUT3', basePath('demo', 'TESTDCB1.TF3'))
        // run the job
        int rc = this.asm(basePath("demo", "TESTDCB1"), *options)
        // Load files to fileData
        loadFile(basePath('demo', 'TESTDCB1.TF1'), 'TF1')
        loadFile(basePath('demo', 'TESTDCB1.TF2'), 'TF2')
        loadFile(basePath('demo', 'TESTDCB1.TF3'), 'TF3')
        // print the fileData
        this.printOutput()
        // Check files equal
        assert fileData.get('TF1') == fileData.get('TF2')
        assert rc == 0
    }

}
