package org.z390.test

import org.junit.jupiter.api.Test

class RunAsmDemos extends z390Test {

    var options = [
        'trace',
        'nocon',
        'noloadhigh', 
        "SYSMAC(${basePath('mac')})",
        "SYSCPY(${basePath('mac')})",
        "SYSOBJ(${basePath('linklib')})"
    ]

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
    void test_DEMOBMK1() {
        int rc = this.asmlg(basePath("demo", "DEMOBMK1"), *options, "MAXSIZE(150)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMOBMK2() {
        int rc = this.asmlg(basePath("demo", "DEMOBMK2"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMODFP1() {
        int rc = this.asmlg(basePath("demo", "DEMODFP1"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMONUM1() {
        int rc = this.asm(basePath("demo", "DEMONUM1"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMONUM2() {
        int rc = this.asmlg(basePath("demo", "DEMONUM2"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMOSTR1() {
        int rc = this.asmlg(basePath("demo", "DEMOSTR1"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_DEMOWTO2() {
        int rc = this.asmlg(basePath("demo", "DEMOWTO2"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_SIEVE() {
        int rc = this.asmlg(basePath("demo", "SIEVE"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_STDDEVLB() {
        int rc = this.asmlg(basePath("demo", "STDDEVLB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTDCB1() {
        // Set input files
        env.put('SYSUT1', basePath('demo', 'TESTDCB1.TF1'))
        env.put('SYSUT2', basePath('demo', 'TESTDCB1.TF2'))
        env.put('SYSOUT', basePath('demo', 'TESTDCB1.TF3'))
        // run the job
        int rc = this.asmlg(basePath("demo", "TESTDCB1"), *options)
        // Load files to fileData
        loadFile(basePath('demo', 'TESTDCB1.TF1'), 'TF1')
        loadFile(basePath('demo', 'TESTDCB1.TF2'), 'TF2')
        loadFile(basePath('demo', 'TESTDCB1.TF3'), 'SYSOUT')
        // print the fileData
        this.printOutput()
        // Check files equal
        assert fileData.get('TF1') == fileData.get('TF2')
        assert rc == 0
    }
    @Test
    void test_TESTDCB2() {
        // Set input files
        env.put('SYSUT1', basePath('demo', 'TESTDCB2.TF1'))
        env.put('SYSUT2', basePath('demo', 'TESTDCB2.TF2'))
        env.put('SYSOUT', basePath('demo', 'TESTDCB2.TF3'))
        // run the job
        int rc = this.asmlg(basePath("demo", "TESTDCB2"), *options)
        // Load files to fileData
        loadFile(basePath('demo', 'TESTDCB2.TF1'), 'TF1')
        loadFile(basePath('demo', 'TESTDCB2.TF2'), 'TF2')
        loadFile(basePath('demo', 'TESTDCB2.TF3'), 'SYSOUT')
        // print the fileData
        this.printOutput()
        // Check files equal
        assert fileData.get('TF1') == fileData.get('TF2')
        assert rc == 0
    }
}
