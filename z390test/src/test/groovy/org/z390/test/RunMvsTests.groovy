package org.z390.test

import org.junit.jupiter.api.Test

class RunMvsTests extends z390Test {

    var options = ['notiming', 'stats', 'bal', 'noloadhigh']
    var sysmacStd = "SYSMAC(+${basePath('mac')})"
    var sysmacMvsAndMac = "SYSMAC(${basePath('mvs', 'maclib')}+${basePath('mac')})"

    @Test
    void test_DEMO() {
        int rc = this.asmlg(basePath('mvs', 'demo', 'DEMO'), *options ,sysmacStd)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_CVT1() {
        int rc = this.asm(basePath('mvs', 'test', 'TESTCVTX'), *options, sysmacStd)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath('mvs', 'test', 'TESTCVT1'), *options, sysmacMvsAndMac)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_MVS1() {
        int rc = this.asmlg(basePath('mvs', 'test', 'TESTMVS1'), *options, sysmacMvsAndMac)
        this.printOutput()
        assert rc == 0
    }
}
