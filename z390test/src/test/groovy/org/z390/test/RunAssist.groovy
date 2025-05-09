package org.z390.test

import org.junit.jupiter.api.Test


class RunAssist extends z390Test {

    var options = ['ASSIST', "SYSMAC(${basePath("mac")})"]

    @Test
    void test_DEMOAST1() {
        this.env.put('XPRNT', basePath('assist', 'demo', 'DEMOAST1.XPR'))
        int rc = this.asmlg(basePath("assist", "demo", "DEMOAST1"), 'TRACEALL', *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTAST1() {
        this.env = [
            'XREAD': basePath('assist', 'test', 'TESTAST1.XRD'),
            'XPRNT': basePath('assist', 'test', 'TESTAST1.XPR'),
            'XPNCH': basePath('assist', 'test', 'TESTAST1.XPH'),
            'XGET': basePath('assist', 'test', 'TESTAST1.XGT'),
            'XPUT': basePath('assist', 'test', 'TESTAST1.XPT')
        ]
        int rc = this.asmlg(basePath("assist", "test", "TESTAST1"), 'TRACEALL', *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TSTXDECI() {
        this.env.put('XPRNT', basePath('assist', 'test', 'TSTXDECI.XPR'))
        int rc = this.asmlg(basePath("assist", "test", "TSTXDECI"), *options, 'PARM(v)')
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TSTXHEXI() {
        this.env.put('XPRNT', basePath('assist', 'test', 'TSTXHEXI.XPR'))
        int rc = this.asmlg(basePath("assist", "test", "TSTXHEXI"), *options, 'PARM(v)')
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_SOLP06() {
        this.env.put('XPRNT', basePath('assist', 'test', 'SOLP06.XPR'))
        this.env.put('XREAD', basePath('assist', 'test', 'SOLP06.XRD'))
        int rc = this.asmlg(basePath("assist", "test", "SOLP06"), *options, 'PARM(v)')
        this.printOutput()
        assert rc == 0
        // Load files to fileData and compare
        loadFile(basePath('demo', 'SOLP06.TF1'), 'TF1')
        loadFile(basePath('demo', 'SOLP06.XPR'), 'XPR')
        assert fileData.get('TF1') == fileData.get('XPR')
        assert rc == 0
    }

}
