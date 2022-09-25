package org.z390.test

import org.junit.jupiter.api.Test


class RunAssist extends z390Test {

    var options = ['ASSIST', 'TRACEALL', "SYSMAC(${basePath("mac")})", "SYSOBJ(${basePath("linklib")})"]

    @Test
    void test_DEMOAST1() {
        this.env.put('XPRNT', basePath('assist', 'demo', 'DEMOAST1.XPR'))
        int rc = this.asmlg(basePath("assist", "demo", "DEMOAST1"), *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTINS1() {
        this.env = [
            'XREAD': basePath('assist', 'test', 'TESTAST1.XRD'),
            'XPRNT': basePath('assist', 'test', 'TESTAST1.XPR'),
            'XPNCH': basePath('assist', 'test', 'TESTAST1.XPH'),
            'XGET': basePath('assist', 'test', 'TESTAST1.XGT'),
            'XPUT': basePath('assist', 'test', 'TESTAST1.XPT')
        ]
        int rc = this.asmlg(basePath("assist", "test", "TESTAST1"), *options)
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

}
