package org.z390.test

import org.junit.jupiter.api.Test

class RunAsmTests extends z390Test {

    var options = ['trace', 'noloadhigh', "SYSMAC(${basePath("mac")})", "SYSOBJ(${basePath("linklib")})"]

    @Test
    void test_TESTINS1() {
        int rc = this.asm(basePath("tests", "TESTINS1"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTINS2() {
        int rc = this.asmlg(basePath("tests", "TESTINS2"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTINS3() {
        int rc = this.asmlg(basePath("tests", "TESTINS3"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTINS4() {
        int rc = this.asmlg(basePath("tests", "TESTINS4"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTINS5() {
        int rc = this.asmlg(basePath("tests", "TESTINS5"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTDFP1() {
        int rc = this.asmlg(basePath("tests", "TESTDFP1"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTDFP2() {
        int rc = this.asmlg(basePath("tests", "TESTDFP2"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTLITS() {
        int rc = this.asmlg(basePath("tests", "TESTLITS"), *options, 'trace', 'noloadhigh')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTAMPS() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTAMPS"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS215() {
        int rc = this.asmlg(basePath("rt", "mlc", "IS215"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTDC1() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTDC1"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTASC1() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTASC1"), *options, 'ASCII', 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTDC2() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTDC2"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTDC3() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTDC3"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_ZOPCHECK() {
        var syscpyOption = "SYSCPY(${basePath('zopcheck')}+${basePath('mac')})"
        this.env = ['SNAPOUT': basePath('zopcheck', 'SNAPOUT.TXT')]
        int rc = this.asmlg(basePath("zopcheck", "ZOPCHECK"), *options, syscpyOption, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
}
