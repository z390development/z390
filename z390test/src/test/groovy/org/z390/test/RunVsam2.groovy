package org.z390.test

import org.junit.jupiter.api.Test

class RunVsam2 extends z390Test {

    var options = ['trace', 'noloadhigh', 'zvsam(2)', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]

    @Test
    void test_Z390CAT2() {
        int rc = this.asml(basePath("vsam2", "mlc", "Z390CAT2"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_ZVSAM19C() {
        int rc = this.asml(basePath("vsam2", "mlc", "ZVSAM19C"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTGNCB() {
        int rc = this.asml(basePath("vsam2", "mlc", "TESTGNCB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTMDCB() {
        int rc = this.asml(basePath("vsam2", "mlc", "TESTMDCB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTSHCB() {
        int rc = this.asml(basePath("vsam2", "mlc", "TESTSHCB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTTSCB() {
        int rc = this.asml(basePath("vsam2", "mlc", "TESTTSCB"), *options)
        this.printOutput()
        assert rc == 0
    }
}
