package org.z390.test

import org.junit.jupiter.api.Test

class RunVsam2 extends z390Test {

    var options = ['trace', 'noloadhigh', 'zvsam(2)', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]

    // the 4 tests below should use asmlg when ZVSAM19C.390 is in linklib\ asml otherwise

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
