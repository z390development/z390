package org.z390.test

import org.junit.jupiter.api.Test

class RunBr14owe extends z390Test {

    var options = ["SYSMAC(${basePath("mac")})"]

    @Test
    void test_IEFBR14O() {
        int rc = this.asm(basePath("rt", "mlc", "IEFBR14O"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IEFBR14W() {
        int rc = this.asm(basePath("rt", "mlc", "IEFBR14W"), *options)
        this.printOutput()
        assert rc == 4
    }
    @Test
    void test_IEFBR14E() {
        int rc = this.asm(basePath("rt", "mlc", "IEFBR14E"), *options)
        this.printOutput()
        assert rc == 12
    }
}
