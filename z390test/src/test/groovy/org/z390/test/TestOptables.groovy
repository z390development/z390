package org.z390.test

import org.junit.jupiter.api.Test

class TestOptables extends z390Test {

    var options = ['trace', 'noloadhigh', "SYSMAC(${basePath("mac")})"]

    @Test
    void test_optable_DOS() {
        /**
         * test 1 - optable(DOS)
         */
        int rc = this.asm(basePath("rt", "rt", "OPCD#"), "@${basePath("rt", "rt", "OPCD#DOS.OPT")}")
        this.printOutput()
        assert rc == 0
    }
}
