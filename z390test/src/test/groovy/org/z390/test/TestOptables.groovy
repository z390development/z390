package org.z390.test

import org.junit.jupiter.api.Test

class TestOptables extends z390Test {

    var options = ['trace', 'noloadhigh', "SYSMAC(${basePath("mac")})"]

    @Test
    void test_optable_36020() {
        /**
         * test 1 - optable(360-20)
         */
        int rc = this.asml(basePath("rt", "rt", "OPCD#"), *this.options, "@${basePath("rt", "rt", "OPCD#DOS.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_DOS() {
        /**
         * test 1 - optable(DOS)
         */
        env = ['Z390PRN': basePath("rt", "rt", "OPCD#DOS.PRN"),
               'HLASMPRN': basePath("rt", "rt", "OPCD#DOS.TF1")]
        int rc = this.asmlg(basePath("rt", "rt", "OPCD#"), *this.options, "@${basePath("rt", "rt", "OPCD#DOS.OPT")}")
        this.printOutput()
        assert rc == 0
    }
}