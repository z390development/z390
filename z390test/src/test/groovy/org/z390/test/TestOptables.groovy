package org.z390.test

import org.junit.jupiter.api.Test

class TestOptables extends z390Test {

    var options = ["stats", "SYSMAC(${basePath("mac")})"]

    @Test
    void test_optable_36020() {
        /**
         * test 1A - optable(360-20)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#320.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#320.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_S36020() {
        /**
         * test 1B - machine(S360-20)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S360-20.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S360-20.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_DOS() {
        /**
         * test 2 - optable(DOS)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#DOS.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#DOS.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#DOS.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#DOS.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_370() {
        /**
         * test 3A - optable(370)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#370.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#370.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#370.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#370.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_S370() {
        /**
         * test 3B - machine(S370)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S370.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_S370.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#370.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S370.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_XA() {
        /**
         * test 4A - optable(XA)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#XA.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#XA.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#XA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#XA.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_S370XA() {
        /**
         * test 4B - machine(S370XA)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S370XA.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_S370XA.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#XA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S370XA.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH0() {
        /**
         * test 4C - machine(ARCH-0)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-0.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-0.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#XA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-0.OPT")}")
        this.printOutput()
        assert rc == 0
    }
}