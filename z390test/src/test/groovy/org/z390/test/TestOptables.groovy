package org.z390.test

import org.junit.jupiter.api.Test

class TestOptables extends z390Test {

    var options = ["stats", "SYSMAC(${basePath("mac")})"]

/* ********************************************************************************************* */
/*                                                                                               */
/* Section 1 optable option values                                                               */
/*                                                                                               */
/* ********************************************************************************************* */

    @Test
    void test_optable_36020() {
        /**
         * test 1A - optable(360-20)
         *           this optable cannot be compared with HLASM - HLASM does not support this option
         */
        var z390prn = basePath("rt", "mlc", "OPTB#320.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#320.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_DOS() {
        /**
         * test 2A - optable(DOS)
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
    void test_optable_ESA() {
        /**
         * test 5A - optable(ESA)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ESA.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ESA.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ESA.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZOP() {
        /**
         * test 6A - optable(ZOP)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZOP.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZOP.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZOP.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS1() {
        /**
         * test 6B - optable(ZS1)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS1.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS1.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS1.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS1.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_UNI() {
        /**
         * test 98 - optable(UNI)
         *           This table cannot (yet) be compared against HLASM
         */
        var z390prn = basePath("rt", "mlc", "OPTB#UNI.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#UNI.OPT")}")
        this.printOutput()
        assert rc == 0
    }

/* ********************************************************************************************* */
/*                                                                                               */
/* Section 2 machine option values                                                               */
/*                                                                                               */
/* ********************************************************************************************* */

    @Test
    void test_machine_S36020() {
        /**
         * test 1B - machine(S360-20)
         *           this optable cannot be compared with HLASM - HLASM does not support this option
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S360-20.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S360-20.OPT")}")
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

    @Test
    void test_machine_S370ESA() {
        /**
         * test 5B - machine(S370ESA)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S370ESA.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_S370ESA.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S370ESA.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_S390() {
        /**
         * test 5C - machine(S390)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S390.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_S390.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S390.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_S390E() {
        /**
         * test 5D - machine(S390E)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S390E.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_S390E.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S390E.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH1() {
        /**
         * test 5E - machine(ARCH-1)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-1.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-1.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-1.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH2() {
        /**
         * test 5F - machine(ARCH-2)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-2.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-2.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-2.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH3() {
        /**
         * test 5G - machine(ARCH-3)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-3.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-3.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-3.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH4() {
        /**
         * test 5H - machine(ARCH-4)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-4.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-4.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ESA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-4.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries() {
        /**
         * test 6C - machine(zSeries)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries1() {
        /**
         * test 6D - machine(zSeries-1)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-1.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-1.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-1.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS() {
        /**
         * test 6E - machine(ZS)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS1() {
        /**
         * test 6F - machine(ZS-1)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-1.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-1.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-1.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z800() {
        /**
         * test 6G - machine(z800)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z800.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z800.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z800.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z900() {
        /**
         * test 6H - machine(z900)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z900.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z900.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z900.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH5() {
        /**
         * test 6I - machine(ARCH-5)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-5.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-5.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-5.OPT")}")
        this.printOutput()
        assert rc == 0
    }
}