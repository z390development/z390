package org.z390.test

import org.junit.jupiter.api.Test

class TestOptables extends z390Test {

    var options = ["stats", "time(45)", "SYSMAC(${basePath("mac")})"]

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
        var z390prn = basePath("rt", "mlc", "OPTB#360-20.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#360-20.OPT")}")
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
    void test_optable_YOP() {
        /**
         * test 7A - optable(YOP)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#YOP.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#YOP.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#YOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#YOP.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS2() {
        /**
         * test 7B - optable(ZS2)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS2.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS2.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS2.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS2.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z9() {
        /**
         * test 8A - optable(Z9)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z9.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z9.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z9.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z9.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS3() {
        /**
         * test 8B - optable(ZS3)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS3.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS3.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS3.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS3.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z10() {
        /**
         * test 9A - optable(Z10)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z10.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z10.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z10.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z10.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS4() {
        /**
         * test 9B - optable(ZS4)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS4.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS4.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS4.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS4.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z11() {
        /**
         * test 10A - optable(Z11)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z11.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z11.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z11.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS5() {
        /**
         * test 10B - optable(ZS5)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS5.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS5.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS5.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS5.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z12() {
        /**
         * test 11A - optable(Z12)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z12.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z12.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z12.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS6() {
        /**
         * test 11B - optable(ZS6)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS6.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS6.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS6.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS6.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z13() {
        /**
         * test 12A - optable(Z13)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z13.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z13.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z13.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z13.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS7() {
        /**
         * test 12B - optable(ZS7)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS7.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS7.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS7.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS7.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z14() {
        /**
         * test 13A - optable(Z14)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z14.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z14.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z14.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z14.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS8() {
        /**
         * test 13B - optable(ZS8)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS8.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS8.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS8.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS8.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z15() {
        /**
         * test 14A - optable(Z15)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z15.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z15.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z15.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z15.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZS9() {
        /**
         * test 14B - optable(ZS9)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZS9.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZS9.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZS9.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZS9.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_Z16() {
        /**
         * test 15A - optable(Z16)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#Z16.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#Z16.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z16.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#Z16.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_ZSA() {
        /**
         * test 15B - optable(ZSA)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#ZSA.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#ZSA.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#ZSA.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#ZSA.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_optable_UNI() {
        /**
         * test 98 - optable(UNI)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#UNI.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#UNI.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#UNI.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#UNI.OPT")}")
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

    @Test
    void test_machine_z890() {
        /**
         * test 7C - machine(z890)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z890.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z890.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#YOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z890.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z990() {
        /**
         * test 7D - machine(z990)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z990.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z990.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#YOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z990.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries2() {
        /**
         * test 7E - machine(zSeries-2)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-2.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-2.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#YOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-2.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS2() {
        /**
         * test 7F - machine(ZS-2)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-2.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-2.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#YOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-2.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH6() {
        /**
         * test 7G - machine(ARCH-6)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-6.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-6.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#YOP.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-6.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z9() {
        /**
         * test 8C - machine(z9)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z9.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z9.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z9.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z9.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries3() {
        /**
         * test 8D - machine(zSeries-3)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-3.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-3.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z9.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-3.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS3() {
        /**
         * test 8E - machine(ZS-3)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-3.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-3.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z9.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-3.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH7() {
        /**
         * test 8F - machine(ARCH-7)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-7.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-7.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z9.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-7.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z10() {
        /**
         * test 9C - machine(z10)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z10.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z10.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z10.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z10.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries4() {
        /**
         * test 9D - machine(zSeries-4)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-4.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-4.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z10.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-4.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS4() {
        /**
         * test 9E - machine(ZS-4)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-4.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-4.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z10.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-4.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH8() {
        /**
         * test 9F - machine(ARCH-8)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-8.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-8.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z10.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-8.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z11() {
        /**
         * test 10C - machine(z11)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z11.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z11.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z11.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z114() {
        /**
         * test 10D - machine(z114)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z114.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z114.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z114.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z196() {
        /**
         * test 10E - machine(z196)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z196.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z196.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z196.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries5() {
        /**
         * test 10F - machine(zSeries-5)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-5.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-5.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-5.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS5() {
        /**
         * test 10G - machine(ZS-5)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-5.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-5.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-5.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH9() {
        /**
         * test 10H - machine(ARCH-9)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-9.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-9.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z11.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-9.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z12() {
        /**
         * test 11C - machine(z12)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z12.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z12.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z12.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zBC12() {
        /**
         * test 11D - machine(zBC12)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zBC12.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zBC12.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zBC12.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zEC12() {
        /**
         * test 11E - machine(zEC12)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zEC12.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zEC12.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zEC12.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries6() {
        /**
         * test 11F - machine(zSeries-6)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-6.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-6.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-6.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS6() {
        /**
         * test 11G - machine(ZS-6)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-6.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-6.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-6.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH10() {
        /**
         * test 11H - machine(ARCH-10)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-10.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-10.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z12.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-10.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z13() {
        /**
         * test 12C - machine(z13)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z13.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z13.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z13.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z13.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries7() {
        /**
         * test 12D - machine(zSeries-7)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-7.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-7.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z13.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-7.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS7() {
        /**
         * test 12E - machine(ZS-7)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-7.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-7.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z13.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-7.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH11() {
        /**
         * test 12F - machine(ARCH-11)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-11.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-11.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z13.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-11.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z14() {
        /**
         * test 13C - machine(z14)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z14.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z14.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z14.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z14.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries8() {
        /**
         * test 13D - machine(zSeries-8)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-8.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-8.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z14.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-8.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS8() {
        /**
         * test 13E - machine(ZS-8)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-8.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-8.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z14.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-8.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH12() {
        /**
         * test 13F - machine(ARCH-12)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-12.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-12.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z14.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-12.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z15() {
        /**
         * test 14C - machine(z15)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z15.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z15.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z15.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z15.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries9() {
        /**
         * test 14D - machine(zSeries-9)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-9.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-9.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z15.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-9.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS9() {
        /**
         * test 14E - machine(ZS-9)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-9.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-9.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z15.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-9.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH13() {
        /**
         * test 14F - machine(ARCH-13)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-13.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-13.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z15.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-13.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_z16() {
        /**
         * test 15C - machine(z16)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_z16.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_z16.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z16.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_z16.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_zSeries10() {
        /**
         * test 15D - machine(zSeries-10)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_zSeries-10.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_zSeries-10.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z16.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_zSeries-10.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ZS10() {
        /**
         * test 15E - machine(ZS-10)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ZS-10.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ZS-10.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z16.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ZS-10.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_machine_ARCH14() {
        /**
         * test 15F - machine(ARCH-14)
         */
        var z390prn = basePath("rt", "mlc", "OPTB_ARCH-14.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB_ARCH-14.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#Z16.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_ARCH-14.OPT")}")
        this.printOutput()
        assert rc == 0
    }
}