package org.z390.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import static org.junit.jupiter.api.DynamicTest.dynamicTest

class TestOptables extends z390Test {

    var options = ["stats", "time(45)", "SYSMAC(${basePath("mac")})"]

    /* Test template for both factories */
    void test_optb_asmlg(String fileStem, String referenceStem) {
        var z390prn = basePath("rt", "mlc", "${fileStem}.PRN")
        this.env = [
            'Z390PRN' : z390prn,
            'HLASMPRN': basePath("rt", "mlc", "${referenceStem}.TF1")
        ]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options,
                "sysprn(${z390prn})", "@${basePath("rt", "mlc", "${fileStem}.OPT")}")
        this.printOutput()
        assert rc == 0
    }

/* ********************************************************************************************* */
/*                                                                                               */
/* Section 1 optable option values                                                               */
/*                                                                                               */
/* ********************************************************************************************* */

    @Test
    @OptionalTest
    void test_optable_36020() {
        /**
         * test 0A - optable(360-20)
         *           this optable cannot be compared with HLASM - HLASM does not support this option
         */
        var z390prn = basePath("rt", "mlc", "OPTB#360-20.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#360-20.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    /* Factory for all 'normal' test cases */
    @TestFactory
    @OptionalTest
    Collection<DynamicTest> test_optable() {
        var tests = []
        var names = [
            'DOS', '370', 'XA',  'ESA', 'ZOP', 'ZS1', 'YOP', 'ZS2',
            'Z9',  'ZS3', 'Z10', 'ZS4', 'Z11', 'ZS5', 'Z12', 'ZS6',
            'Z13', 'ZS7', 'Z14', 'ZS8', 'Z15', 'ZS9', 'Z16', 'ZSA',
            'Z17', 'ZSB'
        ]
        names.each { name ->
            tests.add(dynamicTest("test optable ${name}",
                    () -> test_optb_asmlg("OPTB#${name}", "OPTB#${name}")))
        }
        return tests
    }

    @Test
    @OptionalTest
    void test_optable_DOS() {
        /**
         * test 1A - optable(DOS)
         */
        var z390prn = basePath("rt", "mlc", "OPTB#DOS.PRN")
        env = ['Z390PRN': basePath("rt", "mlc", "OPTB#DOS.PRN"),
               'HLASMPRN': basePath("rt", "mlc", "OPTB#DOS.TF1")]
        int rc = this.asmlg(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB#DOS.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test // This one is NOT optional!
    void test_optable_UNI() {
        /**
         * test 80 - optable(UNI)
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
    @OptionalTest
    void test_machine_S36020() {
        /**
         * test 0B - machine(S360-20)
         *           this optable cannot be compared with HLASM - HLASM does not support this option
         */
        var z390prn = basePath("rt", "mlc", "OPTB_S360-20.PRN")
        int rc = this.asml(basePath("rt", "mlc", "OPTB#"), *this.options, "sysprn(${z390prn})", "@${basePath("rt", "mlc", "OPTB_S360-20.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @TestFactory
    @OptionalTest
    Collection<DynamicTest> test_machine() {
        var tests = []
        // [machine file suffix, optable reference used for HLASMPRN]
        var cases = [
            ['S370', '370'], ['S370XA', 'XA'], ['ARCH-0', 'XA'],
            ['S370ESA', 'ESA'], ['S390', 'ESA'], ['S390E', 'ESA'],
            ['ARCH-1', 'ESA'], ['ARCH-2', 'ESA'], ['ARCH-3', 'ESA'], ['ARCH-4', 'ESA'],
            ['zSeries', 'ZOP'], ['zSeries-1', 'ZOP'], ['ZS', 'ZOP'], ['ZS-1', 'ZOP'],
            ['z800', 'ZOP'], ['z900', 'ZOP'], ['ARCH-5', 'ZOP'],
            ['z890', 'YOP'], ['z990', 'YOP'], ['zSeries-2', 'YOP'], ['ZS-2', 'YOP'], ['ARCH-6', 'YOP'],
            ['z9', 'Z9'], ['zSeries-3', 'Z9'], ['ZS-3', 'Z9'], ['ARCH-7', 'Z9'],
            ['z10', 'Z10'], ['zSeries-4', 'Z10'], ['ZS-4', 'Z10'], ['ARCH-8', 'Z10'],
            ['z11', 'Z11'], ['z114', 'Z11'], ['z196', 'Z11'],
            ['zSeries-5', 'Z11'], ['ZS-5', 'Z11'], ['ARCH-9', 'Z11'],
            ['z12', 'Z12'], ['zBC12', 'Z12'], ['zEC12', 'Z12'],
            ['zSeries-6', 'Z12'], ['ZS-6', 'Z12'], ['ARCH-10', 'Z12'],
            ['z13', 'Z13'], ['zSeries-7', 'Z13'], ['ZS-7', 'Z13'], ['ARCH-11', 'Z13'],
            ['z14', 'Z14'], ['zSeries-8', 'Z14'], ['ZS-8', 'Z14'], ['ARCH-12', 'Z14'],
            ['z15', 'Z15'], ['zSeries-9', 'Z15'], ['ZS-9', 'Z15'], ['ARCH-13', 'Z15'],
            ['z16', 'Z16'], ['zSeries-10', 'Z16'], ['ZS-10', 'Z16'], ['ARCH-14', 'Z16'],
            ['z17', 'Z17'], ['zSeries-11', 'Z17'], ['ZS-11', 'Z17'], ['ARCH-15', 'Z17']
        ]
        cases.each { item ->
            tests.add(dynamicTest("test machine ${item[0]}",
                    () -> test_optb_asmlg("OPTB_${item[0]}", "OPTB#${item[1]}")))
        }
        return tests
    }
}