package org.z390.test

import org.junit.jupiter.api.Test

class RunAsmTests extends z390Test {

    var options = ['trace', 'noloadhigh', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]
    var optionsRmode31 = ['trace', 'noloadhigh', 'rmode31', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]
    var optionsNoinit = ['trace', 'noinit', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]
    var optionsNoinitMem32 = ['trace', 'noinit', "mem(32)", "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]
    var optionsNoinitNoloadhigh = ['trace', 'noinit', 'noloadhigh', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]
    var optionsNoinitNoloadhighMem32 = ['trace', 'noinit', 'noloadhigh', "mem(32)", "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]

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
        int rc = this.asmlg(basePath("tests", "TESTLITS"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TEDIT() {
        int rc = this.asmlg(basePath("tests", "TEDIT"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTFPC1() {
        env.put('RT1OUT', basePath('tests', 'TESTFPC1.TST'))
        int rc = this.asmlg(basePath("tests", "TESTFPC1"), *options, "SYSOBJ(+${basePath("linklib")})", 'optable(z390) notrace')
        this.printOutput()
        assert rc == 0
        // Load files to fileData
        loadFile(basePath("tests", "TESTFPC1.TF1"), 'TF1')
        loadFile(basePath("tests", "TESTFPC1.TST"), 'TST')
        // Check files equal
        assert fileData.get('TF1') == fileData.get('TST')
    }
    @Test
    void test_TESTFPC2() {
        env.put('RT1OUT', basePath('tests', 'TESTFPC2.TST'))
        int rc = this.asmlg(basePath("tests", "TESTFPC2"), *options, "SYSOBJ(+${basePath("linklib")})", 'optable(z390) notrace')
        this.printOutput()
        assert rc == 0
        // Load files to fileData
        loadFile(basePath("tests", "TESTFPC2.TF1"), 'TF1')
        loadFile(basePath("tests", "TESTFPC2.TST"), 'TST')
        // Check files equal
        assert fileData.get('TF1') == fileData.get('TST')
    }
    @Test
    void test_TESTAMPS() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTAMPS"), *options, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS215() {
        int rc = this.asmlg(basePath("rt", "mlc", "IS215"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS658() {
        int rc = this.asml(basePath("rt", "mlc", "IS658LD"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath("rt", "mlc", "IS658"), *optionsNoinitNoloadhigh)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath("rt", "mlc", "IS658"), *optionsNoinit)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS659() {
        int rc = this.asmlg(basePath("rt", "mlc", "IS659"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS660() {
        int rc = this.asmlg(basePath("rt", "mlc", "IS660"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS714_1() {
        int rc = this.asml(basePath("rt", "mlc", "T714M1"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M2"), *optionsRmode31)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M3"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714B"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath("rt", "mlc", "T714A"), *optionsNoinitNoloadhigh)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS714_2() {
        int rc = this.asml(basePath("rt", "mlc", "T714M1"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M2"), *optionsRmode31)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M3"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714B"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath("rt", "mlc", "T714A"), *optionsNoinitNoloadhighMem32)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS714_3() {
        int rc = this.asml(basePath("rt", "mlc", "T714M1"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M2"), *optionsRmode31)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M3"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714B"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath("rt", "mlc", "T714A"), *optionsNoinit)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS714_4() {
        int rc = this.asml(basePath("rt", "mlc", "T714M1"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M2"), *optionsRmode31)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714M3"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asml(basePath("rt", "mlc", "T714B"), *options)
        this.printOutput()
        assert rc == 0
        rc = this.asmlg(basePath("rt", "mlc", "T714A"), *optionsNoinitMem32)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_IS750() {
        int rc = this.asm(basePath("rt", "mlc", "IS750"))
        this.printOutput()
        assert rc == 12   // Check return code
        assert this.fileData['ERR'].contains("AZ390I field 2 hex length = 8 must be no more than 7"), "First MP not assembled with expected error"
        assert this.fileData['ERR'].contains("AZ390I field 2 length = 3 must be less than field 1 length = 3"), "Second MP not assembled with expected error"
        assert this.fileData['ERR'].contains("AZ390I field 2 hex length = 9 must be no more than 7"), "First DP not assembled with expected error"
        assert this.fileData['ERR'].contains("AZ390I field 2 length = 4 must be less than field 1 length = 4"), "Second DP not assembled with expected error"
    }
    @Test
    void test_TESTDC1() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTDC1"), *options, 'optable(z390)', "SYSOBJ(${basePath("linklib")})")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTASC1() {
        int rc = this.asmlg(basePath("rt", "mlc", "TESTASC1"), *options, 'ASCII', 'optable(z390)', "SYSOBJ(${basePath("linklib")})")
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
    void test_TB2CX2C() {
        int rc = this.asmlg(basePath("rt", "mlc", "TB2CX2C"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_ZOPCHECK() {
        var syscpyOption = "SYSCPY(${basePath('zopcheck')}+${basePath('mac')})"
        this.env = ['SNAPOUT': basePath('zopcheck', 'SNAPOUT.TXT')]
        int rc = this.asmlg(basePath("zopcheck", "ZOPCHECK"), *options, 'bal', syscpyOption, 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
}
