package org.z390.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.Test
import static org.junit.jupiter.api.DynamicTest.dynamicTest

class RunRtTest extends z390Test {
    var sysmac = basePath("mac")
    var syscpy = basePath("mac")
    var libs = ["SYSMAC(+${sysmac})", "SYSCPY(+${syscpy})"]
    var options  = ['noloadhigh bal notiming stats', *libs]

    void test_module(String moduleName) {
        int rc = this.asmlg(basePath("rt", "test", moduleName), *options)
        this.printOutput()
        assert rc == 0
    }

    @TestFactory
    Collection<DynamicTest> test_pgms() {
        var tests = []
        var modules = [
                'TESTACT1', 'TESTAIN1', 'TESTAIN2', 'TESTAIN3', 'TESTASM1', 'TESTASM2', 'TESTCAL1', 'TESTCAL2',
                'TESTCFD1'
        ]
        modules.each {
            module -> tests.add(
                    dynamicTest("test RT program ${module}", () -> test_module(module)))
        }
        return tests
    }

    @Test
    void test_TESTBLD1() {
        // Prerequisites for BLDL/LOAD: TESTSUB1 and DEMO load modules
        int rc = this.asml(basePath("rt", "test", "TESTSUB1"), *rt1Options)
        this.printOutput()
        assert rc == 0
        // the next step duplicates the DEMO assembly - that's easier than synchronizing with RunMvsTests
        rc = this.asml(basePath("mvs", "demo", "DEMO"), *rt1Options)
        this.printOutput()
        assert rc == 0
        
        var sys390 = "SYS390(${basePath('rt', 'test')}+${basePath('mvs', 'demo')})"
        rc = this.asmlg(basePath("rt", "test", "TESTBLD1"), *options, sys390)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTCDE1() {
        int rc = this.asmlg(basePath("rt", "test", "TESTCDE1"), *options)
        this.printOutput()
        assert rc == 0
        loadFile(basePath('rt', 'test', "TESTCDE1.TF1"), 'TF1')  // load reference file
        def expected = extractSnapLines(fileData.get('TF1'))     // extract SNAP lines
        def actual   = extractSnapLines(fileData.get('LOG'))     // actual SNAP lines
        assert expected == actual
    }






    // The below is pre-existing code that we will need to validate/revamp later

    // rt1 = asmlg
    var rt1Options = ['bal', 'notiming', 'stats', *libs]
    // rt3 = mz390
    var rt3Options = ['noasm', 'bal', 'notiming', 'stats', *libs]
    // rt4 = mz390
    var rt4Options = ['bal', 'notiming', 'stats', *libs]
    // rt7 = asml + ez390
    var rt7AsmlOptions = ['bal', 'notiming', *libs]
    var rt7Ez390Options = ['notiming', 'stats', *libs]

    @Test
    void test_TESTERR1() {
        // rt3 = mz390
        int rc = this.mz390(basePath("rt", "test", "TESTERR1"), *rt3Options)
        this.printOutput()
        assert rc == 16
    }
    @Test
    void test_TESTERR2() {
        // rt4 = mz390
        int rc = this.mz390(basePath("rt", "test", "TESTERR2"), *rt4Options, 'ERR(0)')
        this.printOutput()
        assert rc == 12
    }
    @Test
    void test_TESTERR3() {
        // rt3 = mz390
        int rc = this.mz390(basePath("rt", "test", "TESTERR3"), *rt3Options)
        this.printOutput()
        assert rc == 12
    }
    @Test
    void test_TESTERR4() {
        int rc
        rc = this.asmlg(basePath("rt", "test", "TESTERR4"), 'bal', 'notiming', *libs)
        this.printOutput()
        assert rc == 16
        assert this.stdout =~ /TESTERR4 TEST INVALID DCBDSNAM CAUSING S013 ABEND/
        assert this.stdout =~ /EZ390E error\s{2}12 program aborting due to abend S013/
        assert this.fileData['LOG'] =~ /TESTERR4 TEST INVALID DCBDSNAM CAUSING S013 ABEND/
        assert this.fileData['LOG'] =~ /EZ390E error\s{2}12 program aborting due to abend S013/
    }
    @Test
    void test_TESTERR5() {
        // rt1 = asmlg
        int rc = this.asmlg(basePath("rt", "test", "TESTERR5"), *rt1Options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTERR6() {
        // rt7 = asml + mz390
        int rc
        rc = this.asml(basePath("rt", "test", "TESTERR6"), *rt7AsmlOptions)
        assert rc == 0
        rc = this.ez390(basePath("rt", "test", "TESTERR6"), *rt7Ez390Options)
        this.printOutput()
        assert rc == 16
    }
    @Test
    void test_TESTERR7() {
        // rt7 = asml + mz390
        this.env = ['TESTERR7': 'afile.txt']
        int rc
        rc = this.asml(basePath("rt", "test", "TESTERR7"), *rt7AsmlOptions)
        assert rc == 0
        rc = this.ez390(basePath("rt", "test", "TESTERR7"), *rt7Ez390Options)
        this.printOutput()
        assert rc == 16
        assert this.stdout =~ /TESTERR7 TEST MISSING DDNAME AND NO SYNAD ERROR/
        assert this.stdout =~ /EZ390E error\s{2}12 program aborting due to abend S013/
        assert this.fileData['LOG'] =~ /TESTERR7 TEST MISSING DDNAME AND NO SYNAD ERROR/
        assert this.fileData['LOG'] =~ /EZ390E error\s{2}12 program aborting due to abend S013/
    }
    @Test
    void test_TESTERR8() {
        // rt3 = mz390
        int rc = this.mz390(basePath("rt", "test", "TESTERR8"), *rt3Options)
        this.printOutput()
        assert rc == 12
    }
    @Test
    void test_TESTERR9() {
        // rt3 = mz390
        int rc = this.mz390(basePath("rt", "test", "TESTERR9"), *rt3Options)
        this.printOutput()
        assert rc == 16
    }
    @Test
    void test_TESTERRA() {
        // rt4 = mz390
        int rc = this.mz390(basePath("rt", "test", "TESTERRA"), 'CHKMAC(2)', *rt4Options)
        this.printOutput()
        assert rc == 16
    }
}
