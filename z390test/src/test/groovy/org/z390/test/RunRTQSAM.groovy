package org.z390.test

import static org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory

class RunRTQSAM extends z390Test {

    var options = ["SYSMAC(${basePath("mac")})", "TIME(30)", "TRACE"]

    void run_test(String postfix) {
        var dataFile = "TEST${postfix}.TFV"
        if (!new File(basePath('qsam', dataFile)).exists()) {
            dataFile = "TEST${postfix}.Binary.TFV"
        }
        var dataPath = basePath('qsam', dataFile)
        env = ['SNAPOUT': 'DUMMY', 'SYSUT2': dataPath]
        int rc
        rc = asmlg(basePath('qsam', "BLD${postfix}"), *options)
        printOutput()
        assert rc == 0
        env.put('SYSUT1', dataPath)
        asmlg(basePath('qsam', "CHK${postfix}"), *options)
        printOutput()
        assert rc == 0
    }

    @TestFactory
    Collection<DynamicTest> test_QSAM_file_types() {
        var tests = []
        var postfixes = ['V', 'VB', 'VT', 'VL', 'VBL', 'VTL', 'F', 'FL', 'FT', 'FTL']
        postfixes.each {
            postfix -> tests.add(
                    dynamicTest("Test filetype ${postfix}", () -> run_test(postfix)))
        }
        return tests
    }

}
