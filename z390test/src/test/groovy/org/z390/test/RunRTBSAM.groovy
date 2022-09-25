package org.z390.test

import static org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory

class RunRTBSAM extends z390Test {

    var options = ["SYSMAC(${basePath("mac")})"]

    void run_test(String postfix) {
        env = ['SNAPOUT': 'DUMMY', 'SYSUT2': basePath('bsam', "TEST${postfix}.TFV")]
        int rc
        rc = asmlg(basePath('bsam', "BLD${postfix}"), *options)
        printOutput()
        assert rc == 0
        env.put('SYSUT1', basePath('bsam', "TEST${postfix}.TFV"))
        rc = asmlg(basePath('bsam', "CHK${postfix}"), *options)
        printOutput()
        assert rc == 0
        env = ['SNAPOUT': 'DUMMY',
               'SYSUT1': basePath('bsam', "TEST${postfix}.TFV"),
               'SYSUT2': basePath('bsam', "CPYR${postfix}.VES")
        ]
        rc = asmlg(basePath('bsam', "CPYR${postfix}"), *options)
        printOutput()
        assert rc == 0
    }

    @TestFactory
    Collection<DynamicTest> test_BSAM_file_types() {
        var tests = []
        var postfixes = ['W', 'WL']
        postfixes.each {
            postfix -> tests.add(
                    dynamicTest("Test filetype ${postfix}", () -> run_test(postfix)))
        }
        return tests
    }

}
