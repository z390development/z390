package org.z390.test

import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory

import static org.junit.jupiter.api.DynamicTest.dynamicTest

class RunCblTests extends z390Test{

    void test_cobol_module(String moduleName) {
        int rc = this.cblclg(basePath("zcobol", "tests", moduleName))
        this.printOutput()
        assert rc == 0
    }

    @TestFactory
    Collection<DynamicTest> test_COBOL() {
        var tests = []
        var modules = [
                'TESTADD1', 'TESTADD2', 'TESTIF1', 'TESTIF2', 'TESTIF3', 'TESTMOV1', 'TESTMOV2', 'TESTMOV3'
        ]
        modules.each {
            module -> tests.add(
                    dynamicTest("test COBOL ${module}", () -> test_cobol_module(module)))
        }
        return tests
    }

}
