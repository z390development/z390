package org.z390.test

import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.Test

import static org.junit.jupiter.api.DynamicTest.dynamicTest

class RunCblTests extends z390Test{

    void test_cobol_module(String moduleName) {
        int rc = this.cblclg(basePath("zcobol", "tests", moduleName), 'TIME(30)')
        this.printOutput()
        assert rc == 0
    }

    @TestFactory
    Collection<DynamicTest> test_COBOL() {
        var tests = []
        var modules = [
                'TESTADD1', 'TESTADD2', 'TESTBFP1', 'TESTIF1', 'TESTIF2', 'TESTIF3', 'TESTMOV1', 'TESTMOV2', 'TESTMOV3', 'CM101M01'
        ]
        modules.each {
            module -> tests.add(
                    dynamicTest("test COBOL ${module}", () -> test_cobol_module(module)))
        }
        return tests
    }

    @Test
    void test_TESTCALL() {
        var asm_options = ["SYSMAC(${basePath("mac")})"]

        // rem TESTCAL3 statically calls TESTASM4 - link them together
        int rc1 = this.asm(basePath("zcobol", "tests", "TESTASM4"), *asm_options)
        this.printOutput()
        assert rc1 == 0   // Check return code
        int rc2 = this.cblcl(basePath("zcobol", "tests", "TESTCAL3"))
        this.printOutput()
        assert rc2 == 0   // Check return code

        // TESTCAL1 statically calls TESTCAL2 and dynamically calls TESTCAL3
        int rc3 = this.cblc(basePath("zcobol", "tests", "TESTCAL2"))
        this.printOutput()
        assert rc3 == 0   // Check return code
        int rc4 = this.cblclg(basePath("zcobol", "tests", "TESTCAL1"))
        this.printOutput()
        assert rc4 == 0   // Check return code
    }

    void testCM101M03() {
        int rc = this.cblc(basePath("zcobol", "tests", "CM101M03"))
        this.printOutput()
        assert rc == 0   // Check return code
        }
    @Test
    void testCM101M02() {
        int rc = this.cblc(basePath("zcobol", "tests", "CM101M02"))
        this.printOutput()
        assert rc == 8   // Check return code
        assert this.fileData['ERR'].contains("MNOTE 8,'CD NOT SUPPORTED YET'"), "CM101M02.ERR does not contain expected error"
        assert !this.fileData['ERR'].contains("missing copy"), "CM101M02.ERR reports error that should have been fixed"
    }

}
