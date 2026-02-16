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
                'TESTADD1', 'TESTADD2', 'TESTBFP1', 'TESTIF1',  'TESTIF2',  'TESTIF3',  'TESTMOV1', 'TESTMOV2', 'TESTMOV3', 'CM101M01', 'DB101A01', 'NC106A01',
                'TESTCMP1', 'TESTCMP2', 'TESTCMP3', 'TESTCMP4', 'TESTCMP5', 'TESTCMP6', 'TESTCPY1', 'TESTCPY2', 'TESTDFP1', 'TESTDIV1', 'TESTDIV2', 'TESTDSP1',
                'TESTGO1',  'TESTHFP1', 'TESTISP1', 'TESTPIC1', 'TESTPM1',  'TESTPM2',  'TESTSIX1', 'TESTSIX2', 'TESTSUB1', 'TESTSUB2', 'TESTTRC2', 'TESTWS1',
                'YOUZPI1'
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

    @Test
    void testCM101M02() {
        int rc = this.cblc(basePath("zcobol", "tests", "CM101M02"))
        this.printOutput()
        assert rc == 8   // Check return code
        assert this.fileData['ERR'].contains("MNOTE 8,'CD NOT SUPPORTED YET'"), "CM101M02.ERR does not contain expected error"
        assert !this.fileData['ERR'].contains("missing copy"), "CM101M02.ERR reports error that should have been fixed"
    }
    @Test
    void testCM101M03() {
        int rc = this.cblc(basePath("zcobol", "tests", "CM101M03"))
        this.printOutput()
        assert rc == 0   // Check return code
        }
    @Test
    void testCM401M01() {
        int rc = this.cblc(basePath("zcobol", "tests", "CM401M01"))
        this.printOutput()
        assert rc == 8   // Check return code
        assert this.fileData['ERR'].contains("MNOTE 8,'PURGE NOT SUPPORTED YET'"), "CM401M01.ERR does not contain expected error"
        assert !this.fileData['ERR'].contains("missing macro"), "CM401M01.ERR reports error that should have been fixed"
    }
    @Test
    void testTESTFIL1() {
        this.env.put('INFILE', basePath("zcobol", "tests", "TESTFIL1.IN"))
        this.env.put('OUTFILE', basePath("zcobol", "tests", "TESTFIL1.OUT"))
        int rc = this.cblclg(basePath("zcobol", "tests", "TESTFIL1"))
        this.printOutput()
        assert rc == 0   // Check return code
    }
    @Test
    void testTESTFIL2() {
        this.env.put('INFILE', basePath("zcobol", "tests", "TESTFIL2.IN"))
        this.env.put('OUTFILE', basePath("zcobol", "tests", "TESTFIL2.OUT"))
        int rc = this.cblclg(basePath("zcobol", "tests", "TESTFIL2"))
        this.printOutput()
        assert rc == 0   // Check return code
    }
    @Test
    void testTESTPM3() {
        this.env.put('INFILE', basePath("zcobol", "tests", "TESTPM3.IN"))
        this.env.put('OUTFILE', basePath("zcobol", "tests", "TESTPM3.OUT"))
        int rc = this.cblclg(basePath("zcobol", "tests", "TESTPM3"))
        this.printOutput()
        assert rc == 0   // Check return code
    }
    @Test
    void testTESTTRC1() {
        int rc = this.cblclg(basePath("zcobol", "tests", "TESTTRC1"), 'TRUNC')
        this.printOutput()
        assert rc == 0   // Check return code
    }
    void testTESTTRC3() {
        int rc = this.cblclg(basePath("zcobol", "tests", "TESTTRC3"), 'TRUNC NOR64')
        this.printOutput()
        assert rc == 0   // Check return code
    }

    @Test
    void testTESTREPL() {
        int rc = this.cblclg(basePath("zcobol", "tests", "TESTREPL"), 'TIME(30)')
        this.printOutput()
        assert rc == 0   // Check return code
        assert this.fileData['LOG'].contains("TESTREPL - TEST ENDED OK"), "TESTREPL.LOG does not contain expected success message"
    }

    @Test
    void testSORTASC() {
        this.env.put('INFILE', basePath('zcobol', 'tests', 'SORTASC.TF1'))
        this.env.put('OUTFILE', basePath('zcobol', 'tests', 'SORTASC.OUT'))
        int rc = this.cblclg(basePath("zcobol", "tests", "SORTASC"), 'TIME(30)')
        this.printOutput()
        assert rc == 0
        var actual = new File(basePath('zcobol', 'tests', 'SORTASC.OUT')).text
        var expected = new File(basePath('zcobol', 'tests', 'SORTASC.TF2')).text
        assert actual == expected, "SORTASC output does not match reference"
    }

    @Test
    void testSORTDEF() {
        this.env.put('INFILE', basePath('zcobol', 'tests', 'SORTDEF.TF1'))
        this.env.put('OUTFILE', basePath('zcobol', 'tests', 'SORTDEF.OUT'))
        int rc = this.cblclg(basePath("zcobol", "tests", "SORTDEF"), 'TIME(30)')
        this.printOutput()
        assert rc == 0
        var actual = new File(basePath('zcobol', 'tests', 'SORTDEF.OUT')).text
        var expected = new File(basePath('zcobol', 'tests', 'SORTDEF.TF2')).text
        assert actual == expected, "SORTDEF output does not match reference"
    }

    @Test
    void testSORTPROC() {
        this.env.put('INFILE', basePath('zcobol', 'tests', 'SORTPROC.TF1'))
        this.env.put('OUTFILE', basePath('zcobol', 'tests', 'SORTPROC.OUT'))
        int rc = this.cblclg(basePath("zcobol", "tests", "SORTPROC"), 'TIME(30)')
        this.printOutput()
        assert rc == 0
        var actual = new File(basePath('zcobol', 'tests', 'SORTPROC.OUT')).text
        var expected = new File(basePath('zcobol', 'tests', 'SORTPROC.TF2')).text
        assert actual == expected, "SORTPROC output does not match reference"
    }

    @Test
    void testSORTMULT() {
        this.env.put('INFILE', basePath('zcobol', 'tests', 'SORTMULT.TF1'))
        this.env.put('ASCFILE', basePath('zcobol', 'tests', 'SORTMULT_ASC.OUT'))
        this.env.put('DESCFILE', basePath('zcobol', 'tests', 'SORTMULT_DSC.OUT'))
        int rc = this.cblclg(basePath("zcobol", "tests", "SORTMULT"), 'TIME(30)')
        this.printOutput()
        assert rc == 0
        var actualAsc = new File(basePath('zcobol', 'tests', 'SORTMULT_ASC.OUT')).text
        var expectedAsc = new File(basePath('zcobol', 'tests', 'SORTMULT_ASC.TF2')).text
        assert actualAsc == expectedAsc, "SORTMULT ascending output does not match reference"
        var actualDsc = new File(basePath('zcobol', 'tests', 'SORTMULT_DSC.OUT')).text
        var expectedDsc = new File(basePath('zcobol', 'tests', 'SORTMULT_DSC.TF2')).text
        assert actualDsc == expectedDsc, "SORTMULT descending output does not match reference"
    }

}
