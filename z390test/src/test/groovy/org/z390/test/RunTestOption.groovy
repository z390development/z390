package org.z390.test

import org.junit.jupiter.api.Test

class RunTestOption extends z390Test {

    @Test
    void test_TESTOPT_OK() {
        /**
         * test 1 - should assemble okay
         */
        int rc = this.asm(basePath("rt", "mlc", "TESTOPT"), "@${basePath("rt", "opt", "testopt0.OPT")}")
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTOPT1_invalidOption() {
        /**
         * test 2 - should assemble with error for invalid option
         */
        int rc = this.asm(basePath("rt", "mlc", "TESTOPT"), "@${basePath("rt", "opt", "testopt1.OPT")}")
        this.printOutput()
        assert rc == 16
        assert this.fileData['ERR'] =~ /invalid options -\s{2}chksrc\(2X\)/
        assert this.stdout =~ /invalid options -\s{2}chksrc\(2X\)/
    }

    @Test
    void test_TESTOPT2L_validReference() {
        /**
         * test 3 - should assemble okay with valid reference
         */
        var testOpt0FileContents = """
* Test with a valid option
  chksrc(2)  * Check sources for illegal characters
"""
        var innerOptionFilename = createTempFile("testopt0.OPT", testOpt0FileContents)
        var testOpt2LFileContents = """
* Test with a valid subfile
  chksrc(2)  * Check sources for illegal characters
@${innerOptionFilename}
"""
        var OptionFilename = createTempFile("testopt2L.OPT", testOpt2LFileContents)
        int rc = this.asm(basePath("rt", "mlc", "TESTOPT"), "@${OptionFilename}")
        this.printOutput()
        assert rc == 0
        assert fileData['PRN'] =~ /testopt2L.OPT=/
        assert fileData['PRN'] =~ /testopt0.OPT=/
    }

    @Test
    void test_TESTOPT3L_incorrectReference() {
        /**
         * test 4 - should assemble with error for incorrect reference
         */
        int rc = this.asm(basePath("rt", "mlc", "TESTOPT"), "@${basePath("rt", "opt", "testopt3L.OPT")}")
        this.printOutput()
        assert rc == 16
        assert this.stdout =~ /TZ390E abort error 21 - invalid options -\s{2}@rt\/opt\/testoptX/
        assert this.fileData['ERR'] =~ /TZ390E abort error 21 - invalid options -\s{2}@rt\/opt\/testoptX/
    }

    @Test
    void test_TESTOPT4L_selfReference() {
        /**
         * test 5 - should assemble with error for self-reference
         */
        var OptionFileContents = """
* Test with a self-reference
  chksrc(2)  * Check sources for illegal characters
@{{fullFileName}}"""
        var optionFilename = createTempFile('testopt4L.OPT', OptionFileContents)
        int rc = this.asm(basePath("rt", "mlc", "TESTOPT"), "@${optionFilename}")
        this.printOutput()
        assert rc == 16
        assert this.stdout =~ /TZ390E abort error 21 - ignoring .*testopt4L.OPT as it has already been processed as an option file, referenced in .*testopt4L.OPT/
    }

    @Test
    void test_TESTOPT5L_circularReference() {
        /**
         * test 6 - should assemble with error for circular reference
         */
        // Start with a dummy file - we will rewrite contents later, just need filename for now
        var opt5LFilename = createTempFile("testopt5L.OPT", "")
        //
        var opt5LBFileContents = """* Test with a circular reference
  chksrc(2)  * Check sources for illegal characters
@${opt5LFilename}
"""
        var opt5LBFilename = createTempFile("testopt5LB.OPT", opt5LBFileContents)

        var opt5LAFileContents = """* Test with a circular reference
  chksrc(2)  * Check sources for illegal characters
@${opt5LBFilename}
"""
        var opt5LAFilename = createTempFile("testopt5LA.OPT", opt5LAFileContents)
        var opt5LFileContents = """* Test with a circular reference
  chksrc(2)  * Check sources for illegal characters
@${opt5LAFilename}"""
        opt5LFilename = createTempFile("testopt5L.OPT", opt5LFileContents)

        int rc = this.asm(basePath("rt", "mlc", "TESTOPT"), "@${opt5LFilename}")
        this.printOutput()
        assert rc == 16
        assert this.stdout =~ /TZ390E abort error 21 - ignoring .*testopt5L.OPT as it has already been processed as an option file, referenced in .*testopt5L.OPT/
    }
}
