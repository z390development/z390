package org.z390.test

import org.junit.jupiter.api.Test

import java.util.regex.Pattern

class RunCblDemos extends z390Test {

    @Test
    void test_COBOL_HELLO() {
        int rc = this.cblclg(basePath("zcobol", "demo", "HELLO"), 'stats', 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_DATETIME() {
        int rc = this.cblclg(basePath("zcobol", "demo", "DATETIME"), 'stats', 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_POWERS() {
        int rc = this.cblclg(basePath("zcobol", "demo", "POWERS"), 'stats', 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_COPYFILE() {
        this.env = [
             'INFILE' : basePath('zcobol', 'demo', 'COPYFILE.IN'),
             'OUTFILE' : basePath('zcobol', 'demo', 'COPYFILE.OUT')
        ]
        int rc = this.cblclg(basePath("zcobol", "demo", "COPYFILE"), 'stats', 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_COMPSUM() {
        int rc = this.cblc(basePath("zcobol", "demo", "COMPSUM"), 'stats', 'optable(z390)')
        this.printOutput()
        assert rc == 0
        var runOptions = [
                "SYS390(${basePath('zcobol', 'lib')})",
                "SYSMAC(${basePath("mac")})"
        ]
        rc = asmlg(basePath("zcobol", "demo", "CALLCOMP"), *runOptions, 'stats', 'optable(z390)')
        this.printOutput()
        assert rc == 0
    }
}
