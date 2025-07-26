package org.z390.test

import org.junit.jupiter.api.Test

import java.util.regex.Pattern

class RunCblDemos extends z390Test {

    @Test
    // Test use of explicit default extension
    void test_COBOL_HELLO() {
        int rc = this.cblclg(basePath("zcobol", "demo", "HELLO.CBL"))
        this.printOutput()
        assert rc == 0
    }

    @Test
    // Test use of explicit non-default extension
    void test_COBOL_HELLO2() {
        int rc = this.cblclg(basePath("zcobol", "demo", "HELLO2.Cobol"))
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_DATETIME() {
        int rc = this.cblclg(basePath("zcobol", "demo", "DATETIME"))
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_POWERS() {
        int rc = this.cblclg(basePath("zcobol", "demo", "POWERS"))
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_COPYFILE() {
        this.env = [
             'INFILE' : basePath('zcobol', 'demo', 'COPYFILE.IN'),
             'OUTFILE' : basePath('zcobol', 'demo', 'COPYFILE.OUT')
        ]
        int rc = this.cblclg(basePath("zcobol", "demo", "COPYFILE"))
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_COBOL_COMPSUM() {
        int rc = this.cblc(basePath("zcobol", "demo", "COMPSUM"))
        this.printOutput()
        assert rc == 0
        var runOptions = [
                "SYS390(${basePath('zcobol', 'lib')})",
                "SYSMAC(${basePath("mac")})"
        ]
        rc = asmlg(basePath("zcobol", "demo", "CALLCOMP"), *runOptions)
        this.printOutput()
        assert rc == 0
    }
}
