package org.z390.test

import org.junit.jupiter.api.Test

class TestZ390Test extends z390Test {

    var sysmac = basePath("mac")
    var options = ['trace', 'noloadhigh', "SYSMAC(${sysmac})"]

    @Test
    void testAsm() {
        int rc = this.asm(basePath("tests", "TESTINS1"), *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void testAsml() {
        int rc = this.asml(basePath("tests", "TESTINS2"), *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void testAsmlg() {
        int rc = this.asmlg(basePath("tests", "TESTINS2"), *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void testInlineSource() {

        var source = """TESTB    START 0
         USING *,13
         STM   14,12,12(13)
         ST    13,8(13)
         ST    15,4(15)
         LR    13,15
         J
         RETURN (14,12)
         END   TESTB
    """
        String sourceFile = this.createTempFile("INLINE.MLC", source)
        int rc = this.asm(sourceFile, *options)
        this.printOutput()
        assert rc == 12   // Check return code
        assert this.fileData['ERR'] =~ /AZ390 AZ390I invalid relative offset expression/  // check error present
    }
}
