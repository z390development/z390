package org.z390.test

//
// This groovy script is to run as early as possible because it tests our groovy procedures
// If any of the below tests fail, further tetsing is pointless
//

import org.junit.jupiter.api.Test

class BeginZ390Test extends z390Test {

    var sysmac = basePath("mac")
    var options = ['trace', 'noloadhigh', "SYSMAC(${sysmac})"]
//
// validate groovy version of asm/asml/asmlg scripts
//
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
//
// validate groovy script to assemble inline code
//
    @Test
    void testInlineSource() {

        var source = """TESTB    START 0
         STM   14,12,12(13)
         BAS   15,8+72(15)
         USING *,13
         DC    18F'0'
         ST    13,4(15)
         ST    15,8(13)
         LR    13,15
         J
         L     13,4(13)
         RETURN (14,12)
 
         END   TESTB
    """
        String sourceFile = this.createTempFile("INLINE.MLC", source)
        int rc = this.asm(sourceFile, *options)
        this.printOutput()
        assert rc == 12   // Check return code
        assert this.fileData['ERR'] =~ /AZ390 AZ390I invalid relative offset expression/  // check error present
    }
//
// validate groovy version of cblc/cblcl/cblclg scripts
//
    @Test
    void testCblc() {
        int rc = this.cblc(basePath("zcobol", "demo", "HELLO"))
        this.printOutput()
        assert rc == 0
    }
    @Test
    void testCblcl() {
        int rc = this.cblcl(basePath("zcobol", "demo", "HELLO"))
        this.printOutput()
        assert rc == 0
    }
    @Test
    void testCblclg() {
        int rc = this.cblclg(basePath("zcobol", "demo", "HELLO"))
        this.printOutput()
        assert rc == 0
    }

}
