package org.z390.test

import org.junit.jupiter.api.Test

class RunHLASMBuiltInFunctionsTests extends z390Test {

    /*
     * Test the High Level Assembler built-in functions
     */
     
    var options = ["SYSMAC(${basePath("mac")})"]
    var os = System.getProperty("os.name")

    @Test
    void test_TESTOPR2() {
        
        int rc = this.asmlg(basePath("rt", "test", "TESTOPR2"), *options)
        this.printOutput()
        assert rc == 0
    }
    
    @Test
    void test_TOPR2() {

        // File containing report of tests done
        
        this.env.put('REPORT', basePath('rt', 'mlc', 'TOPR2.TST'))
        
        int rc = this.asmlg(basePath("rt", "mlc", "TOPR2"), *options)
        this.printOutput()
        assert rc == 0
    }
}
