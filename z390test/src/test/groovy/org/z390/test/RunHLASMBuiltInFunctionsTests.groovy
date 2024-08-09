package org.z390.test

import org.junit.jupiter.api.Test

class RunHLASMBuiltInFunctionsTests extends z390Test {

    /*
     * Test the High Level Assembler (HLASM) built-in functions
     */
     
    var options = ["SYSMAC(${basePath("mac")})"]

    @Test
    void test_TESTOPR2() {
        
        // Tests that use MNOTE, AIF to check results; no errors
        
        int rc = this.asmlg(basePath("rt", "test", "TESTOPR2"), *options)
        this.printOutput()
        assert rc == 0
    }
    
    @Test
    void test_TOPR2() {

        // Tests that use code to check results; no errors
        
        // Where report of test done using code is written
        
        this.env.put('REPORT', basePath('rt', 'mlc', 'TOPR2.TST'))
        
        int rc = this.asmlg(basePath("rt", "mlc", "TOPR2"), *options)
        this.printOutput()
        assert rc == 0
    }
    
    @Test
    void test_C2BDX00() {

        // Tests for C2B. C2D. C2X with X'00' in argument; no errors
        
        int rc = this.asm(basePath("rt", "mlc", "C2BDX00"), *options)
        this.printOutput()
        assert rc == 0
    }
    
    @Test
    void test_A2BE1() {

        // Error tests for A2B; 1 invalid self-defining term; rc = 12
        
        int rc = this.asm(basePath("rt", "mlc", "A2BE1"), *options)
        this.printOutput()
        assert rc == 12
    }
    
    @Test
    void test_A2BE2() {

        // Error tests for A2B; 1 arithmetic overflow; rc = 12
        
        int rc = this.asm(basePath("rt", "mlc", "A2BE2"), *options)
        this.printOutput()
        assert rc == 12
    }
    
    @Test
    void test_ISBINE1() {

        // Error tests for ISBIN; 1 invalid operand value - length is 0; rc = 8
        
        int rc = this.asm(basePath("rt", "mlc", "ISBINE1"), *options)
        this.printOutput()
        assert rc == 8
    }
    
    @Test
    void test_ISDECE1() {

        // Error tests for ISDEC; 1 invalid operand value - length is 0; rc = 8
        
        int rc = this.asm(basePath("rt", "mlc", "ISDECE1"), *options)
        this.printOutput()
        assert rc == 8
    }
    
    @Test
    void test_ISHEXE1() {

        // Error tests for ISHEX; 1 invalid operand value - length is 0; rc = 8
        
        int rc = this.asm(basePath("rt", "mlc", "ISHEXE1"), *options)
        this.printOutput()
        assert rc == 8
    }
    
    @Test
    void test_ISSYME1() {

        // Error tests for ISSYM; 1 invalid operand value - length is 0; rc = 8
        
        int rc = this.asm(basePath("rt", "mlc", "ISSYME1"), *options)
        this.printOutput()
        assert rc == 8
    }
    
    @Test
    void test_SLAE1() {

        // Error tests for SLA; 10 arithmetic overflows; rc = 8
        
        int rc = this.asm(basePath("rt", "mlc", "SLAE1"), *options)
        this.printOutput()
        assert rc == 8
    }
}
