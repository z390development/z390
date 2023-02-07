package org.z390.test

import org.junit.jupiter.api.Test

class RunZSTRMacTest extends z390Test {
    var convertOptions = ['NOASM', 'STATS', 'NOTIMING']
    var asmOptions = [
            'BAL', 'STATS', 'NOTIMING',
            "SYSMAC(${basePath("mac")})",
            "SYSCPY(${basePath("mac")})",
    ]

    @Test
    void test_ZSTRMAC() {
        /*
        This test checks that the ZSTRMAC1 and ZSTRMAC2 macros work correctly.
        ZSTRMAC1 - Converts structured macros to standard HLASM, written using HLASM
        ZSTRMAC2 - Converts structured macros to standard HLASM, written using Structured macros
         */

        // Step 1: Convert and run structured macro test program TESTSPE1.ZSM using bootstrap
        int rc
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'TESTSPE1.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'TESTSPE1.MLC')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC1'), *convertOptions)
        this.printOutput("Step 1 Convert")
        assert rc == 0
        rc = this.asmlg(basePath('rt', 'test', 'TESTSPE1'), *asmOptions)
        this.printOutput("Step 1 Run")
        assert rc == 0

        // Step 2: Convert and run structured macro test program TESTSPE2.ZSM using bootstrap
        this.setUp()
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'TESTSPE2.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'TESTSPE2.MLC')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC1'), *convertOptions)
        assert rc == 0
        rc = this.asmlg(basePath('rt', 'test', 'TESTSPE2'), *asmOptions)
        this.printOutput("Step 2")
        assert rc == 0

        // Step 3: Convert structured ZSTRMAC2 using bootstrap ZSTRMAC1
        this.setUp()
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'ZSTRMAC2.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'ZSTRMAC2.MLC')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC1'), *convertOptions)
        this.printOutput("Step 3")
        assert rc == 0

        // Step 4: Convert TESTSPE1 using structured ZSTRMAC2
        this.setUp()
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'TESTSPE1.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'TESTSPE1.TXT')
        ]
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC2'), *convertOptions)
        this.printOutput("Step 4")
        assert rc == 0

        // Step 5: Check that bootstrap and structured versions of ZSTRMAC generate same program
        this.setUp()
        this.loadFile(basePath('rt', 'test', 'TESTSPE1.MLC'), 'SPE1_ZSTRMAC1')
        this.loadFile(basePath('rt', 'test', 'TESTSPE1.TXT'), 'SPE1_ZSTRMAC2')
        printOutput("Step 5")
        assert fileData['SPE1_ZSTRMAC1'] == fileData['SPE1_ZSTRMAC2']

        /*
        Instead of generation using the macros, this uses structured macro support built into mz390
        */

        // Step 6: Regen ZSTRMAC2 using mz390 support to ver ZSTRMAC2.txt = mlc
        setUp()
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'ZSTRMAC2.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'ZSTRMAC2.MLC')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC2.ZSM'), *convertOptions)
        this.printOutput("Step 6 Convert ZSTRMAC using mz390")
        assert rc == 0

        setUp()
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'TESTSPE1.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'TESTSPE1.TXT')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC2'), *convertOptions)
        this.printOutput("Step 6 Convert SPE1")
        assert rc == 0

        this.setUp()
        this.loadFile(basePath('rt', 'test', 'TESTSPE1.MLC'), 'TESTSPE1.MLC')
        this.loadFile(basePath('rt', 'test', 'TESTSPE1.TXT'), 'TESTSPE1.TXT')
        printOutput("Step 6 Compare")
        assert fileData['TESTSPE1.MLC'] == fileData['TESTSPE1.TXT']
    }

    @Test
    void test_ACASE() {
        /*
        Test extensions to ACASE added in zstrmac2 C,X,(v1,v2)
        */
        int rc
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'TESTSPE3.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'TESTSPE3.MLC')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC2.ZSM'), *convertOptions)
        assert rc == 0
        rc = this.asmlg(basePath('rt', 'test', 'TESTSPE3'), *asmOptions)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_ZSTRMAC_error_messages() {
        /*
        Test zstrmac error messages
        */
        int rc
        this.env = [
                'SYSUT1': basePath('rt', 'test', 'TESTSPE4.ZSM'),
                'SYSUT2': basePath('rt', 'test', 'TESTSPE4.MLC')
        ]
        println(this.getEnvList())
        rc = this.mz390(basePath('rt', 'test', 'ZSTRMAC2.ZSM'), *convertOptions)
        printOutput()
        assert rc == 8
    }

    @Test
    void test_mz390_SPM() {
        /*
        Test ZSTRMAC SPM's using z390 SPE
        */
        int rc = this.asmlg(basePath('rt', 'test', 'TESTSPM1'), *asmOptions)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_mz390_SPM_genned() {
        /*
        Test ZSTRMAC SPM's macros using gene./rated HLASM compatabile macros
        */
        var stepOptions = [
                "SYSMAC(${basePath('build', 'zstrmac')}+${basePath('mac')})",
                "SYSCPY(${basePath('build', 'zstrmac')}+${basePath('mac')})",
                'BAL', 'NOTIMING', 'STATS']
        int rc = this.asmlg(basePath('rt', 'test', 'TESTSPM1'), *stepOptions)
        this.printOutput()
        assert rc == 0
    }
}