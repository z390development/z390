package org.z390.test

import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.TestInstance.Lifecycle

import static org.junit.jupiter.api.Assumptions.assumeTrue

@TestInstance(Lifecycle.PER_CLASS)
class RunVsam1 extends z390Test {

    var sysmac = "SYSMAC(${basePath('mac')})"
    var syscpy = "SYSCPY(${basePath('mac')})"
    var opt1 = [sysmac, syscpy, "@${basePath('rt', 'vsam', 'RTVSAM1.OPT')}"]
    var opt0 = [sysmac, syscpy, "@${basePath('rt', 'vsam', 'RTVSAM0.OPT')}"]
    var myesds = basePath('rt', 'vsam', 'BLDCAT1.ESDS_F')
    var catalogModule = basePath('rt', 'vsam', 'BLDCAT1.390')

    @BeforeAll
    void buildCatalog() {
        // Step 1: build the catalog (required by BLDESDS0 tests below)
        int rc = this.asml(basePath('rt', 'vsam', 'BLDCAT1'), *opt1)
        this.printOutput('buildCatalog')
        assert rc == 0
    }

    @Test
    void test_BLDESDS0() {
        assumeTrue(new File(catalogModule).exists(),
                'BLDCAT1 catalog must exist (built by buildCatalog)')

        // Validate failure to assemble under zVSAM(0) option
        int rc1 = this.asm(basePath('rt', 'vsam', 'BLDESDS0'), *opt0)
        this.printOutput()
        assert rc1 == 15

        // Validate cluster creation under zVSAM(1) option
        env.put('MYESDS', myesds)
        int rc2 = this.asmlg(basePath('rt', 'vsam', 'BLDESDS0'), *opt1)
        this.printOutput()
        assert rc2 == 0
    }
}
