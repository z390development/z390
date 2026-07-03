package org.z390.test

import org.junit.jupiter.api.Test

class RunScripts extends z390Test {

    var options = ['noloadhigh', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]

    @Test
    // Test generated Q command when EOF is reached; then test normal end when z command is present
    void test_BATCH() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "BATCH.RT")]
        int rc1 = this.asmlg(basePath("demo", "HELLO"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc1 == 16
        assert this.stdout =~ /EZ390E error 109 quitting test mode/, "STDOUT did not contain expected quit message error 109"

        env = ['RTSCRIPT': basePath('rt', 'rt', "BATCHZ.RT")]
        int rc2 = this.asmlg(basePath("demo", "HELLO"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc2 == 0
    }

    @Test
    void test_OPCD_DOS() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "OPCD_DOS.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "OPCD_DOS"), *options, "@${basePath("rt", "rt", "OPCD_DOS.OPT")}", "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_OPCD_ERR() {
        int rc = this.asmlg(basePath("rt", "rt", "OPCD_ERR"), *options, "@${basePath("rt", "rt", "OPCD_ERR.OPT")}")
        this.printOutput()
        assert rc == 12
        // Compare AZ390 diagnostic lines only (filter out MZ390 framing + FID= path)
        loadFile(basePath("rt", "rt", "OPCD_ERR.TF1"), 'TF1')
        def expected = fileData.get('TF1').readLines()
        def errLines = fileData.get('ERR').readLines()
                .findAll { !it.contains('MZ390') && !it.contains('FID=') }
        assert expected == errLines, "OPCD_ERR.ERR diagnostics differ from OPCD_ERR.TF1"
    }
    @Test
    void test_OPCD_UNI() {
        int rc = this.asml(basePath("rt", "rt", "OPCD_UNI"), *options, "@${basePath("rt", "rt", "OPCD_UNI.OPT")}")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_OPCD_XA() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "OPCD_XA.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "OPCD_XA"), *options, "@${basePath("rt", "rt", "OPCD_XA.OPT")}", "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_OPCD_ZZZ() {
        int rc = this.asmlg(basePath("rt", "rt", "OPCD_ZZZ"), *options, "@${basePath("rt", "rt", "OPCD_ZZZ.OPT")}")
        this.printOutput()
        assert rc == 0
    }
}
