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
    @Test
    void test_RPI1540() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI1540.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI1540"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI1544() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI1544.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI1544"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI1609() {
        int rc = this.asmlg(basePath("rt", "rt", "RPI1609"), *options)
        this.printOutput()
        assert rc == 12
    }
    @Test
    void test_RPI1625() {
        int rc = this.asmlg(basePath("rt", "rt", "RPI1625"), *options)
        this.printOutput()
        assert rc == 12
    }
    @Test
    void test_RPI1641() {
        int rc = this.asmlg(basePath("rt", "rt", "RPI1641"), *options, "ZVSAM(2)")
        this.printOutput()
        assert rc == 0
        assert fileData.get('LOG') =~ /zVSAM version 2 in z390/, "LOG did not contain expected zVSAM version"
    }
    @Test
    void test_RPI2000() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2000.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2000"), *options, "test(RTSCRIPT)")
        loadFile(basePath("rt", "rt", "RPI2000.TRE"), "TRE")
        this.printOutput()
        assert rc == 0
        assert fileData.get('TRE') =~ /AR0=00000000/, "AR0 content not correct"
        assert fileData.get('TRE') =~ /AR1=00000001/, "AR1 content not correct"
        assert fileData.get('TRE') =~ /AR2=00000002/, "AR2 content not correct"
        assert fileData.get('TRE') =~ /AR3=00000003/, "AR3 content not correct"
        assert fileData.get('TRE') =~ /AR4=00000004/, "AR4 content not correct"
        assert fileData.get('TRE') =~ /AR5=00000005/, "AR5 content not correct"
        assert fileData.get('TRE') =~ /AR6=00000006/, "AR6 content not correct"
        assert fileData.get('TRE') =~ /AR7=00000007/, "AR7 content not correct"
        assert fileData.get('TRE') =~ /AR8=00000008/, "AR8 content not correct"
        assert fileData.get('TRE') =~ /AR9=00000009/, "AR9 content not correct"
        assert fileData.get('TRE') =~ /AR10=0000000A/, "AR10 content not correct"
        assert fileData.get('TRE') =~ /AR11=0000000B/, "AR11 content not correct"
        assert fileData.get('TRE') =~ /AR12=0000000C/, "AR12 content not correct"
        assert fileData.get('TRE') =~ /AR13=0000000D/, "AR13 content not correct"
        assert fileData.get('TRE') =~ /AR14=0000000E/, "AR14 content not correct"
        assert fileData.get('TRE') =~ /AR15=0000000F/, "AR15 content not correct"
        assert fileData.get('TRE') =~ /AR nn       display specified access register else all AR 0-15/, "AR help text incorrect"
    }
    @Test
    void test_RPI2001A() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2001A.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2001A"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2001B() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2001B.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2001B"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2002A() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2002A.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2002A"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2002B() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2002B.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2002B"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2002C() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2002C.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2002C"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2003() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2003.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2003"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2004() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2004.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2004"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2005() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2005.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2005"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2006() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2006.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2006"), *options, 'mem(32)', "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2007() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2007.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2007"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_RPI2008() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "RPI2008.RT")]
        int rc = this.asmlg(basePath("rt", "rt", "RPI2008"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 0
    }
}
