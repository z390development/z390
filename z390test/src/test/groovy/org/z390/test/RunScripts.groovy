package org.z390.test

import org.junit.jupiter.api.Test

class RunScripts extends z390Test {

    var options = ['noloadhigh', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]

    @Test
    void test_BATCH() {
        env = ['RTSCRIPT': basePath('rt', 'rt', "BATCH.RT")]
        int rc = this.asmlg(basePath("demo", "HELLO"), *options, "test(RTSCRIPT)")
        this.printOutput()
        assert rc == 16
        assert this.stdout =~ /EZ390E error 109 quitting test mode/, "STDOUT did not contain expected quit message error 109"
    }
}
