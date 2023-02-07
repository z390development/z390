package org.z390.test

import org.junit.jupiter.api.Test

class RunZparTests extends z390Test{

    var options = [
            'noloadhigh',
            "SYSMAC(${basePath("mac")})",
            "SYSCPY(${basePath("mac")})",
    ]

    @Test
    void test_ZPAR() {
        int rc
        rc = asmlg(basePath('demo', 'HELLO'), 'TRACE', *options)
        printOutput()
        assert rc == 0

        var macOptions = ['BAL', 'NOASM', 'chksrc(0)', 'chkmac(0)']
        rc = mz390(basePath('zpar', 'ZPARTRS'), *macOptions, "SYSPARM(${basePath('demo', 'HELLO')})")
        assert rc == 0
        loadFile(basePath('demo', 'HELLO.TRS'), 'TRS')
        printOutput()
    }
}
