package org.z390.test

import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.TestInstance.Lifecycle


@TestInstance(Lifecycle.PER_CLASS)
class RunSortTests extends z390Test {

    var options = [
            'noloadhigh time(45)',
            "SYSMAC(${basePath("mac")})",
            "SYSCPY(${basePath("mac")})"
    ]

    var runOptions = [
            'time(45)',
            "SYS390(${basePath('linklib')})"
    ]

    @Test
    void test_TESTSORT() {
        this.env.put('SORTIN', basePath('sort', 'TESTSORT.IN[RECFM=FT,LRECL=80]'))
        this.env.put('SORTOUT', basePath('sort', 'TESTSORT.OUT[RECFM=FT]'))
        this.env.put("SYSIN", basePath('sort', 'TESTSORT.INI'))
        int rc = this.ez390(basePath("sort", "SORT"), *runOptions)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_TESTSRT1() {
        env.put("SYSUT2", basePath('sort', 'TESTSRT1.IN'))
        int rc = this.asmlg(basePath("sort", "TESTSRT1"), *options)
        assert rc == 0

        this.env.put('SORTIN', basePath('sort', 'TESTSRT1.IN[RECFM=F,LRECL=20]'))
        this.env.put('SORTOUT', basePath('sort', 'TESTSRT1.OUT'))
        this.env.put("SYSIN", basePath('sort', 'TESTSRT1.INI'))
        rc = this.ez390(basePath("sort", "SORT"), *runOptions)
        this.printOutput()
        assert rc == 0

        env.put("SYSUT1", basePath('sort', 'TESTSRT1.OUT'))
        rc = this.asmlg(basePath("sort", "TESTSRT2"), *options)
        assert rc == 0

        env.put("SYSUT2", basePath('sort', 'TESTSRT3.IN'))
        rc = this.asmlg(basePath("sort", "TESTSRT3"), 'PARM(100000)', *options)
        printOutput()
        assert rc == 0

        this.env.put('SORTIN', basePath('sort', 'TESTSRT3.IN[RECFM=F,LRECL=4]'))
        this.env.put('SORTOUT', basePath('sort', 'TESTSRT3.OUT'))
        this.env.put("SYSIN", basePath('sort', 'TESTSRT3.INI'))
        rc = this.ez390(basePath("sort", "SORT"), 'PARM(1000)', *runOptions)
        this.printOutput()
        assert rc == 0

        env.put("SYSUT1", basePath('sort', 'TESTSRT3.OUT'))
        rc = this.asmlg(basePath("sort", "TESTSRT4"), 'PARM(100000)', *options)
        assert rc == 0

    }

}
