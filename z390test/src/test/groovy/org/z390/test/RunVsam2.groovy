package org.z390.test

import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestFactory

import static org.junit.jupiter.api.DynamicTest.dynamicTest

class RunVsam2 extends z390Test {

    var options = ['trace', 'noloadhigh', 'zvsam(2)', "SYSMAC(${basePath("mac")})", "SYSCPY(${basePath("mac")})"]
    var sys390 = "SYS390(${basePath('vsam2', 'mlc')}+${basePath('linklib')})"
    var zreproRunOpts = ['DUMP', 'zVSAM(2)', 'STATS', 'noloadhigh', sys390]
    // Absolute: Groovy File I/O (JUnit CWD is still z390test/)
    var dataDirAbs = basePath('vsam2', 'data')
    // Relative to repo root: ez390 env (callZ390 workdir = project_root)
    var catRel = pathJoin('vsam2', 'mlc')
    var dataRel = pathJoin('vsam2', 'data')
    var zrepro = basePath('vsam2', 'mlc', 'ZREPRO')

    @Test
    void test_TESTGNCB() {
        int rc = this.asmlg(basePath("vsam2", "mlc", "TESTGNCB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTMDCB() {
        int rc = this.asmlg(basePath("vsam2", "mlc", "TESTMDCB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTSHCB() {
        int rc = this.asmlg(basePath("vsam2", "mlc", "TESTSHCB"), *options)
        this.printOutput()
        assert rc == 0
    }
    @Test
    void test_TESTTSCB() {
        int rc = this.asmlg(basePath("vsam2", "mlc", "TESTTSCB"), *options)
        this.printOutput()
        assert rc == 0
    }

    // zREPRO LOAD/UNLOAD/VERIFY/BUILDAIX cases

    void deleteVsamFiles(String name) {
        new File(pathJoin(dataDirAbs, "${name}.DTA")).delete()
        new File(pathJoin(dataDirAbs, "${name}.IDX")).delete()
    }

    String vsamDsn(String cluster) {
        return pathJoin(dataRel, "Z390CAT2.${cluster}")
    }

    String qsamDsn(String fileName, String recfm) {
        return pathJoin(dataRel, fileName) + "[RECFM=${recfm}]"
    }

    void runZrepro(String parm, String... extraOpts) {
        int rc = this.ez390(zrepro, "PARM(${parm})", *extraOpts, *zreproRunOpts)
        this.printOutput()
        assert rc == 0, "zREPRO PARM(${parm}) failed rc=${rc}"
    }

    void assertUnloadMatches(String inputName, String unloadName, String label) {
        def expectedFile = new File(pathJoin(dataDirAbs, inputName))
        def actualFile = new File(pathJoin(dataDirAbs, unloadName))
        assert actualFile.exists(), "${label}: unload file missing ${actualFile.path}"
        assert expectedFile.readLines() == actualFile.readLines(),
                "${label}: unload ${unloadName} does not match input ${inputName}"
    }

    void runLoadTest(Map loadCase) {
        def aixNames = (loadCase.aix ?: []) as List
        def label = loadCase.name as String
        def catLoad = catRel + File.separator

        deleteVsamFiles(loadCase.cluster as String)
        aixNames.each { deleteVsamFiles(it as String) }

        this.env = [
                'INQSAM'  : qsamDsn(loadCase.inFile as String, loadCase.recfm as String),
                'OUTVSAM' : vsamDsn(loadCase.cluster as String),
                'Z390CAT2': catLoad
        ]
        runZrepro('LOAD')

        this.env = [
                'INVSAM'  : vsamDsn(loadCase.cluster as String),
                'OUTQSAM' : qsamDsn(loadCase.outFile as String, loadCase.recfm as String),
                'Z390CAT2': catRel
        ]
        runZrepro('UNLOAD')
        assertUnloadMatches(loadCase.inFile as String, loadCase.outFile as String, label)

        this.env = [
                'INVSAM'  : vsamDsn(loadCase.cluster as String),
                'Z390CAT2': catLoad
        ]
        runZrepro('VERIFY')

        aixNames.each { aixName ->
            deleteVsamFiles(aixName as String)
            this.env = [
                    'INVSAM'  : vsamDsn(loadCase.cluster as String),
                    'OUTVSAM' : vsamDsn(aixName as String),
                    'Z390CAT2': catLoad
            ]
            runZrepro('BUILDAIX', 'NOTIME', 'MEM(16)')
        }
    }

    @TestFactory
    Collection<DynamicTest> test_zREPRO_load() {
        var tests = []
        var loadCases = [
                [name: 'TSTLOAD01', desc: 'ESDS-V + AIX',       cluster: 'MYFILE01', inFile: 'MYFILE1V.TXT', outFile: 'MYFILE1V.TXO', recfm: 'VT', aix: ['MYAIX11']],
                [name: 'TSTLOAD02', desc: 'ESDS-F + AIX',       cluster: 'MYFILE02', inFile: 'MYFILE2F.TXT', outFile: 'MYFILE2F.TXO', recfm: 'FT', aix: ['MYAIX2U', 'MYAIX21']],
                [name: 'TSTLOAD03', desc: 'KSDS-V + AIX',       cluster: 'MYFILE03', inFile: 'MYFILE1V.TXT', outFile: 'MYFILE3V.TXO', recfm: 'VT', aix: ['MYAIX31']],
                [name: 'TSTLOAD06', desc: 'RRDS-F',             cluster: 'MYFILE06', inFile: 'MYFILE6R.TXT', outFile: 'MYFILE6R.TXO', recfm: 'FT'],
                [name: 'TSTLOAD07', desc: 'RRDS-V',             cluster: 'MYFILE07', inFile: 'MYFILE6R.TXT', outFile: 'MYFILE7R.TXO', recfm: 'VT'],
                [name: 'TSTLOAD08', desc: 'KSDS-F',             cluster: 'MYFILE08', inFile: 'MYFILE8F.TXT', outFile: 'MYFILE8F.TXO', recfm: 'FT'],
                [name: 'TSTLOAD09', desc: 'KSDS-F + AIX',       cluster: 'MYFILE09', inFile: 'MYFILE9F.TXT', outFile: 'MYFILE9F.TXO', recfm: 'FT', aix: ['MYAIXWD']],
                [name: 'TSTLOAD10', desc: 'ESDS-VS + AIX',      cluster: 'MYFILE10', inFile: 'MYFILEAV.TXT', outFile: 'MYFILE10.TXO', recfm: 'VT', aix: ['MYAIXA1', 'MYAIXAU']],
                [name: 'TSTLOAD11', desc: 'RRDS-VS',            cluster: 'MYFILE11', inFile: 'MYFILEAV.TXT', outFile: 'MYFILE11.TXO', recfm: 'VT'],
                [name: 'TSTLOAD12', desc: 'KSDS-VS + AIX',      cluster: 'MYFILE12', inFile: 'MYFILEAV.TXT', outFile: 'MYFILE12.TXO', recfm: 'VT', aix: ['MYAIXC1']],
                [name: 'TSTLOAD13', desc: 'KSDS-FS + AIX',      cluster: 'MYFILE13', inFile: 'MYFILEDV.TXT', outFile: 'MYFILEDV.TXO', recfm: 'FT', aix: ['MYAIXD1']],
                [name: 'TSTLOADNK', desc: 'KSDS-F (null file)', cluster: 'NULLKSDS', inFile: 'NULLFILE.TXT', outFile: 'NULLKSDS.TXO', recfm: 'FT', aix: ['NULLAIX']],
                [name: 'TSTLOADNE', desc: 'ESDS-F (null file)', cluster: 'NULLESDS', inFile: 'NULLFILE.TXT', outFile: 'NULLESDS.TXO', recfm: 'FT'],
                [name: 'TSTLOADNR', desc: 'RRDS-F (null file)', cluster: 'NULLRRDS', inFile: 'NULLFILE.TXT', outFile: 'NULLRRDS.TXO', recfm: 'FT']
        ]
        loadCases.each { loadCase ->
            tests.add(dynamicTest("test zREPRO ${loadCase.name} (${loadCase.desc})", () -> runLoadTest(loadCase)))
        }
        return tests
    }

    // TSTLOADIN: F-KSDS + AIX, using local vsam2/data/COMPANIES.TXT (not z390Archive)
    @Test
    void test_TSTLOADIN() {
        def catLoad = catRel + File.separator
        deleteVsamFiles('INDIANCO')
        deleteVsamFiles('MYAIXIN')

        this.env = [
                'INQSAM'  : qsamDsn('COMPANIES.TXT', 'FT'),
                'OUTVSAM' : vsamDsn('INDIANCO'),
                'Z390CAT2': catLoad
        ]
        runZrepro('LOAD', 'TIME(600)', 'MAXSIZE(1000)')

        this.env = [
                'INVSAM'  : vsamDsn('INDIANCO'),
                'OUTQSAM' : qsamDsn('COMPANIES.TXO', 'FT'),
                'Z390CAT2': catRel
        ]
        runZrepro('UNLOAD', 'MAXSIZE(1000)')
        assertUnloadMatches('COMPANIES.TXT', 'COMPANIES.TXO', 'TSTLOADIN')

        this.env = [
                'INVSAM'  : vsamDsn('INDIANCO'),
                'Z390CAT2': catLoad
        ]
        runZrepro('VERIFY')

        this.env = [
                'INVSAM'  : vsamDsn('INDIANCO'),
                'OUTVSAM' : vsamDsn('MYAIXIN'),
                'Z390CAT2': catLoad
        ]
        runZrepro('BUILDAIX', 'MEM(16)', 'MAXSIZE(1000)')

        this.env = [
                'INVSAM'  : vsamDsn('MYAIXKS'),
                'OUTQSAM' : qsamDsn('MYAIXKS.TXO', 'VT'),
                'Z390CAT2': catRel
        ]
        runZrepro('UNLOAD')

        this.env = [
                'INVSAM'  : vsamDsn('MYAIXKS'),
                'Z390CAT2': catLoad
        ]
        runZrepro('VERIFY')
    }
}
