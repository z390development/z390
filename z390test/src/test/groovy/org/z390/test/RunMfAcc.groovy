package org.z390.test

import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.Test

import static org.junit.jupiter.api.DynamicTest.dynamicTest

class RunMfAcc extends z390Test{

    /** Helper for SNAP comparison: True for SNAP header lines. */
    static boolean isSnapHeader(String line) {
        line ==~ /(?i)SNAP DUMP.*/
    }

    /** Helper for SNAP comparison: True for standard SNAP hex dump lines (keeps load address). */
    static boolean isSnapDataLine(String line) {
        line ==~ /^\s+[0-9A-Fa-f]{8}\s+\*.*/
    }

    /**
     * From LOG text: from first SNAP header through last SNAP data line.
     * Stops at first line that is neither header nor data (EZ390 trailer, errors, etc.).
     */
    static List<String> extractSnapLines(String logText) {
        def lines = logText.readLines()
        int start = lines.findIndexOf { isSnapHeader(it) }
        if (start < 0) {
            return []
        }
        def result = []
        for (int i = start; i < lines.size(); i++) {
            def line = lines[i]
            if (isSnapHeader(line) || isSnapDataLine(line)) {
                result << line
            } else {
                break   // trailer / non-SNAP — do not include
            }
        }
        return result
    }

    var options  = ['noloadhigh bal notiming stats', "SYSMAC(+${basePath('mac')})", "SYSCPY(+${basePath('mfacc')}+${basePath('mac')})", "SYSOBJ(+${basePath('linklib')})"]
    var P5DW1opt = ['bal notiming stats', "SYSMAC(+${basePath('mac')})", "SYSCPY(+${basePath('mfacc')}+${basePath('mac')})"]

    void test_mfacc_module(String moduleName) {
        int rc = this.asmlg(basePath("mfacc", moduleName), *options)
        this.printOutput()
        assert rc == 0
        loadFile(basePath('mfacc', moduleName+".TF1"), 'TF1') // load reference file
        def expected = extractSnapLines(fileData.get('TF1'))  // extract SNAP lines
        def actual   = extractSnapLines(fileData.get('LOG'))  // actual SNAP lines
        assert expected == actual
    }

    @TestFactory
    Collection<DynamicTest> test_mfacc() {
        var tests = []
        var modules = [
                'P1C1',    'P1DSH1',  'P1DSH1A', 'P1RAFA1', 'P2MD1',  'P3DW1',    'P3LKM1',   'P3MM1',    'P4AN1',  'P4APN2',  'P4DSH1',
                'P4DW1',   'P4RAFA1', 'P4RAFA2', 'P4RJ1',             'P5MM1',    'P6BR1',    'P6MM1',    'P6PJF1', 'P6PL1',   'P6RW1',
                'P7DSH1',  'P7EH1',   'P7RV1',   'P8DSH1',  'P8DSH2', 'P8LM1',    'P8MM1',    'P9DSH1',   'P9MM1',  'P9MM2',   'P10DSH1',
                'P10DSH2', 'P10MB1',  'P11DSH1', 'P11DSH2', 'P11DW1', 'P11FIND1', 'P11FIND2', 'P11MODEL', 'P11WR1', 'P12DSH1', 'P12DSH2',
                'P12DSH3', 'P13DSH1', 'P13SC1',  'P14DSH1', 'P14MW1', 'P15DSH1',  'P15WR1',   'P16DSH1',  'P16WR1', 'P17DW1',  'P17WR1',
                'P18DSH1', 'P19WR1',  'P21DW1',  'P22DSH1', 'P22FS1', 'P22GH1',   'P22MM1',   'P22RL1'
        ]
        modules.each {
            module -> tests.add(
                    dynamicTest("test MFACC ${module}", () -> test_mfacc_module(module)))
        }
        return tests
    }

    @Test
    void test_P5DW1() {
        int rc = this.asmlg(basePath("mfacc", "P5DW1"), *P5DW1opt)
        this.printOutput()
        assert rc == 0
        loadFile(basePath('mfacc', "P5DW1.TF1"), 'TF1')       // load reference file
        def expected = extractSnapLines(fileData.get('TF1'))  // extract SNAP lines
        def actual   = extractSnapLines(fileData.get('LOG'))  // actual SNAP lines
        assert expected == actual
    }

    @Test
    void test_P16AFK1() {
        this.env = [
             'SYSOUT' : basePath('mfacc', 'P16AFK1.OUT')
        ]
        int rc = this.asmlg(basePath("mfacc", "P16AFK1"), *options)
        this.printOutput()
        assert rc == 0
    }

    @Test
    void test_P20WR1() {
        int rc = this.asml(basePath("mfacc", "P20WR1"), *options)
        this.printOutput()
        assert rc == 0
    }
}
