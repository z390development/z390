package org.z390.test

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions


class TestOAttrib extends z390Test {

    var options = ["SYSMAC(${basePath("mac")})"]

    @Test
    void test_OAttrib() {
        //[debug] this.printOutput = true
        int rc = this.asm(basePath('rt', 'mlc', 'ATTRIB$O'), *options)
        this.printOutput()
        assert rc == 0

        // Check that expected OpCodes for each instruction are generated using PRN data
        // Comment at end of assembler line indicates expected opcode output.
        // Comment must be in format of OPC=<expected OpCode> - no spaces in opcode
        // LOCGR    LOCGR    1,2,B'1010'          * OPC=B9E2A012
        // In this example, assertion will check that OpCode B9E2A012 is assigned to this instruction

        var prnData = (String)this.fileData['PRN']

        var opcodeRegex = /^[A-F0-9]{6} (?<actual>[A-F0-9]{4,12})\s*\(\d*\\/\d*\)\d{1,6} .{3,7}\s*(?<code>[A-Z]{3,8}).*\* OPC=(?<expected>[swA-F0-9]{4,12})$/
        var linesToTest = prnData.readLines().grep( ~opcodeRegex )
        println("Found ${linesToTest.size()} opcode tests")
        linesToTest.each {
            var matcher = it =~ opcodeRegex
            matcher.matches()
            Assertions.assertEquals(
                matcher.group("expected"),
                matcher.group("actual"),
                "Check ${matcher.group("code")} opcode")
        }

        // Check that expected O Attrib codes are returned using PRN data
        // Comment at end of assembler line indicates expected O attrib output.
        // Comment must be in format of * <expected O Attrib>
        //(1/60)91          SHOWTYPE SHOWTYPE             * M
        //(1/19)92+         MNOTE 'O'SHOWTYPE = M'
        // In this example, assertion will check that MNOTE 'O'SHOWTYPE = M because prior line comment is M

        String line1Regex = /^[A-F0-9]{6} *\(\d*\/\d*\)\d{1,6} *(?<request>SHOW(OP|TYPE)) (?<code>[A-Z]{1,8}).*\* (?<expected>[AEMSU])$/
        String line2Regex = /^[A-F0-9]{6} *\(\d*\/\d*\)\d{1,6}\+ *MNOTE 'O'(?<code>[A-Z]{1,8}) = (?<actual>[AEMSU])'$/
        var outputLines = prnData.readLines()
        for(int i in 0..outputLines.size()){
            var line1Matcher = outputLines[i] =~ line1Regex
            if(line1Matcher.matches()){
                //[debug] println("Line ${i} = [${outputLines[i]}]")
                // Get next line and match
                var line2Matcher = outputLines[i+1] =~ line2Regex
                if(line2Matcher.matches()){
                    //[debug] println("Line ${i+1} = [${outputLines[i+1]}]")
                    Assertions.assertEquals(
                        line1Matcher.group("expected"),
                        line2Matcher.group("actual"),
                        "Check ${line1Matcher.group("request")} ${line1Matcher.group("code")} O Attrib")
                }
            }
        }
    }
}
