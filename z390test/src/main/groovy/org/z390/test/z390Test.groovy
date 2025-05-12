package org.z390.test

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.TestInfo

class z390Test {

    var project_root = pathJoin('..')
    String stdout = ""
    String stderr = ""
    var env = [:]
    var fileData = [:]
    var message = "z390Test - Test framework for z390 project"
    int cmdTimeMs = 100000
    boolean printOutput = false

    File tempDir = null

    static void main(String[] args) {
        println new z390Test().message
    }

    z390Test() {
        /**
         * class constructor
         */
        if (System.getenv('Z390_PROJECT_ROOT')) {
            this.project_root = new File(System.getenv('Z390_PROJECT_ROOT'))
        }
        if (System.getenv('Z390_PRINT_OUTPUT')) {
            printOutput = true
        }
    }

    @BeforeEach
    void setUp() {
        this.stdout = ""
        this.stderr = ""
        this.fileData = [:]
    }
    @AfterEach
    void cleanUp(TestInfo testInfo) {
        printOutput(testInfo.displayName)
        if (this.tempDir)
            this.tempDir.deleteDir()
    }

    def createTempFile(String fileName, String fileContents, boolean returnExt=true) {
        /**
         * Utility method for creating temp option files used in tests
         */
        if (!tempDir) {
            this.tempDir = File.createTempDir()
            this.tempDir.deleteOnExit()
        }
        var fullFileName = pathJoin(tempDir.absolutePath, fileName)
        println("Creating temp source: ${fullFileName}")
        // to allow files to reference themselves, include {{fullFileName}} in contents
        fullFileName = fullFileName.replace("\\", "\\\\")  // support windows files
        fileContents = fileContents.replaceAll(/\{\{fullFileName}}/, fullFileName)
        new File(fullFileName).with {
            createNewFile()
            write(fileContents)
        }
        String ext = fullFileName.substring(fullFileName.lastIndexOf("."))
        if (!returnExt || ext.toUpperCase() in ['.MLC', '.CBL'])
            fullFileName = filenameWithoutExtension(fullFileName)
        return fullFileName
    }

    String basePath(String... pathItems) {
        var fullPathItems = [this.project_root, *pathItems]
        return new File(fullPathItems.join(File.separator)).getCanonicalPath()
    }

    String basePathRelative(String... pathItems) {
        var fullPathItems = [this.project_root, *pathItems]
        return fullPathItems.join(File.separator)
    }

    static String filenameWithoutExtension(String filename) {
        String result = filename[0..<filename.lastIndexOf('.')]
        return result
    }

    def static pathJoin(String... pathItems) {
        return pathItems.join(File.separator)
    }

    def getEnvList() {
        def envList = []
        this.env.each{item -> envList.add("${item.key}=${item.value}")}
        return envList
    }

    def clean(String asmFileExcludingExtension) {
        /**
         * clear files for module
         */
        var filename_only = new File(asmFileExcludingExtension).name
        var regex = "${filename_only}\\.TR."
        var dirName = new File(asmFileExcludingExtension).getParent().toString()
        var traceFiles = new FileNameByRegexFinder().getFileNames(dirName, regex)
        for (filename in traceFiles) {
            new File(filename).delete()
        }

        for (ext in ["PRN", "ERR", "OBJ", "390", "OBJ", "LOG", "LST", "STA"]) {
            var filename = asmFileExcludingExtension + '.' + ext
            var deleteFile = new File(filename)
            if (deleteFile.exists()) {
                deleteFile.delete()
            }
        }
    }

    def callZ390(String asmFileExcludingExtension, String command, String... args) {
        println("Executing ${command}: ${asmFileExcludingExtension}")
        var cmd = ["java", "-classpath", basePath('z390.jar'),
                   '-Xrs', '-Xms150000K', '-Xmx150000K', command, asmFileExcludingExtension, *args].join(" ")
        println(cmd)
        var proc = cmd.execute(this.getEnvList(), null)   // , workDir);
        var sout = new StringBuilder()
        var serr = new StringBuilder()
        proc.consumeProcessOutput(sout, serr)
        proc.waitForOrKill(this.cmdTimeMs)
        this.stdout += sout
        this.stderr += serr
        return proc.exitValue()
    }

    def getOutput(String asmFileExcludingExtension) {
        for (String ext in ["PRN", "ERR", "LOG", "LST"]) {
            var filename = pathJoin(asmFileExcludingExtension + '.' + ext)
            this.loadFile(filename, ext)
        }
    }

    def loadFile(String filename, String label) {
        var outFile = new File(filename)
        if (outFile.exists()) {
            println("reading ${filename}")
            this.fileData[label] = outFile.text
        }
    }

    static String captureFile(data, label) {
        String linefeed = /\r\n|\n/
        String[] lines = data.toString().split(linefeed)
        def sb = new StringBuilder()
        sb << ("*" * 20) + " ${label} (${lines.length} lines) " + ("*" * 20) + "\n"
        lines.eachWithIndex{ String line, int lineNum ->
            sb << "${String.format('%05d',lineNum)}  ${line}\n"
        }
        return sb.toString()
    }

    def printOutput(String testName="", String... args) {

        // If called without testName, just ignore. Legacy usage
        if (testName == "") return;

        def output = new StringBuilder()
        output << "======================= TEST INFO =======================\n"
        output << "Test Name : ${testName}\n"
        output << "Timestamp : ${new Date()}\n"
        output << "=========================================================\n"

        output << captureFile(this.stdout, 'stdout')
        output << captureFile(this.stderr, 'stderr')
        this.fileData.each { fileExt, data ->
            output << captureFile(data, fileExt)
        }
        
        if (printOutput) {
            print output.toString()
        }
        
        // Write to file
        def outFile = new File("build/z390test-output.txt")
        outFile.parentFile.mkdirs()
        outFile << output.toString()
    }

    def mz390(Map kwargs=[:], String asmFilename, String... args) {
        return asm(kwargs, asmFilename, args)
    }

    def asm(Map kwargs=[:], String asmFilename, String... args) {
        /**
         * Assemble
         */
        this.clean(asmFilename)
        int rc = this.callZ390(asmFilename, 'mz390', args)
        this.getOutput(asmFilename)
        return rc
    }

    def lz390(Map kwargs=[:], String asmFilename, String... args) {
        /**
         * lz390
         * !! NOTE !! Does not clean intermediate files
         */
        int rc = this.callZ390(asmFilename, 'lz390', args)
        this.getOutput(asmFilename)
        return rc
    }

    def asml(Map kwargs=[:], String asmFilename, String... args) {
        /**
         * Assemble and link
         */
        this.clean(asmFilename)
        int rc = this.callZ390(asmFilename, 'mz390', args)
        if (rc == 0) {
            rc = this.callZ390(asmFilename, 'lz390', args)
        }
        this.getOutput(asmFilename)
        return rc
    }

    def ez390(Map kwargs=[:], String asmFilename, String... args) {
        /**
         * ez390
         * !! NOTE !! Does not clean intermediate files
         */
        int rc = this.callZ390(asmFilename, 'ez390', args)
        this.getOutput(asmFilename)
        return rc
    }

    def asmlg(Map kwargs=[:], String asmFilename, String... args) {
        /**
         * Assemble, link and go
         */
        this.clean(asmFilename)
        int rc = this.callZ390(asmFilename, 'mz390', args)
        if (rc == 0) {
            rc = this.callZ390(asmFilename, 'lz390', args)
            if (rc == 0) {
                rc = this.callZ390(asmFilename, 'ez390', args)
            }
        }
        this.getOutput(asmFilename)
        return rc
    }

    def cblc(Map kwargs=[:], String cobFilename, String... args) {
        /**
         * cobol compile
         */
        this.clean(cobFilename)
        int rc
        rc = this.callZ390(cobFilename, 'zc390', *args)
        if (rc != 0) return rc  // exit if issue

        var cobfiledir = new File(cobFilename).getParent()
        var cobOptions = ['BAL', 'NOLISTCALL', 'MAXGBL(1500000)',
                          "SYSMAC(${basePath('zcobol', 'mac')}+${basePath('mac')})".toString(),
                          "SYSCPY(${cobfiledir}+${basePath('zcobol', 'cpy')})".toString()
        ]
        rc = this.callZ390(cobFilename, 'mz390', *(cobOptions + args.toList()))
        return rc
    }

    def cblclg(Map kwargs=[:], String cobFilename, String... args) {
        /**
         * cobol compile link and go
         */
        this.clean(cobFilename)
        int rc
        rc = this.callZ390(cobFilename, 'zc390', *args)
        if (![0, 4].contains(rc)) return rc  // exit if issue

        var cobfiledir = new File(cobFilename).getParent()
        var cobOptions = ['BAL', 'NOLISTCALL', 'MAXGBL(1500000)',
                      "SYSMAC(${basePath('zcobol', 'mac')}+${basePath('mac')})".toString(),
                      "SYSCPY(${cobfiledir}+${basePath('zcobol', 'cpy')})".toString()
        ]
        var runOptions = ["SYS390(${basePath('zcobol', 'lib')}+${cobfiledir})"]

        rc = this.callZ390(cobFilename, 'mz390', *(cobOptions + args.toList()))
        if (rc == 0) {
            rc = this.callZ390(cobFilename, 'lz390', *args)
            if (rc == 0) {
                rc = this.callZ390(cobFilename, 'ez390', *runOptions)
            }
        }
        return rc
    }
}
