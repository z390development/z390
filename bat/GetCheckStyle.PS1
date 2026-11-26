# GetCheckStyle.PS1: ensure the Checkstyle -all.jar matches the version pinned in src/checkstyle.xml
# -CheckVersionOnly checks the installed jar and does not download
param(
    [switch]$CheckVersionOnly
)
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$root = Join-Path $PSScriptRoot '..'
try {
    Set-Location -LiteralPath $root
}
catch {
    Write-Host "GetCheckStyle: cannot enter repository root: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

$configFile   = 'src\checkstyle.xml'
$metaName     = 'org.z390.checkstyle.version'
$outDir       = 'build\checkstyle'
$outFile      = Join-Path $outDir 'checkstyle-all.jar'
$versionFile  = Join-Path $outDir 'version.txt'

# The metadata element MUST be a single line; see comment in src/checkstyle.xml
if (-not (Test-Path -LiteralPath $configFile -PathType Leaf)) {
    Write-Host "GetCheckStyle: $configFile not found" -ForegroundColor Red
    exit 1
}
$pattern = '^\s*<metadata name="' + [regex]::Escape($metaName) + '" value="([^"]+)"\s*/>\s*$'
$matches = @(Select-String -Path $configFile -Pattern $pattern)
if ($matches.Count -eq 0) {
    Write-Host "GetCheckStyle: pinned version not found in $configFile" -ForegroundColor Red
    Write-Host "Expected one line: <metadata name=`"$metaName`" value=`"checkstyle-X.Y.Z`"/>"
    exit 1
}
if ($matches.Count -gt 1) {
    Write-Host "GetCheckStyle: multiple $metaName metadata lines in $configFile" -ForegroundColor Red
    exit 1
}
$tag = $matches[0].Matches[0].Groups[1].Value

$have = ''
if (Test-Path -LiteralPath $versionFile) {
    try {
        $have = (Get-Content -LiteralPath $versionFile -Raw).Trim()
    }
    catch {
        Write-Host "GetCheckStyle: cannot read ${versionFile}: $($_.Exception.Message)" -ForegroundColor Red
        exit 1
    }
}

# $jarOk will be set to True when checkstyle-all.jar is present, not empty, and version.txt records the pin.
$jarOk = $false
if ((Test-Path -LiteralPath $outFile) -and ($have -eq $tag)) {
    try {
        $installed = Get-Item -LiteralPath $outFile
        if ($installed.Length -gt 0) { $jarOk = $true }
    }
    catch {
        Write-Host "GetCheckStyle: cannot read ${outFile}: $($_.Exception.Message)" -ForegroundColor Red
        exit 1
    }
}
# The installed jar already matches the pin. Nothing to download.
if ($jarOk) {
    Write-Host "Checkstyle $tag OK"
    exit 0
}
# Check-only call: report the mismatch and do not download.
# A call without -CheckVersionOnly skips this block.
if ($CheckVersionOnly) {
    Write-Host "GetCheckStyle: $outFile is missing or does not match pin '$tag'" -ForegroundColor Red
    exit 2
}

# The installed jar is missing, empty, or not the pinned version.
# Download that version. -CheckVersionOnly never reaches this point.
try {
    New-Item -ItemType Directory -Force -Path $outDir | Out-Null
}
catch {
    Write-Host "GetCheckStyle: cannot create ${outDir}: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

$uri     = "https://github.com/checkstyle/checkstyle/releases/download/$tag/$tag-all.jar"
$tmpFile = "$outFile.tmp"

Write-Host "Downloading linter $tag from checkstyle repository on github.com..."
Write-Progress -Activity "Download" -Status $uri -PercentComplete 0
$replaced = $false
try {
    Invoke-WebRequest -Uri $uri -OutFile $tmpFile -UseBasicParsing
    $downloaded = Get-Item -LiteralPath $tmpFile
    if ($downloaded.Length -le 0) {
        throw "downloaded file is empty"
    }
    Move-Item -LiteralPath $tmpFile -Destination $outFile -Force
    $replaced = $true
    $installed = Get-Item -LiteralPath $outFile
    if ($installed.Length -le 0) {
        throw "installed jar is empty"
    }
    Set-Content -LiteralPath $versionFile -Value $tag
    $recorded = (Get-Content -LiteralPath $versionFile -Raw).Trim()
    if ($recorded -ne $tag) {
        throw "version file does not record $tag"
    }
    Write-Progress -Activity "Download" -Completed
    Write-Host "Download finished: $tag"
    exit 0
}
catch {
    Write-Progress -Activity "Download" -Completed
    Remove-Item -LiteralPath $tmpFile -Force -ErrorAction SilentlyContinue
    if ($replaced) {
        Remove-Item -LiteralPath $outFile -Force -ErrorAction SilentlyContinue
        Remove-Item -LiteralPath $versionFile -Force -ErrorAction SilentlyContinue
    }
    Write-Host "Download failed: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}
