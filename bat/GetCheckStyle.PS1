# GetCheckStyle.PS1: install or verify the Checkstyle -all.jar pinned in src/checkstyle.xml
param(
    [switch]$VerifyOnly
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

Set-Location -LiteralPath (Join-Path $PSScriptRoot '..')

$configFile   = 'src\checkstyle.xml'
$metaName     = 'org.z390.checkstyle.version'
$outDir       = 'build\checkstyle'
$outFile      = Join-Path $outDir 'checkstyle-all.jar'
$versionFile  = Join-Path $outDir 'version.txt'

# The metadata element MUST be a single line; see comment in src/checkstyle.xml
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
    $have = (Get-Content -LiteralPath $versionFile -Raw).Trim()
}

if ($VerifyOnly) {
    if (-not (Test-Path -LiteralPath $outFile)) {
        Write-Host "GetCheckStyle: $outFile is missing - run with *All" -ForegroundColor Red
        exit 1
    }
    if ($have -ne $tag) {
        Write-Host "GetCheckStyle: installed '$have' does not match pin '$tag' - run with *All" -ForegroundColor Red
        exit 1
    }
    Write-Host "Checkstyle $tag OK"
    exit 0
}

New-Item -ItemType Directory -Force -Path $outDir | Out-Null

if ((Test-Path -LiteralPath $outFile) -and ($have -eq $tag)) {
    Write-Host "Checkstyle $tag already present"
    exit 0
}

$uri     = "https://github.com/checkstyle/checkstyle/releases/download/$tag/$tag-all.jar"
$tmpFile = "$outFile.tmp"

Write-Host "Downloading linter $tag from checkstyle repository on github.com..."
Write-Progress -Activity "Download" -Status $uri -PercentComplete 0
try {
    Invoke-WebRequest -Uri $uri -OutFile $tmpFile -UseBasicParsing
    Move-Item -LiteralPath $tmpFile -Destination $outFile -Force
    Set-Content -LiteralPath $versionFile -Value $tag
    Write-Progress -Activity "Download" -Completed
    Write-Host "Download finished: $tag"
    exit 0
}
catch {
    Write-Progress -Activity "Download" -Completed
    if (Test-Path -LiteralPath $tmpFile) { Remove-Item -LiteralPath $tmpFile -Force }
    Write-Host "Download failed: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}
