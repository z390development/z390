# Suppress Invoke-WebRequest's built-in progress bar
$ProgressPreference = 'SilentlyContinue'

# Request latest release information
$apiUrl = 'https://api.github.com/repos/checkstyle/checkstyle/releases/latest'
$release = Invoke-RestMethod -Uri $apiUrl -Headers @{ 'User-Agent' = 'z390-build' }

# Tag is usually "checkstyle-13.4.2"
$tag = $release.tag_name

$asset = $release.assets | Where-Object { $_.name -match '-all\.jar$' } | Select-Object -First 1
if (-not $asset) {
    Write-Host "No -all.jar asset found for $tag" -ForegroundColor Red
    exit 1
}

$uri     = $asset.browser_download_url
$outFile = "build\checkstyle\checkstyle-all.jar"
$versionFile = 'build\checkstyle\version.txt'

# Issue status message
Write-Host "Downloading linter from checkstyle repository on github.com..."

# Define our own progress bar
Write-Progress -Activity "Download" -Status $uri -PercentComplete 0

# Download linter jar and report results
try {
    Invoke-WebRequest -Uri $uri -OutFile $outFile -UseBasicParsing
    $tag | Set-Content -Path $versionFile -NoNewline
    Write-Progress -Activity "Download" -Completed
    Write-Host "Download finished: $tag"
    exit 0
}
catch {
    Write-Progress -Activity "Download" -Completed
    Write-Host "Download failed: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}