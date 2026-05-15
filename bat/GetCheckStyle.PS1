# Suppress Invoke-WebRequest's built-in progress bar
$ProgressPreference = 'SilentlyContinue'

# Define source URI and target location in current repository clone
$uri     = "https://github.com/checkstyle/checkstyle/releases/download/checkstyle-10.21.4/checkstyle-10.21.4-all.jar"
$outFile = "build\checkstyle\checkstyle-all.jar"

# Issue status message
Write-Host "Downloading linter from checkstyle repository on github.com..."

# Define our own progress bar
Write-Progress -Activity "Download" -Status $uri -PercentComplete 0

# Download linter jar and report results
try {
    Invoke-WebRequest -Uri $uri -OutFile $outFile -UseBasicParsing
    Write-Progress -Activity "Download" -Completed
    Write-Host "Download finished."
    exit 0
}
catch {
    Write-Progress -Activity "Download" -Completed
    Write-Host "Download failed: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}