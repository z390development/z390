# check_text_files.ps1: report control bytes and missing final line endings.
#
# A file is in scope when this repository's .gitattributes marks it as plain
# text. text=auto counts as text: Git's content probe is ignored, because
# that probe misclassifies EBCDIC files. -text files are out of scope.
# Setting eol also marks a file as text, which is how Git treats it.
#
# Zero-byte files are ignored. Every other in-scope file is reported when:
#
# * it contains a byte below 0x20 other than CR (0x0D) or LF (0x0A)
# * it does not end with a line-end marker (CR, LF, or CRLF)
#
# CR and LF are the line endings accepted above, so they are not also reported
# as unexpected control bytes. Every other byte below 0x20 is, including NUL,
# HT (tab), and SUB (Ctrl-Z). Each hit reports the count and the first offset.
#
# Tracked paths come from git ls-files. Attribute values come from
# git check-attr so matching follows Git's .gitattributes rules. System
# and global attribute files are not consulted.
#
#   powershell -NoProfile -ExecutionPolicy Bypass -File bat\check_text_files.ps1
#
# Exit 0 when nothing was reported, 1 when a control byte, a missing line
# ending, an unreadable path, or a git failure was reported, 2 when this
# directory is not a checkout with .gitattributes.

[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$ControlName = @(
    'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL',
    'BS',  'HT',  'LF',  'VT',  'FF',  'CR',  'SO',  'SI',
    'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB',
    'CAN', 'EM',  'SUB', 'ESC', 'FS',  'GS',  'RS',  'US'
)

# Compiled so the per-file scan is not a PowerShell byte loop.
$scanSource = @'
using System;
using System.Collections.Generic;

namespace Z390.TextCheck
{
    public sealed class ControlHit
    {
        public int Value;
        public int Count;
        public int First;
    }

    public static class Scanner
    {
        public static ControlHit[] Scan(byte[] data)
        {
            int[] counts = new int[32];
            int[] first = new int[32];
            for (int v = 0; v < 32; v++)
                first[v] = -1;
            for (int i = 0; i < data.Length; i++)
            {
                int v = data[i];
                if (v >= 0x20 || v == 0x0A || v == 0x0D)
                    continue;
                if (counts[v] == 0)
                    first[v] = i;
                counts[v]++;
            }
            var hits = new List<ControlHit>();
            for (int v = 0; v < 32; v++)
            {
                if (counts[v] == 0)
                    continue;
                hits.Add(new ControlHit { Value = v, Count = counts[v], First = first[v] });
            }
            return hits.ToArray();
        }
    }
}
'@

Add-Type -TypeDefinition $scanSource -Language CSharp

function Write-Out([string]$Text) {
    [Console]::Out.WriteLine($Text)
}

function Write-Err([string]$Text) {
    [Console]::Error.WriteLine($Text)
}

function Get-RepoRoot {
    $here = $PSScriptRoot
    if (-not $here) {
        $here = (Get-Location).ProviderPath
    }
    $candidate = Get-Item -LiteralPath $here
    while ($null -ne $candidate) {
        $git = Join-Path $candidate.FullName '.git'
        $attr = Join-Path $candidate.FullName '.gitattributes'
        if ((Test-Path -LiteralPath $git) -and (Test-Path -LiteralPath $attr -PathType Leaf)) {
            return $candidate.FullName
        }
        $candidate = $candidate.Parent
    }
    return $here
}

# Keep a byte[] intact. PowerShell unrolls a returned byte[] into single bytes.
function Split-OnNul([byte[]]$Data) {
    $parts = New-Object 'System.Collections.Generic.List[byte[]]'
    if ($null -eq $Data -or $Data.Length -eq 0) {
        return ,$parts
    }
    $start = 0
    for ($i = 0; $i -lt $Data.Length; $i++) {
        if ($Data[$i] -ne 0) {
            continue
        }
        $len = $i - $start
        $chunk = New-Object byte[] $len
        if ($len -gt 0) {
            [Array]::Copy($Data, $start, $chunk, 0, $len)
        }
        $parts.Add($chunk)
        $start = $i + 1
    }
    if ($start -lt $Data.Length) {
        $len = $Data.Length - $start
        $chunk = New-Object byte[] $len
        [Array]::Copy($Data, $start, $chunk, 0, $len)
        $parts.Add($chunk)
    }
    return ,$parts
}

function Convert-GitText([byte[]]$Data) {
    if ($null -eq $Data -or $Data.Length -eq 0) {
        return ''
    }
    # Git for Windows can prefix the .gitattributes path with a UTF-8 BOM when
    # check-attr stdout is a pipe. Drop that marker so the path still matches
    # git ls-files.
    $start = 0
    if ($Data.Length -ge 3 -and $Data[0] -eq 0xEF -and $Data[1] -eq 0xBB -and $Data[2] -eq 0xBF) {
        $start = 3
    }
    return [System.Text.Encoding]::UTF8.GetString($Data, $start, $Data.Length - $start)
}

function Invoke-GitRaw {
    param(
        [Parameter(Mandatory)][string]$Repo,
        [Parameter(Mandatory)][string[]]$GitArgs,
        [byte[]]$Stdin
    )

    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = 'git'
    $psi.Arguments = ((@('-c', 'core.attributesFile=') + $GitArgs) -join ' ')
    $psi.WorkingDirectory = $Repo
    $psi.UseShellExecute = $false
    $psi.CreateNoWindow = $true
    $psi.RedirectStandardInput = $true
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    # Environment is inherited. Drop overrides that would scan a different tree,
    # then pin attribute lookup to this repository.
    foreach ($name in @('GIT_DIR', 'GIT_WORK_TREE')) {
        if ($psi.EnvironmentVariables.ContainsKey($name)) {
            $psi.EnvironmentVariables.Remove($name)
        }
    }
    $psi.EnvironmentVariables['GIT_ATTR_NOSYSTEM'] = '1'
    $psi.EnvironmentVariables['GIT_TERMINAL_PROMPT'] = '0'

    $proc = New-Object System.Diagnostics.Process
    $proc.StartInfo = $psi
    try {
        try {
            [void]$proc.Start()
        }
        catch {
            Write-Err "git $($GitArgs -join ' ') failed: $($_.Exception.Message)"
            exit 1
        }

        $stdout = New-Object System.IO.MemoryStream
        $stderr = New-Object System.IO.MemoryStream
        try {
            # Start reading before stdin is written, so a large path list cannot deadlock.
            $outTask = $proc.StandardOutput.BaseStream.CopyToAsync($stdout)
            $errTask = $proc.StandardError.BaseStream.CopyToAsync($stderr)
            if ($Stdin -and $Stdin.Length -gt 0) {
                $proc.StandardInput.BaseStream.Write($Stdin, 0, $Stdin.Length)
            }
            $proc.StandardInput.Close()
            $proc.WaitForExit()
            $outTask.Wait()
            $errTask.Wait()
            $errBytes = $stderr.ToArray()
            $outBytes = $stdout.ToArray()
        }
        finally {
            $stdout.Dispose()
            $stderr.Dispose()
        }

        $errText = [System.Text.Encoding]::UTF8.GetString($errBytes).Trim()
        if ($proc.ExitCode -ne 0) {
            Write-Err "git $($GitArgs -join ' ') failed ($($proc.ExitCode)): $errText"
            exit 1
        }
        return ,$outBytes
    }
    finally {
        $proc.Dispose()
    }
}

function Get-TrackedFiles([string]$Repo) {
    $raw = Invoke-GitRaw -Repo $Repo -GitArgs @('ls-files', '-s', '-z')
    $entries = New-Object System.Collections.Generic.List[object]
    foreach ($record in (Split-OnNul -Data $raw)) {
        if ($record.Length -eq 0) {
            continue
        }
        $tab = -1
        for ($i = 0; $i -lt $record.Length; $i++) {
            if ($record[$i] -eq 9) {
                $tab = $i
                break
            }
        }
        if ($tab -lt 0) {
            Write-Err 'git ls-files returned a record without a path'
            exit 1
        }
        $metaLen = $tab
        $metaBytes = New-Object byte[] $metaLen
        if ($metaLen -gt 0) {
            [Array]::Copy($record, 0, $metaBytes, 0, $metaLen)
        }
        $pathLen = $record.Length - $tab - 1
        $pathBytes = New-Object byte[] $pathLen
        if ($pathLen -gt 0) {
            [Array]::Copy($record, $tab + 1, $pathBytes, 0, $pathLen)
        }
        $mode = ((Convert-GitText $metaBytes) -split ' ', 2)[0]
        $entries.Add([pscustomobject]@{
            Mode      = $mode
            Path      = (Convert-GitText $pathBytes)
            PathBytes = $pathBytes
        })
    }
    return ,$entries
}

function Get-AttributeMap([string]$Repo, $Entries) {
    $info = New-Object System.Collections.Hashtable ([StringComparer]::Ordinal)
    if ($Entries.Count -eq 0) {
        return $info
    }
    $ms = New-Object System.IO.MemoryStream
    try {
        foreach ($entry in $Entries) {
            $ms.Write($entry.PathBytes, 0, $entry.PathBytes.Length)
            $ms.WriteByte(0)
        }
        $payload = $ms.ToArray()
    }
    finally {
        $ms.Dispose()
    }
    $raw = Invoke-GitRaw -Repo $Repo -GitArgs @('check-attr', '--stdin', '-z', 'text', 'eol') -Stdin $payload
    $parts = Split-OnNul -Data $raw
    while ($parts.Count -gt 0 -and $parts[$parts.Count - 1].Length -eq 0) {
        $parts.RemoveAt($parts.Count - 1)
    }
    if (($parts.Count % 3) -ne 0) {
        Write-Err 'git check-attr returned an unexpected record layout'
        exit 1
    }
    for ($index = 0; $index -lt $parts.Count; $index += 3) {
        $path = Convert-GitText $parts[$index]
        $attr = Convert-GitText $parts[$index + 1]
        $value = Convert-GitText $parts[$index + 2]
        if (-not $info.ContainsKey($path)) {
            $info[$path] = @{}
        }
        $info[$path][$attr] = $value
    }
    return $info
}

function Test-PlainText($Attrs) {
    $text = 'unspecified'
    if ($Attrs.ContainsKey('text')) {
        $text = [string]$Attrs['text']
    }
    if ($text -eq 'unset') {
        return $false
    }
    if ($text -ne 'unspecified') {
        return $true
    }
    $eol = 'unspecified'
    if ($Attrs.ContainsKey('eol')) {
        $eol = [string]$Attrs['eol']
    }
    return ($eol -eq 'lf' -or $eol -eq 'crlf')
}

$repo = Get-RepoRoot
if (-not (Test-Path -LiteralPath (Join-Path $repo '.gitattributes') -PathType Leaf)) {
    Write-Err "No .gitattributes in $repo"
    exit 2
}
if (-not (Test-Path -LiteralPath (Join-Path $repo '.git'))) {
    Write-Err "$repo is not a git checkout"
    exit 2
}

$entries = Get-TrackedFiles -Repo $repo
$attrsByPath = Get-AttributeMap -Repo $repo -Entries $entries

$controlHits = New-Object System.Collections.Generic.List[object]
$missingEol = New-Object System.Collections.Generic.List[object]
$unreadable = New-Object System.Collections.Generic.List[string]
$checked = 0
$skippedEmpty = 0
$skippedNotText = 0
$skippedNotFile = 0

foreach ($entry in $entries) {
    if (-not $attrsByPath.ContainsKey($entry.Path)) {
        $unreadable.Add("$($entry.Path): no attribute result from git check-attr")
        continue
    }
    if (-not (Test-PlainText $attrsByPath[$entry.Path])) {
        $skippedNotText++
        continue
    }
    if ($entry.Mode -ne '100644' -and $entry.Mode -ne '100755') {
        $skippedNotFile++
        continue
    }

    $full = Join-Path $repo ($entry.Path -replace '/', '\')
    try {
        $data = [System.IO.File]::ReadAllBytes($full)
    }
    catch {
        $unreadable.Add("$($entry.Path): $($_.Exception.Message)")
        continue
    }
    if ($data.Length -eq 0) {
        $skippedEmpty++
        continue
    }

    $checked++
    foreach ($hit in [Z390.TextCheck.Scanner]::Scan($data)) {
        $controlHits.Add([pscustomobject]@{
            Path  = $entry.Path
            Value = $hit.Value
            Count = $hit.Count
            First = $hit.First
        })
    }
    $last = [int]$data[$data.Length - 1]
    if ($last -ne 0x0A -and $last -ne 0x0D) {
        $missingEol.Add([pscustomobject]@{
            Path = $entry.Path
            Last = $last
        })
    }
}

Write-Out ("Checked {0} text file(s) marked by .gitattributes (text=auto included without Git's content probe)." -f $checked)
Write-Out ("Skipped {0} empty file(s), {1} file(s) not marked as text, {2} non-regular path(s)." -f $skippedEmpty, $skippedNotText, $skippedNotFile)
Write-Out ''

$groups = @($controlHits | Group-Object -Property Value | Sort-Object { [int]$_.Name })
if ($groups.Count -eq 0) {
    Write-Out 'Control bytes below 0x20 other than CR (0x0D) and LF (0x0A): none'
}
else {
    foreach ($group in $groups) {
        $value = [int]$group.Name
        $files = @($group.Group | Sort-Object -Property Path)
        Write-Out ("Control byte 0x{0:X2} ({1}) in {2} file(s):" -f $value, $ControlName[$value], $files.Count)
        foreach ($item in $files) {
            Write-Out ("  {0} ({1}, first at offset {2})" -f $item.Path, $item.Count, $item.First)
        }
    }
}

Write-Out ''
$missingSorted = @($missingEol | Sort-Object -Property Path)
if ($missingSorted.Count -eq 0) {
    Write-Out 'No line-end marker at end of file: none'
}
else {
    Write-Out ("No line-end marker at end of file: {0} file(s):" -f $missingSorted.Count)
    foreach ($item in $missingSorted) {
        Write-Out ("  {0} (last byte 0x{1:X2})" -f $item.Path, $item.Last)
    }
}

if ($unreadable.Count -gt 0) {
    Write-Err ''
    Write-Err ("Unreadable: {0} path(s):" -f $unreadable.Count)
    foreach ($line in $unreadable) {
        Write-Err "  $line"
    }
}

if ($controlHits.Count -gt 0 -or $missingEol.Count -gt 0 -or $unreadable.Count -gt 0) {
    exit 1
}
exit 0
