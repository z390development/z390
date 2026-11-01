# Compare two text files. CRLF and LF are the same line-end.
# Exit 0 when equal, 1 on the first difference, 2 when a file cannot be read.

$ErrorActionPreference = 'Stop'

function Show-Unit([string]$Kind, [int]$Byte) {
    switch ($Kind) {
        'eof'  { return 'end of file' }
        'crlf' { return 'CRLF' }
        'lf'   { return 'LF' }
        default {
            if ($Byte -ge 32 -and $Byte -le 126) {
                return "'" + [char]$Byte + "'"
            }
            return ('0x{0:X2}' -f $Byte)
        }
    }
}

function Next-Unit([byte[]]$Bytes, [int]$Index) {
    if ($null -eq $Bytes -or $Index -ge $Bytes.Length) {
        return @{ Kind = 'eof'; Byte = -1; Next = $Index }
    }
    $b = [int]$Bytes[$Index]
    if ($b -eq 13 -and ($Index + 1) -lt $Bytes.Length -and [int]$Bytes[$Index + 1] -eq 10) {
        return @{ Kind = 'crlf'; Byte = -1; Next = $Index + 2 }
    }
    if ($b -eq 10) {
        return @{ Kind = 'lf'; Byte = -1; Next = $Index + 1 }
    }
    return @{ Kind = 'byte'; Byte = $b; Next = $Index + 1 }
}

function Test-SameUnit($Left, $Right) {
    $leftEnd = ($Left.Kind -eq 'lf' -or $Left.Kind -eq 'crlf')
    $rightEnd = ($Right.Kind -eq 'lf' -or $Right.Kind -eq 'crlf')
    if ($leftEnd -or $rightEnd) {
        return ($leftEnd -and $rightEnd)
    }
    if ($Left.Kind -eq 'eof' -or $Right.Kind -eq 'eof') {
        return ($Left.Kind -eq 'eof' -and $Right.Kind -eq 'eof')
    }
    return ($Left.Byte -eq $Right.Byte)
}

if ($args.Count -ne 2) {
    [Console]::Error.WriteLine('comparetextfile: two file paths are required')
    exit 2
}

$path1 = [string]$args[0]
$path2 = [string]$args[1]

try {
    $bytes1 = [System.IO.File]::ReadAllBytes($path1)
} catch {
    [Console]::Error.WriteLine("comparetextfile: cannot read $path1")
    exit 2
}
try {
    $bytes2 = [System.IO.File]::ReadAllBytes($path2)
} catch {
    [Console]::Error.WriteLine("comparetextfile: cannot read $path2")
    exit 2
}

$i1 = 0
$i2 = 0
$line = 1
$pos = 1
while ($true) {
    $u1 = Next-Unit $bytes1 $i1
    $u2 = Next-Unit $bytes2 $i2
    $i1 = $u1.Next
    $i2 = $u2.Next
    if (-not (Test-SameUnit $u1 $u2)) {
        $c1 = Show-Unit $u1.Kind $u1.Byte
        $c2 = Show-Unit $u2.Kind $u2.Byte
        Write-Output 'CompareTextFile: files differ'
        Write-Output $path1
        Write-Output $path2
        Write-Output "line $line byte ${pos}: $c1 / $c2"
        exit 1
    }
    if ($u1.Kind -eq 'eof') {
        exit 0
    }
    if ($u1.Kind -eq 'lf' -or $u1.Kind -eq 'crlf') {
        $line++
        $pos = 1
    } else {
        $pos++
    }
}
