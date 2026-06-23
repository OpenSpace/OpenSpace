param(
    [string[]]$Hemispheres = @('north', 'south'),
    [string[]]$IncludeTimestamps = @(),
    [string]$StartTimestamp = '2024-05-10-00-00',
    [string]$EndTimestamp = '2024-05-12-00-00'
)

$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.Drawing

$root = 'C:\Users\alundkvi\Documents\work\OpenSpace\user\data\assets\aurorasaurus\oval_images'
$start = [datetime]::ParseExact($StartTimestamp, 'yyyy-MM-dd-HH-mm', $null)
$end = [datetime]::ParseExact($EndTimestamp, 'yyyy-MM-dd-HH-mm', $null)
$step = [timespan]::FromMinutes(15)
$includeSet = @{}

foreach ($timestampGroup in $IncludeTimestamps) {
    foreach ($timestamp in ($timestampGroup -split ',')) {
        if ([string]::IsNullOrWhiteSpace($timestamp)) {
            continue
        }

        $includeSet[[datetime]::ParseExact($timestamp.Trim(), 'yyyy-MM-dd-HH-mm', $null)] = $true
    }
}

function Get-TimeMap {
    param([string]$Directory)

    $map = @{}
    Get-ChildItem -Path $Directory -Filter '*.png' | ForEach-Object {
        $timestamp = [datetime]::ParseExact($_.BaseName, 'yyyy-MM-dd-HH-mm', $null)
        $map[$timestamp] = $_.FullName
    }
    return $map
}

function New-ColorMatrix {
    param([float]$Alpha)

    return New-Object System.Drawing.Imaging.ColorMatrix @(,
        ([single[][]]@(
            @([single]1, [single]0, [single]0, [single]0, [single]0),
            @([single]0, [single]1, [single]0, [single]0, [single]0),
            @([single]0, [single]0, [single]1, [single]0, [single]0),
            @([single]0, [single]0, [single]0, [single]$Alpha, [single]0),
            @([single]0, [single]0, [single]0, [single]0, [single]1)
        ))
    )
}

function New-InterpolatedImage {
    param(
        [string]$PreviousPath,
        [string]$NextPath,
        [float]$Weight,
        [string]$OutputPath
    )

    $previous = [System.Drawing.Bitmap]::FromFile($PreviousPath)
    $next = [System.Drawing.Bitmap]::FromFile($NextPath)

    try {
        if ($previous.Width -ne $next.Width -or $previous.Height -ne $next.Height) {
            throw "Image size mismatch between '$PreviousPath' and '$NextPath'"
        }

        $bitmap = New-Object System.Drawing.Bitmap(
            $previous.Width,
            $previous.Height,
            [System.Drawing.Imaging.PixelFormat]::Format32bppArgb
        )

        try {
            $graphics = [System.Drawing.Graphics]::FromImage($bitmap)
            try {
                $graphics.CompositingMode = [System.Drawing.Drawing2D.CompositingMode]::SourceOver
                $graphics.CompositingQuality = [System.Drawing.Drawing2D.CompositingQuality]::HighQuality
                $graphics.InterpolationMode = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
                $graphics.SmoothingMode = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality
                $graphics.PixelOffsetMode = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
                $graphics.Clear([System.Drawing.Color]::Transparent)

                $rect = New-Object System.Drawing.Rectangle(0, 0, $previous.Width, $previous.Height)

                $previousAttributes = New-Object System.Drawing.Imaging.ImageAttributes
                $previousAttributes.SetColorMatrix((New-ColorMatrix (1.0 - $Weight)))
                $graphics.DrawImage(
                    $previous,
                    $rect,
                    0,
                    0,
                    $previous.Width,
                    $previous.Height,
                    [System.Drawing.GraphicsUnit]::Pixel,
                    $previousAttributes
                )
                $previousAttributes.Dispose()

                $nextAttributes = New-Object System.Drawing.Imaging.ImageAttributes
                $nextAttributes.SetColorMatrix((New-ColorMatrix $Weight))
                $graphics.DrawImage(
                    $next,
                    $rect,
                    0,
                    0,
                    $next.Width,
                    $next.Height,
                    [System.Drawing.GraphicsUnit]::Pixel,
                    $nextAttributes
                )
                $nextAttributes.Dispose()

                $bitmap.Save($OutputPath, [System.Drawing.Imaging.ImageFormat]::Png)
            }
            finally {
                $graphics.Dispose()
            }
        }
        finally {
            $bitmap.Dispose()
        }
    }
    finally {
        $previous.Dispose()
        $next.Dispose()
    }
}

$summary = @()

foreach ($hemisphere in $Hemispheres) {
    $directory = Join-Path $root $hemisphere
    $timeMap = Get-TimeMap -Directory $directory
    $generated = 0

    $expected = @()
    for ($current = $start; $current -le $end; $current = $current + $step) {
        $expected += $current
    }

    foreach ($timestamp in $expected) {
        if ($includeSet.Count -gt 0 -and -not $includeSet.ContainsKey($timestamp)) {
            continue
        }

        if ($timeMap.ContainsKey($timestamp)) {
            continue
        }

        $previousTime = $null
        $nextTime = $null

        for ($scan = $timestamp - $step; $scan -ge $start; $scan = $scan - $step) {
            if ($timeMap.ContainsKey($scan)) {
                $previousTime = $scan
                break
            }
        }

        for ($scan = $timestamp + $step; $scan -le $end; $scan = $scan + $step) {
            if ($timeMap.ContainsKey($scan)) {
                $nextTime = $scan
                break
            }
        }

        if ($null -eq $previousTime -or $null -eq $nextTime) {
            continue
        }

        $weight = ($timestamp - $previousTime).TotalMinutes /
            ($nextTime - $previousTime).TotalMinutes
        $outputPath = Join-Path $directory ($timestamp.ToString('yyyy-MM-dd-HH-mm') + '.png')

        New-InterpolatedImage `
            -PreviousPath $timeMap[$previousTime] `
            -NextPath $timeMap[$nextTime] `
            -Weight $weight `
            -OutputPath $outputPath

        $timeMap[$timestamp] = $outputPath
        $generated++
    }

    $summary += [pscustomobject]@{
        Hemisphere = $hemisphere
        Generated = $generated
        Total = (Get-ChildItem -Path $directory -Filter '*.png').Count
    }
}

$summary | Format-Table -AutoSize
