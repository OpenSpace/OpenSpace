param(
    [string[]]$Hemispheres = @('north', 'south'),
    [string]$StartTimestamp = '2024-05-10-00-00',
    [string]$EndTimestamp = '2024-05-10-14-45',
    [string]$ReferenceTimestamp = '2024-05-10-15-00'
)

$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.Drawing

$root = 'C:\Users\alundkvi\Documents\work\OpenSpace\user\data\assets\aurorasaurus\oval_images'
$start = [datetime]::ParseExact($StartTimestamp, 'yyyy-MM-dd-HH-mm', $null)
$end = [datetime]::ParseExact($EndTimestamp, 'yyyy-MM-dd-HH-mm', $null)
$step = [timespan]::FromMinutes(15)

foreach ($hemisphere in $Hemispheres) {
    $directory = Join-Path $root $hemisphere
    $referencePath = Join-Path $directory ($ReferenceTimestamp + '.png')
    $reference = [System.Drawing.Bitmap]::FromFile($referencePath)

    try {
        for ($current = $start; $current -le $end; $current = $current + $step) {
            $outputPath = Join-Path $directory ($current.ToString('yyyy-MM-dd-HH-mm') + '.png')
            $bitmap = New-Object System.Drawing.Bitmap(
                $reference.Width,
                $reference.Height,
                [System.Drawing.Imaging.PixelFormat]::Format32bppArgb
            )

            try {
                $graphics = [System.Drawing.Graphics]::FromImage($bitmap)
                try {
                    $graphics.Clear([System.Drawing.Color]::Transparent)
                    $bitmap.Save($outputPath, [System.Drawing.Imaging.ImageFormat]::Png)
                }
                finally {
                    $graphics.Dispose()
                }
            }
            finally {
                $bitmap.Dispose()
            }
        }
    }
    finally {
        $reference.Dispose()
    }
}

Write-Output 'Transparent placeholders generated'
