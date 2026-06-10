##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2026                                                                     #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

[CmdletBinding()]
param(
  [Parameter(HelpMessage = "Do not create the full OpenSpace.zip containing the sync folder")]
  [switch]$SkipSync,
  [Parameter(HelpMessage = "Skip downloading vc_redist.x64.exe")]
  [switch]$SkipRedistributable,
  [Parameter(HelpMessage = "Skip the Tracy-enabled build and only produce the non-Tracy build")]
  [switch]$SkipTracy
)

###
# Header
###
$ErrorActionPreference = "Stop"

if (-not (Test-Path "C:\Program Files\7-Zip\7z.exe")) {
  Write-Error "7-Zip is required but not installed"
}
Set-Alias Start-SevenZip "C:\Program Files\7-Zip\7z.exe"

if (-not (Test-Path "openspace.cfg")) {
  Write-Error "This script must be executed from the OpenSpace base folder"
}



function Invoke-CMake {
  param(
    [switch]$Tracy
  )
  Write-Host "Run CMake"
  $tracyArgs = if ($Tracy) { @("-D", "TRACY_ENABLE=ON", "-D", "SGCT_TRACY_SUPPORT=ON") } else { @() }
  $cmakeArgs = @(
    "-D", "SGCT_BUILD_TESTS=OFF",
    "-D", "GHOUL_HIGH_DEBUG_MODE=OFF",
    "-D", "GHOUL_HAVE_TESTS=OFF",
    "-D", "OPENSPACE_HAVE_TESTS=OFF"
  ) + $tracyArgs + @("-S", ".", "-B", "build-deploy")
  cmake @cmakeArgs

  if ($LASTEXITCODE -ne 0) {
    Write-Error "CMake configure failed with exit code $LASTEXITCODE"
  }

  Write-Host "Build OpenSpace in RelWithDebInfo mode"
  $cmakeArgs = @(
    "--build", "build-deploy",
    "--config", "RelWithDebInfo",
    "--target", "OpenSpace", "AssetBuilder", "TaskRunner",
    "--parallel",
    "--", "/p:CL_MPcount=16"
  )
  cmake @cmakeArgs
  if ($LASTEXITCODE -ne 0) {
    Write-Error "CMake build failed with exit code $LASTEXITCODE"
  }
}


Write-Host "Removing old folders"
foreach ($item in "bin-old", "build-deploy", "pdbs.zip", "OpenSpace.zip", "OpenSpace-minimal.zip") {
  if (Test-Path $item) { Remove-Item -Recurse -Force $item }
}

Write-Host "Moving existing folders out of the way"
if (Test-Path "bin") { Move-Item "bin" "bin-old" }

if (!$SkipRedistributable) {
  Write-Host "Download the Microsoft redistributable"
  Invoke-WebRequest -Uri "http://aka.ms/vs/17/release/vc_redist.x64.exe" -OutFile "vc_redist.x64.exe"
}

if (!$SkipTracy) {
  # Build with Tracy
  Write-Host "Build with Tracy"
  Invoke-CMake -Tracy

  # Save the OpenSpace.exe with Tracy
  Move-Item "bin\RelWithDebInfo\OpenSpace.exe" "OpenSpace-Tracy.exe"
  Remove-Item -Recurse -Force "bin"
  Remove-Item -Recurse -Force "build-deploy"
}

# Build without Tracy
Write-Host "Regular Build"
Invoke-CMake

Write-Host "Save the PDB files"
Start-SevenZip a "pdbs.zip" "bin/RelWithDebInfo/*.pdb"
if ($LASTEXITCODE -ne 0) {
  Write-Error "7-Zip failed with exit code $LASTEXITCODE"
}

# The binary files are created in the RelWithDebInfo subdirectory and we want them in bin
Move-Item "bin\RelWithDebInfo\*" "bin"
Remove-Item -Recurse -Force "bin\RelWithDebInfo"

Write-Host "Removing unnecessary files"
foreach ($item in "bin\*.pdb", "bin\codegen-tool.exe", "bin\Qt6Svg.dll", "bin\iconengines", "bin\imageformats", "bin\networkinformation") {
  if (Test-Path $item) { Remove-Item -Recurse -Force $item }
}

Write-Host "Create minimal zip file"

# Need to manually add any new weird paths that don't match the wildcards below
$sevenZipArgs = @(
  "a", "-tzip", "-mx=7", "-mfb=128", "-mpass=5", "OpenSpace-minimal.zip",
  "bin/*",
  "config/*",
  "data/*",
  "documentation/*",
  "scripts/*",
  "shaders/*",
  "ACKNOWLEDGMENTS.md",
  "CITATION.cff",
  "COMMIT.md",
  "CREDITS.md",
  "LICENSE.md",
  "openspace.cfg",
  "README.md",
  "modules/*/shaders/*",
  "modules/*/scripts/*",
  "modules/globebrowsing/gdal_data/*",
  "modules/molecule/ext/mold/src/shaders/*",
  "modules/webgui/ext/nodejs/node.exe",
  "-x!documentation/.git"
)
if (Test-Path "vc_redist.x64.exe") {
  $sevenZipArgs += "vc_redist.x64.exe"
}
Start-SevenZip @sevenZipArgs
if ($LASTEXITCODE -ne 0) {
  Write-Error "7-Zip failed with exit code $LASTEXITCODE"
}

if ((-not $SkipSync) -and (Test-Path "sync")) {
  Copy-Item "OpenSpace-minimal.zip" "OpenSpace.zip"
  Start-SevenZip @("a", "-tzip", "-mx=7", "-mfb=128", "-mpass=5", "OpenSpace.zip", "sync")
  if ($LASTEXITCODE -ne 0) {
    Write-Error "7-Zip failed with exit code $LASTEXITCODE"
  }
}

Write-Host "Clean up"
foreach ($item in "bin", "vc_redist.x64.exe", "build-deploy") {
  if (Test-Path $item) { Remove-Item -Recurse -Force $item }
}
if (Test-Path "bin-old") { Move-Item "bin-old" "bin" }
