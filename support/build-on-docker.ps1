##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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

#Requires -Version 5.1
<#
.SYNOPSIS
    Compiles the local OpenSpace working tree in every Linux build container and reports
    which of the supported platforms fail to compile.

.DESCRIPTION
    Meant for developers who want to check that a feature compiles on all supported Linux
    distribution/compiler combinations before pushing it. The containers are the ones from
    the OpenSpace/docker repository (https://github.com/OpenSpace/docker), folder "build".

    1. Takes a snapshot of the local working tree, including uncommitted changes and
       untracked files that are not ignored. The index and working tree are not touched.
    2. Builds the openspace-<platform> image for every *.Dockerfile that does not have an
       image yet (or every one of them with -Rebuild).
    3. Configures and compiles the snapshot in each container, all at once by default or
       one after another with -Sequential. The containers are removed afterwards.
    4. Prints a summary with the errors of every platform that failed. The full logs are
       written to build/docker-logs/<timestamp>.

    The vcpkg binary cache volume and its per-image layout are shared with build-all.ps1
    from the docker repository, so dependencies are only built once per platform.

.EXAMPLE
    .\support\build-on-docker.ps1
    .\support\build-on-docker.ps1 -Sequential
    .\support\build-on-docker.ps1 -Platform ubuntu-2404-gcc13, archlinux-clang
    .\support\build-on-docker.ps1 -DockerPath C:\src\docker\build -Rebuild
#>
[CmdletBinding()]
param(
  [Parameter(HelpMessage = "The 'build' folder of the OpenSpace/docker repository")]
  [string] $DockerPath = (Join-Path $PSScriptRoot "..\..\docker\build"),

  [Parameter(HelpMessage = "Dockerfile base names to build on, for example ubuntu-2404-gcc13. Default: all")]
  [string[]] $Platform = @(),

  [Parameter(HelpMessage = "Build on one container after another instead of all at once")]
  [switch] $Sequential,

  [Parameter(HelpMessage = "CMake configure and build preset to use")]
  [string] $Preset = "linux-ninja-debug",

  [Parameter(HelpMessage = "Rebuild the images even if they already exist")]
  [switch] $Rebuild,

  [Parameter(HelpMessage = "Docker volume holding the vcpkg binary caches")]
  [string] $Volume = "vcpkg-cache",

  [Parameter(HelpMessage = "Where the vcpkg cache volume is mounted inside the containers")]
  [string] $CacheMount = "/mnt/vcpkg-cache",

  [Parameter(HelpMessage = "Folder for the build logs. Default: build/docker-logs/<timestamp>")]
  [string] $LogDirectory = "",

  [Parameter(HelpMessage = "Maximum number of error lines shown per platform in the summary")]
  [int] $MaxErrors = 20
)

###
# Header
###
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# Exit codes of the script that runs inside the containers, see $runScript below
$ExitConfigure = 10
$ExitCompile = 20

function Write-Step {
  param([string] $Message)
  Write-Host ""
  Write-Host "==> $Message" -ForegroundColor Cyan
}

# Runs a native command, discarding its stderr, and throws if it fails. Windows PowerShell
# turns redirected stderr output into terminating errors, so it has to be relaxed here
function Invoke-Native {
  param([string] $Exe, [string[]] $Arguments)
  $ErrorActionPreference = "Continue"
  $output = & $Exe @Arguments 2>$null
  if ($LASTEXITCODE -ne 0) {
    throw "'$Exe $($Arguments -join ' ')' failed with exit code $LASTEXITCODE"
  }
  return $output
}

# Returns whether a native command succeeds, without any output
function Test-Native {
  param([string] $Exe, [string[]] $Arguments)
  $ErrorActionPreference = "Continue"
  $null = & $Exe @Arguments 2>$null
  return $LASTEXITCODE -eq 0
}

function Format-Elapsed {
  param([TimeSpan] $Span)
  return "{0:hh\:mm\:ss}" -f $Span
}

if (-not (Get-Command docker -ErrorAction SilentlyContinue)) {
  throw "docker was not found on PATH"
}
if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
  throw "git was not found on PATH"
}
if (-not (Test-Native docker @("info"))) {
  throw "Docker is not running or cannot be reached ('docker info' failed)"
}
if (-not (Test-Path (Join-Path $DockerPath "*.Dockerfile"))) {
  throw "No *.Dockerfile found in '$DockerPath'. Clone https://github.com/OpenSpace/docker " +
        "next to the OpenSpace folder or pass the path to its 'build' folder with -DockerPath"
}
$DockerPath = (Resolve-Path $DockerPath).Path

$repo = (Invoke-Native git @("-C", $PSScriptRoot, "rev-parse", "--show-toplevel")).Trim()
$repo = (Resolve-Path $repo).Path

$dockerfiles = @(Get-ChildItem -Path $DockerPath -Filter "*.Dockerfile" | Sort-Object Name)
if ($Platform.Count -gt 0) {
  $unknown = @($Platform | Where-Object { $_ -notin $dockerfiles.BaseName })
  if ($unknown.Count -gt 0) {
    throw "Unknown platform(s): $($unknown -join ', '). Available: $($dockerfiles.BaseName -join ', ')"
  }
  $dockerfiles = @($dockerfiles | Where-Object { $_.BaseName -in $Platform })
}

if (-not $LogDirectory) {
  $LogDirectory = Join-Path $repo "build\docker-logs\$(Get-Date -Format 'yyyyMMdd-HHmmss')"
}
New-Item -ItemType Directory -Force -Path $LogDirectory | Out-Null
$LogDirectory = (Resolve-Path $LogDirectory).Path

$snapshotDir = Join-Path ([IO.Path]::GetTempPath()) "openspace-docker-$([Guid]::NewGuid().ToString('N'))"
New-Item -ItemType Directory -Path $snapshotDir | Out-Null

$results = [ordered] @{}
$running = @{}

try {
  ###
  # 1. Snapshot of the working tree
  ###
  $branch = (Invoke-Native git @("-C", $repo, "rev-parse", "--abbrev-ref", "HEAD")).Trim()
  $commit = (Invoke-Native git @("-C", $repo, "rev-parse", "--short", "HEAD")).Trim()
  $changes = @(Invoke-Native git @("-C", $repo, "status", "--porcelain")).Count

  Write-Step "Creating a snapshot of $repo"
  Write-Host "    Branch $branch at $commit with $changes uncommitted or untracked change(s)"

  # Stage everything into a copy of the index so that the user's index stays untouched.
  # The tree is then exported from the object database with LF line endings, as the
  # working tree itself might have been checked out with CRLF line endings
  $index = Join-Path $snapshotDir "index"
  $realIndex = (Invoke-Native git @("-C", $repo, "rev-parse", "--path-format=absolute", "--git-path", "index")).Trim()
  if (Test-Path $realIndex) {
    Copy-Item $realIndex $index
  }
  $previousIndex = $env:GIT_INDEX_FILE
  try {
    $env:GIT_INDEX_FILE = $index
    Invoke-Native git @("-C", $repo, "-c", "core.safecrlf=false", "add", "--all") | Out-Null
    $tree = (Invoke-Native git @("-C", $repo, "write-tree")).Trim()
  }
  finally {
    $env:GIT_INDEX_FILE = $previousIndex
    Remove-Item $index -ErrorAction SilentlyContinue
  }

  $tarball = Join-Path $snapshotDir "openspace.tar"
  Invoke-Native git @("-C", $repo, "-c", "core.autocrlf=false", "-c", "core.eol=lf", "archive", "--format=tar", "--output=$tarball", $tree) | Out-Null
  Write-Host ("    Snapshot is {0:N0} MB" -f ((Get-Item $tarball).Length / 1MB)) -ForegroundColor DarkGray

  # The script that runs inside each container. It is written to a file instead of being
  # passed to `bash -c` to avoid any quoting issues on the way. CMakeLists.txt asks git
  # for the branch and commit, so the snapshot is turned into a repository again
  $runScript = @(
    '#!/bin/bash'
    'exec 2>&1'
    'branch="$1"'
    'preset="$2"'
    'cache="$3"'
    'set -e'
    'mkdir -p "$cache"'
    'mkdir /OpenSpace'
    'cd /OpenSpace'
    'tar -xf /snapshot/openspace.tar'
    'git init -q -b "$branch"'
    'git add --all'
    'git -c user.name=build-on-docker -c user.email=build-on-docker@localhost commit -q -m snapshot'
    'set +e'
    '# Split OPENSPACE_CMAKE_ARGS on whitespace so that each -D lands as its own argument'
    'read -r -a extra_cmake_args <<< "${OPENSPACE_CMAKE_ARGS:-}"'
    'echo "### Configuring with preset $preset ${extra_cmake_args[*]}"'
    "cmake --preset `"`$preset`" `"`${extra_cmake_args[@]}`" || exit $ExitConfigure"
    'echo "### Compiling"'
    '# Keep going after the first failure so that all compile errors are reported'
    "cmake --build --preset `"`$preset`" -- -k 0 || exit $ExitCompile"
    'echo "### Done"'
  ) -join "`n"
  [IO.File]::WriteAllText((Join-Path $snapshotDir "run.sh"), "$runScript`n", (New-Object Text.UTF8Encoding $false))

  ###
  # 2. Images
  ###
  $platforms = [System.Collections.Generic.List[string]]::new()
  foreach ($dockerfile in $dockerfiles) {
    $name = $dockerfile.BaseName
    $tag = "openspace-$name"

    if (-not $Rebuild -and (Test-Native docker @("image", "inspect", $tag))) {
      $platforms.Add($name)
      continue
    }

    Write-Step "Building image $tag"
    docker build --tag $tag --file $dockerfile.FullName $DockerPath
    if ($LASTEXITCODE -eq 0) {
      $platforms.Add($name)
    }
    else {
      Write-Host "    Building the image failed" -ForegroundColor Red
      $results[$name] = [pscustomobject] @{
        Status = "image build failed"; Elapsed = $null; Errors = @(); Log = $null
      }
    }
  }

  ###
  # 3. Builds
  ###
  $stale = @(Invoke-Native docker @("ps", "--all", "--quiet", "--filter", "name=^openspace-check-"))
  if ($stale.Count -gt 0) {
    Write-Host "Removing $($stale.Count) container(s) left over from a previous run" -ForegroundColor DarkGray
    Invoke-Native docker (@("rm", "--force") + $stale) | Out-Null
  }

  function Start-Build {
    param([string] $Name)
    $log = Join-Path $LogDirectory "$Name.log"
    $arguments = @(
      "run", "--rm",
      "--name", "openspace-check-$Name",
      "--volume", "${snapshotDir}:/snapshot:ro",
      "--volume", "${Volume}:${CacheMount}",
      "--env", "VCPKG_DEFAULT_BINARY_CACHE=$CacheMount/$Name",
      "openspace-$Name",
      "bash", "/snapshot/run.sh", $branch, $Preset, "$CacheMount/$Name"
    )
    # The paths are quoted as they might contain spaces; the rest never does
    $argumentLine = ($arguments | ForEach-Object { if ($_ -match '\s') { "`"$_`"" } else { $_ } }) -join " "
    $process = Start-Process -FilePath "docker" -ArgumentList $argumentLine -NoNewWindow -PassThru `
      -RedirectStandardOutput $log -RedirectStandardError "$log.stderr"
    # Accessing the handle makes sure that ExitCode is available once the process ends
    $null = $process.Handle
    Write-Host "    Started openspace-$Name" -ForegroundColor DarkGray
    return [pscustomobject] @{ Process = $process; Started = Get-Date; Log = $log }
  }

  function Complete-Build {
    param([string] $Name, $Build)
    $exitCode = $Build.Process.ExitCode
    $elapsed = (Get-Date) - $Build.Started

    # Errors from docker itself, for example when the container could not be started
    $stderr = "$($Build.Log).stderr"
    if ((Test-Path $stderr) -and (Get-Item $stderr).Length -gt 0) {
      Add-Content -Path $Build.Log -Value (Get-Content $stderr)
    }
    Remove-Item $stderr -ErrorAction SilentlyContinue

    $status = switch ($exitCode) {
      0              { "ok" }
      $ExitConfigure { "configure failed" }
      $ExitCompile   { "compile failed" }
      default        { "failed (exit code $exitCode)" }
    }

    $errors = @()
    if ($exitCode -ne 0 -and (Test-Path $Build.Log)) {
      $errors = @(
        Select-String -Path $Build.Log -Pattern '\berror\b:', '^FAILED:', 'CMake Error', '^docker:' |
          ForEach-Object { $_.Line.Trim() } |
          Select-Object -Unique
      )
      if ($errors.Count -eq 0) {
        $errors = @(Get-Content $Build.Log -Tail 10)
      }
    }

    $color = if ($exitCode -eq 0) { "Green" } else { "Red" }
    Write-Host ("    openspace-{0}: {1} after {2}" -f $Name, $status, (Format-Elapsed $elapsed)) -ForegroundColor $color
    return [pscustomobject] @{
      Status = $status; Elapsed = $elapsed; Errors = $errors; Log = $Build.Log
    }
  }

  $mode = if ($Sequential) { "one after another" } else { "in parallel" }
  Write-Step "Compiling on $($platforms.Count) platform(s) $mode"
  Write-Host "    Logs are written to $LogDirectory" -ForegroundColor DarkGray

  $queue = [System.Collections.Generic.Queue[string]]::new([string[]] $platforms)
  $overall = Get-Date
  while ($queue.Count -gt 0 -or $running.Count -gt 0) {
    while ($queue.Count -gt 0 -and (-not $Sequential -or $running.Count -eq 0)) {
      $name = $queue.Dequeue()
      $running[$name] = Start-Build -Name $name
    }

    Start-Sleep -Seconds 5

    foreach ($name in @($running.Keys)) {
      if ($running[$name].Process.HasExited) {
        Write-Host ("`r{0}`r" -f (" " * 100)) -NoNewline
        $results[$name] = Complete-Build -Name $name -Build $running[$name]
        $running.Remove($name)
      }
    }

    if ($running.Count -gt 0) {
      $line = "    {0} running, {1} queued, {2} elapsed (Ctrl+C to abort)" -f
        $running.Count, $queue.Count, (Format-Elapsed ((Get-Date) - $overall))
      Write-Host "`r$line" -NoNewline -ForegroundColor DarkGray
    }
  }
}
finally {
  # Only containers that are still running when the script is aborted are left here
  foreach ($name in @($running.Keys)) {
    Write-Host ""
    Write-Host "Stopping openspace-check-$name" -ForegroundColor Yellow
    Test-Native docker @("rm", "--force", "openspace-check-$name") | Out-Null
  }
  Remove-Item -Recurse -Force $snapshotDir -ErrorAction SilentlyContinue
}

###
# 4. Summary
###
Write-Step "Summary for $branch ($commit + $changes local change(s))"
$failures = 0
foreach ($name in $results.Keys) {
  $result = $results[$name]
  $ok = $result.Status -eq "ok"
  if (-not $ok) { $failures++ }
  $elapsed = if ($result.Elapsed) { Format-Elapsed $result.Elapsed } else { "--:--:--" }
  $color = if ($ok) { "Green" } else { "Red" }
  Write-Host ("    {0,-36} {1,-20} {2}" -f "openspace-$name", $result.Status, $elapsed) -ForegroundColor $color
}

foreach ($name in $results.Keys) {
  $result = $results[$name]
  if ($result.Status -eq "ok") { continue }

  Write-Host ""
  Write-Host "openspace-$name - $($result.Status)" -ForegroundColor Red
  if ($result.Log) {
    Write-Host "    Log: $($result.Log)" -ForegroundColor DarkGray
  }
  foreach ($line in ($result.Errors | Select-Object -First $MaxErrors)) {
    Write-Host "    $line"
  }
  if ($result.Errors.Count -gt $MaxErrors) {
    Write-Host "    ... and $($result.Errors.Count - $MaxErrors) more, see the log" -ForegroundColor DarkGray
  }
}

Write-Host ""
if ($failures -gt 0) {
  Write-Host "$failures of $($results.Count) platform(s) failed" -ForegroundColor Red
  exit 1
}
Write-Host "All $($results.Count) platform(s) compiled successfully" -ForegroundColor Green
