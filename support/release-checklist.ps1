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

#Requires -Version 7
<#
.SYNOPSIS
    Walks through the steps that are needed to prepare an OpenSpace release.

.DESCRIPTION
    Meant for the person preparing a release. The script has to be started from the root
    of the OpenSpace repository with a clean working tree.

    For a new release, the script asks whether it is a minor or a major release and for
    the version number, suggesting one based on the latest releases/vX.Y.Z tag. It then
    goes through the steps that apply to that release type one after another and asks
    after each of them whether it should be marked as done.

    The progress is stored in release.json in the current working directory, which is
    written every time a step is marked as done. If the file exists when the script
    starts, the release is resumed from it and the steps that are already done are
    skipped. Delete the file to start over.

    A step is added as a function in the Steps section together with an entry in $Steps.
#>

###
# Header
###
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$StateFile = "release.json"

function Exit-WithError {
  param([string] $Message)
  Write-Host "Error: $Message" -ForegroundColor Red
  exit 1
}

function Save-State {
  param($State)
  ConvertTo-Json -InputObject $State | Set-Content -LiteralPath $StateFile -Encoding utf8
}

###
# Steps
###
# Every step is a function that is called with an object that has the properties Type
# ("minor" or "major") and Version ("X.Y.Z"). If the function throws, the script stops
# without marking the step as done. To add a step, write its function here and add an
# entry to $Steps in the order in which the steps should run. The Id is what is stored
# in release.json, so it has to be unique and should not change once it is in use

# function Update-Version {
#   param($Release)
#   Write-Host "Set the version in CMakeLists.txt to $($Release.Version)"
# }

$Steps = @(
  # @{
  #   Id = "update-version"
  #   Title = "Update the version number"
  #   Function = ${function:Update-Version}
  #   AppliesTo = @("minor", "major")
  # }
)

###
# Startup checks
###
if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
  Exit-WithError "git was not found on the PATH"
}

$root = git -C $PSScriptRoot rev-parse --show-toplevel
if ($LASTEXITCODE -ne 0) {
  Exit-WithError "Could not find the root of the repository that contains this script"
}
$root = [IO.Path]::GetFullPath($root)
if ($root -ne [IO.Path]::GetFullPath((Get-Location).ProviderPath)) {
  Exit-WithError "The script has to be run from the root of the repository ($root)"
}

###
# Release state
###
$isResumed = Test-Path -LiteralPath $StateFile
if ($isResumed) {
  try {
    $json = Get-Content -LiteralPath $StateFile -Raw | ConvertFrom-Json
    $state = [ordered] @{
      type = $json.type
      version = $json.version
      completed = @($json.completed)
    }
  }
  catch {
    Exit-WithError "$StateFile could not be read: $_"
  }
}
else {
  do {
    $type = (Read-Host "Release type (minor/major)").Trim().ToLower()
  } until ($type -in "minor", "major")

  $versions = foreach ($tag in (git tag --list "releases/v*")) {
    if ($tag -match '^releases/v(\d+\.\d+\.\d+)$') {
      [version] $Matches[1]
    }
  }
  $latest = $versions | Sort-Object -Descending | Select-Object -First 1

  $suggestion = $null
  if ($latest) {
    Write-Host "The latest release is $latest"
    if ($type -eq "major") {
      $suggestion = "$($latest.Major).$($latest.Minor + 1).0"
    }
    else {
      $suggestion = "$($latest.Major).$($latest.Minor).$($latest.Build + 1)"
    }
  }

  $prompt = if ($suggestion) { "Version [$suggestion]" } else { "Version" }
  while ($true) {
    $version = (Read-Host $prompt).Trim()
    if (-not $version) {
      $version = $suggestion
    }
    if ($version -match '^\d+\.\d+\.\d+$') {
      break
    }
    Write-Host "The version has to be of the form X.Y.Z" -ForegroundColor Red
  }

  $state = [ordered] @{ type = $type; version = $version; completed = @() }
  Save-State $state
}

$release = [pscustomobject] @{ Type = $state.type; Version = $state.version }
$applicable = @($Steps | Where-Object { $_.AppliesTo -contains $state.type })

if ($isResumed) {
  $done = @($applicable | Where-Object { $state.completed -contains $_.Id }).Count
  Write-Host "Resuming the release from $StateFile"
  Write-Host "    Type:      $($state.type)"
  Write-Host "    Version:   $($state.version)"
  Write-Host "    Completed: $done of $($applicable.Count) step(s)"
}

###
# Runner
###
foreach ($step in $applicable) {
  if ($state.completed -contains $step.Id) {
    continue
  }

  Write-Host ""
  Write-Host "==> $($step.Title)" -ForegroundColor Cyan
  try {
    # Out-Host prints the output right away instead of after the following prompt
    & $step.Function $release | Out-Host
  }
  catch {
    Exit-WithError "The step failed and was not marked as done: $_"
  }

  do {
    $answer = (Read-Host "Mark as done? [y]es / [n]o / [s]kip").Trim().ToLower()
  } until ($answer -in "y", "n", "s")

  if ($answer -eq "n") {
    Write-Host "Stopping. The step will run again the next time the script is started"
    exit 0
  }
  if ($answer -eq "y") {
    $state.completed = @($state.completed) + $step.Id
    Save-State $state
  }
}

$remaining = @($applicable | Where-Object { $state.completed -notcontains $_.Id }).Count
Write-Host ""
if ($remaining -eq 0) {
  Write-Host "All steps for release $($state.version) are done" -ForegroundColor Green
}
else {
  Write-Host "$remaining skipped step(s) will be offered again on the next run" `
    -ForegroundColor Yellow
}
