@echo off
setlocal

:: This script might have been executed from the support folder
if NOT exist openspace.cfg cd ..

call :RemoveFolders

call :VSRedist

:: Build with tracy
echo ## Build with Tracy
call :RunCMake build-deploy-tracy, 1
call :Build build-deploy-tracy

:: Save the OpenSpace.exe with Tracy
move bin\RelWithDebInfo\OpenSpace.exe .
ren OpenSpace.exe OpenSpace-Tracy.exe
rmdir /S /Q bin

:: Build without tracy
echo ## Build without Tracy
call :RunCMake build-deploy
call :Build build-deploy

echo ### Save the PDB files
"C:\Program Files\7-Zip\7z.exe" a pdbs.zip bin/RelWithDebInfo/*.pdb

call :PostBuildCleanup

move OpenSpace-Tracy.exe bin

call :ZipDistributable OpenSpace-minimal
if exist sync call :ZipDistributable OpenSpace, 1

echo ### Clean up
rmdir /S /Q bin
del vc_redist.x64.exe
if exist bin-old move bin-old bin
if exist build-deploy rmdir /S /Q build-deploy
if exist build-deploy-tracy rmdir /S /Q build-deploy-tracy
exit /b 0



:RemoveFolders
echo ### Removing old folders
if exist bin-old rmdir /S /Q bin-old
if exist build-deploy rmdir /S /Q build-deploy
if exist build-deploy-tracy rmdir /S /Q build-deploy-tracy
if exist pdbs.zip del pdbs.zip
if exist OpenSpace.zip del OpenSpace.zip
if exist OpenSpace-minimal.zip del OpenSpace-minimal.zip

echo ### Moving existing folders out of the way
if exist bin move bin bin-old
exit /b 0



:VSRedist
echo ### Download the Microsoft redistributable
curl "http://aka.ms/vs/17/release/vc_redist.x64.exe" --output vc_redist.x64.exe -L
exit /b 0



:RunCMake
:: The first parameter is the destination folder where we want to build
:: If the second parameter to this subroutine is defined, we want to build with Tracy
echo ### Run CMake into the %~1 folder
if "%~2"=="" (
  set "param="
) else (
  set "param=-D TRACY_ENABLE=ON -DSGCT_TRACY_SUPPORT=ON"
)
cmake^
  -D SGCT_BUILD_TESTS=OFF^
  -D GHOUL_HIGH_DEBUG_MODE=OFF^
  -D GHOUL_HAVE_TESTS=OFF^
  -D OPENSPACE_HAVE_TESTS=OFF^
  -D OPENSPACE_ENABLE_ALL_MODULES=ON^
  %param%^
  -S .^
  -B %~1
exit /b 0



:Build
:: The first parameter is the destination folder where we want to build
echo ### Build OpenSpace in RelWithDebInfo mode
cmake^
  --build %~1^
  --config RelWithDebInfo^
  --target OpenSpace^
  --parallel^
  -- /p:CL_MPcount=16
exit /b 0



:PostBuildCleanup
echo ### Dealing with some files
:: Remove unnecessary files of our own making
del bin\RelWithDebInfo\*.pdb
del bin\RelWithDebInfo\codegen.exe
del bin\RelWithDebInfo\Qt6Svg.dll

:: Remove unnecessary Qt files
rmdir /S /Q bin\RelWithDebInfo\iconengines
rmdir /S /Q bin\RelWithDebInfo\imageformats
rmdir /S /Q bin\RelWithDebInfo\networkinformation

:: The binary files are created in the RelWithDebInfo subdirectory and we want them in bin
robocopy bin\\RelWithDebInfo bin /E /MOV
rmdir /S /Q bin\\RelWithDebInfo
exit /b 0



:ZipDistributable
:: The first parameter specifies the output file
:: If the second parameter to this subroutine is defined, we want to include the sync folder
echo ### Create minimal zip file
if "%~2"=="" (
  set "sync="
) else (
  set "sync=sync"
)

:: Need to manually add any new weird paths that don't match the wildcards below
"C:\Program Files\7-Zip\7z.exe" a -tzip -mx=9 -mfb=257 -mpass=15 %~1.zip^
  bin/*^
  config/*^
  data/*^
  documentation/*^
  scripts/*^
  shaders/*^
  ACKNOWLEDGMENTS.md^
  CITATION.cff^
  CODE_OF_CONDUCT.md^
  COMMIT.md^
  CREDITS.md^
  LICENSE.md^
  openspace.cfg^
  README.md^
  vc_redist.x64.exe^
  ^
  modules/*/shaders/*^
  modules/*/scripts/*^
  modules/globebrowsing/gdal_data/*^
  modules/webgui/ext/nodejs/node.exe^
  ^
  %sync%^
  -x!documentation/.git

exit /b 0
