@echo off

echo ### Removing old folders
if exist bin-old rmdir /S /Q bin-old
if exist build-deploy rmdir /S /Q build-deploy
if exist pdbs.zip del pdbs.zip
if exist OpenSpace.zip del OpenSpace.zip
if exist OpenSpace-minimal.zip del OpenSpace-minimal.zip

echo ### Moving existing folders out of the way
if exist bin move bin bin-old



echo ### Run CMake into the 'build-deploy' folder
cmake^
  -D SGCT_BUILD_TESTS=OFF^
  -D GHOUL_HIGH_DEBUG_MODE=OFF^
  -D GHOUL_HAVE_TESTS=OFF^
  -D OPENSPACE_HAVE_TESTS=OFF^
  -D OPENSPACE_ENABLE_ALL_MODULES=ON^
  -S .^
  -B build-deploy



echo ### Build OpenSpace in RelWithDebInfo mode
cmake^
  --build build-deploy^
  --config RelWithDebInfo^
  --target OpenSpace^
  --parallel^
  -- /p:CL_MPcount=16

if %ERRORLEVEL% NEQ 0 EXIT /B

echo ### Save the PDB files
"C:\Program Files\7-Zip\7z.exe" a pdbs.zip bin/RelWithDebInfo/*.pdb


echo ### Dealing with some files
:: Remove unnecessary files of our own making
del bin\RelWithDebInfo\*.pdb
del bin\RelWithDebInfo\codegen.exe
del bin\RelWithDebInfo\Qt6Svg.dll

:: Remove unnecessary Qt files
rmdir /S /Q bin\RelWithDebInfo\iconengines
rmdir /S /Q bin\RelWithDebInfo\imageformats
rmdir /S /Q bin\RelWithDebInfo\networkinformation

:: Reset the documentation back to the master as it will have been updated locally
cd documentation
copy documentationData.js ..
git checkout HEAD -- documentationData.js
cd ..

:: The binary files are created in the RelWithDebInfo subdirectory and we want them in bin
robocopy bin\\RelWithDebInfo bin /E /MOV
rmdir /S /Q bin\\RelWithDebInfo

:: Download the Microsoft redistributable
curl "http://aka.ms/vs/17/release/vc_redist.x64.exe" --output vc_redist.x64.exe -L


echo ### Create main zip file
:: Need to manually add any new weird paths that don't match the wildcards below
"C:\Program Files\7-Zip\7z.exe" a -tzip -mx=9 -mfb=257 -mpass=15 OpenSpace.zip^
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
  -x!documentation/.git


echo ### Clean up
rmdir /S /Q bin
del vc_redist.x64.exe
if exist bin-old move bin-old bin
if exist build-deploy rmdir /S /Q build-deploy
move documentationData.js documentation
