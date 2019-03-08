def modules = [
    "base",
    "debugging",
    "fieldlines",
    "galaxy",
    "globebrowsing",
    "imgui",
    "iswa",
    "kameleon",
    "kameleonvolume",
    "multiresvolume",
    "spacecraftinstruments",
    "space",
    "touch",
    "toyvolume",
    "volume"
];

def flags = "-DGHOUL_USE_DEVIL=OFF "

for (module in modules) {
    flags += "-DOPENSPACE_MODULE_" + module.toUpperCase() + "=ON "
}

echo flags

parallel linux: {
  node('linux') {
    stage('linux/SCM') {
      deleteDir()
      checkout scm
      sh 'git submodule update --init --recursive'
    }
    stage('linux/Build') {
      cmakeBuild([
        generator: 'Unix Makefiles',
        buildDir: 'build',
        installation: 'InSearchPath',
        steps: [
          [ args: flags + ' --target OpenSpace -- -j4', withCmake: true ],
          [ args: flags + ' --target GhoulTest -- -j4', withCmake: true ],
          [ args: flags + ' --target OpenSpaceTest -- -j4', withCmake: true ]
        ]
      ])
    }
  } // node('linux')
},
windows: {
  node('windows') {
      ws("${env.JENKINS_BASE}/O/${env.BRANCH_NAME}/${env.BUILD_ID}") {
        stage('windows/SCM') {
          deleteDir()
          checkout scm
          bat 'git submodule update --init --recursive'
        }
        stage('windows/Build') {
          cmakeBuild([
            generator: 'Visual Studio 15 2017 Win64',
            buildDir: 'build',
            installation: 'InSearchPath',
            steps: [
              [ args: flags + ' -- /p:Configuration=Debug /target:OpenSpace /nologo /verbosity:minimal /m:2', withCmake: true ],
              [ args: flags + ' -- /p:Configuration=Debug /target:"Unit Tests"\\OpenSpaceTest /nologo /verbosity:minimal /m:2', withCmake: true ]
            ]
          ])
        }
      }
    } // node('windows')
},
osx: {
  node('osx') {
    stage('osx/SCM') {
      deleteDir()
      checkout scm
      sh 'git submodule update --init --recursive'
    }
    stage('osx/Build') {
      cmakeBuild([
        generator: 'Xcode',
        buildDir: 'build',
        installation: 'InSearchPath',
        steps: [
          [ args: flags + '-- -parallelizeTargets -jobs 4 -target OpenSpace -target OpenSpaceTest', withCmake: true ]
        ]
      ])
    }
  } // node('osx')
}



// stage('Build') {
//     parallel linux: {
//         node('linux') {
//             timeout(time: 90, unit: 'MINUTES') {
                
//                 deleteDir()
//                 checkout scm
//                 sh 'git submodule update --init --recursive'
//                 sh '''
//                     mkdir -p build
//                     cd build
//                     cmake .. ''' +
//                     flags + ''' ..
//                 make -j4 OpenSpace GhoulTest OpenSpaceTest
//                 '''
//             }
//         }
//     },
//     windows: {
//         node('windows') {
//             timeout(time: 90, unit: 'MINUTES') {
//                 // We specify the workspace directory manually to reduce the path length and thus try to avoid MSB3491 on Visual Studio
//                 ws("${env.JENKINS_BASE}/O/${env.BRANCH_NAME}/${env.BUILD_ID}") {
//                     deleteDir()
//                     checkout scm
//                     bat '''
//                         git submodule update --init --recursive
//                         if not exist "build" mkdir "build"
//                         cd build
//                         cmake -G "Visual Studio 15 2017 Win64" .. ''' +
//                         flags + ''' ..
//                         msbuild.exe OpenSpace.sln /nologo /verbosity:minimal /p:Configuration=Debug /target:OpenSpace /target:"Unit Tests"\\GhoulTest /target:"Unit Tests"\\OpenSpaceTest
//                     '''
//                 }
//             }
//         }
//     },
//     osx: {
//         node('osx') {
//             timeout(time: 90, unit: 'MINUTES') {
//                 deleteDir()
//                 checkout scm
//                 sh 'git submodule update --init --recursive'
//                 sh '''
//                     export PATH=${PATH}:/usr/local/bin:/Applications/CMake.app/Contents/bin
//                     export CMAKE_BUILD_TOOL=/Applications/CMake.app/Contents/bin/CMake
//                     srcDir=$PWDo
//                     if [ ! -d ${srcDir} ]; then
//                       mkdir ${srcDir}
//                     fi
//                     if [ ! -d ${srcDir}/build ]; then
//                       mkdir ${srcDir}/build
//                     fi
//                     cd ${srcDir}/build
//                     /Applications/CMake.app/Contents/bin/cmake -G Xcode ${srcDir} .. ''' +
//                     flags + '''
//                     xcodebuild -parallelizeTargets -jobs 4 -target OpenSpace -target GhoulTest -target OpenSpaceTest
//                     '''
//             }
//         }
//     }
// }
