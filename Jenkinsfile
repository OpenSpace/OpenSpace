def modules = [
    "base",
    "debugging",
    "fieldlines",
    "galaxy",
    "globebrowsing",
    "iswa",
    "kameleon",
    "kameleonvolume",
    "multiresvolume",
    "newhorizons",
    "onscreengui",
    "space",
    "toyvolume",
    "volume"
];

def flags = "-DGHOUL_USE_DEVIL=OFF "

for (module in modules) {
    flags += "-DOPENSPACE_OPENSPACE_MODULE_" + module.toUpperCase() + "=ON "
}

echo flags

stage('Build') {
    parallel linux: {
        node('linux') {
            timeout(time: 90, unit: 'MINUTES') {
                
                deleteDir()
                checkout scm
                sh 'git submodule update --init --recursive'
                sh '''
                    mkdir -p build
                    cd build
                    cmake .. ''' +
                    flags + ''' ..
                make -j4
                '''
            }
        }
    },
    windows: {
        node('windows') {
            timeout(time: 90, unit: 'MINUTES') {
                // We specify the workspace directory manually to reduce the path length and thus try to avoid MSB3491 on Visual Studio
                ws("C:/J/O/${env.BRANCH_NAME}") {
                    deleteDir()
                    checkout scm
                    bat '''
                        git submodule update --init --recursive
                        if not exist "build" mkdir "build"
                        cd build
                        cmake -G "Visual Studio 15 2017 Win64" .. ''' +
                        flags + ''' ..
                        msbuild.exe OpenSpace.sln /nologo /verbosity:minimal /m:4 /p:Configuration=Debug
                    '''
                }
            }
        }
    },
    osx: {
        node('osx') {
            timeout(time: 90, unit: 'MINUTES') {
                deleteDir()
                checkout scm
                sh 'git submodule update --init --recursive'
                sh '''
                    export PATH=${PATH}:/usr/local/bin:/Applications/CMake.app/Contents/bin
                    export CMAKE_BUILD_TOOL=/Applications/CMake.app/Contents/bin/CMake
                    srcDir=$PWD
                    if [ ! -d ${srcDir} ]; then
                      mkdir ${srcDir}
                    fi
                    if [ ! -d ${srcDir}/build ]; then
                      mkdir ${srcDir}/build
                    fi
                    cd ${srcDir}/build
                    /Applications/CMake.app/Contents/bin/cmake -G Xcode ${srcDir} .. ''' +
                    flags + '''
                    xcodebuild -quiet -parallelizeTargets -jobs 4
                    '''
            }
        }
    }
}
