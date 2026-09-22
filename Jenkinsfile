// OpenSpace CI pipeline
//
// Required plugins: Pipeline, Git, Warnings Next Generation, JUnit, Workspace Cleanup

def url = 'https://github.com/OpenSpace/OpenSpace'
def branch = env.BRANCH_NAME

// The CHANGE_BRANCH only exists if we are building a PR branch in which case it returns
// the original branch
if (env.CHANGE_BRANCH) {
  branch = env.CHANGE_BRANCH
}

// Checks out the repository with all of its submodules. In contrast to a manual
// `git clone`, this reports the commit and the changelog back to Jenkins
def scmConfig = scmGit(
  userRemoteConfigs: [[url: url]],
  branches: [[name: "*/${branch}"]],
  extensions: [
    cleanBeforeCheckout(),
    cloneOption(shallow: true, depth: 1, noTags: true, timeout: 30),
    submodule(recursiveSubmodules: true, parentCredentials: true, shallow: true, depth: 1, threads: 4, timeout: 60)
  ]
)

//
// Pipeline start
//

parallel tools: {
  node('tools') {
    stage('tools/scm') {
      checkout scmConfig
    }
    stage('tools/cppcheck') {
      sh(
        script: 'cppcheck --enable=all --xml --xml-version=2 -i ext --suppressions-list=support/cppcheck/suppressions.txt include modules src tests 2> cppcheck.xml',
        label: 'CPPCheck'
      )
      recordIssues(id: 'tools-cppcheck', tool: cppCheck(pattern: 'cppcheck.xml'))
    }
    cleanWs()
  } // node('tools')
},
linux_gcc: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux-gcc') {
      stage('linux-gcc/scm') {
        checkout scmConfig
      }

      stage('linux-gcc/build') {
        sh(script: 'cmake --preset linux-makefiles-release', label: 'Configure')
        sh(script: 'cmake --build --preset linux-makefiles-release --parallel 6', label: 'Compile')
        recordIssues(id: 'linux-gcc', tool: gcc())
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('linux-gcc/test') {
          timeout(time: 15, unit: 'MINUTES') {
            sh(
              script: 'xvfb-run --auto-servernum ctest --preset linux-makefiles-release --output-junit test-results.xml',
              label: 'Run unit tests',
              returnStatus: true
            )
          }
          junit(testResults: '**/test-results.xml', keepLongStdio: true)
        }
      }
      cleanWs()
    } // node('linux-gcc')
  }
},
linux_clang: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux-clang') {
      stage('linux-clang/scm') {
        checkout scmConfig
      }

      stage('linux-clang/build') {
        sh(script: 'cmake --preset linux-makefiles-release', label: 'Configure')
        sh(script: 'cmake --build --preset linux-makefiles-release --parallel 6', label: 'Compile')
        recordIssues(id: 'linux-clang', tool: clang())
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('linux-clang/test') {
          timeout(time: 15, unit: 'MINUTES') {
            sh(
              script: 'xvfb-run --auto-servernum ctest --preset linux-makefiles-release --output-junit test-results.xml',
              label: 'Run unit tests',
              returnStatus: true
            )
          }
          junit(testResults: '**/test-results.xml', keepLongStdio: true)
        }
      }
      cleanWs()
    } // node('linux-clang')
  }
},
windows_msvc: {
  if (env.USE_BUILD_OS_WINDOWS == 'true') {
    node('windows') {
      stage('windows-msvc/scm') {
        checkout scmConfig
      }

      stage('windows-msvc/build') {
        bat(script: 'cmake --preset windows-msvc', label: 'Configure')
        bat(script: 'cmake --build --preset windows-msvc --parallel 8', label: 'Compile')
        recordIssues(id: 'windows-msvc', tool: msBuild())
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('windows-msvc/test') {
          timeout(time: 15, unit: 'MINUTES') {
            bat(
              script: 'ctest --preset windows-msvc --output-junit test-results.xml',
              label: 'Run unit tests',
              returnStatus: true
            )
          }
          junit(testResults: '**/test-results.xml', keepLongStdio: true)
        }
      }
      cleanWs()
    } // node('windows')
  }
}
