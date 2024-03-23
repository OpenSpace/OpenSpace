import groovy.io.FileType

library('sharedSpace'); // jenkins-pipeline-lib


def url = 'https://github.com/OpenSpace/OpenSpace';
def branch = env.BRANCH_NAME;

// The CHANGE_BRANCH only exists if we are building a PR branch in which case it returns
// the original branch
if (env.CHANGE_BRANCH) {
  branch = env.CHANGE_BRANCH;
}

@NonCPS
def readDir() {
  def dirsl = [];
  new File("${workspace}").eachDir() {
    dirs -> println dirs.getName()
    if (!dirs.getName().startsWith('.')) {
      dirsl.add(dirs.getName());
    }
  }
  return dirs;
}

def moduleCMakeFlags() {
  def modules = [];
  // using new File doesn't work as it is not allowed in the sandbox

  if (isUnix()) {
     modules = sh(returnStdout: true, script: 'ls -d modules/*').trim().split('\n');
  };
  else {
    modules = bat(returnStdout: true, script: '@dir modules /b /ad /on').trim().split('\r\n');
  }

  def flags = '';
  for (module in modules) {
      flags += "-DOPENSPACE_MODULE_${module.toUpperCase()}=ON "
  }
  return flags;
}

//
// Pipeline start
//

parallel tools: {
  node('tools') {
    stage('tools/scm') {
      deleteDir();
      gitHelper.checkoutGit(url, branch, false);
    }
    stage('tools/cppcheck') {
      sh(
        script: 'cppcheck --enable=all --xml --xml-version=2 -i ext --suppressions-list=support/cppcheck/suppressions.txt include modules src tests 2> cppcheck.xml',
        label: 'CPPCheck'
      )
      recordIssues(
        id: 'tools-cppcheck',
        tool: cppCheck(pattern: 'cppcheck.xml')
      )
    }
    cleanWs()
  } // node('tools')
},
linux_gcc_make: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux-gcc') {
      stage('linux-gcc-make/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }

      stage('linux-gcc-make/build') {
          def cmakeCompileOptions = moduleCMakeFlags();
          cmakeCompileOptions += ' -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS:STRING="-DGLM_ENABLE_EXPERIMENTAL"';
          cmakeCompileOptions += ' -DOpenGL_GL_PREFERENCE:STRING=GLVND -DASSIMP_BUILD_MINIZIP=1';
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Make(), compileHelper.Gcc(), cmakeCompileOptions, '', 'build-make');
          compileHelper.recordCompileIssues(compileHelper.Gcc());
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('linux-gcc-make/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/codegentest');
          }
        }

        stage('linux-gcc-make/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/SGCTTest');
          }
        }

        stage('linux-gcc-make/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/GhoulTest');
          }
        }

        stage('linux-gcc-make/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/OpenSpaceTest');
          }
        }
      }
      cleanWs()
    } // node('linux')
  }
},
linux_gcc_ninja: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux-gcc') {
      stage('linux-gcc-ninja/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }

      stage('linux-gcc-ninja/build') {
          def cmakeCompileOptions = moduleCMakeFlags();
          cmakeCompileOptions += '-DMAKE_BUILD_TYPE=Release';
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Ninja(), compileHelper.Gcc(), cmakeCompileOptions, '', 'build-ninja');
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('linux-gcc-ninja/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/codegentest');
          }
        }

        stage('linux-gcc-ninja/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/SGCTTest');
          }
        }

        stage('linux-gcc-ninja/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/GhoulTest');
          }
        }

        stage('linux-gcc-ninja/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/OpenSpaceTest');
          }
        }
      }
      cleanWs()
    } // node('linux')
  }
},
linux_clang_make: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux-clang') {
      stage('linux-clang-make/scm') {
        deleteDir()
        gitHelper.checkoutGit(url, branch);
      }

      stage('linux-clang-make/build') {
          def cmakeCompileOptions = moduleCMakeFlags()
          cmakeCompileOptions += ' -DMAKE_BUILD_TYPE=Release'
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Make(), compileHelper.Clang(), cmakeCompileOptions, '', 'build-make');
          compileHelper.recordCompileIssues(compileHelper.Clang());
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('linux-clang-make/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/codegentest');
          }
        }

        stage('linux-clang-make/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/SGCTTest');
          }
        }

        stage('linux-clang-make/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/GhoulTest');
          }
        }

        stage('linux-clang-make/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/OpenSpaceTest');
          }
        }
      }
      cleanWs()
    } // node('linux')
  }
},
linux_clang_ninja: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux-clang') {
      stage('linux-clang-ninja/scm') {
        deleteDir()
        gitHelper.checkoutGit(url, branch);
      }

      stage('linux-clang-ninja/build') {
          def cmakeCompileOptions = moduleCMakeFlags()
          cmakeCompileOptions += '-DMAKE_BUILD_TYPE=Release'
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Ninja(), compileHelper.Clang(), cmakeCompileOptions, '', 'build-ninja');
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('linux-clang-ninja/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/codegentest');
          }
        }

        stage('linux-clang-ninja/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/SGCTTest');
          }
        }

        stage('linux-clang-ninja/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/GhoulTest');
          }
        }

        stage('linux-clang-ninja/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/OpenSpaceTest');
          }
        }
      }
      cleanWs()
    } // node('linux')
  }
},
windows_msvc: {
  if (env.USE_BUILD_OS_WINDOWS == 'true') {
    node('windows') {
      stage('windows-msvc/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }

      stage('windows-msvc/build') {
        compileHelper.build(compileHelper.VisualStudio(), compileHelper.VisualStudio(), moduleCMakeFlags(), '', 'build-msvc');
        compileHelper.recordCompileIssues(compileHelper.VisualStudio());
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('windows-msvc/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin\\Debug\\codegentest');
          }
        }

        stage('windows-msvc/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin\\Debug\\SGCTTest');
          }
        }

        stage('windows-msvc/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin\\Debug\\GhoulTest');
          }
        }

        stage('windows-msvc/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin\\Debug\\OpenSpaceTest');
          }
        }
      }
      cleanWs()
    } // node('windows')
  }
},
// windows_ninja: {
//   if (env.USE_BUILD_OS_WINDOWS == 'true') {
//     node('windows') {
//       ws("${env.JENKINS_BASE}/O/${env.BRANCH_NAME}/${env.BUILD_ID}") {
//         stage('windows-ninja/scm') {
//           deleteDir();
//           gitHelper.checkoutGit(url, branch);
//         }
//         stage('windows-ninja/build') {
//           compileHelper.build(compileHelper.Ninja(), compileHelper.VisualStudio(), moduleCMakeFlags(), '', 'build-ninja');
//         }
//         stage('windows-ninja/test') {
//           // Currently, the unit tests are failing on Windows
//           // testHelper.runUnitTests('bin\\Debug\\OpenSpaceTest')
//         }
//       } // node('windows')
//       cleanWs()
//     } // node('windows')
//   }
// },
macos_make: {
  if (env.USE_BUILD_OS_MACOS == 'true') {
    node('macos') {
      stage('macos-make/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }

      stage('macos-make/build') {
          compileHelper.build(compileHelper.Make(), compileHelper.Clang(), moduleCMakeFlags(), '', 'build-make');
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('macos-make/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug/codegentest');\
          }
        }

        stage('macos-make/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug\\SGCTTest');
          }
        }

        stage('macos-make/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug\\GhoulTest');
          }
        }

        stage('macos-make/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug/OpenSpaceTest');
          }
        }
      }
      cleanWs()
    } // node('macos')
  }
},
macos_xcode: {
  if (env.USE_BUILD_OS_MACOS == 'true') {
    node('macos') {
      stage('macos-xcode/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }

      stage('macos-xcode/build') {
          compileHelper.build(compileHelper.Xcode(), compileHelper.Xcode(), moduleCMakeFlags(), '', 'build-xcode');
      }

      if (env.RUN_UNIT_TESTS == 'true') {
        stage('macos-xcode/test-codegen') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug/codegentest');
          }
        }

        stage('macos-xcode/test-sgct') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug\\SGCTTest');
          }
        }

        stage('macos-xcode/test-ghoul') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug\\GhoulTest');
          }
        }

        stage('macos-xcode/test-openspace') {
          timeout(time: 2, unit: 'MINUTES') {
            testHelper.runUnitTests('bin/Debug/OpenSpaceTest');
          }
        }
      }
       cleanWs()
    } // node('macos')
  }
}
