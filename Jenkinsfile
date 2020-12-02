import groovy.io.FileType

library('sharedSpace'); // jenkins-pipeline-lib


def url = 'https://github.com/OpenSpace/OpenSpace';
def branch = env.BRANCH_NAME;

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

  // def dirs = readDir();
  // def currentDir = new File('.')
  // def dirs = []
  // currentDir.eachFile FileType.DIRECTORIES, {
  //     dirs << it.name
  // }
  // def moduleFlags = [
  //   'atmosphere',
  //   'base',
  //   // 'cefwebgui',
  //   'debugging',
  //   'digitaluniverse',
  //   'fieldlines',
  //   'fieldlinessequence',
  //   'fitsfilereader',
  //   'gaia',
  //   'galaxy',
  //   'globebrowsing',
  //   'imgui',
  //   'iswa',
  //   'kameleon',
  //   'kameleonvolume',
  //   'multiresvolume',
  //   'server',
  //   'space',
  //   'spacecraftinstruments',
  //   'space',
  //   'spout',
  //   'sync',
  //   'touch',
  //   'toyvolume',
  //   'volume',
  //   // 'webbrowser',
  //   // 'webgui'
  // ];

  def flags = '';
  for (module in modules) {
      flags += "-DOPENSPACE_MODULE_${module.toUpperCase()}=ON "
  }
  return flags;
}

// echo flags

//
// Pipeline start
//

parallel tools: {
  node('tools') {
    stage('tools/scm') {
      deleteDir();
      gitHelper.checkoutGit(url, branch, false);
      helper.createDirectory('build');
    }
    stage('tools/cppcheck') {
      sh(
        script: 'cppcheck --enable=all --xml --xml-version=2 -i ext --suppressions-list=support/cppcheck/suppressions.txt include modules src tests 2> build/cppcheck.xml',
        label: 'CPPCheck'
      )
      recordIssues(
        id: 'tools-cppcheck',
        tool: cppCheck(pattern: 'build/cppcheck.xml')
      ) 
    }  
    cleanWs()
  } // node('tools')
},
linux_gcc: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux' && 'gcc') {
      stage('linux-gcc/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }
      stage('linux-gcc/build(make)') {
          def cmakeCompileOptions = moduleCMakeFlags();
          cmakeCompileOptions += ' -DMAKE_BUILD_TYPE=Release';
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Make(), compileHelper.Gcc(), cmakeCompileOptions, 'OpenSpace', 'build-make');
          compileHelper.recordCompileIssues(compileHelper.Gcc());
      }
      stage('linux-gcc/build(ninja)') {
          def cmakeCompileOptions = moduleCMakeFlags();
          cmakeCompileOptions += '-DMAKE_BUILD_TYPE=Release';
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Ninja(), compileHelper.Gcc(), cmakeCompileOptions, 'OpenSpace', 'build-ninja');
      }
      stage('linux-gcc/test') {
        // testHelper.runUnitTests('build/OpenSpaceTest');
      }
      cleanWs()
    } // node('linux')
  }
},
linux_clang: {
  if (env.USE_BUILD_OS_LINUX == 'true') {
    node('linux' && 'clang') {
      stage('linux-clang/scm') {
        deleteDir()
        gitHelper.checkoutGit(url, branch);
      }
      stage('linux-clang/build(make)') {
          def cmakeCompileOptions = moduleCMakeFlags()
          cmakeCompileOptions += ' -DMAKE_BUILD_TYPE=Release'
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Make(), compileHelper.Clang(), cmakeCompileOptions, 'OpenSpace', 'build-make');
          compileHelper.recordCompileIssues(compileHelper.Clang());
      }
      stage('linux-clang/build(ninja)') {
          def cmakeCompileOptions = moduleCMakeFlags()
          cmakeCompileOptions += '-DMAKE_BUILD_TYPE=Release'
          // Not sure why the linking of OpenSpaceTest takes so long
          compileHelper.build(compileHelper.Ninja(), compileHelper.Clang(), cmakeCompileOptions, 'OpenSpace', 'build-ninja');
      }
      stage('linux-clang/test') {
        // testHelper.runUnitTests('build/OpenSpaceTest');
      }
      cleanWs()
    } // node('linux')
  }
},
windows: {
  if (env.USE_BUILD_OS_WINDOWS == 'true') {
    node('windows') {
      ws("${env.JENKINS_BASE}/O/${env.BRANCH_NAME}/${env.BUILD_ID}") {
        stage('windows/scm') {
          deleteDir();
          gitHelper.checkoutGit(url, branch);
        }
        stage('windows/build(msvc)') {
          compileHelper.build(compileHelper.VisualStudio(), compileHelper.VisualStudio(), moduleCMakeFlags(), '', 'build-msvc');
          compileHelper.recordCompileIssues(compileHelper.VisualStudio());
        }
        stage('windows/build(ninja)') {
          compileHelper.build(compileHelper.Ninja(), compileHelper.VisualStudio(), moduleCMakeFlags(), '', 'build-ninja');
        }
        stage('windows/test') {
          // Currently, the unit tests are failing on Windows
          // testHelper.runUnitTests('bin\\Debug\\OpenSpaceTest')
        }
      } // node('windows')
      cleanWs()
    } // node('windows')
  }
},
macos: {
  if (env.USE_BUILD_OS_MACOS == 'true') {
    node('macos') {
      stage('macos/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }
      stage('macos/build(make)') {
          compileHelper.build(compileHelper.Make(), compileHelper.Clang(), moduleCMakeFlags(), '', 'build-make');
      }
      stage('macos/build(xcode)') {
          compileHelper.build(compileHelper.Xcode(), compileHelper.Xcode(), moduleCMakeFlags(), '', 'build-xcode');
      }
      stage('macos/test') {
        // Currently, the unit tests are crashing on OS X
        // testHelper.runUnitTests('build/Debug/OpenSpaceTest')
      }
      cleanWs()
    } // node('macos')
  }
}
