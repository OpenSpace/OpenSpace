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
  def dirs = readDir();
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
  for (module in dirs) {
      flags += "-D OPENSPACE_MODULE_${module.toUpperCase()}=ON "
  }
  return flags;
}

// echo flags

//
// Pipeline start
//

parallel master: {
  node('master') {
    stage('master/scm') {
      deleteDir();
      gitHelper.checkoutGit(url, branch);
      helper.createDirectory('build');
    }
    stage('master/cppcheck/create') {
      sh 'cppcheck --enable=all --xml --xml-version=2 -i ext --suppressions-list=support/cppcheck/suppressions.txt include modules src tests 2> build/cppcheck.xml';
    }
    stage('master/cloc/create') {
      sh 'cloc --by-file --exclude-dir=build,data,ext --xml --out=build/cloc.xml --force-lang-def=support/cloc/langDef --quiet .';
    }
  }
},
linux: {
  node('linux') {
    stage('linux/scm') {
      deleteDir()
      gitHelper.checkoutGit(url, branch);
    }
    stage('linux/build') {
        // Not sure why the linking of OpenSpaceTest takes so long
        compileHelper.build(compileHelper.Make(), compileHelper.Gcc(), moduleCMakeFlags(), 'OpenSpace', 'build-all');
    }
    stage('linux/warnings') {
      compileHelper.recordCompileIssues(compileHelper.Gcc());
    }
    stage('linux/test') {
      // testHelper.runUnitTests('build/OpenSpaceTest');
    }
  } // node('linux')
},
windows: {
  node('windows') {
    ws("${env.JENKINS_BASE}/O/${env.BRANCH_NAME}/${env.BUILD_ID}") {
      stage('windows/scm') {
        deleteDir();
        gitHelper.checkoutGit(url, branch);
      }
      stage('windows/build') {
        compileHelper.build(compileHelper.VisualStudio(), compileHelper.VisualStudio(), moduleCMakeFlags(), '', 'build-all');
      }
      stage('windows/warnings') {
        compileHelper.recordCompileIssues(compileHelper.VisualStudio());
      }
      stage('windows/test') {
        // Currently, the unit tests are failing on Windows
        // testHelper.runUnitTests('bin\\Debug\\OpenSpaceTest')
      }
    } // node('windows')
  }
},
osx: {
  node('osx') {
    stage('osx/scm') {
      deleteDir();
      gitHelper.checkoutGit(url, branch);
    }
    stage('osx/build') {
        compileHelper.build(compileHelper.Xcode(), compileHelper.Clang(), moduleCMakeFlags(), '', 'build-all');
    }
    stage('osx/warnings') {
      compileHelper.recordCompileIssues(compileHelper.Clang());
    }
    stage('osx/test') {
      // Currently, the unit tests are crashing on OS X
      // testHelper.runUnitTests('build/Debug/OpenSpaceTest')
    }
  } // node('osx')
}

//
// Post-build actions
//
node('master') {
  stage('master/cppcheck/publish') {
    // publishCppcheck(pattern: 'build/cppcheck.xml');
  }
  stage('master/cloc/publish') {
    sloccountPublish(encoding: '', pattern: 'build/cloc.xml');
  }
  stage('master/notifications') {
    slackHelper.sendChangeSetSlackMessage(currentBuild);
  }
}
