##########################################################################################
#                                                                                        #
# OpenSpace Visual Testing                                                               #
#                                                                                        #
# Copyright (c) 2024-2026                                                                #
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

import asyncio
import glob
import os
import subprocess
import time
from openspace import Api
from .test import Test, TestResult



def write_configuration_overwrite(base_path, data_path):
  """
  Creates a openspace.cfg override file that sets up a common testing environment. These
  are mostly for enabling caching to reduce the amount of testing and removing as much of
  the logging as possible which would otherwise pollute the usage results
  """
  with open(f"{base_path}/openspace.cfg.override", "w") as f:
    # Use a common sync folder outside of the build to prevent redownloading of data
    sync_location = f"{data_path}/sync"
    f.write(f"Paths.SYNC = os.getenv([[OPENSPACE_SYNC]]) or [[{sync_location}]]\n")

    # Enable MRF caching for the same reason and to reduce dependency on external servers
    mrf_location = f"{data_path}/mrf"
    f.write("ModuleConfigurations.GlobeBrowsing.MRFCacheEnabled = true\n")
    f.write(f"ModuleConfigurations.GlobeBrowsing.MRFCacheLocation = [[{mrf_location}]]\n")

    # Disable CEF WebGUI
    f.write("ModuleConfigurations.CefWebGui.Enabled = false\n")

    # Disable the WebSocket as it is throwing error messages that pollute the log
    f.write("Server.Interfaces[2].Enabled = false\n")

    # Remove the version checking URL as it would otherwise count as a "user"
    f.write("VersionCheckUrl = [[]]\n")

    # Setting this to true will cause OpenGL errors to appear in the log
    f.write("CheckOpenGLState = true\n")

    # We can reduce the amount of time that we have to wait for OpenSpace to shut down
    f.write("ShutdownCountdown = 0.25\n")



async def setup_test_run(openspace):
  """
  Setup settings that are common to all test runs. These are, in general, settings that
  are reasonably different between runs of OpenSpace, such as the local time, the commit
  hash, and others
  """
  # We always want to start paused to prevent some timing-related inconsistencies
  await openspace.time.setPause(True)

  # Unless explicitly added, we don't want display elements that show variable content
  #  User Interface: Making the screenshots nicer to look at
  #  Dashboard: Framerate
  #  ScreenLog: Log message retention
  #  Version: Contains the commit hash
  #  Camera: Not technically needed, but results in a cleaner screenshot
  await openspace.setPropertyValueSingle("Dashboard.IsEnabled", False)
  await openspace.setPropertyValueSingle("RenderEngine.ShowLog", False)
  await openspace.setPropertyValueSingle("RenderEngine.ShowVersion", False)
  await openspace.setPropertyValueSingle("RenderEngine.ShowCamera", False)



async def internal_run(openspace, test, shutdown=True):
  """
  This function runs the actual test with the library object passed into it. It first sets
  up default values, then runs the individual instructions for the test, and retrieves
  other information such as the screenshot location, and the commit hash from OpenSpace.

  This function assumes that the `openspace` library object is already authenticated and
  connected to the OpenSpace instance and is ready to take commands.

  If `shutdown` is False, the OpenSpace instance will not be shut down after the test.
  """
  print("  Starting test")
  await setup_test_run(openspace)
  await test.run(openspace)
  print("  Finished test")

  # Get the location of the screenshot folder from OpenSpace. It should always be the
  # same but this is just to make sure it will work
  screenshot_folder = await openspace.absPath("${SCREENSHOTS}")

  # Get the commit hash from OpenSpace itself
  version = await openspace.version()
  commit = version["Commit"]

  if shutdown:
    await openspace.toggleShutdown()

  return screenshot_folder, commit



def run_single_test(test_path, executable, per_profile_wait) -> TestResult:
  """
  Run the single test provided by `test_path` using the OpenSpace executable provided by
  `executable`. This will include starting OpenSpace as a subprocess using a known
  configuration file and the profile required by the profile, establishing a connection
  using the Python API to the OpenSpace instance, running the test's instructions and
  returning the results.

   - `test_path`: The path to the ostest file that should be run. This file must exist
   - `executable`: The path to the OpenSpace executable that should be run for the tests
  """
  print(f"Running test: {test_path}")
  test = Test(test_path)

  # Skip the test if the test-creator asked for it
  if test.skipTest:
    print(f"  Skipping test {test_path}")
    return None

  start_time = time.perf_counter()
  print(f"  Starting OpenSpace (Profile: {test.profile})")
  process = subprocess.Popen(
    [
      os.path.abspath(executable),
      "--config", f"{os.getcwd()}/1920-1080.json",
      "--profile", test.profile,
      "--bypassLauncher"
    ],
    cwd=os.path.dirname(os.path.abspath(executable)),
    stdout=subprocess.DEVNULL,
    stderr=subprocess.PIPE
  )

  # Add a sleeping time instead of repeatedly trying to reconnect. Starting up OpenSpace
  # in general takes longer than this, so we don't actually lose any time. If a
  # per-profile wait time is specified, we use that, otherwise we wait 15 seconds
  wait_timer = per_profile_wait.get(test.profile, 15)
  time.sleep(wait_timer)

  async def mainLoop():
    """
    The main loop of an async event loop that is needed for the OpenSpace Python API to
    work correctly. This will connect to OpenSpace, which then triggers the rest of the
    test run
    """
    print("  Connecting...")
    os_api = Api("localhost", 4681)
    os_api.connect()
    openspace = await os_api.singleReturnLibrary()
    # Injecting the main API into the library as we use it in some test instructions
    openspace.__api__ = os_api
    print("  Connected to OpenSpace")
    screenshot_folder, commit = await asyncio.create_task(internal_run(openspace, test))
    os_api.disconnect()
    return screenshot_folder, commit

  screenshot_folder, commit = asyncio.new_event_loop().run_until_complete(mainLoop())


  # Another wait while OpenSpace is shutting down
  time.sleep(5)

  # Get the error log from the OpenSpace subprocess
  error_log = process.stderr.read().decode()

  # Kill the OpenSpace subprocess
  process.kill()
  end_time = time.perf_counter()

  # Collect all screenshots taken by the test
  files = glob.glob(f"{screenshot_folder}/*.png")
  print(f"Test images: {files}")

  result = TestResult()
  result.group = test.group
  result.name = test.name
  result.files = files
  result.timing = end_time - start_time
  result.commit = commit
  result.error = error_log
  return result



def run_single_test_attached(test_path) -> TestResult:
  """
  Run the single test provided by `test_path` against an already-running OpenSpace
  instance. Unlike `run_single_test`, this function does not start or stop OpenSpace —
  it only connects to the running instance, executes the test, and then disconnects.

   - `test_path`: The path to the ostest file that should be run. This file must exist
  """
  print(f"Running test (attached): {test_path}")
  test = Test(test_path)

  start_time = time.perf_counter()

  async def mainLoop():
    """
    The main loop of an async event loop that is needed for the OpenSpace Python API to
    work correctly. This will connect to OpenSpace, which then triggers the rest of the
    test run
    """
    print("  Connecting...")
    os_api = Api("localhost", 4681)
    os_api.connect()
    openspace = await os_api.singleReturnLibrary()
    # Injecting the main API into the library as we use it in some test instructions
    openspace.__api__ = os_api
    print("  Connected to OpenSpace")
    screenshot_folder, commit = await asyncio.create_task(
      internal_run(openspace, test, shutdown=False)
    )
    os_api.disconnect()
    return screenshot_folder, commit

  screenshot_folder, commit = asyncio.new_event_loop().run_until_complete(mainLoop())

  end_time = time.perf_counter()

  # Collect all screenshots taken by the test
  files = glob.glob(f"{screenshot_folder}/*.png")
  print(f"Test images: {files}")

  result = TestResult()
  result.group = test.group
  result.name = test.name
  result.files = files
  result.timing = end_time - start_time
  result.commit = commit
  result.error = ""
  return result
