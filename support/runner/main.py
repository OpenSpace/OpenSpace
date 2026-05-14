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

import argparse
import datetime
import json
import os
import requests
import shutil
import time
from pathlib import Path
from testsuite.openspace import write_configuration_overwrite, run_single_test
from testsuite.test import TestResult



# TODO: Retry openspace api connection until it works
# TODO: 'screenshot' command has optional argument to determine sub-test name
# TODO: Instead of waiting a fixed amount of time when starting OpenSpace, we can listen
#       to the finished loading event instead

def submit_image(result: TestResult, hardware: str, timestamp: str, file: str,
                 runner: str, url: str) -> None:
  """
  Submits a new candidate image to the provided URL. This function logs a method
  indicating whether the image submission succeeded
  """
  with open(file, "rb") as f:
    res = requests.post(
      url,
      data = {
        "group": result.group,
        "name": result.name,
        "hardware": hardware,
        "runnerID": runner,
        "timestamp": timestamp,
        "timing": result.timing,
        "commitHash": result.commit
      },
      files = {
        "file": f,
        "log": result.error
      }
    )
  if res.status_code == 200:
    print("Image submitted successfully")
    print(f"  Group: {result.group}")
    print(f"  Name: {result.name}")
    print(f"  Hardware: {hardware}")
  else:
    print(f"Image submission failed with error {res.status_code}")
    print(res.text)



def store_image(result: TestResult, file: str) -> None:
  """
  Stores the images of the provided `TestResult` locally by creating the necessary folders
  if they don't exist and then saving the image. Only the latest test result are stored.
  """
  dest_folder = Path("tests") / result.group
  dest_folder.mkdir(parents=True, exist_ok=True)
  destination = dest_folder / f"{result.name}.png"
  print(f"Copying file {file} -> {destination}")
  shutil.copy(file, destination)



if __name__ == "__main__":
  parser = argparse.ArgumentParser()
  parser.add_argument(
    "-t", "--test",
    dest="test",
    type=str,
    help="A comma-separated list of specific tests that should be run. If this value is "
      "omitted, all tests will be run. Each of the comma-separated entries should be "
      "a path relative to the base visual testing folder without a file extension. For "
      "example, if there is a test file called `default/earth.ostest`, then the value "
      "provided to this argument should be `default/earth`.",
    required=False
  )
  parser.add_argument(
    "-o", "--overwrite",
    dest="overwrite_path",
    type=str,
    help="This specifies the base path to the folder where data is stored that is reused "
      "between different test runs. The overwrite file will contain settings that we "
      "want all regularly executing test machines to have, such as using caching, "
      "reusing synchronization folders, etc.",
    required=False
  )
  parser.add_argument(
    "-a", "--attach",
    dest="attach",
    help="Run a single test against an already-running OpenSpace instance without "
      "starting or closing it. Requires exactly one test to be specified via --test. "
      "When used, --dir is optional; if omitted the test path is resolved relative to "
      "the current working directory.",
    required=False,
    action="store_true",
    default=False
  )
  args = parser.parse_args()

  # Finding the root OpenSpace folder
  root_dir = Path(__file__).resolve().parents[2]


  total_start: float = time.perf_counter()
  submit_images = False
  submit_url = ""
  hardware = ""
  runner_id = ""
  per_profile_wait: dict[str, int] = {}
  if Path("config.json").exists():
    submit_images = True
    with open("config.json") as f:
      config = json.load(f)
      url: str = config["url"]
      submit_url = f"{url}/api/submit-test"
      hardware = config["hardware"]
      runner_id = config["id"]
      per_profile_wait = config["per-profile-wait"]
    print(f"Submit URL: {submit_url}")
    print(f"Hardware: {hardware}")
    print(f"ID: {runner_id}")
    print(f"Per Profile wait: {per_profile_wait}")
  else:
    print("No 'config.json' provided. Test results will be stored locally instead")


  executable: Path | None = None
  if args.attach:
    if not args.test or "," in args.test:
      raise SystemExit("--attach requires exactly one test, not a comma-separated list")
  else:
    # Find the executable location and its name
    if os.name == "nt":
      # Windows
      executable = root_dir / "bin" / "RelWithDebInfo" / "OpenSpace.exe"
    else:
      # Linux
      executable = root_dir / "bin" / "OpenSpace"
    if not executable.exists():
      raise SystemExit(f"Could not find executable '{executable}'")

    if args.overwrite_path is not None:
      write_configuration_overwrite(root_dir, Path(args.overwrite_path))

  if executable is None:
    raise SystemExit("Could not find executable")


  # Running the tests
  if args.attach:
    test_arg = args.test.strip()
    path = Path(test_arg) if Path(test_arg).is_file() else root_dir / "visualtests" / f"{test_arg}.ostest"

    if not path.is_file():
      raise SystemExit(f"Could not find test '{path}'")

    timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat()
    try:
      result = run_single_test(path, None, {})
    except Exception as e:
      print(f"Test '{path}' failed with error: {e}")
    else:
      if result is not None:
        for file in result.files:
          if submit_images:
            submit_image(result, hardware, timestamp, file, runner_id, submit_url)
          else:
            store_image(result, file)

  else:
    if args.test is None:
      print("Running all tests in OpenSpace folder")
      files = list((root_dir / "visualtests").rglob("*.ostest"))
    else:
      tests = [t.strip() for t in args.test.split(",")]
      print(f"Running tests: {tests}")
      files = [root_dir / "visualtests" / f"{test}.ostest" for test in tests]
      for path in files:
        if not Path(path).is_file():
          raise SystemExit(f"Could not find test '{path}'")

    for file in files:
      timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat()

      try:
        result = run_single_test(file, executable, per_profile_wait)
      except Exception as e:
        print(f"Test '{file}' failed with error: {e}")
        continue
      if result is None:
        continue
      for img in result.files:
        if submit_images:
          submit_image(result, hardware, timestamp, img, runner_id, submit_url)
        else:
          store_image(result, img)
      time.sleep(5.0)

  total_end = time.perf_counter()
  print(f"Total time for all tests: {total_end - total_start}")
