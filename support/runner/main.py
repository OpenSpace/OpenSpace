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
import glob
import json
import os
import requests
import shutil
import time
from pathlib import Path
from testsuite.constants import test_base_dir
from testsuite.openspace import write_configuration_overwrite, run_single_test, run_single_test_attached
from testsuite.test import TestResult



# TODO: Retry openspace api connection until it works
# TODO: 'screenshot' command has optional argument to determine sub-test name
# TODO: Instead of waiting a fixed amount of time when starting OpenSpace, we can listen
#       to the finished loading event instead

def submit_image(result: TestResult, hardware: str, timestamp: str, file: str,
                 runner: str, url: str):
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



def store_image(result: TestResult, file: str):
  """
  Stores the images of the provided `TestResult` locally by creating the necessary folders
  if they don't exist and then saving the image. Only the latest test result are stored.
  """
  dest_folder = f"tests/{result.group}"
  os.makedirs(dest_folder, exist_ok=True)
  destination = f"{dest_folder}/{result.name}.png"
  print(f"Copying file {file} -> {destination}")
  shutil.copy(file, destination)



def setup_argparse():
  """
  Creates and sets up a parser for commandline arguments. This function returns the parsed
  arguments as a dictionary.
  """
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
    "-dr", "--dry-run",
    dest="dry_run",
    help="Provides a full list of the tests to be run in the proper order, but does not "
      "run any tests.",
    required=False,
    action="store_true",
    default=False
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
  return args


if __name__ == "__main__":
  global_start = time.perf_counter()
  if os.path.exists("config.json"):
    submit_images = True
    with open("config.json") as f:
      config = json.load(f)
      url = config["url"]
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
    submit_images = False


  args = setup_argparse()

  dir = f"{Path(__file__).resolve().parent}/../.."
  if args.attach:
    if not args.test:
      raise Exception("--attach requires exactly one test to be specified via --test")
    if "," in args.test:
      raise Exception("--attach requires exactly one test, not a comma-separated list")
  else:
    # Find the executable location and its name
    if os.name == "nt":
      # Windows
      executable = f"{dir}/bin/RelWithDebInfo/OpenSpace.exe"
    else:
      # Linux
      executable = f"{dir}/bin/OpenSpace"

    if not os.path.exists(executable):
      raise Exception(f"Could not find executable '{executable}'")

  if not args.attach and args.overwrite_path != None:
    write_configuration_overwrite(dir, args.overwrite_path)



  # Running the tests
  if args.attach:
    test_arg = args.test.strip()
    if os.path.isfile(test_arg):
      path = test_arg
    else:
      path = f"{dir}/{test_base_dir}/{test_arg}.ostest"

    if not os.path.isfile(path):
      raise Exception(f"Could not find test '{path}'")

    timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat()
    try:
      result = run_single_test_attached(path)
    except Exception as e:
      print(f"Test '{path}' failed with error: {e}")
    else:
      for file in (result.files if result is not None else []):
        if submit_images:
          submit_image(result, hardware, timestamp, file, runner_id, submit_url)
        else:
          store_image(result, file)

  elif args.test is None:
    print("Running all tests in OpenSpace folder")
    files = glob.glob(f"{dir}/{test_base_dir}/**/*.ostest", recursive=True)
    for file in files:
      # Normalize the path endings to always do forward slashes
      file = file.replace(os.sep, "/")
      timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat()
      if args.dry_run:
        print(f"Test: '{file}' run against executable '{executable}'")
        continue

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
  elif not args.attach:
    tests = args.test.split(",")
    print(f"Running tests: {tests}")
    for test in tests:
      path = f"{dir}/{test_base_dir}/{test}.ostest"
      if not os.path.isfile(path):
        raise Exception(f"Could not find test '{path}'")

      timestamp = datetime.datetime.now(datetime.timezone.utc).isoformat()

      if args.dry_run:
        print(f"Test: '{path}' run against executable '{executable}'")
        continue

      try:
        result = run_single_test(path, executable, per_profile_wait)
      except Exception as e:
        print(f"Test '{path}' failed with error: {e}")
        continue
      if result is None:
        continue
      for file in result.files:
        if submit_images:
          submit_image(result, hardware, timestamp, file, runner_id, submit_url)
        else:
          store_image(result, file)
      time.sleep(5.0)

  global_end = time.perf_counter()
  print(f"Total time for all tests: {global_end - global_start}")
