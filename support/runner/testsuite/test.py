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
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any
from .instruction import Instruction

@dataclass
class TestResult:
  """
  Stores the result of a single test run. It has the following members:
    - `group`: The name of the group for which this is the test result
    - `name`: The name of the test for which is the result
    - `files`: The list of screenshots that were taken during the test. Each entry in this
               list is a path to an image file
    - `timing`: The number of seconds it took to execute the test
    - `commit`: The commit hash for OpenSpace that was used to run the test
    - `error`: The contents of the error stream that was captured during the test run
  """
  group: str = ""
  name: str = ""
  files: list[str] = field(default_factory=list)
  timing: float = 0.0
  commit: str = ""
  error: str = ""

class Test:
  """
  This class represents an entire test run, consisting of multiple Instructions and a
  profile that should be used.

  For now only a single screenshot instruction is supported.
  """
  skipTest: bool
  profile: str
  instructions: list[Instruction]
  group: str
  name: str
  test_path: str

  def __init__(self, path: str) -> None:
    if not Path(path).is_file():
      raise Exception(f"Could not find test {path}")

    self.test_path = Path(path).as_posix()

    with open(path) as f:
      content = json.load(f)

    if "profile" not in content:
      raise Exception(f"Missing 'profile' in test {path}'")
    self.profile = content["profile"]

    if "commands" not in content:
      raise Exception(f"Missing 'commands' in test {path}")

    self.skipTest = content.get("skip_test", False)

    self.instructions = []
    for command in content["commands"]:
      try:
        self.instructions.append(Instruction(command))
      except Exception as error:
        raise Exception(f"Error loading test {path}: {error}")

    number_screenshot_instruction = sum(1 for i in self.instructions if i.type == "screenshot")
    if number_screenshot_instruction == 0:
      raise Exception(f"Error loading test {path}: No screenshot instruction")
    if number_screenshot_instruction > 1:
      raise Exception(f"Error loading test {path}: Only a single screenshot supported")


    # Get the testname by removing everything before (and including) "test/visual" and
    # also removing the extension
    start_idx = self.test_path.find("visualtests") + len("visualtests") + 1
    full = self.test_path[start_idx:-len(".ostest")]
    parts = full.split("/")

    # The last part is the name of test, all others are combined to make the grouping
    self.group = "-".join(parts[0:-1])
    self.name = parts[-1]


  async def run(self, openspace: Any, os_api: Any) -> None:
    """
    Runs the actual instructions on the provided OpenSpace API instance. There is a
    mandatory 1s wait time between every instructions
    """
    for instruction in self.instructions:
      await instruction.run(openspace, os_api)
      await asyncio.sleep(1.0)
