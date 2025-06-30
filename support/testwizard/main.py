##########################################################################################
#                                                                                        #
# OpenSpace Visual Test Wizard                                                           #
#                                                                                        #
# Copyright (c) 2025                                                                     #
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

from openspace import Api
import asyncio
import json
from tabulate import tabulate

async def createCommandNavigationState(openspace):
  include = ""
  while include != "y" and include != "n":
    include = input("Include timestamp? (y/n, default 'y'): ")
    include = include or "y"

  navstate = await openspace.navigation.getNavigationState()
  res = {
    "type": "navigationstate",
    "value": {
      "anchor": navstate["Anchor"],
      "position": navstate["Position"]
    }
  }

  if "Aim" in navstate:
    res["value"]["aim"] = navstate["Aim"]
  if "Pitch" in navstate:
    res["value"]["pitch"] = navstate["Pitch"]
  if "ReferenceFrame" in navstate:
    res["value"]["referenceFrame"] = navstate["ReferenceFrame"]
  if "Up" in navstate:
    res["value"]["up"] = navstate["Up"]
  if "Yaw" in navstate:
    res["value"]["yaw"] = navstate["Yaw"]

  if include == "y" and ("Timestamp" in navstate):
    res["value"]["timestamp"] = navstate["Timestamp"]

  return res


async def createCommandAsset(openspace):
  assets = await openspace.asset.rootAssets()
  folder = await openspace.absPath("${ASSETS}")
  assets = [asset[len(folder)+1:asset.find(".")] for index,asset in assets.items()]

  print("List of assets:")
  for index,asset in enumerate(assets):
    print(  f"  ({index+1}): {asset}")

  asset = ""
  while not asset.isnumeric():
    asset = input("Select which asset to load: ")

    if asset == "":
      return None

    idx = int(asset) - 1

    if idx < 0 or idx >= len(assets):
      print("Invalid index")
      continue

  asset = assets[idx]

  return {
    "type": "asset",
    "value": f"{asset}.asset"
  }


async def createCommandProperty(openspace):
  uri = input("URI of the property: ")
  if uri == "":
    return None

  if uri.find("*") != -1 or uri.find("{") != -1:
    # The URI refers to multiple property values, so we need to as for the specific value
    value = input("Enter value of the property: ")
  else:
    has_property = await openspace.hasProperty(uri)
    if not has_property:
      print(f"Property '{uri}' not found")
      return None

    # This is a property identifier that we can use to request a value for
    value = await openspace.propertyValue(uri)

  return {
    "type": "property",
    "value": {
      "property": uri,
      "value": value
    }
  }


async def createCommandWait(openspace):
  wait = input("Number of seconds to wait: ")
  if wait == "":
    return None

  return {
    "type": "wait",
    "value": wait
  }


async def createCommandScript(openspace):
  script = input("Script that should be executed: ")

  return {
    "type": "script",
    "value": script
  }


async def createCommandTime(openspace):
  time = await openspace.time.UTC()

  return {
    "type": "time",
    "value": time
  }


async def createCommandPause(openspace):
  pause = input("New pause state: (y/n). Default 'y': ")
  pause = pause or "y"

  p = ""
  if pause == "y":
    p = "true"
  else:
    p = "false"

  return {
    "type": "pause",
    "value": p
  }


async def createCommandDeltatime(openspace):
  dt = await openspace.time.deltaTime()

  return {
    "type": "deltatime",
    "value": dt
  }


async def createCommandAction(openspace):
  actions = await openspace.action.actions()
  actions = [action for index,action in actions.items()]

  print("List of actions:")
  for index,action in enumerate(actions):
    print(  f"  ({index+1}): {action["Name"]} ({action["Identifier"]})")

  action = ""
  while not action.isnumeric():
    action = input("Select which action to execute: ")

    if action == "":
      return None

    idx = int(action) - 1

    if idx < 0 or idx >= len(actions):
      print("Invalid index")
      continue

  action = actions[idx]

  return {
    "type": "action",
    "value": action["Identifier"]
  }


AllCommands = [
  [
    "Navigation state",
    createCommandNavigationState,
    "Stores the current position of the camera (and optionally the time). When the regression test is run, the saved camera position is restored."
  ],
  [
    "Asset",
    createCommandAsset,
    "Causes the regression test to load a specific asset file that needs to be provided manually."
  ],
  [
    "Property",
    createCommandProperty,
    "Requests the current value of a specified property. When the regression test is run, the specified property will be set to the stored value."
  ],
  [
    "Wait",
    createCommandWait,
    "Causes the test to wait for a specified number of seconds before progressing to the next command in the test."
  ],
  [
    "Script",
    createCommandScript,
    "Requests a Lua script that will be executed by the regression test."
  ],
  [
    "Time",
    createCommandTime,
    "Stores the current in-game time and restore it when the regression test is run."
  ],
  [
    "Pause",
    createCommandPause,
    "Will either pause or resume the simulation time while running the test. This command should generally not be used as a changing clock can generally easily trigger small deviations that will cause image tests to fail."
  ],
  [
    "Deltatime",
    createCommandDeltatime,
    "Requests a value for the delta time, which will the be set when the regression test is run. This command should not be used as it requires the in-game time to be unpaused, which can easily trigger small deviations that cause image tests to fail."
  ],
  [
    "Action",
    createCommandAction,
    "Provides a selection of registered actions that can be triggered in the regression test."
  ]
]


async def internalRun(openspace):
  test = {}
  profile = await openspace.profile()
  profile = profile[:profile.find('.')]
  profile = profile.replace("\\", "/")
  test["profile"] = profile

  name = input("Enter the name of the test: ")
  if name == "":
    raise "Must provide a name for the test"

  test["commands"] = []


  while True:
    print()
    print("Select command to add to the test:")
    for index,cmd in enumerate(AllCommands):
      print(f"  ({index+1}): {cmd[0]}")
    print("Finalize the test by selecting the empty value.")
    selection = input()
    if selection == "":
      break

    if not selection.isnumeric():
      print("Invalid index")
      continue

    idx = int(selection) - 1

    if idx < 0 or idx >= len(AllCommands):
      print("Invalid index")
      continue

    cmd = AllCommands[idx]
    func = cmd[1]
    command = await func(openspace)
    if command:
      print(f"Adding command {cmd[0]}")
      test["commands"].append(command)

  # Add screenshot command at the end
  test["commands"].append({ "type": "screenshot" })

  # Save the test to disk
  with open(f"{name}.ostest", "w") as fp:
    json.dump(test, fp, indent=2)
    fp.write("\n")

async def mainLoop():
  api = Api("localhost", 4681)
  api.connect()
  openspace = await api.singleReturnLibrary()

  await asyncio.create_task(internalRun(openspace))
  api.disconnect()

print("OpenSpace Visual Test Creation Wizard")
print("=====================================")
print("This wizard helps creating image regression test files. To use it, first start ")
print("OpenSpace with the profile for which to create a test. Then, execute this script ")
print("and select the commands in the order in which the test should execute them. ")
print("After picking all commands, leaving the prompt empty finalizes the test.")
print()
print(tabulate([[x[0], x[2]] for x in AllCommands], tablefmt="plain", maxcolwidths=[20, 60]))
print()

asyncio.new_event_loop().run_until_complete(mainLoop())
