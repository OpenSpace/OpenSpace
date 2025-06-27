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

async def createCommandNavigationState(openspace):
  include = input("Include timestamp (y/n). Default 'y'.")
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
  asset = input("Enter the name of the asset to load (path relative to data/asset). The '.asset' extension is added automatically.")
  if asset == "":
    return {}

  return {
    "type": "asset",
    "value": f"{asset}.asset"
  }


async def createCommandProperty(openspace):
  uri = input("Enter the URI of the property.")
  if uri == "":
    return {}

  value = openspace.propertyValue(uri)

  return {
    "type": "property",
    "value": {
      "property": uri,
      "value": value
    }
  }


async def createCommandWait(openspace):
  wait = input("Enter the number of seconds to wait.")
  if wait == "":
    return {}

  return {
    "type": "wait",
    "value": wait
  }


async def createCommandPause(openspace):
  pause = input("New pause state: (y/n). Default 'y'.")
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


async def createCommandScript(openspace):
  script = input("Enter the script that should be executed.")

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


async def createCommandDeltatime(openspace):
  dt = await openspace.time.deltaTime()

  return {
    "type": "deltatime",
    "value": dt
  }


async def createCommandAction(openspace):
  actions = await openspace.action.actions()
  print("List of actions.")
  for index,action in enumerate(actions):
    print(  f"  ({index+1}): {action}")
  action = input("Select which action to to execute or enter the URI of the action.")

  if action == "":
    return {}

  if action.isnumeric():
    action = actions[action]

  return {
    "type": "action",
    "value": action
  }


async def internalRun(openspace):
  test = {}

  print("OpenSpace Visual Test Creation Wizard")
  print("=====================================")
  print()
  text = input("Enter the name of the test: ")
  test["profile"] = input("Name of the profile to use: ")


  print()
  test["commands"] = []
  while True:
    print("Commands to add to the test")
    print("  ( 1): Navigation state")
    print("  ( 2): Asset")
    print("  ( 3): Property")
    print("  ( 4): Wait")
    print("  ( 5): Pause")
    print("  ( 6): Script")
    print("  ( 7): Time")
    print("  ( 8): Deltatime")
    print("  ( 9): Action")
    print("Finalize the test by entering an empty value")
    selection = input()
    if selection == "":
      break

    match int(selection):
      case 1:
        test["commands"].append(await createCommandNavigationState(openspace))
      case 2:
        test["commands"].append(await createCommandAsset(openspace))
      case 3:
        test["commands"].append(await createCommandProperty(openspace))
      case 4:
        test["commands"].append(await createCommandWait(openspace))
      case 5:
        test["commands"].append(await createCommandPause(openspace))
      case 6:
        test["commands"].append(await createCommandScript(openspace))
      case 7:
        test["commands"].append(await createCommandTime(openspace))
      case 8:
        test["commands"].append(await createCommandDeltatime(openspace))
      case 9:
        test["commands"].append(await createCommandAction(openspace))

  with open(f"{text}.ostest", "w") as fp:
    json.dump(test, fp)

async def mainLoop():
  api = Api("localhost", 4681)
  api.connect()
  openspace = await api.singleReturnLibrary()

  await asyncio.create_task(internalRun(openspace))
  api.disconnect()

asyncio.new_event_loop().run_until_complete(mainLoop())
