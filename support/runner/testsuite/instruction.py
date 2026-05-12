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



Allowed_Types = [
  "action",
  "asset",
  "deltatime",
  "navigationstate",
  "pause",
  "property",
  "recording",
  "screenshot",
  "script",
  "time",
  "wait"
]



class Instruction:
  """
  This object represents an individual test instruction. An entire test is made up of many
  instructions. At least one of the instructions should be a screenshot instruction that
  causes the creation of a testable image. See the README file for more information about
  which types of instructions are supported and what parameters they take. In general,
  each paramater needs a `type` that identifies which kind of instruction it is and most
  instructions need a `value` that contains parameters for the instruction.
  """
  def __init__(self, obj):
    if not "type" in obj:
      raise Exception("Missing key 'type'")

    if not obj["type"] in Allowed_Types:
      type = obj["type"]
      raise Exception(f"Invalid type '{type}'")

    self.type = obj["type"]

    # Just as simplification as not all types need a 'value' object
    if not "value" in obj:
      obj["value"] = {}

    self.value = obj["value"]



  def __repr__(self):
    if self.value == {}:
      return f"({self.type})"
    else:
      return f"({self.type}: {self.value})"



  def is_screenshot(self):
    return self.type == "screenshot"



  async def run(self, openspace):
    """
    Runs this instruction against the OpenSpace API object `openspace` that was passed to
    this function. If this instruction is not a valid instruction, either because it has
    a type that is not recognized, or it is missing essential parameters, an Exception is
    raised.
    """

    match self.type:
      case "action":
        print(f"    Action: {self.value}")
        await openspace.action.triggerAction(self.value)

      case "asset":
        print(f"    Asset: {self.value}")
        await openspace.asset.add(self.value)

      case "deltatime":
        print(f"    Deltatime: {self.value}")
        await openspace.time.setDeltaTime(self.value)

      case "navigationstate":
        v = {
          "Anchor": self.value["anchor"],
          "Position": self.value["position"]
        }
        if "aim" in self.value:
          v["Aim"] = self.value["aim"]
        if "referenceFrame" in self.value:
          v["ReferenceFrame"] = self.value["referenceFrame"]
        if "up" in self.value:
          v["Up"] = self.value["up"]
        if "yaw" in self.value:
          v["Yaw"] = self.value["yaw"]
        if "pitch" in self.value:
          v["Pitch"] = self.value["pitch"]
        if "timestamp" in self.value:
          v["Timestamp"] = self.value["timestamp"]

        print(f"    NavigationState: {v}")
        await openspace.navigation.setNavigationState(v, "timestamp" in self.value)

      case "pause":
        print(f"    Set Pause: {self.value}")
        await openspace.time.setPause(self.value)

      case "property":
        prop = self.value["property"]
        val = self.value["value"]
        print(f"    Set Property: {prop} -> {val}")
        await openspace.setPropertyValue(prop, val)

      case "recording":
        print(f"    Start Playback: {self.value}")
        await openspace.sessionRecording.startPlayback(self.value)

      case "screenshot":
        print("    Take Screenshot")
        # We'll wait an extra 5 seconds before taking a screenshot just to be sure that
        # everything is finished
        await asyncio.sleep(5)

        # Take the screenshot
        await openspace.takeScreenshot()

        # Give the screenshot writing some time to finish. It will be a maximum of two
        # frames to write a screenshot + whatever time it takes to write the actual
        # screenshot. The writing should be on the order of 100 ms + about 35 ms for two
        # frames get us to 135 ms. Lets be on the safe side with a 35x margin and go for
        # a wait of 5 seconds
        await asyncio.sleep(5)

      case "script":
        print(f"    Script: {self.value}")
        await openspace.__api__.executeLuaScript(self.value, False, False)

      case "time":
        print(f"    Set Time: {self.value}")
        await openspace.time.setTime(self.value)

      case "wait":
        print(f"    Wait: {self.value}")
        await asyncio.sleep(int(self.value))

      case _:
        raise Exception(f"Unrecognized instruction type '{self.type}'")
