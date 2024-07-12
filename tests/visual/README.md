# Visual Test Specification
All `.ostest` files in these folders specify visual image tests that are automatically run to ensure that changes in OpenSpace do not negatively impact the rendered results. The test results are available at https://regression.openspaceproject.com.

The files are organized in folders, where the folder name is used as the "group" name for the tests within, and the filename of each test (without the `.ostest` extension) is used as the name of the test. In general, the top-level folder should name a profile or a specific use case that is tested by the files within, such as testing all renderables. Additional subfolders can be used within each top-level folder.

## Test Structure
Each test must have a `screenshot` instruction as the **last** entry, which causes an image to be created that is used as the result of the test. Only exactly one `screenshot` instruction per test is currently supported. Each `.ostest` file is a JSON file with two top-level keys: `profile` provides the name of the profile that should be loaded before running these test instructions, and `commands` is a list of instructions that should be executed after the profile is loaded. All instructions must have a `type` and `value` key to determine which type of instruction it is and the parameters for that instruction.

By default on the servers that generate tests for https://regression.openspaceproject.com, all tests always start paused, MRF caching is enabled, and the user interface and dashboard items are disabled.

### Best practices
  - All tests should start with the instruction to set a specific time to improve reproducibility
  - The fewer instructions there are per test, the better
  - Adding `wait` instructions to ensure OpenSpace has time to load dynamic datasets increases reliability, but too many `wait`s will slow down the overall testing
  - Avoid `recording` and use `navigationstate` and `time` instead
  - Avoid `script` if possible and use dedicated instructions when they exist. If we see the same `script` instruction used in several tests, they can be upgraded to a dedicated instruction at a later stage

### Instructions
  - `action`: Triggers an action that must already be defined in the profile or previously defined in the test. The provided value must be a string that is the identifier of the action that should be triggered.

    Example: `{ "type": "action", "value": "os.FadeDownTrails" }`

    Script Equivalent: `openspace.action.triggerAction`

  - `asset`: Loads a given asset file. The provided value must be a string that is the path to the asset file to be loaded. This is specified relative to the `data/asset` folder inside OpenSpace.

    Example: `{ "type": "asset", "value": "path/to/file.asset" }`

    Script Equivalent: `openspace.asset.add`

  - `deltatime`: Instantly changes the delta time in OpenSpace to the provided value. The provided value must be a number that is the delta time in seconds per real-time second that the engine should be set to.

    Example: `{ "type": "deltatime", "value": 10 }`

    Script Equivalent: `openspace.time.setDeltaTime`

  - `navigationstate`: Instantly moves the camera to the provided navigation state. The provided value must be an object that must contain at least an `anchor` and `position` key and may optionally contain the keys `aim`, `referenceFrame`, `up`, `yaw`, `pitch`, and `timestamp`. All these values are then used to instantaneously set the position and rotation of the camera.

    Example: `{ "type": "navigationstate", "value": { "anchor": "Juno", "pitch": -0.0165756, "position": [ -22.49081, 1.191533, 26.35740 ], "up": [ 0.0288083, 0.999373, -0.0205962 ], "yaw": 0.152454 } }`

    Script Equivalent: `openspace.navigation.setNavigationState`

  - `pause`: Determines whether the in-game clock should be paused or resumed. The provided value must be a boolean that is the clock state after the instruction

    Example: `{ "type": "pause", "value": false }`

    Script Equivalent: `openspace.time.setPause`

  - `property`: Instantly sets a specific property or group of properties to the specified value. The provided value must be an object containing another `property` and `value` key. The (other) `property` key is the identifier or regex for the property or properties that should be set. The (other) `value` key is the new value for the property where the type must match the (other) `property`.

    Example: `{ "type": "property", "value": { "property": "Scene.Constellations.Renderable.Enabled", "value": true } }`

    Script Equivalent: `openspace.setPropertyValue`

  - `recording`: Triggers the playback of a session recording. The provided value is the name of the session recording file that should be played.

    Example: `{ "type": "recording", "value": "solarsystem.osrec" }`

    Script Equivalent: `openspace.sessionRecording.startPlayback`

  - `screenshot`: Takes a screenshot of the application. At the moment, there can be only exactly one instruction of this type and it must be the last instruction in the test. This instruction is the only one to not use the `value` key.

    Example: `{ "type": "screenshot" }`

    Script Equivalent: `openspace.takeScreenshot`

  - `script`: Instantly executes the script that is passed in as a value. That value must be a string that is the Lua script to execute.

    Example: `{ "type": "script", "value": "openspace.printError('Hello world')" }`

    Script Equivalent: `value`

  - `time`: Sets the in-game time to the provided value. The value can be either a string, which needs to be a valid date-time string, or a number, which represents the number of seconds past the J2000 epoch.

    Example: `{ "type": "time", "value": "2016-07-01T00:00:01.00" }`

    Script Equivalent: `openspace.time.setTime`

  - `wait`: Causes the test to wait for the specified number of seconds. Note that the OpenSpace testing instance is still running in the background and is, for example continuing to load dynamic content while the test is waiting.

    Example: `{ "type": "wait", "value": 2 }`

    Script Equivalent: none
