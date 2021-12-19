-- Helper functions that are useful to customize the openspace.cfg loading

-- ####################################################################################### 
-- ##                          Public functions                                         ##
-- ####################################################################################### 

-- SGCT related functions
sgct = {}
sgct.config = {}

-- Creates a configuration file similar to the default 'single.xml':
-- The parameter is a table and can contain the follow attributes:

-- first argument: horizontal window size {default: 1280}
-- second argument: vertical window size {default: 720}
-- res: A table containing the horizontal and vertical resolution [example: res={3840, 2160}]
-- pos: The position of the window on the screen [example: pos={50, 100}] {default: {50, 50}}
-- fullScreen: Whether the application should run in exclusive full screen [example: fullScreen=true] {default: false}
-- border: Whether the application should have window decorations (aka. border) [example: border=false] {default: true}
-- monitor: Determines the monitor on which the application is started [example: monitor=2] {default: 0}
-- shared: Determines whether the contents of the window should be shared using the SPOUT library [example: shared=true] {default: false}

-- Expert settings:
-- vsync: Whether the rendering speed is locked to the refreshrate [example: vsync=true] {default: false}
-- refreshRate: If vsync is enabled, this is the target framerate [example: refreshRate=30] {default: infinity}
-- stereo: Select the stereo rendering mode as supported by SGCT [example: stereo='anaglyph_red_cyan'] {default: 'none'}
-- msaa: The multisampling anti-aliasing factor [example: msaa=8] {default: 4}
-- scene: Global settings to all scene objects (offset, orientation, scaling; each optional)
--      [example: scene = {offset = {x = 1.0, y = 1.0, z = 2.0}, orientation = { yaw = 120, pitch = 15, roll = 0.0 }, scale = 10.0}]
-- sgctDebug: Determines whether a higher debug level in SGCT is enabled [example: sgctDebug=true] {default: false}
-- fov: The field of view settings [example: fov={ left=20, right=30, up=10, down=50}] {default: { left=40.0, right=40.0, up=22.5, down=22.5}}
-- tracked: Determines whether the aspect ratio of the camera defined at application startup should persist when the window is resized [example: tracked=false] {default: true}

-- Thus this function can be called the following ways:
-- sgct.config.single() -> Leading to a 1280x720 resolution window
-- sgct.config.single(1920, 1080) -> Leading to a 1920x1080 resolution window
-- sgct.config.single(640, 360, res={3840, 2160}) -> 640x360 window with 4K rendering resolution
-- sgct.config.single(msaa=1) -> 1280x720 resolution without multisampling
function sgct.config.single(arg) end



-- Creates a configuration file similar to the default 'single_fisheye.xml'
-- The parameter is a table and can contain the follow attributes:

-- first argument: horizontal window size {default: 1280}
-- second argument: vertical window size {default: 720}
-- res: A table containing the horizontal and vertical resolution [example: res={3840, 2160}]
-- pos: The position of the window on the screen [example: pos={50, 100}] {default: {50, 50}}
-- fullScreen: Whether the application should run in exclusive full screen [example: fullScreen=true] {default: false}
-- border: Whether the application should have window decorations (aka. border) [example: border=false] {default: true}
-- monitor: Determines the monitor on which the application is started [example: monitor=2] {default: 0}
-- shared: Determines whether the contents of the window should be shared using the SPOUT library [example: shared=true] {default: false}

-- Expert settings:
-- vsync: Whether the rendering speed is locked to the refreshrate [example: vsync=true] {default: false}
-- refreshRate: If vsync is enabled, this is the target framerate [example: refreshRate=30] {default: infinity}
-- stereo: Select the stereo rendering mode as supported by SGCT [example: stereo='anaglyph_red_cyan'] {default: 'none'}
-- msaa: The multisampling anti-aliasing factor [example: msaa=8] {default: 4}
-- scene: Global settings to all scene objects (offset, orientation, scaling; each optional)
--      [example: scene = {offset = {x = 1.0, y = 1.0, z = 2.0}, orientation = { yaw = 120, pitch = 15, roll = 0.0 }, scale = 10.0}]
-- sgctDebug: Determines whether a higher debug level in SGCT is enabled [example: sgctDebug=true] {default: false}
-- fov: The field of view for the fisheye [example: fov=360] {default: 180}
-- quality: The quality setting for the cubemap textures [example: quality="4k"] {default: "1k"}
-- tilt: The forwards tilt of the fisheye, relative to the center [example: tilt=90] {default: 0.0}
-- background: The background color used outside of the fisheye rendering [example: backgruound={r=1.0, g=0.25, b=0.25, a=1.0}] {default: {r=0.1, g=0.1, b=0.1, a=1.0}}

-- Thus this function can be called the following ways:
-- sgct.config.fisheye() -> Leading to a 1280x720 resolution window
-- sgct.config.fisheye(640, 640) -> Leading to a 640x650 resolution window
-- sgct.config.fisheye(640, 360, res={3840, 3840}) -> 640x360 window with 4K rendering resolution
-- sgct.config.fisheye(msaa=1) -> 1280x720 resolution without multisampling
function sgct.config.fisheye(arg) end

-- Global variable storing the name of the config function called at initialization
sgctconfiginitializeString = ""

--[[
########################################################################################## 
                            Internal helper functions
########################################################################################## 
]]--

function generateSingleViewportFOV(down, up, left, right, tracked)
  return [[
{
  "pos": { "x": 0.0, "y": 0.0 },
  "size": { "x": 1.0, "y": 1.0 },
  "tracked": ]] .. tostring(tracked) .. [[,
  "projection": {
    "type": "PlanarProjection",
    "fov": {
      "left": ]] .. left .. [[,
      "right": ]] .. right .. [[,
      "up": ]] .. up .. [[,
      "down": ]] .. down .. [[
    },
    "orientation": { "heading": 0.0, "pitch": 0.0, "roll": 0.0 }
  }
}
]]
end



function generateFisheyeViewport(fov, quality, tilt, background, crop, offset, tracked) 
  local background_fragment = [[
{
  "r": ]] .. background["r"] .. [[,
  "g": ]] .. background["g"] .. [[,
  "b": ]] .. background["b"] .. [[,
  "a": ]] .. background["a"] .. [[
}]]

local crop_fragment = ""
  if crop then
    crop_fragment = [[
{
  "left": ]] .. crop["left"] .. [[,
  "right": ]] .. crop["right"] .. [[,
  "top": ]] .. crop["top"] .. [[,
  "bottom": ]] .. crop["bottom"] .. [[
}]]
  else
    crop_fragment = "{}"
  end

  local offset_fragment = ""
  if offset then
    offset_fragment = [[
{
  "x": ]] .. offset["x"] .. [[,
  "y": ]] .. offset["y"] .. [[,
  "z": ]] .. offset["z"] .. [[
}]]
  else
    offset_fragment = "{}"
  end

  return [[
{
  "pos": { "x": 0.0, "y": 0.0 },
  "size": { "x": 1.0, "y": 1.0 },
  "tracked": ]] .. tostring(tracked) .. [[,
  "projection": {
    "type": "FisheyeProjection",
    "fov": ]] .. fov .. [[,
    "quality": "]] .. quality .. [[",
    "tilt": ]] .. tilt .. [[,
    "background": ]] .. background_fragment .. [[,
    "crop": ]] .. crop_fragment .. [[,
    "offset": ]] .. offset_fragment .. [[
  }
}
]]
end



function generateWindow(arg)
  local resolution_fragment = ""
  if arg["res"] then
    arg["res"][1] = arg["res"][1] or arg["windowSize"][1]
    arg["res"][2] = arg["res"][2] or arg["windowSize"][2]

    resolution_fragment =
      [[ "res": { "x": ]] .. arg["res"][1] .. [[ "y": ]] .. arg["res"][1] .. [[},]]
  end

  local tags
  if arg["tags"] then
    tags = table.concat(arg["tags"], ",")
  end

  return [[
{
  "name": "OpenSpace",
  "fullscreen": ]] .. tostring(arg["fullScreen"]) .. [[,
  "msaa": ]] .. arg["msaa"] .. [[,
  "border": ]] .. tostring(arg["border"]) .. [[,
  "monitor": ]] .. arg["monitor"] .. [[,
  "tags": [ ]] .. tags .. [[ ],
  "stereo": "]] .. arg["stereo"] .. [[",
  "size": { "x": ]] .. arg["windowSize"][1] .. [[, "y": ]] .. arg["windowSize"][2] .. [[ },
  "pos": { "x": ]] .. arg["pos"][1] .. [[, "y": ]] .. arg["pos"][2] .. [[ },
  ]] .. resolution_fragment .. [[
  "viewports": [
    ]] .. arg["viewport"] .. [[
  ]
}
]]
end



function generateUser(arg)
  return [[
{
  "eyeSeparation": 0.06,
  "pos": { "x": 0.0, "y": 0.0, "z": 0.0 }
}
]]
end



function generateScene(arg)
  local scene = arg["scene"]

  if scene == nil then
    return "{}"
  end

  local offset = scene["offset"] or { x = 0.0, y = 0.0, z = 0.0 }
  local orientation = scene["orientation"] or { yaw = 0.0, pitch = 0.0, roll = 0.0 }
  local scale = scene["scale"] or 1.0

  return [[
{
  "offset": {
    "x": ]] .. offset["x"] .. [[,
    "y": ]] .. offset["y"] .. [[,
    "z": ]] .. offset["z"] .. [[
  },
  "orientation": {
    "yaw": ]] .. orientation["yaw"] .. [[,
    "pitch": ]] .. orientation["pitch"] .. [[,
    "roll": ]] .. orientation["roll"] .. [[
   },
   "scale": ]] .. scale .. [[
}
]]
end



function generateSettings(arg)
  local v
  if arg["vsync"] then
    v = 1
  else
    v = 0
  end

  local refresh_fragment = ""
  if arg["refreshRate"] then
    refresh_fragment = [[ "refreshRate": ]] .. arg["refreshRate"] .. ","
  end

  return [[
{
  "display": {
    ]] .. refresh_fragment .. [[
    "swapInterval": ]] .. v .. [[
  }
}
]]
end



function generateCluster(arg)
  return [[
{
  "version": 1,
  "masterAddress": "127.0.0.1",
  "debug": ]] .. tostring(arg["sgctDebug"]) .. [[,
  "settings": ]] .. (arg["settings"] or "{}") .. [[,
  "scene": ]] .. (arg["scene"] or "{}") .. [[,
  "nodes": [
    {
      "address": "127.0.0.1",
      "port": 20401,
      "windows": [
        ]] .. arg["window"] .. [[
      ]
    }
  ],
  "users": [
    ]] .. arg["user"] .. [[
  ]
}
]]
end



function generateSingleWindowConfig(arg)
  assert(
    type(arg["res"]) == "table" or type(arg["res"]) == "nil",
    "res must be a table or nil"
  )
  if (type(arg["res"]) == "table") then
    assert(type(arg["res"][1]) == "number", "res[1] must be a number")
    assert(type(arg["res"][2]) == "number", "res[2] must be a number")
  end

  assert(
    type(arg["shared"]) == "boolean" or type(arg["shared"]) == "nil",
    "shared must be a boolean or nil"
  )

  assert(
    type(arg["tags"]) == "table" or type(arg["tags"]) == "nil",
    "tags must be a table or nil"
  )
  if (type(arg["tags"]) == "table") and (next(arg["tags"]) ~= nil) then
    for index, value in ipairs(arg["tags"]) do
      assert(type(value) == "string", "Each tag must be a string")
    end
  end

  assert(
    type(arg["pos"]) == "table" or type(arg["pos"]) == "nil",
    "pos must be a table or nil"
  )
  if (type(arg["pos"]) == "table") then
    assert(type(arg["pos"][1]) == "number", "pos[1] must be a number")
    assert(type(arg["pos"][2]) == "number", "pos[2] must be a number")
  end

  assert(
    type(arg["fullScreen"]) == "boolean" or type(arg["fullScreen"]) == "nil",
    "fullScreen must be a boolean or nil"
  )

  assert(
    type(arg["monitor"]) == "number" or type(arg["monitor"]) == "nil",
    "monitor must be a number or nil"
  )

  assert(
    type(arg["border"]) == "boolean" or type(arg["border"]) == "nil",
    "border must be a boolean or nil"
  )

  assert(
    type(arg["msaa"]) == "number" or type(arg["msaa"]) == "nil",
    "msaa must be a number or nil"
  )

  assert(
    type(arg["vsync"]) == "boolean" or type(arg["vsync"]) == "nil",
    "vsync must be a boolean or nil"
  )

  assert(
    type(arg["refreshRate"]) == "number" or type(arg["refreshRate"]) == "nil",
    "refreshRate must be a number or nil"
  )
  
  assert(
    type(arg["stereo"]) == "string" or type(arg["stereo"]) == "nil",
    "stereo must be a boolean or nil"
  )

  assert(
    type(arg["sgctDebug"]) == "boolean" or type(arg["sgctDebug"]) == "nil",
    "sgctDebug must be a boolean or nil"
  )

  assert(
    type(arg["scene"]) == "table" or type(arg["scene"]) == "nil",
    "scene must be a table or nil"
  )
  if type(arg["scene"]) == "table" then
    local offset = arg["scene"]["offset"]
    assert(
      type(offset) == "table" or type(offset) == "nil",
      "scene['offset'] must be a table or nil"
    )
    if type(offset) == "table" then
      assert(type(offset["x"]) == "number", "scene['offset']['x'] must be a number")
      assert(type(offset["y"]) == "number", "scene['offset']['y'] must be a number")
      assert(type(offset["z"]) == "number", "scene['offset']['z'] must be a number")
    end

    local orientation = arg["scene"]["orientation"]
    assert(
      type(orientation) == "table" or type(orientation) == "nil",
      "scene['orientation] must be a table or nil"
    )
    if type(orientation) == "table" then
      assert(type(orientation["yaw"]) == "number", "orientation['yaw'] must be a number")
      assert(type(orientation["pitch"]) == "number", "orientation['pitch'] must be a number")
      assert(type(orientation["roll"]) == "number", "orientation['roll'] must be a number")
    end

    local scale = arg["scene"]["scale"]
    assert(
      type(scale) == "number" or type(scale) == "nil",
      "scene['scale'] must be a number or nil"
    )
  end

  assert(type(arg["viewport"]) == "string", "viewport must be a string")

  -- Then setting reasonable default values
  arg["vsync"] = arg["vsync"] or false
  arg["fullScreen"] = arg["fullScreen"] or false
  arg["monitor"] = arg["monitor"] or 0
  arg["tags"] = arg["tags"] or {}
  arg["msaa"] = arg["msaa"] or 1
  arg["border"] = arg["border"] or true
  arg["stereo"] = arg["stereo"] or "none"
  arg["pos"] = arg["pos"] or { 50, 50 }
  arg["sgctDebug"] = arg["sgctDebug"] or false
  arg["windowSize"] = arg["windowSize"] or { 1280, 720 }

  if arg["shared"] then
    local t = arg["tags"]
    t[#t + 1] = "Spout"
  end


  arg["scene"] = generateScene(arg)
  arg["settings"] = generateSettings(arg)
  arg["window"] = generateWindow(arg)
  arg["user"] = generateUser(arg)

  return generateCluster(arg)
end


function normalizeArg(arg)
  arg = arg or {}

  assert(
    type(arg[1]) == "number" or type(arg[1]) == "nil",
    "First argument must be a number or nil"
  )
  assert(
    type(arg[2]) == "number" or type(arg[2]) == "nil",
    "Second argument must be a number or nil"
  )
  if (type(arg[1]) == "number") then
    if (type(arg[2]) == "nil") then
      arg[2] = arg[1] * 9/16
    end
    arg["windowSize"] = { arg[1], arg[2] }
    arg[1] = nil
    arg[2] = nil
  end

  return arg
end


function sgct.makeConfig(config)
  -- local configFile = os.tmpname() .. ".json"
  local configFile = "C:/Development/testing.json"
  local file = io.open(configFile, "w+")
  file:write(config)
  io.close(file)
  return configFile
end



function sgct.config.single(arg)
  arg = normalizeArg(arg)

  assert(
    type(arg["windowSize"]) == "table" or type(arg["windowSize"]) == "nil",
    "windowSize must be a table or nil"
  )

  assert(
    type(arg["fov"]) == "table" or type(arg["fov"]) == "nil",
    "fov must be a table or nil"
  )
  if (type(arg["fov"]) == "table") then
    assert(type(arg["fov"]["left"]) == "number",  "fov['left'] must be a number")
    assert(type(arg["fov"]["right"]) == "number", "fov['right'] must be a number")
    assert(type(arg["fov"]["up"]) == "number",    "fov['up'] must be a number")
    assert(type(arg["fov"]["down"]) == "number",  "fov['down'] must be a number")
  else
    local defaultFov = 40
    local defaultRatio = 16/9 -- comes from default res 1280 x 720
    local tanDefaultFov = math.tan(math.pi * defaultFov / 180)

    if (type(arg["windowSize"]) == "table") then
      assert(type(arg["windowSize"][1]) == "number", "windowSize[1] must be a number")
      assert(type(arg["windowSize"][2]) == "number", "windowSize[2] must be a number")
      local tanHorizontalFov = tanDefaultFov
      local tanVerticalFov = tanDefaultFov

      local ratio = arg["windowSize"][1] / arg["windowSize"][2]

      -- ratio between w and h should be
      -- same as tan(horizontalFov) and tan(verticalFov)

      if (ratio > 1) then
        tanVerticalFov = tanHorizontalFov / ratio
      else
        tanHorizontalFov = tanVerticalFov * ratio
      end

      -- sgct expects fov in degrees
      arg["fov"] = {
        down = 180 * math.atan(tanVerticalFov) / math.pi,
        up = 180 * math.atan(tanVerticalFov) / math.pi,
        left = 180 * math.atan(tanHorizontalFov) / math.pi,
        right = 180 * math.atan(tanHorizontalFov) / math.pi
      }
    else
      -- sgct expects fov in degrees
      arg["fov"] = {
        down = 180 * math.atan(tanDefaultFov / defaultRatio) / math.pi,
        up = 180 * math.atan(tanDefaultFov / defaultRatio) / math.pi,
        left = 180 * math.atan(tanDefaultFov) / math.pi,
        right = 180 * math.atan(tanDefaultFov) / math.pi
      }
    end
  end

  assert(
    type(arg["tracked"]) == "boolean" or type(arg["tracked"]) == "nil",
    "tracked must be a boolean or nil"
  )
  sgctconfiginitializeString = "sgct.config.single"

  arg["tracked"] = arg["tracked"] or false

  arg["viewport"] = generateSingleViewportFOV(
    arg["fov"]["down"],
    arg["fov"]["up"], 
    arg["fov"]["left"],
    arg["fov"]["right"],
    arg["tracked"]
  )
  return sgct.makeConfig(generateSingleWindowConfig(arg))
end



function sgct.config.fisheye(arg)
  arg = normalizeArg(arg)

  assert(
    type(arg["fov"]) == "number" or type(arg["fov"]) == "nil",
    "fov must be a number or nil"
  )

  assert(
    type(arg["quality"]) == "string" or type(arg["quality"]) == "nil",
    "quality must be a string or nil"
  )

  assert(
    type(arg["tilt"]) == "number" or type(arg["tilt"]) == "nil",
    "tilt must be a number or nil"
  )

  assert(
    type(arg["background"]) == "table" or type(arg["background"]) == "nil",
    "background must be a table or nil"
  )
  if type(arg["background"]) == "table" then
    assert(type(background["r"]) == "number", "backgroud['r'] must be a number")
    assert(type(background["g"]) == "number", "backgroud['g'] must be a number")
    assert(type(background["b"]) == "number", "backgroud['b'] must be a number")
    assert(type(background["a"]) == "number", "backgroud['a'] must be a number")
  end

  assert(
    type(arg["crop"]) == "table" or type(arg["crop"]) == "nil",
    "crop must be a table or nil"
  )
  if type(arg["crop"]) == "table" then
    assert(
        type(arg["crop"]["left"]) == "number", "crop['left'] must be a number"
    )
    assert(
        type(arg["crop"]["right"]) == "number", "crop['right'] must be a number"
    )
    assert(
        type(arg["crop"]["top"]) == "number", "crop['top'] must be a number"
    )
    assert(
        type(arg["crop"]["bottom"]) == "number", "crop['bottom'] must be a number"
    )
  end

  assert(
    type(arg["offset"]) == "table" or type(arg["offset"]) == "nil",
    "offset must be a table or nil"
  )
  if type(arg["offset"]) == "table" then
    assert(type(arg["offset"]["x"]) == "number", "offset['x'] must be a number")
    assert(type(arg["offset"]["y"]) == "number", "offset['y'] must be a number")
    assert(type(arg["offset"]["z"]) == "number", "offset['z'] must be a number")
  end

  sgctconfiginitializeString = "sgct.config.fisheye"

  arg["fov"] = arg["fov"] or 180.0
  arg["quality"] = arg["quality"] or "1k"
  arg["tilt"] = arg["tilt"] or 90.0
  arg["background"] = arg["background"] or { r = 0.0, g = 0.0, b = 0.0, a = 1.0 }
  arg["tracked"] = arg["tracked"] or false

  arg["viewport"] = generateFisheyeViewport(
    arg["fov"],
    arg["quality"],
    arg["tilt"],
    arg["background"],
    arg["crop"],
    arg["offset"],
    arg["tracked"]
  )

  return sgct.makeConfig(generateSingleWindowConfig(arg))
end
