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
-- vsync: Whether the rendering speed is locked to the refreshrate [example: vsync=true] {default: true}
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
  local result = {}

  table.insert(result,   [[            {]])

  if tracked ~= nil then
    table.insert(result, [[              "tracked": ]] .. tostring(tracked) .. [[,]])
  end

  table.insert(result,   [[              "projection": {]])
  table.insert(result,   [[                "type": "PlanarProjection",]])
  table.insert(result,   [[                "fov": {]])
  table.insert(result,   [[                  "left": ]] .. left .. [[,]])
  table.insert(result,   [[                  "right": ]] .. right .. [[,]])
  table.insert(result,   [[                  "up": ]] .. up .. [[,]])
  table.insert(result,   [[                  "down": ]] .. down)
  table.insert(result,   [[                }]])
  table.insert(result,   [[              }]])
  table.insert(result,   [[            }]])

  return table.concat(result, "\n")
end



function generateFisheyeViewport(fov, quality, tilt, background, crop, offset, tracked)
  local result = {}

  table.insert(result,   [[           {]])

  if tracked ~= nil then
    table.insert(result, [[             "tracked": ]] .. tostring(tracked) .. [[,]])
  end

  table.insert(result,   [[             "projection": {]])
  table.insert(result,   [[               "type": "FisheyeProjection",]])

  if fov then
    table.insert(result, [[               "fov": ]] .. fov .. [[,]])
  end
  table.insert(result,   [[               "quality": "]] .. quality .. [[",]])
  table.insert(result,   [[               "tilt": ]] .. tilt .. [[,]])

  if crop then
    table.insert(result, [[               "crop": {]])
    table.insert(result, [[                 "left": ]] .. crop["left"] .. [[,]])
    table.insert(result, [[                 "right": ]] .. crop["right"] .. [[,]])
    table.insert(result, [[                 "top": ]] .. crop["top"] .. [[,]])
    table.insert(result, [[                 "bottom": ]] .. crop["bottom"])
    table.insert(result, [[               },]])
  end

  if offset then
    table.insert(result, [[               "offset": {]])
    table.insert(result, [[                 "x": ]] .. offset["x"] .. [[,]])
    table.insert(result, [[                 "y": ]] .. offset["y"] .. [[,]])
    table.insert(result, [[                 "z": ]] .. offset["z"])
    table.insert(result, [[               },]])
  end

  table.insert(result,   [[               "background": {]])
  table.insert(result,   [[                 "r": ]] .. background["r"] .. [[,]])
  table.insert(result,   [[                 "g": ]] .. background["g"] .. [[,]])
  table.insert(result,   [[                 "b": ]] .. background["b"] .. [[,]])
  table.insert(result,   [[                 "a": ]] .. background["a"])
  table.insert(result,   [[               }]])
  table.insert(result,   [[             }]])
  table.insert(result,   [[           }]])

  return table.concat(result, "\n")
end



function generateWindow(result, fullScreen, msaa, border, monitor, tags, stereo, pos,
                        size, res, viewport)
  table.insert(result,   [[          "name": "OpenSpace",]])

  if fullScreen ~= nil then
    table.insert(result, [[          "fullscreen": ]] .. tostring(fullScreen) .. [[,]])
  end

  if msaa then
    table.insert(result, [[          "msaa": ]] .. msaa .. [[,]])
  end

  if border ~= nil then
    table.insert(result, [[          "border": ]] .. tostring(border) .. [[,]])
  end

  if monitor then
    table.insert(result, [[          "monitor": ]] .. monitor .. [[,]])
  end

  if #(tags) > 0 then
    for i, v in ipairs(tags) do
      tags[i] = "\"" .. v .. "\""
    end
    local t = table.concat(tags, [[,]])
    table.insert(result, [[          "tags": [ ]] .. t .. [[ ], ]])
  end

  if stereo then
    table.insert(result, [[          "stereo": "]] .. stereo .. [[",]])
  end

  local px = pos[1]
  local py = pos[2]
  table.insert(result,   [[          "pos": { "x": ]] .. px .. [[, "y": ]] .. py .. [[ },]])

  local sx = size[1]
  local sy = size[2]
  table.insert(result,   [[          "size": { "x": ]] .. sx .. [[, "y": ]] .. sy .. [[ },]])

  if res then
    res[1] = res[1] or size[1]
    res[2] = res[2] or size[2]
    table.insert(result, [[          "res": {]])
    table.insert(result, [[            "x": ]] .. res[1] .. [[,]])
    table.insert(result, [[            "y": ]] .. res[2])
    table.insert(result, [[          },]])
  end

  table.insert(result,   [[          "viewports": []])

  table.insert(result, viewport)
  table.insert(result,   [[          ] ]])
end



function generateUser(result)
  table.insert(result, [[    {]])
  table.insert(result, [[      "eyeseparation": 0.06,]])
  table.insert(result, [[      "pos": { "x": 0.0, "y": 0.0, "z": 0.0 }]])
  table.insert(result, [[    }]])
end



function generateScene(result, scene)
  if scene == nil then
    return nil
  end

  local offset = scene["offset"] or { x = 0.0, y = 0.0, z = 0.0 }
  local orientation = scene["orientation"] or { yaw = 0.0, pitch = 0.0, roll = 0.0 }
  local scale = scene["scale"] or 1.0

  table.insert(result, [[{]])
  table.insert(result, [[  "offset": {]])
  table.insert(result, [[    "x": ]] .. offset["x"] .. [[,]])
  table.insert(result, [[    "y": ]] .. offset["y"] .. [[,]])
  table.insert(result, [[    "z": ]] .. offset["z"] .. [[,]])
  table.insert(result, [[  },]])
  table.insert(result, [[  "orientation": {]])
  table.insert(result, [[    "yaw": ]] .. orientation["yaw"] .. [[,]])
  table.insert(result, [[    "pitch": ]] .. orientation["pitch"] .. [[,]])
  table.insert(result, [[    "roll": ]] .. orientation["roll"] .. [[,]])
  table.insert(result, [[  },]])
  table.insert(result, [[  "scale": ]] .. scale)
  table.insert(result, [[}]])
end



function generateSettings(result, refreshRate, vsync)
  table.insert(result,   [[    "display": {]])

  if refreshRate then
    table.insert(result, [[      "refreshrate": ]] .. refreshRate .. [[,]])
  end

  if vsync then
    table.insert(result, [[      "swapinterval": 1]])
  else
    table.insert(result, [[      "swapinterval": 0]])
  end

  table.insert(result,   [[    }]])
end



function generateCluster(arg, viewport)
  local result = {}

  table.insert(result,   [[{]])
  table.insert(result,   [[  "version": 1,]])
  table.insert(result,   [[  "masteraddress": "127.0.0.1",]])

  if arg["sgctDebug"] ~= nil then
    table.insert(result, [[  "debug": ]] .. tostring(arg["sgctdebug"]) .. [[,]])
  end

  table.insert(result, [[  "settings": {]])
  generateSettings(result, arg["refreshRate"], arg["vsync"])
  table.insert(result, [[  },]])

  if arg["scene"] then
    table.insert(result, [[  "scene": {]])
    generateScene(result, arg["scene"])
    table.insert(result, [[  },]])
  end

  table.insert(result,   [[  "nodes": []])
  table.insert(result,   [[    {]])
  table.insert(result,   [[      "address": "127.0.0.1",]])
  table.insert(result,   [[      "port": 20401,]])
  table.insert(result,   [[      "windows": []])
  table.insert(result,   [[        {]])
  generateWindow(result, arg["fullScreen"], arg["msaa"], arg["border"], arg["monitor"],
    arg["tags"], arg["stereo"], arg["pos"], arg["size"], arg["res"], viewport)
  table.insert(result,   [[        }]])
  table.insert(result,   [[      ] ]])
  table.insert(result,   [[    }]])
  table.insert(result,   [[  ],]])
  table.insert(result,   [[  "users": []])
  generateUser(result)
  table.insert(result,   [[  ] ]])
  table.insert(result,   [[}]])

  return table.concat(result, "\n")
end



function check(type_str, arg, param, subparam_type)
  local t = type(arg[param])
  assert(t == type_str or t == "nil", param .. " must be a " .. type_str .. " or nil")

  if type_str == "table" and subparam_type and arg[param] then
    for k, v in pairs(arg[param]) do
      assert(
        type(v) == subparam_type,
        param .. "[" .. k .. "] must be a " .. subparam_type
      )
    end
  end
end



function generateSingleWindowConfig(arg, viewport)
  check("table", arg, "res", "number")
  check("table", arg, "tags", "string")
  check("table", arg, "pos", "number")
  check("boolean", arg, "shared")
  check("boolean", arg, "fullScreen")
  check("boolean", arg, "border")
  check("boolean", arg, "vsync")
  check("boolean", arg, "sgctDebug")
  check("number", arg, "monitor")
  check("number", arg, "msaa")
  check("number", arg, "refreshRate")
  check("string", arg, "stereo")

  check("table", arg, "scene")
  if arg["scene"] then
    check("table", arg["scene"], "offset", "number")
    check("table", arg["scene"], "orientation", "number")
    check("number", arg["scene"], "scale")
  end

  if arg["shared"] ~= nil then
    local t = arg["tags"]
    t[#t + 1] = "Spout"
  end

  return generateCluster(arg, viewport)
end



function sgct.makeConfig(config)
  local configFile = os.tmpname() .. ".json"
  local file = io.open(configFile, "w+")
  file:write(config)
  io.close(file)
  return configFile
end



function sgct.config.single(arg)
  arg = arg or {}

  if type(arg[1]) == "number" and type(arg[2]) == "number" then
    arg["size"] = { arg[1], arg[2] }
    arg[1] = nil
    arg[2] = nil
  else
    -- No numbers specified, therefore we want to use the screen resolution of the primary
    -- monitor to derive the resolution
    -- ScreenResolution is a variable that got injected into the openspace.cfg by the
    -- OpenSpace code prior to loading this file

    local scale_factor = 2.0/3.0;
    arg["size"] = { ScreenResolution.x * scale_factor, ScreenResolution.y * scale_factor }
  end

  check("table", arg, "size", "number")
  check("table", arg, "fov", "number")

  arg["vsync"] = arg["vsync"] or false
  arg["tags"] = arg["tags"] or {}
  arg["pos"] = arg["pos"] or { 50, 50 }
  arg["size"] = arg["size"] or { 1280, 720 }

  if (not arg["fov"]) then
    local tanDefaultFov = math.tan(math.rad(40.0))

    local tanHorizontalFov
    local tanVerticalFov
    local aspectRatio = arg["size"][1] / arg["size"][2]
    if (aspectRatio > 1) then
      tanHorizontalFov = tanDefaultFov
      tanVerticalFov = tanDefaultFov / aspectRatio
    else
      tanHorizontalFov = tanDefaultFov * aspectRatio
      tanVerticalFov = tanDefaultFov
    end

    arg["fov"] = {
      down = math.deg(math.atan(tanVerticalFov)),
      up = math.deg(math.atan(tanVerticalFov)),
      left = math.deg(math.atan(tanHorizontalFov)),
      right = math.deg(math.atan(tanHorizontalFov))
    }
  end

  check("boolean", arg, "tracked")
  sgctconfiginitializeString = "sgct.config.single"

  arg["tracked"] = arg["tracked"] or true

  local viewport = generateSingleViewportFOV(arg["fov"]["down"], arg["fov"]["up"],
    arg["fov"]["left"], arg["fov"]["right"], arg["tracked"])

  return sgct.makeConfig(generateSingleWindowConfig(arg, viewport))
end



function sgct.config.fisheye(arg)
  arg = arg or {}

  check("number", arg, 1)
  check("number", arg, 2)

  if type(arg[1]) == "number" and type(arg[2]) == "number" then
    arg["size"] = { arg[1], arg[2] }
    arg[1] = nil
    arg[2] = nil
  else
    -- No numbers specified, therefore we want to use the screen resolution of the primary
    -- monitor to derive the resolution
    -- ScreenResolution is a variable that got injected into the openspace.cfg by the
    -- OpenSpace code prior to loading this file

    local scale_factor = 2.0/3.0;
    arg["size"] = { ScreenResolution.x * scale_factor, ScreenResolution.y * scale_factor }
  end

  check("number", arg, "fov")
  check("number", arg, "tilt")
  check("string", arg, "quality")
  check("table", arg, "background", "number")
  check("table", arg, "crop", "number")
  check("table", arg, "offset", "number")

  sgctconfiginitializeString = "sgct.config.fisheye"

  arg["vsync"] = arg["vsync"] or false
  arg["tags"] = arg["tags"] or {}
  arg["pos"] = arg["pos"] or { 50, 50 }
  arg["size"] = arg["size"] or { 1024, 1024 }

  arg["quality"] = arg["quality"] or "1k"
  arg["tilt"] = arg["tilt"] or 90.0
  arg["background"] = arg["background"] or { r = 0.0, g = 0.0, b = 0.0, a = 1.0 }
  arg["tracked"] = arg["tracked"] or false

  local viewport = generateFisheyeViewport(arg["fov"], arg["quality"], arg["tilt"],
    arg["background"], arg["crop"], arg["offset"], arg["tracked"])

  return sgct.makeConfig(generateSingleWindowConfig(arg, viewport))
end
