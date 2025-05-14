-- Helper functions that are useful to customize the openspace.cfg loading. Only edit this
-- file if you know what you are doing. There are a few implicit variables that are passed
-- into this file and are available: `ScreenResolution` contains the size (in pixels) of
-- the main monitor. `TableToJson` is a function that receives a Lua table and returns the
-- string-representation of that table converted to JSON

-- #######################################################################################
-- ##                          Public functions                                         ##
-- #######################################################################################

-- SGCT related functions
sgct = {}
sgct.config = {}

-- Creates a configuration file similar to the default 'single.json':
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



-- Global variable storing the name of the config function called at initialization
sgctconfiginitializeString = ""




--[[
##########################################################################################
                            Internal helper functions
##########################################################################################
]]--

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



function generateSingleViewportFOV(down, up, left, right, tracked)
  local result = {
    projection = {
      type = "PlanarProjection",
      fov = {
        left = left,
        right = right,
        up = up,
        down = down
      }
    }
  }

  if tracked ~= nil then
    result["tracked"] = tracked
  end

  return result
end



function generateWindow(fullScreen, msaa, border, monitor, tags, stereo, pos, size, res,
                        viewport)
  local result = {
    name = "OpenSpace",
    pos = { x = pos[1], y = pos[2] },
    size = { x = size[1], y = size[2] },
    viewports = { viewport }
  }

  if fullScreen ~= nil then
    result["fullscreen"] = fullScreen
  end

  if msaa then
    result["msaa"] = msaa
  end

  if border ~= nil then
    result["border"] = border
  end

  if monitor then
    result["monitor"] = monitor
  end

  if #(tags) > 0 then
    for i, v in ipairs(tags) do
      tags[i] = "\"" .. v .. "\""
    end
    local t = table.concat(tags, [[,]])
    result["tags"] = t
  end

  if stereo then
    result["stereo"] = stereo
  end

  if res then
    res[1] = res[1] or size[1]
    res[2] = res[2] or size[2]
    result["res"] = { x = res[1], y = res[2] }
  end

  return result
end



function generateScene(result, scene)
  if scene == nil then
    return nil
  end

  local offset = scene["offset"] or { x = 0.0, y = 0.0, z = 0.0 }
  local orientation = scene["orientation"] or { yaw = 0.0, pitch = 0.0, roll = 0.0 }
  local scale = scene["scale"] or 1.0

  local result = {
    offset = offset,
    orientation = orientation,
    scale = scale
  }

  return result
end



function generateSettings(refreshRate, vsync)
  local result = {
    display = {}
  }

  if refreshRate then
    result["display"]["refreshrate"] = refreshRate
  end

  if vsync then
    result["display"]["swapinterval"] = 1
  else
    result["display"]["swapinterval"] = 0
  end

  return result
end



function generateCluster(arg)
  local viewport = generateSingleViewportFOV(arg["fov"]["down"], arg["fov"]["up"],
    arg["fov"]["left"], arg["fov"]["right"], arg["tracked"])

  local result = {
    version = 1,
    masteraddress = "127.0.0.1",
    settings = generateSettings(arg["refreshRate"], arg["vsync"]),
    nodes = {
      {
        address = "127.0.0.1",
        port = 20401,
        windows = {
          generateWindow(arg["fullScreen"], arg["msaa"], arg["border"], arg["monitor"],
          arg["tags"], arg["stereo"], arg["pos"], arg["size"], arg["res"], viewport)
        }
      }
    },
    users = {
      {
        eyeseparation = 0.06,
        pos = { x = 0.0, y = 0.0, z = 0.0 }
      }
    }
  }

  if arg["sgctDebug"] ~= nil then
    result["debug"] = arg["sgctdebug"]
  end

  if arg["scene"] then
    result["scene"] = generateScene(arg["scene"])
  end

  return result
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

  sgctconfiginitializeString = "sgct.config.single"

  arg["tracked"] = arg["tracked"] or true

  --
  -- Create a file in the operating system's `temp` folder
  --

  local config = generateCluster(arg)
  -- tmpname returns the name to a randomly chosen file within the `temp` folder
  local tempFile = os.tmpname()
  -- `package.config` contains the path separator used on the current platform
  local directorySeparator = package.config:match("([^\n]*)\n?")
  -- Remove the filename and only get the path to the temp folder
  local separatorIdx = tempFile:reverse():find(directorySeparator)
  local tempFolder = tempFile:sub(1, #tempFile - separatorIdx + 1)

  local configFile = tempFolder .. "openspace-window-configuration.json"
  local file = io.open(configFile, "w+")
  file:write(TableToJson(config))
  io.close(file)
  return configFile
end
