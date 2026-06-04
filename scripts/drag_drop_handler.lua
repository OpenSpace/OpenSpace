-- This script gets two parameters in its global scope:
-- filename:   The full path for the file that was dropped on the application.
--             Example:  C:/OpenSpace/openspace.cfg
-- basename:   Only the name of the actual file with extension, but without the full rest
--             of the path.
--             Example:  openspace.cfg
-- extension:  The extention of the file
--             Example: .cfg
--
-- From this script, we need to return the script that we want to be executed in response
-- to the drag event.  If we don't want anything to happen, don't return anything or
-- return an empty string

---@type string
Filename = Filename

---@type string
Basename = Basename

---@type string
Extension = Extension


if Filename == nil or Filename == "" or
   Basename == nil or Basename == "" or
   Extension == nil or Extension == "" then
  do return "" end
end

-- Lua doesn't enjoy \ that are used by Windows extensively. So we convert all \ into /
Filename = Filename:gsub("\\", "/")
Basename = Basename:gsub("\\", "/")
local basename_without_extension = Basename:sub(0, #Basename - Extension:len())

local is_image_file = function(extension)
  return extension == ".png" or extension == ".jpg" or extension == ".jpeg" or
         extension == ".tif" or extension == ".tga" or extension == ".bmp" or
         extension == ".psd"  or extension == ".hdr" or
         extension == ".pic" or extension == ".pnm"
end

local is_video_file = function(extension)
  return extension == ".mp4" or extension == ".webm" or extension == ".mkv" or
         extension == ".avi" or extension == ".mov" or extension == ".wmv" or
         extension == ".mpg" or extension == ".m4v" or extension == ".gif"
end

local is_asset_file = function(extension)
  return extension == ".asset" or extension == ".jasset"
end

local is_recording_file = function(extension)
  return extension == ".osrec" or extension == ".osrectxt"
end

local is_geojson_file = function(extension)
  return extension == ".geojson"
end

local is_wms_file = function(extension)
  return extension == ".wms"
end

local is_model_file = function(extension)
  return
    extension == ".obj" or extension == ".fbx" or extension == ".gltf" or
    extension == ".glb" or extension == ".dae" or extension == ".zae" or
    extension == ".blend" or extension == ".3ds" or extension == ".prj" or
    extension == ".ase" or extension == ".ask" or extension == ".x" or
    extension == ".stl" or extension == ".ifc" or extension == ".ifczip" or
    extension == ".xgl" or extension == ".zgl" or extension == ".ply" or
    extension == ".dxf" or extension == ".lwo" or extension == ".lws" or
    extension == ".mot" or extension == ".lxo" or extension == ".ac" or
    extension == ".ac3d" or extension == ".acc" or extension == ".ms3d" or
    extension == ".cob" or extension == ".scn" or extension == ".amf" or
    extension == ".md3" or extension == ".mdl" or extension == ".md2" or
    extension == ".smd" or extension == ".vta" or extension == ".mdc" or
    extension == ".md5anim" or extension == ".md5mesh" or extension == ".md5camera" or
    extension == ".nff" or extension == ".enff" or extension == ".raw" or
    extension == ".sib" or extension == ".off" or extension == ".irr" or
    extension == ".irrmesh" or extension == ".q3o" or extension == ".q3s" or
    extension == ".b3d" or extension == ".3d" or extension == ".uc" or
    extension == ".mesh" or extension == ".mesh.xml" or extension == ".ogex" or
    extension == ".pk3" or extension == ".bsp" or extension == ".ndo" or
    extension == ".assbin" or extension == ".3mf" or extension == ".x3d" or
    extension == ".x3db" or extension == ".m3d" or extension == ".osmodel"
end


if is_image_file(Extension) then
  return [[
  openspace.addScreenSpaceRenderable({
    Identifier = openspace.makeIdentifier("]] .. basename_without_extension .. [["),
    Type = "ScreenSpaceImageLocal",
    TexturePath = "]] .. Filename .. [["
  });]]
elseif is_video_file(Extension) then
  return [[
    openspace.addScreenSpaceRenderable({
      Identifier = openspace.makeIdentifier("]] .. basename_without_extension .. [["),
      Type = "ScreenSpaceVideo",
      Video = "]] .. Filename .. [["
    });]]
elseif is_asset_file(Extension) then
  return [[
    if openspace.asset.isLoaded("]] .. Filename .. [[") ~= true then
      openspace.printInfo("Adding asset: ']] .. Filename .. [[' (drag-and-drop)");
    end
    openspace.asset.add("]] .. Filename .. '");'
elseif is_recording_file(Extension) then
  return 'openspace.sessionRecording.startPlayback("' .. Filename .. '")'
elseif is_geojson_file(Extension) then
  return 'openspace.globebrowsing.addGeoJsonFromFile("' .. Filename .. '")'
elseif is_wms_file(Extension) then
  return [[
    openspace.globebrowsing.addLayer(
      openspace.navigation.getNavigationState().Anchor,
      "ColorLayers",
      {
        Identifier = openspace.makeIdentifier("]] .. basename_without_extension .. [["),
        FilePath = ']] .. Filename .. [['
      }
    )
  ]]
elseif is_model_file(Extension) then
return [[
  local identifier = openspace.makeIdentifier("]] .. basename_without_extension .. [[")
  local camera_position = openspace.navigation.getNavigationState().Position
  local camera_position_length = math.sqrt(camera_position[1]^2 + camera_position[2]^2 + camera_position[3]^2)
  local normalized_camera_position = {
    camera_position[1] / camera_position_length,
    camera_position[2] / camera_position_length,
    camera_position[3] / camera_position_length
  }
  local shifted_camera_position = {
    camera_position[1] - normalized_camera_position[1] * 50.0,
    camera_position[2] - normalized_camera_position[2] * 50.0,
    camera_position[3] - normalized_camera_position[3] * 50.0
  }
  openspace.addSceneGraphNode({
    Identifier = identifier,
    Parent = openspace.navigation.getNavigationState().Anchor,
    Transform = {
      Translation = {
        Type = "StaticTranslation",
        Position = shifted_camera_position
      }
    },
    Renderable = {
      Type = "RenderableModel",
      GeometryFile = "]] .. Filename .. [[",
      PerformShading = true,
      LightSources = {
        {
          Type = "SceneGraphLightSource",
          Identifier = "Sun",
          Node = "SunIAU",
          Intensity = 1.0
        }
      }
    }
  })
  
  openspace.scheduleScript("openspace.navigation.setFocus('" .. identifier .. "', true, true)", 0.1)
  ]]
end
