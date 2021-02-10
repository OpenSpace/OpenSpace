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

if filename == nil or filename == "" or
   basename == nil or basename == "" or
   extension == nil or extension == "" then
  do return "" end
end

-- Lua doesn't enjoy \ that are used by Windows extensively. So we convert all \ into /
filename = filename:gsub("\\", "/")
basename = basename:gsub("\\", "/")
basename_without_extension = basename:sub(0, #basename - extension:len())

is_image_file = function(extension)
  return extension == ".png" or extension == ".jpg" or extension == ".jpeg" or
         extension == ".tif" or extension == ".tga" or extension == ".bmp" or
         extension == ".psd" or extension == ".gif" or extension == ".hdr" or
         extension == ".pic" or extension == ".pnm"
end

if is_image_file(extension) then
  identifier = basename_without_extension:gsub(" ", "_")
  return [[openspace.addScreenSpaceRenderable({
    Identifier = "]] .. identifier .. [[",
    Type = "ScreenSpaceImageLocal",
    TexturePath = "]] .. filename .. [["
  });]]
elseif extension == ".asset" then
  return [[openspace.asset.add("]] .. filename .. [[")]]
elseif extension == ".osrec" or extension == ".osrectxt" then
  return [[openspace.sessionRecording.startPlayback("]] .. basename .. [[")]]
end
