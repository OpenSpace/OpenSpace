local M = {}

ModuleStr = ""
AssetStr = ""
KeyStr = ""
TimeStr = ""
MarkNodesStr = ""
PropertyStr = ""
CameraStr = ""

function tableSize(T)
  local size = 0
  for _ in pairs(T) do
    size = size + 1
  end
  return size
end

function generateAsset(T, fileOut)
  file = io.open(fileOut, "w")
  io.output(file)

  --Module section
  for i,j in pairs(T["Module"]) do
    if not isBlank(j[2]) and not isBlank(j[3]) then
      ModuleStr = ModuleStr.."if openspace.modules.isLoaded('"..j[1].."') then\n"
      ModuleStr = ModuleStr.."  "..j[2].."\nelse\n".."  "..j[3].."\nend\n"
    elseif not isBlank(j[3]) then
      ModuleStr = ModuleStr.."if not openspace.modules.isLoaded('"..j[1].."') then\n"
      ModuleStr = ModuleStr.."  "..j[3].."\nend\n"
    elseif not isBlank(j[2]) then
      ModuleStr = ModuleStr.."if openspace.modules.isLoaded('"..j[1].."') then\n"
      ModuleStr = ModuleStr.."  "..j[2].."\nend\n"
    end
  end
 
  --Asset section 
  AssetStr = AssetStr.."asset.require('./base');\n"
  AssetStr = AssetStr.."local assetHelper = asset.require('util/asset_helper')\n"
  AssetStr = AssetStr.."local propertyHelper = asset.require('util/property_helper')\n"
  AssetStr = AssetStr.."local sceneHelper = asset.require('util/scene_helper')\n"
  AssetStr = AssetStr.."local renderableHelper = asset.require('util/renderable_helper')\n"
  local assetType = ""
  for i,j in pairs(T["Asset"]) do
    if isBlank(j[2]) then
      assetType = "required"
    else
      if not (j[2] == "required") and not (j[2] == "requested") then
        printError("Asset arg 2/2 must be either 'required' or 'requested'.")
        os.exit()
      else
        assetType = j[2]
      end
    end
    AssetStr = AssetStr.."asset."..assetType.."('"..j[1].."')\n"
  end

  --Keybindings section
  if not (tableSize(T["Keybinding"]) == 0) then
    KeyStr = KeyStr.."local Keybindings = {\n"
    for i,j in pairs(T["Keybinding"]) do
      KeyStr = KeyStr.."  {\n"
      KeyStr = KeyStr.."    Key = \""..j[1].."\",\n"
      KeyStr = KeyStr.."    Documentation = \""..j[2].."\",\n"
      KeyStr = KeyStr.."    Name = \""..j[3].."\",\n"
      KeyStr = KeyStr.."    GuiPath = \""..j[4].."\",\n"
      KeyStr = KeyStr.."    Local = "..j[5]..",\n"
      KeyStr = KeyStr.."    Command = \""..j[6].."\"\n"
      KeyStr = KeyStr.."  },\n"
    end
    KeyStr = KeyStr.."}\n"
  end

  --Time section
  for i,j in pairs(T["Time"]) do
    if not (j[1] == "absolute") and not (j[1] == "relative") then
      printError("Time arg 1/1 must be either 'absolute' or 'relative'.")
      os.exit()
    elseif (j[1] == "absolute") then
      TimeStr = TimeStr.."  openspace.time.setTime(\""..j[2].."\")\n"
    elseif (j[1] == "relative") then
      TimeStr = TimeStr.."  openspace.time.setTime("
      TimeStr = TimeStr.."openspace.time.advancedTime(now, \""
      TimeStr = TimeStr..j[2].."\"))\n"
    end
  end

  --MarkNodes section
  mkNodLen = tableSize(T["MarkNodes"])
  if not (mkNodLen == 0) then
    MarkNodesStr = MarkNodesStr.."  openspace.markInterestingNodes({"
    for i,j in pairs(T["MarkNodes"]) do
      MarkNodesStr = MarkNodesStr.."\""..j.."\""
      if (i < mkNodLen) then
        MarkNodesStr = MarkNodesStr..", "
      end
    end
    MarkNodesStr = MarkNodesStr.."})\n"
  end

  --Property section
  for i,j in pairs(T["Property"]) do
    if not (j[1] == "setPropertyValue") and not (j[1] == "setPropertyValueSingle") then
      printError("Property arg 1/1 must be 'setPropertyValue[Single]'.")
      os.exit()
    else
      PropertyStr = PropertyStr.."  openspace."..j[1].."('"..j[2].."', "..j[3]..")\n"
    end
  end

  --Camera section
  for i,j in pairs(T["Camera"]) do
    if (j[1] == "setNavigationState") then
      CameraStr = CameraStr.."  openspace.navigation.setNavigationState({"
      CameraStr = CameraStr.."Anchor = "..j[2]..", "
      if not isBlank(j[3]) then
        CameraStr = CameraStr.."Aim = "..j[3]..", "
      end
      if not isBlank(j[4]) then
        CameraStr = CameraStr.."ReferenceFrame = "..j[4]..", "
      end
      CameraStr = CameraStr.."Position = {"..j[5].."}, "
      if not isBlank(j[6]) then
        CameraStr = CameraStr.."Up = {"..j[6].."}, "
      end
      if not isBlank(j[7]) then
        CameraStr = CameraStr.."Yaw = "..j[7]..", "
      end
      if not isBlank(j[8]) then
        CameraStr = CameraStr.."Pitch = "..j[8]
      end
      CameraStr = CameraStr.."})\n"
    elseif (j[1] == "goToGeo") then
      CameraStr = CameraStr.."  openspace.navigation.goToGeo("
      if not isBlank(j[2]) then
        CameraStr = CameraStr..j[2]..", "
      end
      CameraStr = CameraStr..j[3]..", "..j[4]
      if not isBlank(j[5]) then
        CameraStr = CameraStr..", "..j[5]
      end
      CameraStr = CameraStr..")\n"
    else
      printError("Camera arg 1/1 must be 'setNavigationState' or 'goToGeo'.")
      os.exit()
    end
  end


  --Write the file
  io.write(ModuleStr)
  io.write("\n")
  io.write(AssetStr)
  io.write("\n")
  io.write(KeyStr)
  io.write("\n")
  io.write("asset.onInitialize(function ()\n")
  io.write(TimeStr)
  io.write("\n")
  if not (tableSize(T["Keybinding"]) == 0) then
    io.write("  sceneHelper.bindKeys(Keybindings)")
    io.write("\n")
  end
  io.write(MarkNodesStr)
  io.write("\n")
  io.write(PropertyStr)
  io.write("\n")
  io.write(CameraStr)
  io.write("\n")
  io.write("end)\n")

  io.close(file)
end
M.generateAsset = generateAsset

return M
