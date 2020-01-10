version = ""
modulesTable = {}
assetsTable = {}
propertiesTable = {}
timeTable = {}
cameraTable = {}
markNodesTable = {}
keybindingsTable = {}
resultTable = {}
insideSection = false
currFunction = "None"
currSection = "None"
numLinesVersion = 0
lineIndex = 1


function printError(message)
  print("Error @ line "..lineIndex..": "..message)
end

function splitByTab(inputstr)
  sep = "\t"
  t = {}
  for match in (inputstr..sep):gmatch("(.-)"..sep) do
    table.insert(t, match)
  end
  return t;
end

function parseVersion(line)
  numLinesVersion = numLinesVersion + 1
  if numLinesVersion > 1 then
    printError("Too many lines in Version section.")
    os.exit()
  else
    lineS = splitByTab(line)
    if tableLen(lineS) > 1 then
      printError("No tabs allowed in version entry.")
      os.exit()
    else
      version = line
    end
  end
end

function parseMarkNodes(line)
  lineS = splitByTab(line)
  if tableLen(lineS) > 1 then
    printError("No tabs allowed in MarkNodes entry.")
    os.exit()
  else
    table.insert(markNodesTable, line)
  end
end

function parseModule(line)
  t = {}
  t = splitByTab(line)
  if tableLen(t) ~= 3 then
    printError("3 fields requried in a Module entry.")
    os.exit()
  else
    table.insert(modulesTable, t)
  end
end

function parseAsset(line)
  t = {}
  t = splitByTab(line)
  if tableLen(t) ~= 2 then
    printError("2 fields required in a Asset entry.")
    os.exit()
  else
    local req = "required"
    if t[2] == "requested" then
      req = "requested"
    end
    table.insert(assetsTable, {t[1], req})
  end
end

function parseProperty(line)
  t = {}
  t = splitByTab(line)
  if tableLen(t) ~= 3 then
    printError("3 fields required in a Property entry.")
    os.exit()
  elseif isBlank(t[1]) then
    printError("Property set command (arg 1/3) is required.")
    os.exit()
  elseif isBlank(t[2]) then
    printError("Property name (arg 2/3) is required.")
    os.exit()
  elseif isBlank(t[3]) then
    printError("Property value to set (arg 3/3) is required.")
    os.exit()
  end

  if t[1] ~= "setPropertyValue" and t[1] ~= "setPropertyValueSingle" then
    printError("Property set command '"..t[1].."' is not supported.")
    os.exit()
  end

  table.insert(propertiesTable, t)
end

function parseKeybinding(line)
  local numReqFields = 6
  t = {}
  t = splitByTab(line)
  if tableLen(t) < numReqFields then
    printError(numReqFields.." fields required in a Keybinding entry.")
    os.exit()
  elseif isBlank(t[1]) then
    printError("Keybinding key (arg 1/6) is required.")
    os.exit()
  elseif isBlank(t[2]) then
    printError("Keybinding documentation (arg 2/6) is required.")
    os.exit()
  elseif isBlank(t[3]) then
    printError("Keybinding name (arg 3/6) is required.")
    os.exit()
  elseif isBlank(t[4]) then
    printError("Keybinding GuiPath (arg 4/6) is required.")
    os.exit()
  elseif isBlank(t[5]) then
    printError("Keybinding local(T/F) (arg 5/6) is required.")
    os.exit()
  elseif isBlank(t[6]) then
    printError("Keybinding script to execute (arg 6/6) is required.")
    os.exit()
  end

  --If there are more than 6 fields then combine the final fields together
  --assuming that this is a lua script that contains tabs
  if tableLen(t) > numReqFields then
    for i=(numReqFields + 1),tableLen(t) do
      t[numReqFields] = t[numReqFields]..t[i]
    end
  end

  if t[5] ~= "true" and t[5] ~= "false" then
    printError("Keybinding local arg must be true or false.")
    os.exit()
  end

  table.insert(keybindingsTable, {t[1], t[2], t[3], t[4], t[5], t[6]})
end

function parseTime(line)
  t = {}
  t = splitByTab(line)
  if tableLen(t) ~= 2 then
    printError("2 fields required in a Time entry.")
    os.exit()
  elseif isBlank(t[1]) then
    printError("Time set type (arg 1/2) is required.")
    os.exit()
  elseif isBlank(t[2]) then
    printError("Time value to set (arg 2/2) is required.")
    os.exit()
  end

  if t[1] ~= "absolute" and t[1] ~= "relative" then
    printError("Time set type '"..t[1].."' is not supported.")
    os.exit()
  end

  table.insert(timeTable, t)
end

function parseCamera(line)
  t = {}
  t = splitByTab(line)

  local cmd = t[1]
  if cmd == "setNavigationState" then
    if tableLen(t) ~= 8 then
      printError("8 fields required in camera 'setNavigationState' line.")
      os.exit()
    elseif isBlank(t[2]) then
      printError("Camera setNavigationState Anchor (arg 1/7) is required.")
      os.exit()
    elseif isBlank(t[5]) then
      printError("Camera setNavigationState position vector (arg 4/7) is required.")
      os.exit()
    end
  elseif cmd == "goToGeo" then
    if tableLen(t) ~= 5 then
      printError("5 fields required in camera 'goToGeo' line.")
      os.exit()
    elseif isBlank(t[3]) then
      printError("Camera goToGeo Latitude (arg 2/4) is required.")
      os.exit()
    elseif isBlank(t[4]) then
      printError("Camera goToGeo Longitude (arg 3/4) is required.")
      os.exit()
    end
  else
    printError("Camera position command '"..cmd.."' is not supported.")
    os.exit()
  end

  table.insert(cameraTable, t)
end

function file_exists(file)
  local f = io.open(file, "rb")
  if f then
    f:close()
  end
  return f ~= nil
end

function lines_from(file)
  if not file_exists(file) then
    return {}
  end
  lines = {}
  for line in io.lines(file) do 
    lines[#lines + 1] = line
  end
  return lines
end

function determineSection(header)
  header = header:sub(2)
  for _,i in pairs(parsingSections) do
    if i.section == header then
      currSection = i.section
      currFunction = i.func
      return true
    end
  end
  return false
end

function isBlank(line)
  return line:match("%S") == nil
end

function parseCurrentSection(line)
  currFunction(line)
end

function tableLen(T)
  local size = 0
  for _ in pairs(T) do
    size = size + 1
  end
  return size
end

function parseProfile(fileIn)
  local lines = lines_from(fileIn)
  
  for k,v in pairs(lines) do
    if insideSection then
      if isBlank(v) then
        insideSection = false
      else
        parseCurrentSection(v)
      end
    elseif v:sub(1, 1) == "#" then
      if determineSection(v) then
        insideSection = true
      end
    end
    lineIndex = lineIndex + 1
  end
  
  resultTable["Version"] = version
  resultTable["Module"] = modulesTable
  resultTable["Asset"] = assetsTable
  resultTable["Property"] = propertiesTable
  resultTable["Time"] = timeTable
  resultTable["Camera"] = cameraTable
  resultTable["MarkNodes"] = markNodesTable
  resultTable["Keybinding"] = keybindingsTable

  return resultTable
end

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
  AssetStr = AssetStr.."asset.require('base');\n"
  AssetStr = AssetStr.."local assetHelper = asset.require('util/asset_helper')\n"
  AssetStr = AssetStr.."local propertyHelper = asset.require('util/property_helper')\n"
  AssetStr = AssetStr.."local sceneHelper = asset.require('util/scene_helper')\n"
  AssetStr = AssetStr.."local renderableHelper = asset.require('util/renderable_helper')\n"
  local assetType = ""
  for i,j in pairs(T["Asset"]) do
    if isBlank(j[2]) then
      assetType = "require"
    else
	  if (j[2] == "required") then
	    assetType = "require"
	  elseif (j[2] == "requested") then
	    assetType = "request"
      else
        printError("Asset arg 2/2 must be either 'required' or 'requested'.")
        os.exit()
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
      KeyStr = KeyStr.."    Command = "..j[6].."\n"
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
      TimeStr = TimeStr.."  local now = openspace.time.currentWallTime();"
      TimeStr = TimeStr.." openspace.time.setTime("
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
      CameraStr = CameraStr.."  openspace.globebrowsing.goToGeo("
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
  io.write(ModuleStr.."\n")
  io.write(AssetStr.."\n")
  io.write(KeyStr.."\n")
  io.write("asset.onInitialize(function ()\n")
  io.write(TimeStr.."\n")
  if not (tableSize(T["Keybinding"]) == 0) then
    io.write("  sceneHelper.bindKeys(Keybindings)\n")
  end
  io.write(MarkNodesStr.."\n")
  io.write(PropertyStr.."\n")
  io.write(CameraStr.."\n")
  io.write("end)\n")

  io.close(file)
end

--[[
########################################################################################## 
                                          M a i n
########################################################################################## 
]]--

ModuleStr = ""
AssetStr = ""
KeyStr = ""
TimeStr = ""
MarkNodesStr = ""
PropertyStr = ""
CameraStr = ""

parsingSections = {
  {section = "Version", func = parseVersion},
  {section = "Module", func = parseModule},
  {section = "Asset", func = parseAsset},
  {section = "Property", func = parseProperty},
  {section = "Keybinding", func = parseKeybinding},
  {section = "Time", func = parseTime},
  {section = "Camera", func = parseCamera},
  {section = "MarkNodes", func = parseMarkNodes}
}

profilePathIn = openspace.profile.getProfileInputPath()
scenePathOut = openspace.profile.getSceneOutputPath()

profileIn = profilePathIn..".profile"
assetOut  = scenePathOut..".scene"

local resultTable = parseProfile(profileIn)
generateAsset(resultTable, assetOut)
