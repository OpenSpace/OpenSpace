
function dirListing(dirname)
  f = io.popen('ls ' .. dirname)
  files = {}
  for name in f:lines() do
    table.insert(files, name)
  end
  return files
end

function listTleFiles(dirname)
  files = dirListing(dirname)
  listOfFiles = {}
  for tleFile in values(files) do
    startIdx, endIdx = string.find(tleFile, ".tle")
    fLen = string.len(tleFile)
    if( endIdx == fLen ) then
      table.insert(listOfFiles, tleFile)
    end
  end
  return listOfFiles 
end

function values(t)
  local i = 0
  return function () i = i + 1; return t[i] end
end

function trimString(s)
  s = s:gsub("^%s*(.-)%s*$", "%1")
  s = s:gsub("%s+", "_")
  s = s:gsub("[%-()]", "")
  return s
end

function getTitleFromFile(filename)
  f = io.open(filename, "r")
  if f then
      line1 = f:read('*l')
  end
  f:close()
  --Remove spaces and invalid characters from title line
  return trimString(line1)
end

function getPeriodFromFile(filename)
  f = io.open(filename, "r")
  if f then
      --Ignore the first 2 lines
      f:read('*l')
      f:read('*l')
      --Ignore first 7 numbers on line 3
      for x=1,7 do
        f:read('*n')
      end
      period = f:read('*n')
  end
  f:close()
  return period
end

function getNumLinesInFile(filename)
  local ctr = 0
  for _ in io.lines(filename) do
    ctr = ctr + 1
  end
  return ctr
end

--Check format of a TLE file and return nonzero if there is a format error
function checkTleFileFormat(file)
  lines = getNumLinesInFile(file)
  if( lines ~= 3 ) then
    return -1
  end

  myfile = io.open(file, "r")
  if myfile then
      myfile:read('*l') --title line
      lineNum = myfile:read('*n')
      myfile:read('*l') --rest of line 1
      if( lineNum ~= 1 ) then
        return -1
      end
      lineNum = myfile:read('*n')
      myfile:read('*l') --rest of line 2
      if( lineNum ~= 2 ) then
        return -1
      end
  else
    return -1
  end
  myfile:close()
  return 0
end

function  getSat(title, file)
  return {
      Name = title,
      Parent = "EarthInertial",
      Renderable = {
          Type = "RenderablePlane",
          Size = {3.0, 4.0},
          Origin = "Center",
          Body = "TLE",
          Billboard = true,
          Texture = "tle.jpg"
      },
      Transform = {
          Translation = {
              Type = "TLETranslation",
              Body = title,
              Observer = "EarthInertial",
              File = file
          },
          Scale = {
              Type = "StaticScale",
              Scale = 1,
          }
      }
  }
end

function getSatTrail(title, file, per, color)
  trailName = title .. "_trail"

  return {
      Name = trailName,
      Parent = "EarthInertial",
      Renderable = {
          Type = "RenderableTrailOrbit",
          Translation = {
              Type = "TLETranslation",
              Body = title,
              Observer = "EarthInertial",
              File = file
          },
          Color = color,
          Period = per,
          Resolution = 160
      },
      GuiName = "/Satellites/" .. trailName
  }
end

-------------------------------------------------------------
--Subdirectory name and color scheme for each satellite group
satelliteGroups = {
    { dir = "leo",
      trailColor = {1.0, 0.0, 0.0}
    },
    { dir = "meo",
      trailColor = {0.9, 0.6, 0.0}
    },
    { dir = "heo",
      trailColor = {0.9, 0.9, 0.0}
    },
}

modElements = {}  
fileErr = ""
for sOrbit in values(satelliteGroups) do
  sOrbit.dir = "../satellites/" .. sOrbit.dir
  for elem in values(listTleFiles(sOrbit.dir)) do
    elem = sOrbit.dir .. "/" .. elem
    if( checkTleFileFormat(elem) == 0 ) then
      title = getTitleFromFile(elem)
      per = getPeriodFromFile(elem)
      if( per ~= nil ) then
        per = 1.0 / per * 2 --trail for 2x a single revolution
        table.insert(modElements, getSat(title, elem))
        table.insert(modElements, getSatTrail(title,
                     elem, per, sOrbit.trailColor))
      else
        fileErr = fileErr .. elem .. ","
      end
    else
      fileErr = fileErr .. elem .. ","
    end
  end
end

if (fileErr == "") then
  return modElements
else
  return "Invalid file: " .. fileErr
end
