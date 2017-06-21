DOWNLOAD = false

function dirListing(dirname)
  f = io.popen('ls ' .. dirname)
  files = {}
  for name in f:lines() do
    table.insert(files, name)
  end
  return files
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

function getPeriodFromFile(line2)
  return tonumber(string.sub(line2, 53, 63))
end

function getNumLinesInFile(filename)
  local ctr = 0
  for _ in io.lines(filename) do
    ctr = ctr + 1
  end
  return ctr
end

function isEmpty(s)
  return s == nil or s == ''
end

--Check format of a set of 3 TLE file lines and return nonzero if there is a format error
function checkTleFileFormat(lineArr)
  if isEmpty(lineArr[1]) or isEmpty(lineArr[2]) or isEmpty(lineArr[3]) then
    return -1
  end
  if string.sub(lineArr[2], 1, 2) ~= "1 " then
    return -1
  end
  if string.sub(lineArr[3], 1, 2) ~= "2 " then
    return -1
  end
  return 0
end


function  getSat(title, file, lineNum)
  return {
      Name = title,
      Parent = "EarthInertial",
      Renderable = {
          Type = "RenderablePlane",
          Size = 3e4,
          Origin = "Center",
          Body = "TLE",
          Billboard = true,
          Texture = "satB.png"
      },
      Transform = {
          Translation = {
              Type = "TLETranslation",
              Body = title,
              Observer = "EarthInertial",
              File = file,
              LineNum = lineNum
          },
          Scale = {
              Type = "StaticScale",
              Scale = 1,
          }
      }
  }
end

function getSatTrail(title, file, lineNum, per, color)
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
              File = file,
              LineNum = lineNum
          },
          Color = color,
          Period = per,
          Resolution = 160
      },
      GuiName = "/Satellites/" .. trailName
  }
end

-------------------------------------------------------------
--Name, URL, and color scheme for each satellite group
satelliteGroups = {
    { title = "GPS",
      url = "http://celestrak.com/NORAD/elements/gps-ops.txt",
      trailColor = {0.9, 0.5, 0.0}
    },
    { title = "SpaceStations",
      url = "http://celestrak.com/NORAD/elements/stations.txt",
      trailColor = {0.9, 0.0, 0.0}
    },
    { title = "Geostationary",
      url = "http://celestrak.com/NORAD/elements/geo.txt",
      trailColor = {0.9, 0.9, 0.0}
    },
}

modElements = {}  
fileErr = ""
for sOrbit in values(satelliteGroups) do
  filename = sOrbit.url:match("([^/]+)$")
  filenameSansExt = filename:gsub(filename:match "(%.%w+)$", "")
  sOrbit.path = "satellites/tle/" .. filename
 
  if DOWNLOAD then
    openspace.downloadFile(sOrbit.url, sOrbit.path)
  end
  sOrbit.path = "../" .. sOrbit.path
  pathFromScenegraphParent = "./" .. sOrbit.path
  
  line = {} 
  myfile = io.open(sOrbit.path, "r")
  lines = getNumLinesInFile(sOrbit.path)
  --now loop through the tle file and get each set of 3 lines
  if myfile then
    for n=1,lines,3 do
      line[1] = myfile:read('*l') --title line
      line[2] = myfile:read('*l')
      line[3] = myfile:read('*l')
      if( checkTleFileFormat(line) == 0 ) then
        title = trimString(line[1])
        per = getPeriodFromFile(line[3])
        per = 1.0 / per * 2 --trail for 2x a single revolution
        table.insert(modElements, getSat(filenameSansExt .. "_" .. title, pathFromScenegraphParent, n))
        table.insert(modElements, getSatTrail(filenameSansExt .. "_" .. title,
                     pathFromScenegraphParent, n, per, sOrbit.trailColor))
      else
        fileErr = " TLE file syntax error on line " .. n .. ": " .. sOrbit.path
        break
      end
    end
  else
    fileErr = " File not found: " .. sOrbit.path
    break
  end
end
assert(fileErr == "", fileErr)


if (fileErr == "") then
  return modElements
else
  return "Invalid file: " .. fileErr
end
