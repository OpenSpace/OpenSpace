openspace.globebrowsing.documentation = {
    {
        Name = "createTemporalGibsGdalXml",
        Arguments = {{ "layerName", "String" }, { "resolution", "String" }, { "format", "String" }},
        Documentation = [[
            Creates an XML configuration for a temporal GIBS dataset to be used in
            a TemporalTileprovider
        ]]
    },
    {
        Name = "createGibsGdalXml",
        Arguments = {{ "layerName", "String" }, { "date", "String" }, { "resolution",  "String" }, { "format", "String" }},
        Documentation =
            "Creates an XML configuration for a GIBS dataset." ..
            "Arguments are: layerName, date, resolution, format." ..
            "For all specifications, see " ..
            "https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products" ..
            "Usage:" ..
            "openspace.globebrowsing.addLayer(" ..
                "\"Earth\"," ..
                "\"ColorLayers\"," ..
                "{" ..
                    "Name = \"MODIS_Terra_Chlorophyll_A\"," ..
                    "FilePath = openspace.globebrowsing.createGibsGdalXml(" ..
                        "\"MODIS_Terra_Chlorophyll_A\"," ..
                        "\"2013-07-02\"," ..
                        "\"1km\"," ..
                        "\"png\"" ..
                    ")" ..
                "}" ..
            ")"
    },
    {
        Name = "addGibsLayer",
        Arguments = {{ "layer", "String" }, { "resolution", "String" }, { "format", "String" }, { "startDate", "String" }, { "endDate", "String" }},
        Documentation = "Adds a new layer from NASA GIBS to the Earth globe. Arguments " ..
            "are: imagery layer name, imagery resolution, start date, end date, format. " ..
             "For all specifications, see " ..
            "https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products" ..
            "Usage:" ..
            "openspace.globebrowsing.addGibsLayer('AIRS_Temperature_850hPa_Night', '2km', '2013-07-15', 'Present', 'png')"
    },
    {
        Name = "parseInfoFile",
        Arguments = {{ "file", "String" }},
        Documentation =
            "Parses the passed info file and return the table with the information " ..
            "provided in the info file. The return table contains the optional keys: " ..
            "'Color', 'Height', 'Node', 'Location', 'Identifier'." ..
            "Usage: local t = openspace.globebrowsing.parseInfoFile(file)" ..
            "openspace.globebrowsing.addLayer(\"Earth\", \"ColorLayers\", t.color)" ..
            "openspace.globebrowsing.addLayer(\"Earth\", \"HeightLayers\", t.height)"
    },
    {
        Name = "addBlendingLayersFromDirectory",
        Arguments = {{ "directory", "String" }, { "nodeName", "String" }},
        Documentation =
            "Retrieves all info files recursively in the directory passed as the first " ..
            "argument to this function. The color and height tables retrieved from these " ..
            "info files are then added to the RenderableGlobe identified by name passed " ..
            "to the second argument." ..
            "Usage: openspace.globebrowsing.addBlendingLayersFromDirectory(directory, \"Earth\")"
    },
    {
        Name = "addFocusNodesFromDirectory",
        Arguments = {{ "directory", "String" }, { "nodeName", "String" }},
        Documentation =
            "Retrieves all info files recursively in the directory passed as the first " ..
            "argument to this function. The name and location retrieved from these info " ..
            "files are then used to create new SceneGraphNodes that can be used as " ..
            "focus nodes. " ..
            "Usage: openspace.globebrowsing.addFocusNodesFromDirectory(directory, \"Mars\")"
    },
    {
        Name = "addFocusNodeFromLatLong",
        Arguments = {{ "name", "String" }, { "globeIdentifier", "String" }, { "latitude", "Number" }, { "longitude", "Number" }, { "altitude", "Number" }},
        Documentation =
            "Creates a new SceneGraphNode that can be used as focus node. " ..
            "Usage: openspace.globebrowsing.addFocusNodeFromLatLong(" ..
            "\"Olympus Mons\", \"Mars\", -18.65, 226.2, optionalAltitude)"
    },
    {
        Name = "loadWMSServersFromFile",
        Arguments = {{ "filePath", "String" }},
        Documentation =
            "Loads all WMS servers from the provided file and passes them to the " ..
            "'openspace.globebrowsing.loadWMSCapabilities' file."
    }
}

openspace.globebrowsing.addGibsLayer = function(layerName, resolution, format, startDate, endDate)
    if endDate == 'Present' then
        endDate = ''
    end

    local layer = {
        Identifier = layerName,
        Type = "TemporalTileLayer",
        Mode = "Prototyped",
        Prototyped = {
            Time = {
                Start = startDate,
                End = endDate
            },
            TemporalResolution = "1d",
            TimeFormat = "YYYY-MM-DD",
            Prototype = openspace.globebrowsing.createTemporalGibsGdalXml(layerName, resolution, format)
        }
    }

    openspace.globebrowsing.addLayer(
        'Earth',
        'ColorLayers',
        layer
    )
end

openspace.globebrowsing.createGibsGdalXml = function (layerName, date, resolution, format)
    local tileLevel = 5
    -- These resolutions are defined by GIBS: https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+API+for+Developers#GIBSAPIforDevelopers-Script-levelAccessviaGDAL
    if resolution == "2km" then
        tileLevel = 5
    elseif resolution == "1km" then
        tileLevel = 6
    elseif resolution == "500m" then
        tileLevel = 7
    elseif resolution == "250m" then
        tileLevel = 8
    elseif resolution == "125m" then
        tileLevel = 9
    elseif resolution == "62.5m" then
        tileLevel = 10
    elseif resolution == "31.25m" then
        tileLevel = 11
    elseif resolution == "15.625m" then
        tileLevel = 12
    else
        openspace.printError("Unknown resolution: " .. resolution)
        return ""
    end

    local rasterCount = 3
    if format == "jpg" then
        if layerName == "ASTER_GDEM_Greyscale_Shaded_Relief" then
            rasterCount = 1
        else
            rasterCount = 3
        end
    elseif format == "png" then
        rasterCount = 4
    else
        openspace.printError("Unknown format \"" .. format .. "\". Use 'jpg' or 'png'")
        return ""
    end

    local gdalWmsTemplate =
    "<GDAL_WMS>" ..
        "<Service name=\"TMS\">" ..
            "<ServerUrl>https://gibs.earthdata.nasa.gov/wmts/epsg4326/best/" ..
            layerName .. "/default/" .. date .. "/" .. resolution ..
            "/${z}/${y}/${x}." .. format .. "</ServerUrl>" ..
        "</Service>" ..
        "<DataWindow>" ..
            "<UpperLeftX>-180.0</UpperLeftX>" ..
            "<UpperLeftY>90</UpperLeftY>" ..
            "<LowerRightX>396.0</LowerRightX>" ..
            "<LowerRightY>-198</LowerRightY>" ..
            "<TileLevel>" .. tileLevel .. "</TileLevel>" ..
            "<TileCountX>2</TileCountX>" ..
            "<TileCountY>1</TileCountY>" ..
            "<YOrigin>top</YOrigin>" ..
        "</DataWindow>" ..
        "<Projection>EPSG:4326</Projection>" ..
        "<BlockSizeX>512</BlockSizeX>" ..
        "<BlockSizeY>512</BlockSizeY>" ..
        "<BandsCount>" .. rasterCount .. "</BandsCount>" ..
        "<UnsafeSSL>true</UnsafeSSL>" ..
        "<ZeroBlockHttpCodes>400,204,404</ZeroBlockHttpCodes>" ..
        "<ZeroBlockOnServerException>true</ZeroBlockOnServerException>" ..
        "<Timeout>5</Timeout>" ..
    "</GDAL_WMS>"

    return gdalWmsTemplate
end

openspace.globebrowsing.createTemporalGibsGdalXml = function (layerName, resolution, format)
    return openspace.globebrowsing.createGibsGdalXml(layerName, "${OpenSpaceTimeId}", resolution, format)
end

openspace.globebrowsing.parseInfoFile = function (file)
    -- We are loading these values from an external info file and since we are switching
    -- to a strict Lua, we need to predefine these global variables
    local function declare(name)
        rawset(_G, name, "")
    end

    declare("Name")
    declare("Identifier")
    declare("Description")
    declare("ColorFile")
    declare("HeightFile")
    declare("Location")
    declare("ZIndex")

    local dir = openspace.directoryForPath(file)
    local file_func, error = loadfile(file)
    if file_func then
        file_func()
    else
        openspace.printError('Error loading file "' .. file .. '": '.. error)
        return nil
    end

    -- Hoist the global variables into local space
    local Name = rawget(_G, "Name")
    local Identifier = rawget(_G, "Identifier")
    local Description = rawget(_G, "Description")
    local ColorFile = rawget(_G, "ColorFile")
    local HeightFile = rawget(_G, "HeightFile")
    local Location = rawget(_G, "Location")
    local ZIndex = rawget(_G, "ZIndex")

    -- Now we can start
    local name = Name or Identifier
    local identifier = Identifier
    local zIndex = ZIndex
    if zIndex == "" then
        zIndex = nil
    end

    if identifier == "" then
        openspace.printError('Error loading file "' .. file .. '": No "Identifier" found')
        return nil
    end

    local color = nil
    if ColorFile and ColorFile ~= "" then
        color = {
            Identifier = identifier,
            Name = name,
            Description = Description or "",
            FilePath = dir .. '/' .. ColorFile,
            BlendMode = "Color",
            ZIndex = zIndex
        }
    end

    local height = nil
    if HeightFile and HeightFile ~= "" then
        height = {
            Identifier = identifier,
            Name = name,
            Description = Description or "",
            FilePath = dir .. '/' .. HeightFile,
            TilePixelSize = 65,
            ZIndex = zIndex
        }
    end

    local location = nil
    if Location then
        location = Location
    end

    return {
        Color = color,
        Height = height,
        Name = name,
        Location = location,
        Identifier = identifier
    }
end

openspace.globebrowsing.addBlendingLayersFromDirectory = function (dir, node_name)
    local function ends_with(str, ending)
        return ending == '' or str:sub(-#ending) == ending
    end

    -- Walking the directory with an empty string will cause the current working directory
    -- to be walked recursively. This is probably not what the users expects (and it is
    -- also one of the default values in the globebrowsing customization script), so we
    -- ignore the empty string here

    if dir == nil or dir == '' then
        openspace.printError("No directory specified")
        return
    end
    if node_name == nil or node_name == '' then
        openspace.printError("No node name specified")
        return
    end

    local files = openspace.walkDirectoryFiles(dir, true, true)

    for _, file in pairs(files) do
        if file and file:find('.info') and ends_with(file, '.info') then
            local t = openspace.globebrowsing.parseInfoFile(file)

            if t and t.Color then
                openspace.printInfo("Adding color layer '" .. t.Color["Identifier"] .. "'")
                openspace.globebrowsing.addLayer(node_name, "ColorLayers", t.Color)
            end
            if t and t.Height then
                openspace.printInfo("Adding height layer '" .. t.Height["Identifier"] .. "'")
                openspace.globebrowsing.addLayer(node_name, "HeightLayers", t.Height)
            end
        end
    end
end

openspace.globebrowsing.addFocusNodesFromDirectory = function (dir, node_name)
    local files = openspace.walkDirectoryFiles(dir, true, true)

    for _, file in pairs(files) do
        if file and file:find('.info') then
            local t = openspace.globebrowsing.parseInfoFile(file)

            if node_name and t and t.Location then
                openspace.printInfo("Creating focus node for '" .. node_name .. "'")

                local lat = t.Location.Center[1]
                local long = t.Location.Center[2]

                local identifier = node_name .. " - " .. t.Identifier
                local name = node_name .. " - " .. t.Name

                openspace.addSceneGraphNode({
                    Identifier = identifier,
                    Parent = node_name,
                    Transform = {
                        Translation = {
                            Type = "GlobeTranslation",
                            Globe = node_name,
                            Latitude = lat,
                            Longitude = long,
                            UseHeightmap = true
                        }
                    },
                    GUI = {
                        Path = "/Other/Bookmarks",
                        Name = name
                    }
                })
            end
        end
    end
end

openspace.globebrowsing.addFocusNodeFromLatLong = function (name, globe_identifier, lat, long, altitude)
    altitude = altitude or 0;

    local identifier = globe_identifier .. "-" .. name

    openspace.addSceneGraphNode({
        Identifier = identifier,
        Parent = globe_identifier,
        Transform = {
            Translation = {
                Type = "GlobeTranslation",
                Globe = globe_identifier,
                Latitude = lat,
                Longitude = long,
                FixedAltitude = altitude
            }
        },
        GUI = {
            Path = "/Other/Bookmarks",
            Name = identifier
        }
    })

    return identifier
end


openspace.globebrowsing.loadWMSServersFromFile = function (file_path)
    local servers = dofile(file_path)

    for key, value in pairs(servers) do
        local globe = key
        for _,val in pairs(value) do
            openspace.globebrowsing.loadWMSCapabilities(
                val["Name"],
                globe,
                val["URL"]
            )
        end
    end
end
