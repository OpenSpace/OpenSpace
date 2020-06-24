openspace.globebrowsing.documentation = {
    {
        Name = "createTemporalGibsGdalXml",
        Arguments = "string, string, string, string, string, string",
        Documentation =
            "Creates an XML configuration for a temporal GIBS dataset." ..
            "Arguments are: Name, Start date, end date, time resolution, time format," ..
            "resolution, file format. For all specifications, see " ..
            "https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products" ..
            "Usage:" ..
            "openspace.globebrowsing.addLayer(" ..
                "\"Earth\"," ..
                "\"ColorLayers\"," ..
                "{" ..
                    "Type = \"TemporalTileLayer\"," ..
                    "Name = \"MODIS_Terra_Chlorophyll_A\"," ..
                    "FilePath = openspace.globebrowsing.createTemporalGibsGdalXml(" ..
                        "\"MODIS_Terra_Chlorophyll_A\"," ..
                        "\"2013-07-02\"," ..
                        "\"Yesterday\"," ..
                        "\"1d\"," ..
                        "\"1km\"," ..
                        "\"png\"" ..
                    ")" ..
                "}" ..
            ")"
    },
    {
        Name = "createGibsGdalXml",
        Arguments = "string, string, string, string",
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
                    "FilePath = openspace.globebrowsing.createTemporalGibsGdalXml(" ..
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
        Arguments = "string, string, string, string, string",
        Documentation = "Adds a new layer from NASA GIBS to the Earth globe. Arguments " ..
            "are: imagery layer name, imagery resolution, start date, end date, format. " ..
             "For all specifications, see " ..
            "https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products" ..
            "Usage:" ..
            "openspace.globebrowsing.addGibsLayer('AIRS_Temperature_850hPa_Night', '2km', '2013-07-15', 'Present', 'png')"
    },
    {
        Name = "parseInfoFile",
        Arguments = "string",
        Documentation =
            "Parses the passed info file and returns two tables. The first return value " ..
            "contains the table for the color layer of a RenderableGlobe. The second " ..
            "return value contains the table for the height layer of a RenderableGlobe." ..
            "Usage: local color, height = openspace.globebrowsing.parseInfoFile(file)" ..
            "openspace.globebrowsing.addLayer(\"Earth\", \"ColorLayers\", color)" ..
            "openspace.globebrowsing.addLayer(\"Earth\", \"HeightLayers\", height)"

    },
    {
        Name = "addBlendingLayersFromDirectory",
        Arguments = "string, string",
        Documentation =
            "Retrieves all info files recursively in the directory passed as the first " ..
            "argument to this function. The color and height tables retrieved from these " ..
            "info files are then added to the RenderableGlobe identified by name passed " ..
            "to the second argument." ..
            "Usage: openspace.globebrowsing.addBlendingLayersFromDirectory(directory, \"Earth\")"
    },
    {
        Name = "addFocusNodesFromDirectory",
        Arguments = "string, string",
        Documentation =
            "Retrieves all info files recursively in the directory passed as the first " ..
            "argument to this function. The name and location retrieved from these info " ..
            "files are then used to create new SceneGraphNodes that can be used as " ..
            "focus nodes. " ..
            "Usage: openspace.globebrowsing.addFocusNodesFromDirectory(directory, \"Mars\")"
    },
    {
        Name = "addFocusNodeFromLatLong",
        Arguments = "string, string, number, number, number",
        Documentation =
            "Creates a new SceneGraphNode that can be used as focus node. " ..
            "Usage: openspace.globebrowsing.addFocusNodeFromLatLong(" ..
            "\"Olympus Mons\", \"Mars\", -18.65, 226.2, optionalAltitude)"
    },
    {
        Name = "loadWMSServersFromFile",
        Arguments = "string",
        Documentation =
            "Loads all WMS servers from the provided file and passes them to the " ..
            "'openspace.globebrowsing.loadWMSCapabilities' file."
    }
}

openspace.globebrowsing.addGibsLayer = function(layer, resolution, format, startDate, endDate)
    if endDate == 'Present' then
        endDate = ''
    end
    local xml = openspace.globebrowsing.createTemporalGibsGdalXml(layer, startDate, endDate, '1d', resolution, format)
    openspace.globebrowsing.addLayer('Earth', 'ColorLayers', { Identifier = layer,  Type = "TemporalTileLayer", FilePath = xml })
end

openspace.globebrowsing.createTemporalGibsGdalXml = function (layerName, startDate, endDate, timeResolution, resolution, format)
    temporalTemplate =
        "<OpenSpaceTemporalGDALDataset>" ..
        "<OpenSpaceTimeStart>" .. startDate .. "</OpenSpaceTimeStart>" ..
        "<OpenSpaceTimeEnd>" .. endDate .. "</OpenSpaceTimeEnd>" ..
        "<OpenSpaceTimeResolution>" .. timeResolution .. "</OpenSpaceTimeResolution>" ..
        "<OpenSpaceTimeIdFormat>YYYY-MM-DD</OpenSpaceTimeIdFormat>" ..
        openspace.globebrowsing.createGibsGdalXml(layerName, "${OpenSpaceTimeId}", resolution, format) ..
        "</OpenSpaceTemporalGDALDataset>"
    return temporalTemplate
end

openspace.globebrowsing.createGibsGdalXml = function (layerName, date, resolution, format)
    tileLevel = 5
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

    rasterCount = 3
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

    gdalWmsTemplate =
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

openspace.globebrowsing.parseInfoFile = function (file)
    Name = nil
    Identifier = nil
    Description = nil
    ColorFile = nil
    HeightFile = nil

    local dir = openspace.directoryForPath(file)
    dofile(file)

    local name = Name or Identifier
    local identifier = Identifier or Name

    local color = nil
    if ColorFile then
        color = {
            Identifier = identifier,
            Name = name,
            Description = Description or "",
            FilePath = dir .. '/' .. ColorFile,
            BlendMode = "Color"
        }
    end

    local height = nil
    if HeightFile then
        height = {
            Identifier = identifier,
            Name = name,
            Description = Description or "",
            FilePath = dir .. '/' .. HeightFile,
            TilePixelSize = 90
        }
    end

    local location = nil
    if Location then
        location = Location
    end

    return name, color, height, location, identifier
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
            local c, h
            _, c, h, _ = openspace.globebrowsing.parseInfoFile(file)

            if c then
                openspace.printInfo("Adding color layer '" .. c["Identifier"] .. "'")
                openspace.globebrowsing.addLayer(node_name, "ColorLayers", c)
            end
            if h then
                openspace.printInfo("Adding height layer '" .. h["Identifier"] .. "'")
                openspace.globebrowsing.addLayer(node_name, "HeightLayers", h)
            end
        end
    end
end

openspace.globebrowsing.addFocusNodesFromDirectory = function (dir, node_name)
    local files = openspace.walkDirectoryFiles(dir, true, true)

    for _, file in pairs(files) do
        if file and file:find('.info') then
            local n, l
            n, _, _, l, i = openspace.globebrowsing.parseInfoFile(file)

            if n and l then
                openspace.printInfo("Creating focus node for '" .. n .. "'")

                local lat = l.Center[2]
                local long = l.Center[1]
                local a, b, c = openspace.globebrowsing.getGeoPosition(node_name, lat, long, 0.0)
                local p = { a, b, c }

                local identifier = node_name .. " - " .. i
                local name = node_name .. " - " .. n

                openspace.addSceneGraphNode({
                    Identifier = identifier,
                    Parent = node_name,
                    Transform = {
                        Translation = {
                            Type = "StaticTranslation",
                            Position = { p[1], p[2], p[3] }
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

    local a, b, c = openspace.globebrowsing.getGeoPosition(globe_identifier, lat, long, altitude)
    local p = { a, b, c }
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
