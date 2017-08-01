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
    }
}

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
    "</GDAL_WMS>"

    return gdalWmsTemplate
end

openspace.globebrowsing.parseInfoFile = function (file)
    local dir = openspace.directoryForPath(file)
    dofile(file)

    local color = {
        Name = Name,
        Description = Description or "",
        FilePath = dir .. '/' .. ColorFile,
        BlendMode = "Color"
    }

    local height = {
        Name = Name,
        Description = Description or "",
        FilePath = dir .. '/' .. HeightFile,
        TilePixelSize = 90
    }

    return color, height
end

openspace.globebrowsing.addBlendingLayersFromDirectory = function (dir, node_name)
    local files = openspace.walkDirectoryFiles(dir, true, true)

    for _, file in pairs(files) do
        if file:find('.info') then
            c, h = openspace.globebrowsing.parseInfoFile(file)

            openspace.globebrowsing.addLayer(node_name, "ColorLayers", c)
            openspace.globebrowsing.addLayer(node_name, "HeightLayers", h)
        end
    end
end