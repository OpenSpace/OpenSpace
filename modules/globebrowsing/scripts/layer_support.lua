openspace.globebrowsing.documentation = {
    {
        Name = "createTextureLayers",
        Arguments = "table",
        Documentation = "Creates a table used in the 'ColorLayers', 'GrayScaleLayers', or 'GrayScaleColorOverlays' of a RenderableGlobe."
    },
    {
        Name = "createHeightLayers",
        Arguments = "table",
        Documentation = "Creates a table used in the 'HeightLayers' of a RenderableGlobe."
    },
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
    }
}

-- Creates a table used in the 'ColorLayers', 'GrayScaleLayers', or 'GrayScaleColorOverlays'
-- of a RenderableGlobe
-- Usage:
-- table.unpack(openspace.globebrowsing.createTextureLayers(p))
-- where p is an array that contains tables with 'Name' and 'Texture' values
openspace.globebrowsing.createTextureLayers = function (patches)
    result = {}
    for k,v in pairs(patches) do
        table.insert(result, { Name = v["Name"], FilePath = v["Texture"] })
    end
    return result
end

-- Creates a table used in the 'HeightLayers' of a RenderableGlobe
-- Usage:
-- table.unpack(openspace.globebrowsing.openspace.globebrowsing.createHeightLayers(p))
-- where p is an array that contains tables with 'Name' and 'Height' values
openspace.globebrowsing.createHeightLayers = function (patches)
    result = {}
    for k,v in pairs(patches) do
        table.insert(result, { Name = v["Name"], FilePath = v["Height"], TilePixelSize = 90, PerformPreProcessing = true })
    end
    return result
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
    "</GDAL_WMS>"

    return gdalWmsTemplate
end