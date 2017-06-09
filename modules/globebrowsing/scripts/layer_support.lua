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

openspace.globebrowsing.createGibsGdalXml = function (layerName, date, resolution, format)
    tileLevel = 5
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
        print("Unknown resolution")
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
        print("Unknown format. Use 'jpg' or 'png'")
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
    "</GDAL_WMS>"

    return gdalWmsTemplate
end