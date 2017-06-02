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
        table.insert(result, { Name = v["Name"], FilePath = v["Height"], TilePixelSize = 90, DoPreProcessing = true })
    end
    return result
end
