openspace.softwareintegration.documentation = {
    {
        Name = "addRenderable",
        Arguments = "string, vec3, string, float, float, string",
        Documentation = "TODO"
    },
    {
        Name = "removeRenderable",
        Arguments = "string",
        Documentation = "TODO"
    },
    {
        Name = "updateProperties",
        Arguments = "string, vec3, vec3",
        Documentation = "TODO"
    }
}

openspace.softwareintegration.addRenderable = function (id, colors, file, alpha, size, guiName)

    local RenderablePointsCloud = {
        Identifier = id,
        Renderable = {
            Type = "RenderablePointsCloud",
            Color = colors,
            File = file,
            Opacity = alpha,
            Size = size,
        },
        GUI = {
            Name = guiName,
            Path = "/Examples"
        }
    }

    openspace.addSceneGraphNode(RenderablePointsCloud)
end

openspace.softwareintegration.removeRenderable = function (id)    
    if openspace.hasSceneGraphNode(id) then
        openspace.removeSceneGraphNode(id)
    end
end

openspace.softwareintegration.updateProperties = function (id, argument, string)    
    if string == "alpha" then 
        openspace.setPropertyValueSingle('Scene.' .. id .. '.Renderable.Opacity', argument)
    elseif string == "color" then
       openspace.setPropertyValueSingle('Scene.' .. id .. '.Renderable.Color', argument)  
    elseif string == "size" then
       openspace.setPropertyValueSingle('Scene.' .. id .. '.Renderable.Size', argument)
    end  
end
