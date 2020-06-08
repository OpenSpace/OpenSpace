openspace.autonavigation.documentation = {
    {
        Name = "removeRenderedPath",
        Arguments = "",
        Documentation = "Remove the rendered path, if any."
    },
    {
        Name = "renderPath",
        Arguments = "int, [bool], [bool]",
        Documentation = "Render the currently active path, using linear segments. " .. 
            "The first argurment is the number of samples per path segment. " ..
            "The optional second argument can be used to render the points of " ..
            "the path as spheres, if set to true." ..
            "The optional third argument will visualize camera orientation if set to true"
    },
    {
        Name = "removeControlPoints",
        Arguments = "",
        Documentation = "Remove the rendered control points, if any."
    },
    {
        Name = "renderControlPoints",
        Arguments = "",
        Documentation = "Test method for rendering the control points of the camera path spline."
    },
}

openspace.autonavigation.removeRenderedPath = function () 
    local path_identifier = "Camera_Path"
    if openspace.hasSceneGraphNode(path_identifier) then
        openspace.removeSceneGraphNode(path_identifier) 
    end
end

openspace.autonavigation.renderPath = function (nrLinesPerSegment, renderPoints, renderOrientations)
    local path_identifier = "Camera_Path"
    local label_point = "Point"
    local label_line = "Line"
    local lineColor = {1.0, 1.0, 0.0}
    local lineWidth = 4
    local sphereTexture = "${MODULES}/autonavigation/textures/red.png" 
    local sphereRadius = 2000000 
    local sphereSegments = 50
    local label_orientation = "Orientation"
    local orientationLineColor = {1.0, 0.0, 0.0}
    local orientationLineWidth = 2
    local orientationLineLength = 30 * sphereRadius

    if openspace.hasSceneGraphNode(path_identifier) then
        openspace.removeSceneGraphNode(path_identifier) 
    end

    local path = { Identifier = path_identifier }
    openspace.addSceneGraphNode(path)

    local points = openspace.autonavigation.getPathPositions(nrLinesPerSegment)
    local viewDirections = openspace.autonavigation.getPathViewDirections(nrLinesPerSegment)
    local nrPoints = 0
    for _ in pairs(points) do 
        nrPoints = nrPoints + 1 
    end

    -- points
    for key, point in pairs(points) do
        local node = {
            Identifier = label_point .. key,
            Transform = {
                Translation = {
                    Type = "StaticTranslation",
                    Position = point 
                }
            }, 
            Parent = path_identifier
        }

        if renderPoints then 
            node.Renderable = {
                Type = "RenderableSphere",
                Enabled = true,
                Segments = sphereSegments,
                Size = sphereRadius,
                Texture = sphereTexture,
                Opacity = 1
            }
        end

        openspace.addSceneGraphNode(node)
    end

    -- lines between points
    for i = 1,(nrPoints-1) do
        local node = {
            Identifier = label_line .. i,
            Renderable = {
                Enabled = true,
                Type = "RenderableNodeLine",
                StartNode = label_point .. i,
                EndNode = label_point .. (i+1),
                Color = lineColor,
                LineWidth = lineWidth,
                Opacity = 0.5
            }, 
            Parent = path_identifier
        }
        openspace.addSceneGraphNode(node)
    end

    -- lines for view direction
    if renderOrientations then

        for key, point in pairs(points) do
            local viewPos = { 
                tonumber(point[1]) + tonumber(viewDirections[key][1]) * orientationLineLength,
                tonumber(point[2]) + tonumber(viewDirections[key][2]) * orientationLineLength,
                tonumber(point[3]) + tonumber(viewDirections[key][3]) * orientationLineLength
            }

            local node = {
                Identifier = label_orientation .. label_point .. key,
                Transform = {
                    Translation = {
                        Type = "StaticTranslation",
                        Position = viewPos
                    }
                }, 
                Parent = path_identifier
            }
            openspace.addSceneGraphNode(node)
        end

        for i = 1,nrPoints do
            local node = {
                Identifier = label_orientation .. label_line .. i,
                Renderable = {
                    Enabled = true,
                    Type = "RenderableNodeLine",
                    StartNode = label_point .. i,
                    EndNode = label_orientation .. label_point .. i,
                    Color = orientationLineColor,
                    LineWidth = orientationLineWidth,
                    Opacity = 1
                }, 
                Parent = path_identifier
            }
            openspace.addSceneGraphNode(node)
        end                                         
    end

end

openspace.autonavigation.removeControlPoints = function () 
    local base_identifier = "Path_Control_Points"
    if openspace.hasSceneGraphNode(base_identifier) then
        openspace.removeSceneGraphNode(base_identifier) 
    end
end

openspace.autonavigation.renderControlPoints = function ()
    local base_identifier = "Path_Control_Points"
    local label_point = "ControlPoint"
    local sphereTexture = "${MODULES}/autonavigation/textures/yellow.png" 
    local sphereRadius = 2100000 -- TODO: optional input? Not same the other one or interference appears
    local sphereSegments = 50

    if openspace.hasSceneGraphNode(base_identifier) then
        openspace.removeSceneGraphNode(base_identifier)
    end

    local base = { Identifier = base_identifier}
    openspace.addSceneGraphNode(base)

    local points = openspace.autonavigation.getControlPoints()

    for key, point in pairs(points) do
        local node = {
            Identifier = label_point .. key,
            Transform = {
                Translation = {
                    Type = "StaticTranslation",
                    Position = point 
                }
            }, 
            Renderable = {
                Type = "RenderableSphere",
                Enabled = true,
                Segments = sphereSegments,
                Size = sphereRadius,
                Texture = sphereTexture,
                Opacity = 1
            },
            Parent = base_identifier
        }
        openspace.addSceneGraphNode(node)
    end
end
