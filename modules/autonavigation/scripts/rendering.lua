openspace.autonavigation.documentation = {
    {
        Name = "removeRenderedPath",
        Arguments = "",
        Documentation = "Remove the rendered path, if any."
    },
    {
        Name = "renderPath",
        Arguments = "int",
        Documentation = "Render the currently active path, using linear segments. " .. 
            "The argurment is the number of samples per path segment."
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

openspace.autonavigation.renderPath = function (nrLinesPerSegment)
    local path_identifier = "Camera_Path"
    local label_point = "Point"; 
    local label_line = "Line"; 
    local lineColor = {1.0, 1.0, 0.0}
    local lineWidth = 4

    if openspace.hasSceneGraphNode(path_identifier) then
        openspace.removeSceneGraphNode(path_identifier) 
    end

    local path = { Identifier = path_identifier }
    openspace.addSceneGraphNode(path)

    local points = openspace.autonavigation.getPathPositions(nrLinesPerSegment)
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
        openspace.addSceneGraphNode(node)
    end
    -- TODO: add option to render the points as well?

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
                Opacity = 1
            }, 
            Parent = path_identifier
        }
        openspace.addSceneGraphNode(node)
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
    local sphereTexture = "${DATA}/test2.jpg"
    local sphereRadius = 1000000 -- TODO: optional input?
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