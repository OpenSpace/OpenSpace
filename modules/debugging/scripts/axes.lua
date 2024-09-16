openspace.debugging.documentation = {
  {
    Name = "createCoordinateAxes",
    Arguments = {
      { "nodeIdentifier", "String?" },
      { "scale", "Number?" }
    },
    Documentation = [[
      Creates a new scene graph node that show the coordinate system used for the
      currently selected focus node. The first argument specifies the name of the
      scene graph node for which the axes should be added. If this parameter is
      not specified, the current focus node is used instead. The second argument
      provides the length of the coordinate axis in meters. If this value is not
      specified 2.5 times the interaction sphere of the selected node is used
      instead.
    ]]
  }
}

openspace.debugging.createCoordinateAxes = function (nodeIdentifier, scale)
  local node = nodeIdentifier or openspace.navigation.getNavigationState().Anchor
  local sphere = openspace.propertyValue("Scene." .. node .. ".EvaluatedInteractionSphere")
  if sphere == -1 then
    sphere = 1
  end
  local size = scale or sphere * 2.5

  local nodespec = {
    Identifier = node .. "_DebugAxes",
    Parent = node,
    Transform = {
      Scale = {
        Type = "StaticScale",
        Scale = size
      }
    },
    Renderable = {
      Type = "RenderableCartesianAxes"
    },
    GUI = {
      Name = node .. " (Debug Axes)",
      Path = openspace.propertyValue("Scene." .. node .. ".GuiPath")
    }
  }

  openspace.addSceneGraphNode(nodespec)
end
