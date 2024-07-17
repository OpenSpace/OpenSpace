openspace.debugging.documentation = {
  {
    Name = "createCoordinateAxes",
    Arguments = {},
    Documentation = [[
      Creates a new scene graph node that show the coordinate system used for the
      currently selected focus node.
    ]]
  }
}

openspace.debugging.createCoordinateAxes = function ()
  local anchor = openspace.navigation.getNavigationState().Anchor
  local radius = openspace.propertyValue("Scene." .. anchor .. ".EvaluatedInteractionSphere")

  local node = {
    Identifier = anchor .. "_DebugAxes",
    Parent = anchor,
    Transform = {
      Scale = {
        Type = "StaticScale",
        Scale = radius * 2.5
      }
    },
    Renderable = {
      Type = "RenderableCartesianAxes"
    },
    GUI = {
      Name = anchor .. " (Debug Axes)",
      Path = openspace.propertyValue("Scene." .. anchor .. ".GuiPath")
    }
  }

  openspace.addSceneGraphNode(node)
end
