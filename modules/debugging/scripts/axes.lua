openspace.debugging.documentation = {
  {
    Name = "createCoordinateAxes",
    Arguments = {
      { "nodeIdentifier", "String?" },
      { "scale", "Number?" }
    },
    Documentation = [[
      Creates a new scene graph node that show the coordinate system used for the
      currently selected focus node, or a specified scene graph node.

      Usage:
      ```lua
      -- To add coordinate axes for the currently selected focus node, do not specify any
      -- parameters
      openspace.debugging.createCoordinateAxes()
      ```

      \\param nodeIdentifier The identifier of the scene graph node for which the axes
                             should be added. If this parameter is not specified, the
                             current focus node is used instead.
      \\param scale An optional parameter that specifies the size of the coordinate axes,
                    in meters. If not specified, the size is set to 2.5 times the
                    interaction sphere of the selected node.
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
