openspace.gaia.documentation = {
  {
    Name = "addClippingBox",
    Arguments = {
      { "name", "String" },
      { "size", "vec3" },
      { "position", "vec3" }
    },
    Documentation = [[
      Creates a clipping box for the Gaia renderable specified by the first argument.

      The position and size values are given in Kiloparsec.
    ]]
  },
  {
    Name = "removeClippingBox",
    Arguments = {},
    Documentation = ""
  },
  {
    Name = "addClippingSphere",
    Arguments = {
      { "name", "String" },
      { "radius", "Number" }
    },
    Documentation = [[
      Creates a clipping sphere for the Gaia renderable specified by the first argument.

      The radius is given in Kiloparsec.
    ]]
  },
  {
    Name = "removeClippingSphere",
    Arguments = {},
    Documentation = ""
  }
}

openspace.gaia.addClippingBox = function (name, size, position)
  local grid_identifier = "Filtering_Box"
  local kilo_parsec_in_meter = 30856775814913700000

  if openspace.hasSceneGraphNode(grid_identifier) then
    openspace.removeSceneGraphNode(grid_identifier)
  end

  local grid = {
    Identifier = grid_identifier,
    Transform = {
      Translation = {
        Type = "StaticTranslation",
        Position = {
          position[1] * kilo_parsec_in_meter,
          position[2] * kilo_parsec_in_meter,
          position[3] * kilo_parsec_in_meter
        }
      },
      Scale = {
        Type = "StaticScale",
        Scale = kilo_parsec_in_meter
      }
    },
    Renderable = {
      Type = "RenderableBoxGrid",
      Color = { 0.6, 0.5, 0.7 },
      LineWidth = 2.0,
      Size = {
        size[1],
        size[2],
        size[3]
      }
    },
    GUI = {
      Name = "Filtering Grid",
      Path = "/Other/Grids"
    }
  }

  openspace.addSceneGraphNode(grid)
  openspace.setPropertyValue(
    "Scene." .. name .. ".Renderable.FilterPosX",
    { (position[1] - size[1] / 2), (position[1] + size[1] / 2) }
  )
  openspace.setPropertyValue(
    "Scene." .. name .. ".Renderable.FilterPosY",
    { (position[2] - size[2] / 2), (position[2] + size[2] / 2) }
  )
  openspace.setPropertyValue(
    "Scene." .. name .. ".Renderable.FilterPosZ",
    { (position[3] - size[3] / 2), (position[3] + size[3] / 2) }
  )
end

openspace.gaia.removeClippingBox = function()
  local grid_identifier = "Filtering_Box"

  if openspace.hasSceneGraphNode(grid_identifier) then
    openspace.removeSceneGraphNode(grid_identifier)
  end
end

openspace.gaia.addClippingSphere = function (name, radius)
  local grid_identifier = "Filtering_Sphere"
  local kilo_parsec_in_meter = 30856775814913700000

  if openspace.hasSceneGraphNode(grid_identifier) then
    openspace.removeSceneGraphNode(grid_identifier)
  end

  local grid = {
    Identifier = grid_identifier,
    Transform = {
      Scale = {
        Type = "StaticScale",
        Scale = radius * kilo_parsec_in_meter
      }
    },
    Renderable = {
      Type = "RenderableSphericalGrid",
      Color = { 0.6, 0.5, 0.7 },
      LineWidth = 1.0,
    },
    GUI = {
      Name = "Filtering Sphere",
      Path = "/Other/Grids"
    }
  }

  openspace.addSceneGraphNode(grid)
  openspace.setPropertyValue(
    "Scene." .. name .. ".Renderable.FilterDist",
    { 0.0, radius }
  )
end

openspace.gaia.removeClippingSphere = function()
  local grid_identifier = "Filtering_Sphere"

  if openspace.hasSceneGraphNode(grid_identifier) then
    openspace.removeSceneGraphNode(grid_identifier)
  end
end
