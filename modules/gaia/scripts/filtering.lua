openspace.gaia.documentation = {
  {
    Name = "addClippingBox",
    Arguments = {
      { "identifier", "String" },
      { "size", "vec3" },
      { "position", "vec3" }
    },
    Documentation = [[
      Creates a clipping box for a specific Gaia dataset, that can be used to filter out
      stars that are outside of the box. The box is visualized as a grid in the scene.

      Note that only one clipping box can be active at a time. If a new box is added, the
      old one will be removed.

      \\param identifier The identifier of the scene graph node with a
                         [RenderableGaiaStars](#gaiamission_renderablegaiastars) to be
                         filtered
      \\param size The size of each dimension of the box, in Kiloparsec
      \\param position The position of the center of the box, specified in galactic
                       coordinates in Kiloparsec
    ]]
  },
  {
    Name = "removeClippingBox",
    Arguments = {},
    Documentation = "Remove any added clipping box."
  },
  {
    Name = "addClippingSphere",
    Arguments = {
      { "identifier", "String" },
      { "radius", "Number" }
    },
    Documentation = [[
      Creates a clipping sphere for a specific Gaia dataset, that can be used to filter
      out stars that are outside of the sphere. The sphere is visualized as a grid in the
      scene.

      Note that only one clipping sphere can be active at a time. If a new one is added,
      the old one will be removed.

      \\param identifier The identifier of the scene graph node with a
                         [RenderableGaiaStars](#gaiamission_renderablegaiastars) to be
                         filtered
      \\param radius The desired radius outside of the clipping sphere, in Kiloparsec
    ]]
  },
  {
    Name = "removeClippingSphere",
    Arguments = {},
    Documentation = "Remove any added clipping sphere."
  }
}

openspace.gaia.addClippingBox = function (identifier, size, position)
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
  openspace.setPropertyValueSingle(
    "Scene." .. identifier .. ".Renderable.FilterPosX",
    { (position[1] - size[1] / 2), (position[1] + size[1] / 2) }
  )
  openspace.setPropertyValueSingle(
    "Scene." .. identifier .. ".Renderable.FilterPosY",
    { (position[2] - size[2] / 2), (position[2] + size[2] / 2) }
  )
  openspace.setPropertyValueSingle(
    "Scene." .. identifier .. ".Renderable.FilterPosZ",
    { (position[3] - size[3] / 2), (position[3] + size[3] / 2) }
  )
end

openspace.gaia.removeClippingBox = function()
  local grid_identifier = "Filtering_Box"

  if openspace.hasSceneGraphNode(grid_identifier) then
    openspace.removeSceneGraphNode(grid_identifier)
  end

  openspace.setPropertyValue("Scene.*.Renderable.FilterPosX", { 0.0, 0.0 })
  openspace.setPropertyValue("Scene.*.Renderable.FilterPosY", { 0.0, 0.0 })
  openspace.setPropertyValue("Scene.*.Renderable.FilterPosZ", { 0.0, 0.0 })
end

openspace.gaia.addClippingSphere = function (identifier, radius)
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
  openspace.setPropertyValueSingle(
    "Scene." .. identifier .. ".Renderable.FilterDist",
    { 0.0, radius }
  )
end

openspace.gaia.removeClippingSphere = function()
  local grid_identifier = "Filtering_Sphere"

  if openspace.hasSceneGraphNode(grid_identifier) then
    openspace.removeSceneGraphNode(grid_identifier)
  end

  openspace.setPropertyValue("Scene.*.Renderable.FilterDist", { 0.0, 0.0 })
end
