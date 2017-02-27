-- RotationMatrix = {
--     0, 1, 0,
--     0, 0, 1,
--     1, 0, 0
-- }

return {
--     {
--         Name = "Soho",
--         Parent = "SolarSystemBarycenter",
--         Transform = {
--             Translation = {
--                 Type = "SpiceTranslation",
--                 Body = "SOHO",
--                 Observer = "SUN",
--                 Kernels = "${OPENSPACE_DATA}/spice/soho_2017.bsp"
--             },
--             Scale = {
--                 Type = "StaticScale",
--                 Scale = 5000,
--             },
--             Rotation = {
--                 Type = "SpiceRotation",
--                 SourceFrame = "SOHO_SPACECRAFT",
--                 DestinationFrame = "GALACTIC",
--             }
--         }
--     },

-- -- SOHO Body module
--     {
--         Name = "Part1",
--         Parent = "Soho",

--         Renderable = {
--             Type = "RenderableModel",
--            -- Body = "SOHO",
--             Geometry = {
--                 Type = "MultiModelGeometry",
--                 GeometryFile = "SOHO_part1.STL"
--             },
--             Textures = {
--                 Type = "simple",
--                 Color = "textures/tex_01.png"
--             },
--             Rotation = { ModelTransform = RotationMatrix }
--         },
--     },
--     {
--         Name = "Part2",
--         Parent = "Soho",
--         Renderable = {
--             Type = "RenderableModel",
--            -- Body = "SOHO",
--             Geometry = {
--                 Type = "MultiModelGeometry",
--                 GeometryFile = "SOHO_part2.STL"
--             },
--             Textures = {
--                 Type = "simple",
--                 Color = "textures/tex_01.png"
--             },
--             Rotation = { ModelTransform = RotationMatrix }
--         },
--     },
-- SOHO Trail module
    {
        Name = "SohoTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "SOHO",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/soho_2017.bsp"
            },
            Color = { 1.0, 0.5, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    }

}
