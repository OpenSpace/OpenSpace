return {
    -- {
    --     Name = "Stereo",
    --     Parent = "SolarSystemBarycenter",
    --     Renderable = {
    --         Type = "RenderableModel",
    --         Body = "STEREO AHEAD",
    --         Geometry = {
    --             Type = "MultiModelGeometry",
    --             GeometryFile = "Stereo-2016-comp.lwo"
    --         },
    --         Textures = {
    --             Type = "simple",
    --             Color = "textures/tex_01.png"
    --         },
    --     },
    --     Transform = {
    --         Translation = {
    --             Type = "SpiceTranslation",
    --             Body = "STEREO AHEAD",
    --             Observer = "SUN",
    --             Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
    --         }
    --     }
    -- },
    -- Stereo A Trail
    {
        Name = "STEREO A",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    }
}
