return {
    -- RenderableGlobe module
    {
        Name = "Europa",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_EUROPA",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "SpiceTranslation",
                Target = "EUROPA",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 1560800,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Europa Texture",
                        FilePath = "textures/europa.jpg",
                        Enabled = true,
                    },
                },
            },
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    },

    -- Trail module
    {   
        Name = "EuropaTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "EUROPA",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period =  85 / 24,
            Resolution = 1000
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    }
}
