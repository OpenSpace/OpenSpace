return {
    -- RenderableGlobe module
    {
        Name = "Ganymede",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_GANYMEDE",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "SpiceTranslation",
                Body = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {2631000, 2631000, 2631000},
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Ganymede Texture",
                        FilePath = "textures/ganymede.jpg",
                        Enabled = true,
                    },
                },
            },
        }
    },
    -- Trail module
    {   
        Name = "GanymedeTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.3, 0.3 },
            Period =  172 / 24,
            Resolution = 1000
        }
    }
}
