return {
    {
        Name = "Titan",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 2576000,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Titan Texture",
                        FilePath = "textures/titan.jpg",
                        Enabled = true
                    }
                }
            }
        },        
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "TITAN",
                Observer = "SATURN BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/sat375.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_ENCELADUS",
                DestinationFrame = "GALACTIC"
            }
        },
        GuiPath = "/Solar System/Planets/Saturn/Moons"
    },
    {
        Name = "TitanTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "TITAN",
                Observer = "SATURN BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period = 16,
            Resolution = 1000
        },
        GuiPath = "/Solar System/Planets/Saturn/Moons"
    }
}
