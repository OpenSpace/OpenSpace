return {
    {
        Name = "Iapetus",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 734000,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Iapetus Texture",
                        FilePath = "textures/iapetus.jpg",
                        Enabled = true
                    }
                }
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "IAPETUS",
                Observer = "SATURN BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/sat375.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_ENCELADUS",
                DestinationFrame = "GALACTIC"
            }
        }
    },
    {
        Name = "IapetusTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "IAPETUS",
                Observer = "SATURN BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period = 79,
            Resolution = 1000
        }
    }
}
