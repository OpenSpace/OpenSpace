return {
    {
        Name = "Dione",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 561400,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Dione Texture",
                        FilePath = "textures/dione.jpg",
                        Enabled = true
                    }
                }
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "DIONE",
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
        Name = "DioneTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "DIONE",
                Observer = "SATURN BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period = 66 / 24,
            Resolution = 1000
        }
    }
}
