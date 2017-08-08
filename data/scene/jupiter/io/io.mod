return {
    -- RenderableGlobe module
    {
        Name = "Io",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_IO",
                DestinationFrame = "GALACTIC"
            },
            Translation = {
                Type = "SpiceTranslation",
                Target = "IO",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 1821600,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Io Texture",
                        FilePath = "textures/io.jpg",
                        Enabled = true
                    }
                }
            }
        }
    },
    -- Trail module
    {   
        Name = "IoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "IO",
                Observer = "JUPITER BARYCENTER"
            },
            Color = { 0.4, 0.4, 0.2 },
            Period =  42 / 24,
            Resolution = 1000
        }
    }
}
