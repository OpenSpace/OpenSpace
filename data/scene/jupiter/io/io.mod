return {
    -- Io module
    {   
        Name = "Io",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_IO", -- should exist. 
            Body = "IO",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 1.8213E6,
                Segments = 100
            },
            ColorTexture = "textures/io.jpg",
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "IO",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_IO",
                DestinationFrame = "IAU_JUPITER",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        }
    },
    -- IoTrail module
    {   
        Name = "IoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "IO",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.4, 0.2 },
            Period =  42 / 24,
            Resolution = 1000
        }
    }
}
