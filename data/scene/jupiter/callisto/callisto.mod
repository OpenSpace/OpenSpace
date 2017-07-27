return {
    -- Callisto module
    {   
        Name = "Callisto",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_CALLISTO", -- should exist. 
            Body = "CALLISTO",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 2.631E6,
                Segments = 100
            },
            ColorTexture = "textures/callisto.jpg",
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "CALLISTO",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_CALLISTO",
                DestinationFrame = "IAU_JUPITER",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        }
    },
    -- CallistoTrail module
    {   
        Name = "CallistoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "CALLISTO",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.3, 0.01 },
            Period =  17,
            Resolution = 1000
        }
    }
}
