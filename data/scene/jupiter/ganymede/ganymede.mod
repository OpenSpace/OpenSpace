return {
    -- Ganymede module
    {   
        Name = "Ganymede",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_GANYMEDE", -- should exist. 
            Body = "JUPITER BARYCENTER",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 2.631E6,
                Segments = 100
            },
            ColorTexture = "textures/ganymede.jpg",
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_GANYMEDE",
                DestinationFrame = "IAU_JUPITER",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        }
    },
    -- GanymedeTrail module
    {   
        Name = "GanymedeTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.3, 0.3 },
            Period =  172 / 24,
            Resolution = 1000
        }
    }
}
