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
                Radius = { 2.631, 6},
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/ganymede.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "GANYMEDE",
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
                Body = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.3, 0.3 },
            Period =  172 / 24,
            Resolution = 1000
        }
    }
}
