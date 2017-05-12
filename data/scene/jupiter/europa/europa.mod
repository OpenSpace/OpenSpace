return {
    -- Europa module
    {   
        Name = "Europa",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_EUROPA", -- should exist. 
            Body = "EUROPA",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 1.561E6,
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/europa.jpg",
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
                Body = "EUROPA",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_EUROPA",
                DestinationFrame = "IAU_JUPITER",
            }
        }
    },
    -- EuropaTrail module
    {   
        Name = "EuropaTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "EUROPA",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period =  85 / 24,
            Resolution = 1000
        }
    }
}
