return {
    -- Mars barycenter module
    {
        Name = "MarsBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MARS BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        }
    },
    -- Mars module
    {   
        Name = "Mars",
        Parent = "MarsBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_MARS",
            Body = "MARS BARYCENTER",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.390, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/mars.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MARS",
                DestinationFrame = "ECLIPJ2000",
            },
        }
    },
    -- MarsTrail module
    {   
        Name = "MarsTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MARS BARYCENTER",
                Observer = "SUN",
            },
            Color = { 0.814, 0.305, 0.220 },
            Period = 686.973,
            Resolution = 1000
        }
    }
}
