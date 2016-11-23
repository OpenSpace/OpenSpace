return {
    -- Mercury barycenter module
    {
        Name = "MercuryBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MERCURY BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        }
    },
    -- Mercury module
    {   
        Name = "Mercury",
        Parent = "MercuryBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_MERCURY",
            Body = "MERCURY",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 2.440, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/mercury.jpg",
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
                SourceFrame = "IAU_MERCURY",
                DestinationFrame = "ECLIPJ2000",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        }
    },
    -- MercuryTrail module
    {   
        Name = "MercuryTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MERCURY",
                Observer = "SUN",
            },
            Color = {0.6, 0.5, 0.5 },
            Period = 87.968,
            Resolution = 100
        }
    }
}
