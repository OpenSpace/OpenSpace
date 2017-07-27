return {
    -- Mercury barycenter module
    {
        Name = "MercuryBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "MERCURY BARYCENTER",
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
                Radius = 2.4397E6,
                Segments = 100
            },
            ColorTexture = "textures/mercury.jpg",
        },
        Tag = {"planet_solarSystem", "planet_terrestrial"},
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
                Target = "MERCURY",
                Observer = "SUN",
            },
            Color = {0.6, 0.5, 0.5 },
            Period = 87.968,
            Resolution = 100,
            Tag = {"planetTrail_solarSystem", "planetTrail_terrestrial"}
        }
    }
}
