return {
    -- Neptune barycenter module
    {
        Name = "NeptuneBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "NEPTUNE BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        }
    },

    -- Neptune module
    {   
        Name = "Neptune",
        Parent = "NeptuneBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_NEPTUNE",
            Body = "NEPTUNE BARYCENTER",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 2.4622E7,
                Segments = 100
            },
            ColorTexture = "textures/neptune.jpg",
        },
        Tag = "planet_solarSystem",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_NEPTUNE",
                DestinationFrame = "GALACTIC"
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        }
    },
    -- NeptuneTrail module
    {   
        Name = "NeptuneTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "NEPTUNE BARYCENTER",
                Observer = "SUN",
            },
            Color = {0.2, 0.5, 1.0 },
            Period  = 60200,
            Resolution = 1000,
            Tag = "planetTrail_solarSystem"
        }
    }
}
