return {
    -- Jupiter barycenter module
    {
        Name = "JupiterBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "JUPITER BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- Jupiter module
    {   
        Name = "Jupiter",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_JUPITER",
            Body = "JUPITER BARYCENTER",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 0.71492E8,
                Segments = 200
            },
            ColorTexture = "textures/jupiter.jpg",
        },
        Tag = "planet_solarSystem",
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0, 0}, -- jupiter is at its barycenter
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_JUPITER",
                DestinationFrame = "ECLIPJ2000",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        }
    },
    -- JupiterTrail module
    {   
        Name = "JupiterTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "JUPITER BARYCENTER",
                Observer = "SUN",
            },
            Color = { 0.8, 0.7, 0.7 },
            Period = 4330.595,
            Resolution = 1000,
        },
        Tag = "planetTrail_solarSystem"
    }
}
