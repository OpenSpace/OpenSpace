return {
    -- Moon module
    {   
        Name = "Moon",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_MOON",
            Body = "MOON",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 1.737E6,
                Segments = 100
            },
            Shadow_Group = {
                Source1 = {
                    Name = "Sun",
                    Radius = 696.3E6
                },
                Caster1 = { 
                    Name = "Earth",
                    Radius = 6.371E6
                },
            },
            Textures = {
                Type = "simple",
                Color = "textures/Moon16K.dds",
                --Color = "textures/moonmap4k.jpg",
            },
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MOON",
                Observer = "EARTH BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MOON",
                DestinationFrame = "ECLIPJ2000"
            },
        },
    },
    -- MoonTrail module
    {   
        Name = "MoonTrail",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MOON",
                Observer = "EARTH BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period =  27,
            Resolution = 1000
        },
    }
}
