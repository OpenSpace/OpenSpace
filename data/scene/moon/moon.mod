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
                Radius = { 1.737, 6},
                Segments = 100
            },
            Shadow_Group = {
                Source1 = {
                    Name = "Sun",
                    Radius = {696.3, 6}
                },
                Caster1 = { 
                    Name = "Earth",
                    Radius = {6.371, 6}
                },
            },
            Textures = {
                Type = "simple",
                Color = "textures/Moon16K.dds",
                --Color = "textures/moonmap4k.jpg",
            },
        },
        Ephemeris = {
            Type = "Spice",
            Body = "MOON",
            Reference = "ECLIPJ2000",
            Observer = "EARTH BARYCENTER",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_MOON",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/MOON"
    },
    -- MoonTrail module
    {   
        Name = "MoonTrail",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "MOON",
            Frame = "GALACTIC",
            Observer = "EARTH BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod =  60,
            EarthOrbitRatio = 0.01,
            DayLength = 1.0,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/MoonTrail"
    }
}
