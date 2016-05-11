return {
    -- Jupiter barycenter module
    {
        Name = "JupiterBarycenter",
        Parent = "SolarSystemBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "JUPITER BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
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
                Radius = { 0.71492, 8 },
                Segments = 200
            },
            Textures = {
                Type = "simple",
                Color = "textures/jupiter.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Static" -- jupiter is at its barycenter
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_JUPITER",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/Jupiter"
    },
    -- JupiterTrail module
    {   
        Name = "JupiterTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "JUPITER BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.8, 0.7, 0.7 },
            TropicalOrbitPeriod = 4330.595 ,
            EarthOrbitRatio = 11.857,
            DayLength = 9.9259,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/JupiterTrail"
    }
}
