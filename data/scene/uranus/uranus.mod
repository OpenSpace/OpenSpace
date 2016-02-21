return {
    -- Uranus barycenter module
    {
        Name = "UranusBarycenter",
        Parent = "SolarSystemBarycenter",
        Ephemeris = {
            Type = "Static"
        }
    },

    -- Uranus module
    {   
        Name = "Uranus",
        Parent = "UranusBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
			Frame = "IAU_URANUS",
			Body = "URANUS BARYCENTER",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 2.5362 , 7 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/uranus.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "URANUS BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_URANUS",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/Uranus"
    },
    -- UranusTrail module
    {   
        Name = "UranusTrail",
        Parent = "UranusBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "URANUS BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = {0.60,0.95,1.00 },
            TropicalOrbitPeriod = 30588.740 ,
            EarthOrbitRatio = 83.749,
            DayLength = 17.24,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/UranusTrail"
    }
}
