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
                Radius = { 1.561, 6},
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
        Ephemeris = {
            Type = "Spice",
            Body = "EUROPA",
            Reference = "ECLIPJ2000",
            Observer = "JUPITER BARYCENTER",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_JUPITER",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/EUROPA"
    },
    -- EuropaTrail module
    {   
        Name = "EuropaTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "EUROPA",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod =  60,
            EarthOrbitRatio = 0.01,
            DayLength = 9.9259,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/EuropaTrail"
    }
}
