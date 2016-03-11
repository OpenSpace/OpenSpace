return {
    -- Callisto module
    {   
        Name = "Callisto",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
			Frame = "IAU_CALLISTO", -- should exist. 
			Body = "CALLISTO",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 2.631, 6},
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/callisto.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "CALLISTO",
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
        GuiName = "/Solar/Planets/Callisto"
    },
    -- CallistoTrail module
    {   
        Name = "CallistoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "CALLISTO",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.4, 0.3, 0.01 },
            TropicalOrbitPeriod =  60 ,
            EarthOrbitRatio = 0.045,
            DayLength = 9.9259,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/CallistoTrail"
    }    
}
