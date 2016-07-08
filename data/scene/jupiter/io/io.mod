return {
    -- Io module
    {   
        Name = "Io",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_IO", -- should exist. 
            Body = "IO",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 1.8213, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/io.jpg",
            },
        },
        Ephemeris = {
            Type = "Spice",
            Body = "IO",
            Reference = "ECLIPJ2000",
            Observer = "JUPITER BARYCENTER",
            Kernels = {
                --"${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
                "${SPICE}/jup260.bsp",
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_JUPITER",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/Jupiter"
    },
    -- IoTrail module
    {   
        Name = "IoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "IO",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.4, 0.4, 0.2 },
            TropicalOrbitPeriod =  40 ,
            EarthOrbitRatio = 0.0045,
            DayLength = 9.9259,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/IoTrail"
    }
}
