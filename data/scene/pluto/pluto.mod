return {
    -- Pluto barycenter module
    {
        Name = "PlutoBarycenter",
        Parent = "SolarSystemBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "PLUTO BARYCENTER",
            Reference = "GALACTIC",
            Observer = "SUN",
            Kernels = {
               "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp", 
                  -- "${OPENSPACE_DATA}/spice/Pluto/EPHEMERIDES/nh_plu017.bsp"
            }
        },
    },

    -- Pluto module
    {   
        Name = "Pluto",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_PLUTO",
            Body = "PLUTO",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 1.173 , 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/pluto.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "PLUTO",
            Reference = "GALACTIC",
            Observer = "PLUTO BARYCENTER",
            Kernels = {
               "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp", 
                  -- "${OPENSPACE_DATA}/spice/Pluto/EPHEMERIDES/nh_plu017.bsp"
               -- "C:/Users/michal/NewHorizons/SPICE/nh_plu017.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_PLUTO",
            Reference = "GALACTIC"
        },
        GuiName = "/Solar/Planets/Pluto"
    },
    --[[
    -- PlutoTrail module
    {   
        Name = "PlutoTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "PLUTO BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = {0.58, 0.61, 1.00},
            TropicalOrbitPeriod = 59799.9 ,
            EarthOrbitRatio = 163.73,
            DayLength = 16.11,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/PlutoTrail"
    }
--]]
}
