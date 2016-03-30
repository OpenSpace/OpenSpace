return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Static = true,
        --[[
        Ephemeris = {
            Type = "Kepler",
            Inclination = 0.00041,
            AscendingNode = 349.2,
            Perihelion = 102.8517,
            SemiMajorAxis = 1.00002,
            DailyMotion = 0.9855796,     
            Eccentricity = 0.0166967,
            MeanLongitude = 328.40353
        }
        --]]
        Ephemeris = {
            Type = "Static"
        }
    },
    -- Earth module
    {   
        Name = "Earth",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "Planet",
            Frame = "IAU_EARTH",
			Body = "EARTH",
        },
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        GuiName = "/Solar/Planets/Earth"
    },
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "EARTH",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.5, 0.8, 1.0},
            TropicalOrbitPeriod = 365.242,
            EarthOrbitRatio = 1,
            DayLength = 24
        },
        GuiName = "/Solar/EarthTrail"
    },
}
