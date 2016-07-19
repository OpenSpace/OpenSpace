local BENNU_BODY = "2101955"
return {
    {   
        Name = "Bennu2",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_BENNU",
            Body = BENNU_BODY,
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.371, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_bluemarble.jpg",
                Night = "textures/earth_night.jpg",
                --Height = "textures/earth_bluemarble_height.jpg",                
                -- Depth = "textures/earth_depth.png",
                Reflectance = "textures/earth_reflectance.png",
                Clouds = "textures/earth_clouds.jpg"
            }
        },

        Ephemeris = {
            Type = "Spice",
            Body = BENNU_BODY,
            -- Reference = "ECLIPJ2000",
            Reference = "GALACTIC",
            Observer = "SUN",
        },
        
        GuiName = "/Solar/Bennu"
    },

    {   
        Name = "BennuTrail",
        Parent = "SolarSystemBarycenter",
         Renderable = {
            Type = "RenderableTrail",
            Body = BENNU_BODY,
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.4, 0.0, 0.7},
            TropicalOrbitPeriod = 436.649,
            EarthOrbitRatio = 1.3559, -- worst parameter I've ever seen
            DayLength = 4.288 -- why this for a path??
        },
        GuiName = "/Solar/BennuTrail"
    },
}
