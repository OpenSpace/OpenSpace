return {
    -- Venus barycenter module
    {
        Name = "VenusBarycenter",
        Parent = "SolarSystemBarycenter",
    },

    -- Venus module
    {   
        Name = "Venus",
        Parent = "VenusBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_VENUS",
            Body = "VENUS",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 3.760, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/venus.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "VENUS",
            Observer = "SUN",
            Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_VENUS",
            Reference = "ECLIPJ2000"
        },
    },
    -- VenusTrail module
    {   
        Name = "VenusTrail",
        Parent = "VenusBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "VENUS",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = {1, 0.5, 0.2},
            TropicalOrbitPeriod = 224.695 ,
            EarthOrbitRatio = 0.615,
            DayLength = 2802.0,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
    }
}
