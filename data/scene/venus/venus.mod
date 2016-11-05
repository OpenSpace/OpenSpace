return {
    -- Venus barycenter module
    {
        Name = "VenusBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "VENUS BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        }
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
        Rotation = {
            Type = "SpiceRotation",
            Frame = "IAU_VENUS",
            Reference = "GALACTIC"
        },
    },
    -- VenusTrail module
    {   
        Name = "VenusTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "VENUS BARYCENTER",
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
