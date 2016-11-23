return {
    -- Pluto barycenter module
    {
        Name = "PlutoBarycenter",
        Parent = "SolarSystemBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "PLUTO BARYCENTER",
            Observer = "SUN",
            Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp", 
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
            Observer = "PLUTO BARYCENTER",
            Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp", 
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_PLUTO",
            Reference = "GALACTIC"
        },
    },
{   
        Name = "Charon",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_CHARON",
            Body = "CHARON",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.035 , 5 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/gray.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "CHARON",
            Observer = "PLUTO BARYCENTER",
            Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_CHARON",
            Reference = "ECLIPJ2000"
        },
    },
    -- CharonTrail module
    {   
        Name = "CharonTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "CHARON",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00,0.62,1.00},
            Period = 6.38725,
            Resolution = 1000,
        },
    },
    -- PlutoTrail module
    {   
        Name = "PlutoTrailSolarSystem",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "PLUTO BARYCENTER",
                Observer = "SUN",
            },
            Color = {0.58, 0.61, 1.00},
            Period = 247.92 * 365.242,
            Resolution = 1000
        },
        GuiName = "/Solar/PlutoTrail"
    },
    {
        Name = "PlutoTrailPluto",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "PLUTO",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.58, 0.61, 1.00},
            Period = 6.38725,
            Resolution = 1000
        },
        GuiName = "/Solar/PlutoTrail"
    }

}
