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
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "VENUS BARYCENTER",
                Observer = "SUN",
            },
            Color = { 1.0, 0.5, 0.2 },
            Period = 224.695,
            Resolution = 1000,
            Tag = "defaultTrails"        
        }
    }
}
