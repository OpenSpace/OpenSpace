local BENNU_BODY = "2101955"

return {
    ------------------------
    --        Bennu       --
    ------------------------
    {   
        Name = "Bennu2",
        Parent = "SolarSystemBarycenter",

        Ephemeris = {
            Type = "Spice",
            Body = BENNU_BODY,
            -- Reference = "ECLIPJ2000",
            Reference = "GALACTIC",
            Observer = "SUN",
        },

        Renderable = {
            Type = "RenderableModel",
            Body = BENNU_BODY,
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/BennuUntextured.obj",
                Magnification = 4,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/osirisTex.png",
            },
            Rotation = {
                Source = "IAU_BENNU",
                Destination = "GALACTIC"
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
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
