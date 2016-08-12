local BENNU_BODY = "2101955"

return {
    ------------------------
    --     Osiris Rex     --
    ------------------------
    {
        Name = "OsirisRex",
        Parent = "Bennu2",
        Renderable = {
            Type = "RenderableModel",
            Body = "OSIRIS-REX",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/Osiris.obj",
                Magnification = 0,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/osirisTex.png",
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Ephemeris = {
            Type = "Spice",
            Body = "OSIRIS-REX",
            -- Reference = "ECLIPJ2000",
            Reference = "GALACTIC",
            Observer = BENNU_BODY,
        },
        Rotation = {
            Source = "ORX_SPACECRAFT",
            Destination = "GALACTIC"
        },

        GuiName = "/Solar/OsirisRex"
    },

    {   
        Name = "OsirisRexPath",
        Parent = "Bennu2",
        Renderable = {
            Type = "RenderablePath",
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = BENNU_BODY,
            RGB = { 0.8, 0.0, 0.5},
            TimeSteps = 900,
            PointSteps = 4, -- Every fourth point is marked differently to show hours 
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },  
            DrawLine = true,
            
            StartTime = "2016 SEP 8 12:00:00",
            EndTime = "2023 SEP 24 12:00:00"
        },
        GuiName = "/Solar/OsirisRexTrail"
    },
}
