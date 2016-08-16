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
        Name = "ORX_OCAMS_POLYCAM",
        Parent = "OsirisRex",
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
            Type = "Static",
        },
        Rotation = {
            Source = "ORX_OCAMS_POLYCAM",
            Destination = "ORX_SPACECRAFT"
        },
        Transform = {
            Translation = {1,0,0}, -- Translation relative to parent
            --Rotation = {0,0,0}, -- Euler angles relative to parent (not implemented)
            --Scale = {1,1,1}, -- Scale relative to parent (not implemented)
        },
        
        GuiName = "/Solar/ORX_OCAMS_POLYCAM"
    },


    --[[
    {   
        Name = "ORX_OCAMS_POLYCAM FOV",
        Parent = "ORX_OCAMS_POLYCAM",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "OSIRIS-REX",
            Frame = "GALACTIC",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "ORX_OCAMS_POLYCAM",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Bennu2",
            }
        },
        GuiName = "/Solar/ORX_OCAMS_POLYCAM"
    },
]]




    {   
        Name = "POLYCAM",
        Parent = "ORX_OCAMS_POLYCAM",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "OSIRIS-REX",
            Frame = "ORX_OCAMS_POLYCAM",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {
                Name       = "ORX_OCAMS_POLYCAM",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
            },
            PotentialTargets = {
                BENNU_BODY -- Bennu
            }
        },
        GuiName = "/Solar/POLYCAM"
    },
    -- Latest image taken by POLYCAM
    --[[
    { 
        Name = "ImagePlaneOsirisRex",
        Parent = "OsirisRex",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "IAU_BENNU",
            DefaultTarget = BENNU_BODY,
            Spacecraft = "OSIRIS-REX",
            Instrument = "ORX_OCAMS_POLYCAM",
            Moving = false,
            Texture = "textures/defaultProj.png",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        }, 
    },
    ]]

    {   
        Name = "OsirisRexPathSolar",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePath",
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.0, 1.0, 0.5},
            TimeSteps = 86400, -- Number of seconds in a day
            PointSteps = 10, -- Every 10 days
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },  
            DrawLine = true,
            
            StartTime = "2016 SEP 8 12:00:00",
            EndTime = "2023 SEP 24 12:00:00"
        },
        GuiName = "/Solar/OsirisRexPathSolar"
    },
    {   
        Name = "OsirisRexPathLocal",
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
        GuiName = "/Solar/OsirisRexPathLocal"
    },
}
