local BENNU_BODY = "2101955"

return {
    ------------------------
    --     Osiris Rex     --
    ------------------------
    {
        Name = "OsirisRex",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableModel",
            Body = "OSIRIS-REX",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/osiris_BASE.obj",
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
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = "OSIRIS-REX",
                Reference = "GALACTIC",
                Observer = "SUN",
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ORX_SPACECRAFT",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
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
                GeometryFile = "models/osiris_POLYCAM.obj",
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

        Transform = {
            Translation = {
                Type = "StaticEphemeris",
                Position = {-2.476, 2.710, 3.364},
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ORX_OCAMS_POLYCAM",
                DestinationFrame = "ORX_SPACECRAFT",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        GuiName = "/Solar/ORX_OCAMS_POLYCAM"
    },

    {
        Name = "ORX_REXIS",
        Parent = "OsirisRex",
        Renderable = {
            Type = "RenderableModel",
            Body = "OSIRIS-REX",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/osiris_REXIS.obj",
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
        Transform = {
            Translation = {
                Type = "StaticEphemeris",
                Position = {0, 3.371, 2.712},
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ORX_REXIS",
                DestinationFrame = "ORX_SPACECRAFT",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        
        GuiName = "/Solar/ORX_REXIS"
    },

    {   
        Name = "POLYCAM FOV",
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
        GuiName = "/Solar/POLYCAM FOV"
    },

    {   
        Name = "REXIS FOV",
        Parent = "ORX_REXIS",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "OSIRIS-REX",
            Frame = "ORX_REXIS",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {
                Name       = "ORX_REXIS",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
            },
            PotentialTargets = {
                BENNU_BODY -- Bennu
            }
        },
        GuiName = "/Solar/REXIS FOV"
    },
    --[[
    -- Latest image taken by POLYCAM
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
    -- POLYCAM FoV square
    {
        Name = "FovImagePlane",
        Parent = "OsirisRex",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "IAU_BENNU",
            DefaultTarget = BENNU_BODY,
            Spacecraft = "OSIRIS-REX",
            Instrument = "ORX_OCAMS_POLYCAM",
            Moving = true,
            Texture = "textures/squarefov.png",
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

    -- Comet Dance trail
    {   
        Name = "OsirisRexAsteroidTrail",
        Parent = "Bennu2",
        Renderable = {
            Type = "RenderableTrail",
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = BENNU_BODY,
            TropicalOrbitPeriod = 20000.0,
            EarthOrbitRatio = 2,
            DayLength = 25,
            RGB = { 0.9, 0.2, 0.9 },
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png"
            },  
            StartTime = "2016 SEP 8 12:00:00",
            EndTime = "2023 SEP 24 12:00:00"
        },
        GuiName = "OsirisRexAsteroidTrail"
    },
}
