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
            },        },
        
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
        Name = "OsirisRexTrailLocal",
        Parent = "BennuBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = BENNU_BODY,
            -- Optional rendering properties
            LineColor = { 0.9, 0.2, 0.9 },
            PointColor = { 0.9, 0.2, 0.9 },
            LineFade = 0.5, -- [0,1]
            RenderPart = 0.06,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2016 SEP 8 23:05:00.50",
                End = "2023 SEP 24 12:00:00",
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 3, 
        },
        GuiName = "OsirisRexTrailLocal"
    },
    {   
        Name = "OsirisRexTrailGlobal",
        Parent = "LodEarth",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "OSIRIS-REX",
            Frame = "IAU_EARTH",
            Observer = "EARTH",
            -- Optional rendering properties
            LineColor = { 0.9, 0.9, 0.0 },
            PointColor = { 0.9, 0.9, 0.0 },
            LineFade = 0.0, -- [0,1]
            RenderPart = 1,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2016 SEP 8 23:05:00.50",
                End = "2016 SEP 9 00:05:00",
            },
            SampleDeltaTime = 60, -- Seconds between each point
            SubSamples = 59, 
        },
        GuiName = "OsirisRexTrailGlobal"
    },
    {   
        Name = "OsirisRexTrailSolar",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = "SUN",
            -- Optional rendering properties
            LineColor = { 0.2, 0.9, 0.2 },
            PointColor = { 0.2, 0.9, 0.2 },
            LineFade = 0.0, -- [0,1]
            RenderPart = 0.13,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2016 SEP 8 23:05:00.50",
                End = "2023 SEP 24 12:00:00",
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 0, 
        },
        GuiName = "OsirisRexTrailSolar"
    },
}
