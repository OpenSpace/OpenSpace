local BENNU_BODY = "2101955"

return {
    ------------------------
    --        Bennu       --
    ------------------------
    {   
        Name = "BennuBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = BENNU_BODY,
                Reference = "GALACTIC",
                Observer = "SUN",
            },
        },

        GuiName = "/Solar/Bennu"
    },
    {   
        Name = "Bennu2",
        Parent = "BennuBarycenter",

        Renderable = {
            Type = "RenderableModelProjection",
            Body = BENNU_BODY,
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/BennuResized.obj",
                Magnification = 0,
            }, 
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
            Textures = {
                Type = "simple",
                Color =  "textures/osirisTex.png",
                Project = "textures/defaultProj.png",
                Default = "textures/defaultProj.png"
            },
            Rotation = {
                Source = "IAU_BENNU",
                Destination = "GALACTIC"
            },
            Projection = {
                Sequence   = "InstrumentTimes",
                SequenceType = "instrument-times",
                Observer   = "SUN",
                Target     = BENNU_BODY,
                Aberration = "NONE",
            },
            DataInputTranslation = {
                Instruments = {
                    ORX_OCAMS_POLYCAM = {
                        DetectorType = "Camera",
                        Spice = {"ORX_OCAMS_POLYCAM"},
                        Files = {
                            "BaseballDiamond_PolyCam.txt", 
                            "OrbitalB_Site08_PolyCamImages.txt",
                            "Recon_225m_Equatorial_PolyCam",
                        },
                    },
                    ORX_REXIS = {
                        DetectorType = "Camera",
                        Spice = {"ORX_REXIS"},
                        Files = {
                            "DetailedSurvey_EquatorialStations_Spectrometers.txt", 
                            "Recon_225m_Equatorial_spectrometers.txt",
                            "Recon_525m_Equatorial_spectrometers.txt",
                        },
                    },
                },       
                Target = {
                    Body = BENNU_BODY, -- Do we need this?
                },
            },

            Instrument = { -- INVALID DATA - JUST FOR TESTING
                Name       = "ORX_OCAMS_POLYCAM",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
                Fovy       = 5.00,
                Aspect     = 1,
                Near       = 0.01,
                Far        = 1000000,
            },
        },

        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_BENNU",
                DestinationFrame = "GALACTIC",
            },
        },

        GuiName = "/Solar/Bennu"
    },
    {   
        Name = "BennuTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = BENNU_BODY,
            Frame = "GALACTIC",
            Observer = "SUN",
            -- Optional rendering properties
            LineColor = { 0.4, 0.0, 0.7},
            PointColor = { 0.4, 0.0, 0.7},
            LineFade = 0, -- [0,1]
            RenderPart = 0.12,
            LineWidth = 2,
            ShowTimeStamps = false,
            -- Time interval
            TimeRange = {
                Start = "2015 JAN 01 00:00:00.000",
                End = "2023 MAY 31 00:00:00.000",
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 0, 
        },
        GuiName = "OsirisRexTrailLocal"
    },
}
