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
                Type = "SpiceTranslation",
                Body = BENNU_BODY,
                Observer = "SUN",
            },
        },
    },
    {   
        Name = "Bennu",
        Parent = "BennuBarycenter",

        Renderable = {
            Type = "RenderableModelProjection",
            Body = BENNU_BODY,
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/BennuTextured.obj",
                Magnification = 0,
            }, 
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
            Textures = {
                Type = "simple",
                Color =  "textures/gray.png",
                Project = "textures/defaultProj.png",
                Default = "textures/defaultProj.png"
            },
            Projection = {
                Sequence   = "InstrumentTimes",
                SequenceType = "instrument-times",
                Observer   = "OSIRIS-REX",
                Target     = BENNU_BODY,
                Aberration = "NONE",
                AspectRatio = 2,

                DataInputTranslation = {
                    Instruments = {
                        ORX_OCAMS_POLYCAM = {
                            DetectorType = "Camera",
                            Spice = {"ORX_OCAMS_POLYCAM"},
                            Files = {
                                "BaseballDiamond_PolyCam.txt", 
                                --"OrbitalB_Site08_PolyCamImages.txt",
                                "Recon_225m_Equatorial_PolyCam.txt",
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
                    Fovy       = 0.792,
                    Aspect     = 1,
                    Near       = 0.01,
                    Far        = 1000000,
                },
            },

        },

        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_BENNU",
                DestinationFrame = "GALACTIC",
            },
        },
    },
    {   
        Name = "BennuTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Body = BENNU_BODY,
                Observer = "SUN",
            },
            Color = { 0.4, 0.0, 0.7},
            StartTime = "2015 JAN 01 00:00:00.000",
            EndTime = "2023 MAY 31 00:00:00.000",
            SampleInterval = 3600,
        },
    },
}
