local BENNU_BODY = "2101955"

return {
    ------------------------
    --        Bennu       --
    ------------------------
    {   
        Name = "Bennu2",
        Parent = "SolarSystemBarycenter",

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
                    POLY_CAM = {
                        DetectorType = "Camera",
                        Spice = {"ORX_OCAMS_POLYCAM"},
                        Files = {
                            "BaseballDiamond_PolyCam.txt", 
                            "OrbitalB_Site08_PolyCamImages.txt",
                            "Recon_225m_Equatorial_PolyCam",
                        },
                    },
                    SPECTOMETER = {
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
                    Body = BENNU_BODY,
                    --[[
                    Read  = {
                        "TARGET_NAME",
                        "INSTRUMENT_HOST_NAME",
                        "INSTRUMENT_ID", 
                        "START_TIME", 
                        "STOP_TIME", 
                    },
                    Convert = {
                        CHURYUMOV               = {"CHURYUMOV-GERASIMENKO"},
                        ROSETTA                             = {"ROSETTA"              },
                        --NAVCAM                                = {"NAVCAM"},
                        ["ROSETTA-ORBITER"]                        = {"ROSETTA"              },
                        CHURYUMOVGERASIMENKO11969R1           = {"CHURYUMOV-GERASIMENKO"},
                        CHURYUMOVGERASIMENKO           = {"CHURYUMOV-GERASIMENKO"},
                        ["CHURYUMOV-GERASIMENKO1(1969R1)"] = {"CHURYUMOV-GERASIMENKO"},
                        --NAVIGATIONCAMERA                  = {"NAVCAM"               },
                    },
                    ]]
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

        Ephemeris = {
            Type = "Spice",
            Body = BENNU_BODY,
            -- Reference = "ECLIPJ2000",
            Reference = "GALACTIC",
            Observer = "SUN",
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
