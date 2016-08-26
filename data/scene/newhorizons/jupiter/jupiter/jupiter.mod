return {
    -- Jupiter barycenter module
    {
        Name = "JupiterBarycenter",
        Parent = "SolarSystemBarycenter",     
        Ephemeris = {
            Type = "Spice",
            Body = "JUPITER BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp",
            }
        },
    },
    -- JupiterProjection module
    {   
        Name = "JupiterProjection",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Frame = "IAU_JUPITER",
            Body = "JUPITER",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.71492, 8 },
                Segments = 200,
            },
            Textures = {
                Type = "simple",
                Color = "textures/jupiterFlipped_low.jpg",
                Project = "textures/lorriTest1.jpg",
                Sequencing = "true",
            },
            Projection = {
                --Sequence   = "F:/JupiterFullSequence",
                Sequence   = "${OPENSPACE_DATA}/scene/newhorizons/jupiter/jupiter/ProjectionsOfInterest",
                SequenceType = "image-sequence",
                Observer   = "NEW HORIZONS",
                Target     = "JUPITER",
                Aberration = "NONE",
            },
            DataInputTranslation = {
                Instrument = {
                    LORRI = {
                        DetectorType  = "Camera",
                        Spice = {"NH_LORRI"},
                    },        
                },                    
                Target ={ 
                    Read  = {
                        "TARGET_NAME",
                        "INSTRUMENT_HOST_NAME",
                        "INSTRUMENT_ID", 
                        "START_TIME", 
                        "STOP_TIME", 
                        "DETECTOR_TYPE",
                        --"SEQUENCE_ID",
                    },
                    Convert = { 
                        JRINGS      = {"IMAGE-PLANE" },
                        J1IO        = {"IO"          },
                        J2EUROPA    = {"EUROPA"      },
                        J6HIMALIA   = {"IMAGE-PLANE" },
                        J7ELARA     = {"IMAGE-PLANE" },
                        CALIBRATION = {"CALIBRATION" },
                        JUPITER     = {"JUPITER"     },
                        CALLISTO    = {"CALLISTO"    },
                        GANYMEDE    = {"GANYMEDE"    },
                        EARTH       = {"EARTH"       },
                        NEWHORIZONS = {"NEW HORIZONS"},
                        CCD         = {"CAMERA"      },
                        FRAMECCD    = {"SCANNER"     },
                    },
                },
            },
            Instrument = {                
                Name       = "NH_LORRI",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
                Fovy       = 0.2907,
                Aspect     = 1,
                Near       = 0.2,
                Far        = 10000,
            },
            PotentialTargets = {
                "JUPITER", "IO", "EUROPA", "GANYMEDE", "CALLISTO"
            }
        },
        Ephemeris = {
            Type = "Static"
        },

        Rotation = {
            Type = "Spice",
            Frame = "IAU_JUPITER",
            Reference = "GALACTIC"
        },
        GuiName = "/Solar/Planets/Jupiter"
    },
    {
        Name = "JupiterText",
        Parent = "JupiterProjection",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 7.5},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Jupiter-text.png",
            BlendMode = "Additive"
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, -1, 0, 8}
        }
    },    
    -- JupiterTrail module
     {   
         Name = "JupiterTrail",
         Parent = "SolarSystemBarycenter",
         Renderable = {
             Type = "RenderableTrail",
             Body = "JUPITER BARYCENTER",
             Frame = "GALACTIC",
             Observer = "SUN",
             RGB = { 0.8, 0.7, 0.7 },
             TropicalOrbitPeriod = 4330.595 ,
             EarthOrbitRatio = 11.857,
             DayLength = 9.9259,
             Textures = {
                 Type = "simple",
                 Color = "${COMMON_MODULE}/textures/glare_blue.png",
                 -- need to add different texture
             },  
         },
         GuiName = "/Solar/JupiterTrail"
     }
    
}
