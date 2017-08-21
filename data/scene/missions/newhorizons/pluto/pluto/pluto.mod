local pluto_radius = 1.173E6

local NewHorizonsKernels = {
    "${SPICE}/new_horizons/spk/NavPE_de433_od122.bsp",
    "${SPICE}/new_horizons/spk/NavSE_plu047_od122.bsp"
}

local ColorTexture = "textures/pluto.jpg"
-- local ColorTexture = "textures/NH_Pluto_mosaic_16384.png"
local HeightTexture = "textures/NH_Pluto_DTM_16384.png"

return {
    -- Pluto barycenter module
    {
        Name = "PlutoBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "PLUTO BARYCENTER",
                Observer = "SUN",
                Kernels = NewHorizonsKernels
            },
        },
    },
    -- Pluto module
    {   
        Name = "Pluto",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Radius = pluto_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = pluto_radius,
                Segments = 100
            },
            ColorTexture = ColorTexture,
            HeightTexture = HeightTexture,
            Projection = {
                Sequence       = "${OPENSPACE_DATA}/scene/missions/newhorizons/pluto/pluto/images",
                EventFile      = "${OPENSPACE_DATA}/scene/missions/newhorizons/pluto/pluto/assets/core_v9h_obs_getmets_v8_time_fix_nofrcd_mld.txt",
                SequenceType   = "hybrid",
                Observer       = "NEW HORIZONS",
                Target         = "PLUTO",
                Aberration     = "NONE",
                AspectRatio = 2,

                DataInputTranslation = {
                    Instrument = {
                        LORRI = {
                            DetectorType  = "Camera",
                            Spice = {"NH_LORRI"},
                        },
                        RALPH_MVIC_PAN_FRAME = {
                            DetectorType  = "Scanner",
                            StopCommand = "RALPH_ABORT",
                            Spice = {"NH_RALPH_MVIC_FT"},
                        },
                        RALPH_MVIC_COLOR = {
                            DetectorType = "Scanner",
                            StopCommand = "END_NOM",
                            Spice = { "NH_RALPH_MVIC_NIR", 
                                      "NH_RALPH_MVIC_METHANE", 
                                      "NH_RALPH_MVIC_RED", 
                                      "NH_RALPH_MVIC_BLUE" },
                        },
                        RALPH_LEISA = {
                            DetectorType = "Scanner",
                            StopCommand = "END_NOM",
                            Spice = {"NH_RALPH_LEISA"},
                        },    
                        RALPH_MVIC_PAN1 = {
                            DetectorType = "Scanner",
                            StopCommand = "END_NOM",
                            Spice = {"NH_RALPH_MVIC_PAN1"},
                        },
                        RALPH_MVIC_PAN2 = {
                            DetectorType = "Scanner",
                            StopCommand = "END_NOM",
                            Spice = {"NH_RALPH_MVIC_PAN2"},
                        }, 
                        ALICE_Use_AIRGLOW = {
                            DetectorType = "Scanner",
                            StopCommand = "ALICE_END_PIXELLIST",
                            Spice = {"NH_ALICE_AIRGLOW"},
                        },
                        ALICE_Use_AIRGLOW = {
                            DetectorType = "Scanner",
                            StopCommand = "ALICE_END_HISTOGRAM",
                            Spice = {"NH_ALICE_AIRGLOW"},
                        },
                        ALICE_Use_SOCC = {
                            DetectorType = "Scanner",
                            StopCommand = "ALICE_END_PIXELLIST",
                            Spice = {"NH_ALICE_SOC"},
                        },
                        ALICE_Use_SOCC = {
                            DetectorType = "Scanner",
                            StopCommand = "ALICE_END_HISTOGRAM",
                            Spice = {"NH_ALICE_SOC"},
                        },
                        REX_START = {
                            DetectorType = "Scanner",
                            StopCommand = "REX_MODE_OFF",
                            Spice = { "NH_REX" },
                        }
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
                            PLUTO       = {"PLUTO"       },
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
                     "PLUTO",
                     "CHARON",
                     "NIX",
                     "HYDRA",
                     "P5",
                     "P4",
                }
            },
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "PLUTO",
                Observer = "PLUTO BARYCENTER",
                Kernels = NewHorizonsKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_PLUTO",
                DestinationFrame = "GALACTIC",
            }
        },
    },
    {   
       Name = "PlutoBarycenterLabel",
       Parent = "PlutoBarycenter",
       Renderable = {
           Type = "RenderablePlane",
           Billboard = true,
           Size = 5E4,
           Texture = "textures/barycenter.png",
           Atmosphere = {
               Type = "Nishita", -- for example, values missing etc etc
               MieFactor = 1.0,
               MieColor = {1.0, 1.0, 1.0}
           }
       },
    },
    {
        Name = "PlutoText",
        Parent = "Pluto",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^6.3,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Pluto-Text.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, -2000000, 0}
            },
        },
    },
    {
        Name = "PlutoTexture",
        Parent = "Pluto",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10.0^6.4,
            Origin = "Center",
            Billboard = true,
            ProjectionListener = false,
            Texture = "textures/Pluto-Text.png"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, -4000000, 0}
            },
        },
    },
    {
        Name = "PlutoShadow",
        Parent = "Pluto",
        Renderable = {
            Type = "RenderableShadowCylinder",
            TerminatorType = "PENUMBRAL", 
            LightSource = "SUN",
            Observer = "NEW HORIZONS",
            Body = "PLUTO",
            BodyFrame = "IAU_PLUTO",
            Aberration = "NONE",
        },
    },
    -- PlutoBarycentricTrail module
    {   
        Name = "PlutoBarycentricTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "PLUTO",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 6.38723,
            Resolution = 1000
        },
    },
  -- PlutoTrail module
    {   
        Name = "PlutoTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "PLUTO BARYCENTER",
                Observer = "SUN",
            },
            Color = { 0.3, 0.7, 0.3 },
            -- Not the actual Period, but the SPICE kernels we have only
            -- go back to 1850, about 150 yeays ago
            Period = 160 * 365.242, 
            Resolution = 1000
        },
    }    
}
