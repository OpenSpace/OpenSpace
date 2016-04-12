UseAccurateKernels = false

if UseAccurateKernels then
    NewHorizonsKernels = {
        "${SPICE}/nh_kernels/spk/NavPE_de433_od122.bsp",
        "${SPICE}/nh_kernels/spk/NavSE_plu047_od122.bsp"
    }
else
    NewHorizonsKernels = {
        "${SPICE}/NewHorizonsKernels/nh_plu017.bsp"
    }
end

return {
    -- Pluto barycenter module
    {
        Name = "PlutoBarycenter",
        Parent = "SolarSystemBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "PLUTO BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = NewHorizonsKernels
        },
    },
    -- PlutoProjection module
    {   
        Name = "PlutoProjection",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
			Frame = "IAU_PLUTO",
			Body = "PLUTO",
            Geometry = {
                Type = "SimpleSphereProjection",
                Radius = { 1.173 , 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                -- Color = "textures/pluto_highres_180.jpg",
                Color = "textures/Shenk_180.jpg",
                -- Color = "textures/pluto_large.jpg",
                -- Color = "textures/white.png",
				Project = "textures/3.jpg",
				Sequencing = "true",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            },
			Projection = {
				Sequence       = "${OPENSPACE_DATA}/scene/plutoprojectionhybrid/images",
				EventFile      = "${OPENSPACE_DATA}/scene/plutoprojectionhybrid/assets/core_v9h_obs_getmets_v8_time_fix_nofrcd_mld.txt",
                SequenceType   = "hybrid",
				Observer       = "NEW HORIZONS",
				Target         = "PLUTO",
				Aberration     = "NONE",
			},
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
        Ephemeris = {
            Type = "Spice",
            Body = "PLUTO",
            Reference = "GALACTIC",
            Observer = "PLUTO BARYCENTER",
            Kernels = NewHorizonsKernels
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_PLUTO",
            Reference = "GALACTIC"
        },
        GuiName = "/Solar/Planets/Pluto"
    },
	{   
       Name = "PlutoBarycenterLabel",
       Parent = "PlutoBarycenter",
       Renderable = {
           Type = "RenderablePlane",
           Billboard = true,
           Size = { 5, 4 },
           Texture = "textures/barycenter.png",
           Atmosphere = {
               Type = "Nishita", -- for example, values missing etc etc
               MieFactor = 1.0,
               MieColor = {1.0, 1.0, 1.0}
           }
       },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        },
    },
    {
        Name = "PlutoText",
        Parent = "PlutoProjection",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 6.3},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Pluto-Text.png"
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, -20, 0, 5}
        }
    },
	{
        Name = "PlutoTexture",
        Parent = "PlutoProjection",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 6.4},
            Origin = "Center",
            Billboard = true,
			ProjectionListener = true,
            Texture = "textures/Pluto-Text.png"
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 40, 5}
        }
    },
	{
        Name = "PlutoShadow",
        Parent = "PlutoProjection",
        Renderable = {
            Type = "RenderableShadowCylinder",
			TerminatorType = "PENUMBRAL", 
			LightSource = "SUN",
			Observer = "NEW HORIZONS",
			Body = "PLUTO",
			BodyFrame = "IAU_PLUTO",
			MainFrame = "GALACTIC",
			Aberration = "NONE",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 5}
        }
    },
	-- PlutoBarycentricTrail module
    {   
        Name = "PlutoBarycentricTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "PLUTO",
            Frame = "GALACTIC",
            Observer = "PLUTO BARYCENTER",
            RGB = {0.00,0.62,1.00},
            TropicalOrbitPeriod = 120 ,
            EarthOrbitRatio = 0.03,
            DayLength = 1,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/CharonTrail"
    },
  -- PlutoTrail module
    {   
        Name = "PlutoTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "PLUTO BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.3, 0.7, 0.3 },
            TropicalOrbitPeriod = 90588 ,
            EarthOrbitRatio = 248.02,
            DayLength = 9.9259,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/PlutoTrail"
    }	
}
