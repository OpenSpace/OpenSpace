return {
    {
        Name = "Stereo B",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO BEHIND",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/stereob/behind_2012_265_01.depm.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ECLIPJ2000",
                DestinationFrame = "GALACTIC",
                --Kernels = "${OPENSPACE_DATA}/spice/stereo_rtn.tf"
            },
        }
    },
    {
        Name = "SolarImagery_Stereo_O_StereoB_Trail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO BEHIND",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/stereob/behind_2012_265_01.depm.bsp"
            },
            Color = { 0.41, 0.52, 0.9 },
            Period = 365.242,
            Resolution = 1000
        }
    },
    {
        Name = "SolarImagery_Stereo_L_StereoB_Marker",
        Parent = "Stereo B",
        Renderable = {
            Type = "RenderablePlane",
            Size =  10^9.8,
            Origin = "Center",
            Billboard = true,
            Texture = "${OPENSPACE_DATA}/scene/missions/stereo-b/textures/marker.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0, 0}
            }
        }
    },
    -- Plane in front of STEREO
    {
        Name = "SolarImagery_Stereo_StereoB_Image_EUV",
        Parent = "Stereo B",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            --Resolution = 2048,
            StartGamma = 1.75,
            StartResolutionLevel = 1,
            StartInstrument = "SECCHI_EUVI_304",
            -- Temp
         --   MagicOffsetFromCenter = {-0.004750977, -0.011708984},
          --  MagicPlaneFactor = 0.61877,
            Type = "RenderableSolarImagery",
         --   Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = solarImageryDataRootPath .. "/stereob/",
            -- Optional filter on instruments, otherwise get all
            -- Instruments = {
            --     "aia_aia_94",
            --     "aia_aia_193",
            --     "aia_aia_304",
            --     "aia_aia_171",
            -- },
            -- Path to transferfunctions whose name must match the instrument
          ---  TransferfunctionPath = "/media/noven/BE23-1097/solarbrowsingdata/colortables/stereo"
        },
        -- Transform = {
        --     Translation = {
        --          Type = "StaticTranslation",
        --          Position = {0, -300000000, 0}
        --     },
        -- }
    },
    {
        Name = "SolarImagery_Stereo_StereoB_Image_COR",
        Parent = "Stereo B",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            --Resolution = 2048,
            StartResolutionLevel = 1,
            Offset = 0.00001,
            StartInstrument = "SECCHI_COR2_white-light",
            -- Temp
            --MagicOffsetFromCenter = {0.001069336, 0.047768066},
            --MagicPlaneFactor = 0.61877,
            Type = "RenderableSolarImagery",
            --Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = "/media/noven/BE23-1097/solarbrowsingdata/event/stereob/",
            -- Optional filter on instruments, otherwise get all
            -- Instruments = {
            --     "aia_aia_94",
            --     "aia_aia_193",
            --     "aia_aia_304",
            --     "aia_aia_171",
            -- },
            -- Path to transferfunctions whose name must match the instrument
           -- TransferfunctionPath = "/media/noven/BE23-1097/solarbrowsingdata/colortables/stereo"
        },
        -- Transform = {
        --     Translation = {
        --          Type = "StaticTranslation",
        --          Position = {0, -300000000, 0}
        --     },
        -- }
    }
}
