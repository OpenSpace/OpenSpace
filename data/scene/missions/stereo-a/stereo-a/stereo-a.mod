return {
    -- MODEL PLAY - NOT USED
    -- {
    --     Name = "Stereo",
    --     Parent = "SolarSystemBarycenter",
    --     Renderable = {
    --         Type = "RenderableModel",
    --         Body = "STEREO AHEAD",
    --         Geometry = {
    --             Type = "MultiModelGeometry",
    --             GeometryFile = "Stereo-2016-comp.lwo"
    --         },
    --         Textures = {
    --             Type = "simple",
    --             Color = "textures/tex_01.png"
    --         },
    --     },
    --     Transform = {
    --         Translation = {
    --             Type = "SpiceTranslation",
    --             Body = "STEREO AHEAD",
    --             Observer = "SUN",
    --             Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
    --         }
    --     }
    -- },
    -- Stereo A Trail
    {
        Name = "Stereo A",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
            },
            -- Using internal reference frame
            -- Rotation = {
            --     Type = "SpiceRotation",
            --     SourceFrame = "STASCPNT",
            --     DestinationFrame = "GALACTIC",
            --     Kernels = "${OPENSPACE_DATA}/spice/stereo_rtn.tf"
            -- },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ECLIPJ2000",
                DestinationFrame = "GALACTIC",
                --Kernels = "${OPENSPACE_DATA}/spice/stereo_rtn.tf"
            },
        }
    },
    {
        Name = "Stereo A trail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    },
    -- Plane in front of STEREO
    {
        Name = "Stereo A Plane",
        Parent = "Stereo A",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            Resolution = 2048,
            StartResolutionLevel = 2,
            -- Temp
            MagicOffsetFromCenter = {0.001069336, 0.047768066},
            MagicPlaneFactor = 0.61877,
            Type = "RenderableSpacecraftCameraPlane",
            Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = "/media/noven/BE23-1097/solarflare/STEREOA/EUVI-A/2012/07/",
            -- Optional filter on instruments, otherwise get all
            -- Instruments = {
            --     "aia_aia_94",
            --     "aia_aia_193",
            --     "aia_aia_304",
            --     "aia_aia_171",
            -- },
            -- Path to transferfunctions whose name must match the instrument
            TransferfunctionPath = "/home/noven/workspace/OpenSpace/data/stereotransferfunctions"
        },
        -- Transform = {
        --     Translation = {
        --          Type = "StaticTranslation",
        --          Position = {0, -300000000, 0}
        --     },
        -- }
    }
}
