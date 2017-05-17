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
        Name = "Stereo B trail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/stereob/behind_2012_265_01.depm.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    },
    -- Plane in front of STEREO
    {
        Name = "Stereo B Plane",
        Parent = "Stereo B",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            Resolution = 2048,
            StartResolutionLevel = 2,
            -- Temp
            MagicOffsetFromCenter = {-0.004750977, -0.011708984},
            MagicPlaneFactor = 0.61877,
            Type = "RenderableSpacecraftCameraPlane",
            Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = "/media/noven/BE23-1097/solarflare/STEREOB/EUVI-B/2012/07/",
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
