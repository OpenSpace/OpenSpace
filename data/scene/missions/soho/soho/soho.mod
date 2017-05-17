return {
    {
        Name = "Soho",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "SOHO",
                Observer = "SUN",
                Kernels = {"${OPENSPACE_DATA}/spice/soho_2012.bsp",
                           "${OPENSPACE_DATA}/spice/soho_2013.bsp"}
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ECLIPJ2000",
                DestinationFrame = "GALACTIC",
            },
        }
    },
    {
        Name = "Soho trail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "SOHO",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/soho_2012.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    },
    {
        Name = "Soho Plane",
        Parent = "Soho",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            Resolution = 1024,
            StartResolutionLevel = 0,
            -- Temp
            -- // 0.012402246  * 0.7, 0.016015625 * 1.3
            MagicOffsetFromCenter = {0.008681572, -0.020820312},
            MagicPlaneFactor = 0.06,
            Type = "RenderableSpacecraftCameraPlane",
            Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = "/media/noven/BE23-1097/solarflare/soho/LASCO-C2",
            -- Optional filter on instruments, otherwise get all
            -- Instruments = {
            --     "aia_aia_94",
            --     "aia_aia_193",
            --     "aia_aia_304",
            --     "aia_aia_171",
            -- },
            -- Path to transferfunctions whose name must match the instrument
            --TransferfunctionPath = "/home/noven/workspace/OpenSpace/data/stereotransferfunctions"
        },
    }

}
