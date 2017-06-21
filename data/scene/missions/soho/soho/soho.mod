return {
    {
        Name = "Soho",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "SOHO",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/soho_2012.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ECLIPJ2000",
                DestinationFrame = "GALACTIC",
            },
        }
    },
    {
        Name = "SolarImagery_Soho_Trail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "SOHO",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/soho_2012.bsp"
            },
            Color = { 0.18, 0.59, 0.21 },
            Period = 365.242,
            Resolution = 1000
        }
    },
    -- Marker
    {
        Name = "SolarImagery_Soho_Marker",
        Parent = "Soho",
        Renderable = {
            Type = "RenderablePlane",
            Size =  10^9.8,
            Origin = "Center",
            Billboard = true,
            Texture = "${OPENSPACE_DATA}/scene/missions/soho/textures/marker.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0, 0}
            }
        }
    },
    {
        Name = "SolarImagery_Soho_Image_C2",
        Parent = "Soho",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            --Resolution = 1024,
            StartResolutionLevel = 0,
            StartInstrument = "LASCO_C2_white-light",
            -- Temp
        -- // 0.012402246  * 0.7, 0.016015625 * 1.3
           -- MagicOffsetFromCenter = {0.008681572, -0.020820312},
           -- MagicPlaneFactor = 0.06,
            Type = "RenderableSolarImagery",
           -- Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = solarImageryDataRootPath .. "/event/soho/",
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
    },
    {
        Name = "SolarImagery_Soho_Image_C3",
        Parent = "Soho",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            --Resolution = 1024,
            Offset = 0.00001,
            StartResolutionLevel = 0,
            StartInstrument = "LASCO_C3_white-light",
            -- Temp
        -- // 0.012402246  * 0.7, 0.016015625 * 1.3
           -- MagicOffsetFromCenter = {0.008681572, -0.020820312},
           -- MagicPlaneFactor = 0.06,
            Type = "RenderableSolarImagery",
           -- Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            RootPath = solarImageryDataRootPath .. "event/soho/",
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
