return {
    {
        Name = "SolarImagery_SDO_Trail",
        Parent = "EarthInertial",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Frame = "J2000",
                Body = "-136395",
                Observer = "EARTH",
                Kernels = solarImageryDataRootPath .. "/spicekernels/SDO_EPHEM_2010123_2017104_new.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 0.997319,
            Resolution = 1000
        }
    },
    -- Translate in Earth's coordinate system from earth's position
    {
        Name = "SDO",
        Parent = "EarthInertial",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Frame = "J2000",
                Body = "-136395",
                Observer = "EARTH",
                Kernels = solarImageryDataRootPath .. "/spicekernels/SDO_EPHEM_2010123_2017104_new.bsp"
            },
        }
    },
    -- Intermediate step - can't go from J2000 directly to ECLIPJ2000 for some reason
    {
        Name = "SDOGalacticIntermediateNode",
        Parent = "SDO",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "GALACTIC",
                DestinationFrame = "J2000",
            },
        }
    },
    -- We want the sun's north pole which is z-axis in ECLIPJ2000,
    -- Ideally we would want the IK kernel for this
    {
        Name = "SDOCoordinatesystem",
        Parent = "SDOGalacticIntermediateNode",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ECLIPJ2000",
                DestinationFrame = "GALACTIC",
            },
        }
    },
    {
        Name = "SolarImagery_SDO_Marker",
        Parent = "SDOCoordinatesystem",
        Renderable = {
            Type = "RenderablePlane",
            Size =  10^9.8,
            Origin = "Center",
            Billboard = true,
            Texture = solarImageryDataRootPath .. "/markers/marker_sdo.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0, 0},
            }
        }
    },
    {
        Name = "SolarImagery_SDO_Image_AIA",
        Parent = "SDOCoordinatesystem",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            --Resolution = 4096,
            --Enabled = true,
            StartResolutionLevel = 1,
            Type = "RenderableSolarImagery",
            StartInstrument = "AIA_AIA_304",
            --Target = "Sun",
            -- Temp
            --MagicPlaneFactor = 0.7675,
            --MagicPlaneOffset = {0.0, 0.0},
            -- Will recursively find all instruments that match array instruments
            --RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",

            --RootPath = "/media/noven/BE23-1097/solarbrowsingdata/event/sdo/imagedata/2012/07/12/",
            RootPath = solarImageryDataRootPath .. "/event/sdo/",
           -- RootPath = "/Volumes/Untitled/solarflare/SDO/2012/07/12",
            -- Optional filter on instruments, otherwise get all
            -- Instruments = {
            --     "aia_aia_94",
            --     "aia_aia_193",
            --     "aia_aia_304",
            --     "aia_aia_171",
            --     "aia_aia_335",
            -- },
            -- Path to transferfunctions whose name must match the instrument
           -- TransferfunctionPath = "/Users/michaelnoven/workspace/OpenSpace/data/sdotransferfunctions"
            --TransferfunctionPath = "/media/noven/BE23-1097/solarbrowsingdata/colortables/sdo"
        },
    },
}
