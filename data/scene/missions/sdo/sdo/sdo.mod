return {
    {
        Name = "SolarImagery_SDO_Trail",
        Parent = "EarthInertial",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Frame = "J2000",
                Target = "-136395",
                Observer = "EARTH",
                Kernels = solarImageryDataRootPath .. "/spicekernels/SDO_EPHEM_2010123_2017104_new.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 0.997319,
            Resolution = 1000
        },
        GuiPath = "/Solar System/Solar Imagery/SDO trail"
    },
    -- Translate in Earth's coordinate system from earth's position
    {
        Name = "SDO",
        Parent = "EarthInertial",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Frame = "J2000",
                Target = "-136395",
                Observer = "EARTH",
                Kernels = solarImageryDataRootPath .. "/spicekernels/SDO_EPHEM_2010123_2017104_new.bsp"
            },
        },
        GuiPath = "/Solar System/Solar Imagery/SDO"
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
    -- {
    --     Name = "SolarImagery_SDO_Marker",
    --     Parent = "SDOCoordinatesystem",
    --     Renderable = {
    --         Type = "RenderablePlane",
    --         Size =  10^10.3,
    --         Origin = "Center",
    --         Billboard = true,
    --         Texture = solarImageryDataRootPath .. "/markers/marker_sdo.png",
    --         BlendMode = "Additive"
    --     },
    --     Transform = {
    --         Translation = {
    --             Type = "StaticTranslation",
    --             Position = {0, 0, 0},
    --         }
    --     }
    -- },
    {
        Name = "SolarImagery_SDO_Image_AIA",
        Parent = "SDOCoordinatesystem",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            Type = "RenderableSolarImagery",
            -- Will recursively find all instruments with .jp2 or .j2k extension
            RootPath = solarImageryDataRootPath .. "/event/sdo/",
            -- Optional path to transferfunctions whose names must match the instruments name
            TransferfunctionPath = solarImageryDataRootPath .. "/colortables/sdo"
        },
        GuiPath = "/Solar System/Solar Imagery/AIA"
    },
}