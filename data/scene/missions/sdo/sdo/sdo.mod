return {
    {
        Name = "SDO trail",
        Parent = "EarthInertial",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Frame = "J2000",
                Body = "-136395",
                Observer = "EARTH",
                Kernels = "${OPENSPACE_DATA}/spice/SDO_EPHEM_2010123_2017104_new.bsp"
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
                Kernels = "${OPENSPACE_DATA}/spice/SDO_EPHEM_2010123_2017104_new.bsp"
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
        Name = "SDO Plane",
        Parent = "SDOCoordinatesystem",
        Renderable = {
            -- Resolution of imagery, will be moved to metadata later
            Resolution = 4096,
            Type = "RenderableSpacecraftCameraPlane",
            Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            RootPath = "/home/noven/workspace/OpenSpace/data/solarflarej2k/",
            -- Optional filter on instruments, otherwise get all
            -- Instruments = {
            --     "aia_aia_94",
            -- },
            -- Path to transferfunctions whose name must match the instrument
            TransferfunctionPath = "/home/noven/workspace/OpenSpace/data/sdotransferfunctions"
        },
    },
}
