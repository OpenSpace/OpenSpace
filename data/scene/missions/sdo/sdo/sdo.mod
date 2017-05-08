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
            Type = "RenderableSpacecraftCameraPlane",
            Target = "Sun",
            -- Will recursively find all instruments that match array instruments
            RootPath = "/Volumes/Untitled/solarflare/aia/2015/",
            Instruments = {
                "94",
                "131",
                "171",
            },
        },
    },
}
