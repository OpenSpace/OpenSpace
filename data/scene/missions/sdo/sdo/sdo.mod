return {
    {
        Name = "SDO trail",
        Parent = "EarthInertial",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Frame = "J2000",
                Body = "-123456789",
                Observer = "EARTH",
                Kernels = "${OPENSPACE_DATA}/spice/SDO_EPHEM_2017064.bsp"
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
                Body = "-123456789",
                Observer = "EARTH",
                Kernels = "${OPENSPACE_DATA}/spice/SDO_EPHEM_2017064.bsp"
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
            Size = {20.619, 10.220},
            Origin = "Center",
            -- Dummy texture
            Texture = "images/stereo2.png",
        },
    }
}
