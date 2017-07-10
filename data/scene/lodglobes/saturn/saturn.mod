return {
    -- Barycenter module
    {
        Name = "SaturnBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "SATURN BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- RenderableGlobe module
    {   
        Name = "Saturn",
        Parent = "SaturnBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_SATURN",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
            -- No translation, Saturn is in its barycenter
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {60268000, 60268000, 54364000},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Saturn Texture",
                        FilePath = "textures/saturn.jpg",
                        Enabled = true,
                    },
                },
            },
        },
    },
    {
        Name = "SaturnRings",
        Parent = "Saturn",
        Renderable = {
            Type = "RenderableRings",
            Texture = "textures/saturn_rings.png",
            Size = 140220000,
            Offset = { 74500 / 140445.100671159, 1.0 } -- min / max extend
        },
    },
    -- Trail module
    {   
        Name = "SaturnTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "SATURN BARYCENTER",
            Observer = "SUN",
            },
            Color = {0.85,0.75,0.51 },
            Period = 10746.94,
            Resolution = 1000
        },
    }
}
