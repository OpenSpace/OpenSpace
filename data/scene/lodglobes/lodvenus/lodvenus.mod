return {
    -- Barycenter module
    {
        Name = "VenusBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "VENUS BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- RenderableGlobe module
    {   
        Name = "Venus",
        Parent = "VenusBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_VENUS",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
            Translation = {
                Type = "SpiceTranslation",
                Body = "VENUS",
                Observer = "VENUS BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {6051900, 6051900, 6051800},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Venus Texture",
                        FilePath = "textures/venus.jpg",
                        Enabled = true,
                        MinimumPixelSize = 256,
                    },
                },
                GrayScaleLayers = { },
                GrayScaleColorOverlays = { },
                NightLayers = { },
                WaterMasks = { },
                ColorOverlays = { },
                HeightLayers = { },
            },
        },
    },
    -- Trail module
    {   
        Name = "VenusTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "VENUS",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = {1, 0.5, 0.2},
            TropicalOrbitPeriod = 224.695 ,
            EarthOrbitRatio = 0.615,
            DayLength = 2802.0,
        },
    }
}
