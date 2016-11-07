return {
    -- Barycenter module
    {
        Name = "UranusBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "URANUS BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- RenderableGlobe module
    {   
        Name = "Uranus",
        Parent = "UranusBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_URANUS",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
            -- No translation, Uranus is in its barycenter
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {25362000, 25362000, 25362000},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Texture",
                        FilePath = "textures/uranus.jpg",
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
        Name = "UranusTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "URANUS BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = {0.60,0.95,1.00 },
            TropicalOrbitPeriod = 30588.740 ,
            EarthOrbitRatio = 83.749,
            DayLength = 17.24,
        },
    }
}
