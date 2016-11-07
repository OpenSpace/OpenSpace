return {
    -- RenderableGlobe module
    {
        Name = "Callisto",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_CALLISTO",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "SpiceTranslation",
                Body = "CALLISTO",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/jup260.bsp"
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {2631000, 2631000, 2631000},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Callisto Texture",
                        FilePath = "textures/callisto.jpg",
                        Enabled = true,
                        MinimumPixelSize = 112,
                    },
                },
                GrayScaleLayers = { },
                GrayScaleColorOverlays = { },
                NightLayers = { },
                WaterMasks = { },
                ColorOverlays = { },
                HeightLayers = { },
            },
        }
    },
    -- Trail module
    {   
        Name = "CallistoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "CALLISTO",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.4, 0.3, 0.01 },
            TropicalOrbitPeriod =  60 ,
            EarthOrbitRatio = 0.045,
            DayLength = 9.9259,
        }
    },
}
