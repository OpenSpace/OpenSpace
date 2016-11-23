return {
    -- RenderableGlobe module
    {
        Name = "Europa",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_EUROPA",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "SpiceTranslation",
                Body = "EUROPA",
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
            Radii = {1561000, 1561000, 1561000},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Europa Texture",
                        FilePath = "textures/europa.jpg",
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
        }
    },
    -- Trail module
    {   
        Name = "EuropaTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "EUROPA",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod =  60,
            EarthOrbitRatio = 0.01,
            DayLength = 9.9259,
        }
    },
}
