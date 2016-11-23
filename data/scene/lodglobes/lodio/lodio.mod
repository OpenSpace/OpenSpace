return {
    -- RenderableGlobe module
    {
        Name = "Io",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_IO",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "SpiceTranslation",
                Body = "IO",
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
            Radii = {1821300, 1821300, 1821300},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Io Texture",
                        FilePath = "textures/io.jpg",
                        Enabled = true,
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
        Name = "IoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "IO",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.4, 0.4, 0.2 },
            TropicalOrbitPeriod =  40 ,
            EarthOrbitRatio = 0.0045,
            DayLength = 9.9259,
        }
    },
}
