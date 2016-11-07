return {
    -- RenderableGlobe module
    {
        Name = "Ganymede",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_GANYMEDE",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "SpiceTranslation",
                Body = "GANYMEDE",
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
                        Name = "Ganymede Texture",
                        FilePath = "textures/ganymede.jpg",
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
        Name = "GanymedeTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "GANYMEDE",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.4, 0.3, 0.3 },
            TropicalOrbitPeriod =  60 ,
            EarthOrbitRatio = 0.019,
            DayLength = 9.9259,
        }
    },
}
