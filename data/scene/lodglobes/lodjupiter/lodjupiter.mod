return {
    -- Barycenter module
    {
        Name = "JupiterBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "JUPITER BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- RenderableGlobe module
    {
        Name = "Jupiter",
        Parent = "JupiterBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_JUPITER",
                DestinationFrame = "GALACTIC",
            },
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0, 0}, -- jupiter is at its barycenter
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {71492000, 71492000, 71492000},
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Jupiter Texture",
                        FilePath = "textures/jupiter.jpg",
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
        Name = "JupiterTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "JUPITER BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.8, 0.7, 0.7 },
            TropicalOrbitPeriod = 4330.595 ,
            EarthOrbitRatio = 11.857,
            DayLength = 9.9259,  
        }
    },
}