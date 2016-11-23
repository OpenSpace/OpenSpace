return {
    -- Moon module
    {
        Name = "Moon",
        Parent = "EarthBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MOON",
                Observer = "EARTH BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MOON",
                DestinationFrame = "GALACTIC"
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {1738140, 1738140, 1735970}, -- Moons's radius
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 5000, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {

                },
                GrayScaleColorOverlays = {

                },
                GrayScaleLayers = {
                    {
                        Name = "OnMoonColorGrayscale",
                        FilePath = "map_service_configs/OnMoonColor.xml",
                        Enabled = true,
                    },
                },
                NightLayers = {

                },
                WaterMasks = {
                    
                },
                ColorOverlays = {
                    
                },
                HeightLayers = {
                    {
                        Name = "OnMoonHeight",
                        FilePath = "map_service_configs/OnMoonHeight.xml",
                        Enabled = true,
                        DoPreProcessing = true,
                    },
                },
            },
        }
    },
    -- MoonTrail module
    {   
        Name = "MoonTrail",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MOON",
                Observer = "EARTH BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period =  27,
            Resolution = 1000
        },
    }
}
