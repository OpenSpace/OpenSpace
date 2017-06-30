return {
    -- Barycenter module
    {
        Name = "MercuryBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MERCURY",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- RenderableGlobe module
    {   
        Name = "Mercury",
        Parent = "MercuryBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MERCURY",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {2439700, 2439700.0, 2439700.0},
            Frame = "IAU_MERCURY",
            Body = "MERCURY",
            
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Simple Texture",
                        FilePath = "textures/mercury.jpg",
                        Enabled = true,
                    },
                    {
                        Name = "Messenger_Mosaic",
                        FilePath = "map_service_configs/Utah/MessengerMosaic.wms"
                    },
                    {
                        Name = "Messenger_MDIS",
                        FilePath = "map_service_configs/Utah/MessengerMDIS.wms"
                    }
                },
            },
        },
    },
    -- Trail module
    {   
        Name = "MercuryTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MERCURY",
                Observer = "SUN",
            },
            Color = {0.6, 0.5, 0.5 },
            Period = 87.968,
            Resolution = 100
        }
    }
}
