return {
    -- Barycenter module
    {
        Name = "MercuryBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "MERCURY",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        }
    },
    -- RenderableGlobe module
    {   
        Name = "Mercury",
        Parent = "MercuryBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MERCURY",
                DestinationFrame = "GALACTIC"
            }
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 2439700,
            Frame = "IAU_MERCURY",
            Body = "MERCURY",
            CameraMinHeight = 300,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Messenger_MDIS",
                        FilePath = "map_service_configs/Utah/MessengerMDIS.wms",
                        Enabled = true
                    },
                    {
                        Name = "Messenger_Mosaic",
                        FilePath = "map_service_configs/Utah/MessengerMosaic.wms"
                    }
                }
            }
        },
        Tag = { "planet_solarSystem", "planet_terrestrial" },
    },
    -- Trail module
    {   
        Name = "MercuryTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "MERCURY",
                Observer = "SUN",
            },
            Color = {0.6, 0.5, 0.5 },
            Period = 87.968,
            Resolution = 100
        },
        Tag = { "planetTrail_solarSystem", "planetTrail_terrestrial" }
    }
}
