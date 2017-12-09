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
        },
        GuiPath = "/Solar System/Planets/Mercury"
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
                    -- University of Utah based servers
                    {
                        Name = "Messenger MDIS [Utah]",
                        FilePath = "map_service_configs/Utah/MessengerMDIS.wms",
                        Enabled = true
                    },
                    {
                        Name = "Messenger Mosaic [Utah]",
                        FilePath = "map_service_configs/Utah/MessengerMosaic.wms"
                    },
                    -- AWS based servers
                    {
                        Name = "Messenger MDIS [AWS]",
                        FilePath = "map_service_configs/AWS/MessengerMdis.wms",
                    },
                    {
                        Name = "Messenger Mosaic [AWS]",
                        FilePath = "map_service_configs/AWS/MessengerMosaic.wms",
                    }
                }
            }
        },
        Tag = { "planet_solarSystem", "planet_terrestrial" },
        GuiPath = "/Solar System/Planets/Mercury"
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
        Tag = { "planetTrail_solarSystem", "planetTrail_terrestrial" },
        GuiPath = "/Solar System/Planets/Mercury"
    }
}
