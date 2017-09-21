return {
    -- Moon module
    {
        Name = "Moon",
        Parent = "EarthBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "MOON",
                Observer = "EARTH BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MOON",
                DestinationFrame = "GALACTIC"
            }
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = 1738140,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    -- LMMP based servers
                    {
                        Name = "LRO WAC Mosaic [OnMoon]",
                        FilePath = "map_service_configs/OnMoonColor.xml",
                        Enabled = true,
                    },
                    -- Utah based servers
                    {
                        Name = "ClemUvvis",
                        FilePath = "map_service_configs/Utah/ClemUvvis.wms"
                    },
                    {
                        Name = "Kaguya",
                        FilePath = "map_service_configs/Utah/Kaguya.wms"
                    },
                    {
                        Name = "WAC",
                        FilePath = "map_service_configs/Utah/Wac.wms"
                    }
                },
                HeightLayers = {
                    -- LMMP based servers
                    -- {
                    --     Name = "Lunar Elevation [OnMoon]",
                    --     FilePath = "map_service_configs/OnMoonHeight.xml",
                    --     Enabled = true,
                    --     TilePixelSize = 64,
                    --     -- Settings = { Multiplier = 0.5 },
                    -- },
                    -- Utah based servers
                    {
                        Name = "LolaDem",
                        FilePath = "map_service_configs/Utah/LolaDem.wms",
                        Enabled = true,
                        TilePixelSize = 64,
                        Settings = { Multiplier = 0.5 },
                    }
                }
            }
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
                Target = "MOON",
                Observer = "EARTH BARYCENTER"
            },
            Color = { 0.5, 0.3, 0.3 },
            Period =  27,
            Resolution = 1000
        }
    }
}
