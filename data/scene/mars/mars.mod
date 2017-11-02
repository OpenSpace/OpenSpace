local marsEllipsoid = {3396190.0, 3396190.0, 3376200.0}

return {
    -- Barycenter module
    {
        Name = "MarsBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "MARS BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        GuiPath = "/Solar System/Planets/Mars"
    },
    -- RenderableGlobe module
    {   
        Name = "Mars",
        Parent = "MarsBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MARS",
                DestinationFrame = "GALACTIC"
            }
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = marsEllipsoid,
            SegmentsPerPatch = 90,
            Layers = {
                ColorLayers = {
                    {
                        Name = "MOC WA AMNH Color",
                        FilePath = "map_datasets/mars_COL_v006_mars2000_rgb.vrt",
                        Enabled = true
                    },
                    {
                        Name = "Viking MDIM",
                        FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
                        Fallback =  {
                            Name = "Mars Texture",
                            FilePath = "textures/mars.jpg",
                            Enabled = true
                        }
                    },
                    {
                        Name = "MOLA Pseudo Color",
                        FilePath = "map_service_configs/Utah/Mola_PseudoColor.xml"
                    },
                    {
                        Name = "Viking MDIM [Utah]",
                        FilePath = "map_service_configs/Utah/Mdim.xml"
                    },
                    {
                        Name = "Viking MDIM [AWS]",
                        FilePath = "map_service_configs/AWS/Mdim.wms"
                    },
                    {
                        Name = "MOLA Pseudo Color [AWS]",
                        FilePath = "map_service_configs/AWS/Mola_PseudoColor.wms"
                    },
                    {
                        Name = "CTX Mosaic [LiU]",
                        FilePath = "map_service_configs/LiU/CTX.xml",
                        BlendMode = "Color"
                    },
                    {
                        Name = "CTX Mosaic [Utah]",
                        FilePath = "map_service_configs/Utah/CTX.xml",
                        BlendMode = "Color"
                    },
                    {
                        Name = "CTX Mosaic [AWS]",
                        FilePath = "map_service_configs/AWS/CTX.wms",
                        BlendMode = "Color"
                    },
                    {
                        Name = "Themis IR Day [Utah]",
                        FilePath = "map_service_configs/Utah/Themis_IR_Day.xml",
                        BlendMode = "Color"
                    },
                    {
                        Name = "Themis IR Night [Utah]",
                        FilePath = "map_service_configs/Utah/Themis_IR_Night.xml",
                        BlendMode = "Color"
                    },

                    {
                        Name = "Themis IR Day [AWS]",
                        FilePath = "map_service_configs/AWS/Themis_IR_Day.wms",
                        BlendMode = "Color"
                    },
                    {
                        Name = "Themis IR Night [AWS]",
                        FilePath = "map_service_configs/AWS/Themis_IR_Night.wms",
                        BlendMode = "Color"
                    }
                },
                Overlays = {
                    {
                        Type = "TileIndexTileLayer",
                        Name = "Indices"
                    },
                    {
                        Type = "SizeReferenceTileLayer",
                        Name = "Size Reference",
                        Radii = marsEllipsoid
                    }
                },
                HeightLayers = {
                    {
                        Name = "Mola Elevation",
                        FilePath = "map_service_configs/Mars_MGS_MOLA_DEM.xml",
                        TilePixelSize = 90
                    },
                    {
                        Name = "Mola Elevation [Europe]",
                        FilePath = "map_service_configs/LiU/Mola_Elevation.xml",
                        Enabled = true,
                        TilePixelSize = 90
                    },
                    {
                        Name = "Mola Elevation [Utah]",
                        FilePath = "map_service_configs/Utah/Mola_Elevation.xml",
                        TilePixelSize = 90
                    },
                    {
                        Name = "Mola Elevation [AWS]",
                        FilePath = "map_service_configs/AWS/Mola_Elevation.wms",
                        TilePixelSize = 90
                    },
                }
            }
        },
        Tag = { "planet_solarSystem", "planet_terrestrial" },
        GuiPath = "/Solar System/Planets/Mars"
    },

    -- Trail module
    {   
        Name = "MarsTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "MARS BARYCENTER",
                Observer = "SUN"
            },
            Color = { 0.814, 0.305, 0.220 },
            Period = 686.973,
            Resolution = 1000
        },
        Tag = { "planetTrail_solarSystem", "planetTrail_terrestrial" },
        GuiPath = "/Solar System/Planets/Mars"
    }
}
