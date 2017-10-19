--local marsEllipsoid = {3396190.0, 3396190.0, 3376200.0}
local marsEllipsoid = {3396190.0, 3396190.0, 3396190.0}

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
                        FilePath = "map_service_configs/Utah/Themis_IR_Day.xml"
                    },
                    {
                        Name = "Themis IR Night [Utah]",
                        FilePath = "map_service_configs/Utah/Themis_IR_Night.xml"
                    },

                    {
                        Name = "Themis IR Day [AWS]",
                        FilePath = "map_service_configs/AWS/Themis_IR_Day.wms"
                    },
                    {
                        Name = "Themis IR Night [AWS]",
                        FilePath = "map_service_configs/AWS/Themis_IR_Night.wms"
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

    -- Mars Atmosphere
    {
        Name = "MarsAtmosphere",
        Parent = "Mars",        
        Renderable = {
            Type = "RenderableAtmosphere",
            Atmosphere = {
                -- Atmosphere radius in Km
                AtmosphereRadius = 3463.17495,
                --PlanetRadius    = 3396.19,
                --PlanetRadius = 3393.0,
                PlanetRadius = 3386.190,
                PlanetAverageGroundReflectance = 0.1,
                Rayleigh = {
                    Coefficients = {
                        -- Wavelengths are given in 10^-9m
                        Wavelengths = {680, 550, 440},
                        -- Reflection coefficients are given in km^-1
                        Scattering = {19.918E-3, 13.57E-3, 5.75E-3},
                        -- In Rayleigh scattering, the coefficients of 
                        -- absorption and scattering are the same.
                    },
                -- Thichkness of atmosphere if its density were uniform, in Km
                H_R = 10.43979,
                },
                -- Default
                Mie = {
                    Coefficients = {
                        -- Reflection coefficients are given in km^-1
                        Scattering = {53.61771e-3, 53.61771e-3, 53.61771e-3},
                        -- Extinction coefficients are a fraction of the Scattering coefficients
                        Extinction = {53.61771e-3/0.98979, 53.61771e-3/0.98979, 53.61771e-3/0.98979}                        
                    },
                    -- Mie Height scale (atmosphere thickness for constant density) in Km
                    H_M = 3.09526,
                    -- Mie Phase Function Value (G e [-1.0, 1.0]. 
                    -- If G = 1.0, Mie phase function = Rayleigh Phase Function)
                    G = 0.85,
                },
                Image = {
                    ToneMapping = jToneMapping,
                    Exposure = 0.4,
                    Background = 1.8,
                    Gamma = 1.85,                                                                      
                },
                Debug = {
                    -- PreCalculatedTextureScale is a float from 1.0 to N, with N > 0.0 and N in Naturals (i.e., 1, 2, 3, 4, 5....)
                    PreCalculatedTextureScale = 1.0,
                    SaveCalculatedTextures = false, 
                },   
            },
        },
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
