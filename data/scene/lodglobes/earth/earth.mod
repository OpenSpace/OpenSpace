--earthEllipsoid = {6378137.0, 6378137.0, 6356752.314245} -- Earth's radii
earthEllipsoid = {6378137.0, 6378137.0, 6378137.0} -- Earth's radii
return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "EARTH",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    {
    -- The default reference frame for Earth-orbiting satellites
        Name = "EarthInertial",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "J2000",
                DestinationFrame = "GALACTIC",
            }
        },
    },    
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "EARTH",
                Observer = "SUN"
            },
            Color = { 0.5, 0.8, 1.0 },
            -- StartTime = "2016 JUN 01 12:00:00.000",
            -- EndTime = "2017 JAN 01 12:00:00.000",
            -- SampleInterval = 3600
            Period = 365.242,
            Resolution = 1000
        },
        GuiName = "/Solar/EarthTrail"
    },
    -- RenderableGlobe module
    {
        Name = "Earth",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_EARTH",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = earthEllipsoid,
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Atmosphere = {
                -- Atmosphere radius in Km
                --AtmosphereRadius = 6450,
                --AtmosphereRadius = 6420.0,
                AtmosphereRadius = 6447.0,
                --PlanetRadius    = 6378.137,
                PlanetRadius    = 6377.0,
                --PlanetRadius    = 6360.0,
                PlanetAverageGroundReflectance = 0.1,
                Rayleigh = {
                    Coefficients = {
                        -- Wavelengths are given in 10^-9m
                        Wavelengths = {680, 550, 440},
                        -- Reflection coefficients are given in km^-1
                        Scattering = {5.8E-3, 13.5E-3, 33.1E-3},
                        -- In Rayleigh scattering, the coefficients of absorption and scattering are the same.
                    },
                    -- Thichkness of atmosphere if its density were uniform, in Km
                    H_R = 8.0,
                },
                --[[
                Ozone = {
                     Coefficients = {
                        -- Extinction coefficients 
                        Extinction = {3.426, 8.298, 0.356}
                     },
                     H_O = 8.0,
                },
                ]]
                -- Default
                Mie = {
                    Coefficients = {
                        -- Reflection coefficients are given in km^-1
                        Scattering = {4.0e-3, 4.0e-3, 4.0e-3},
                        --Scattering = {2.0e-5, 2.0e-5, 2.0e-5},
                        -- Extinction coefficients are a fraction of the Mie coefficients
                        Extinction = {4.0e-3/0.9, 4.0e-3/0.9, 4.0e-3/0.9}
                    },
                    -- Height scale (atmosphere thickness for constant density) in Km
                    H_M = 1.2,
                    -- Mie Phase Function Value (G e [-1.0, 1.0]. If G = 1.0, Mie phase function = Rayleigh Phase Function)
                    G = 0.85
                },
                -- Clear Sky
                -- Mie = {
                --     Coefficients = {
                --         Scattering = {20e-3, 20e-3, 20e-3},
                --         Extinction = 1.0/0.9,
                --      }
                --     H_M = 1.2,
                --     G = 0.76,
                -- },
                -- Cloudy
                -- Mie = {
                --     Coefficients = {
                --         Scattering = {3e-3, 3e-3, 3e-3},
                --         Extinction = 1.0/0.9,
                --      }
                --     H_M = 3.0,
                --     G = 0.65,
                -- },
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
            Layers = {
                ColorLayers = {
                    {
                        Name = "ESRI VIIRS Combo",
                        Type = "ByLevel",
                        LevelTileProviders = {
                            {
                                MaxLevel = 3, 
                                TileProvider = {
                                    Type = "Temporal",
                                    Name = "Temporal VIIRS SNPP",
                                    FilePath = "map_service_configs/GIBS/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml", }, 
                            },
                            {
                                MaxLevel = 22,
                                TileProvider = {
                                    Name = "ESRI Imagery World 2D",
                                    FilePath = "map_service_configs/ESRI/ESRI_Imagery_World_2D.wms"
                                },
                            },
                        },
                        Settings = {
                            Gamma = 1.5
                        },
                        Enabled = true,
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal_GHRSST_L4_MUR_Sea_Surface_Temperature",
                        FilePath = "map_service_configs/GIBS/Temporal_GHRSST_L4_MUR_Sea_Surface_Temperature.xml",
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal_AMSR2_GCOM_W1_Sea_Ice_Concentration",
                        FilePath = "map_service_configs/GIBS/Temporal_AMSR2_GCOM_W1_Sea_Ice_Concentration.xml",
                    },
                    {
                        Name = "BMNG",
                        FilePath = "map_service_configs/Utah/Bmng.wms"
                    }
                },
                GrayScaleLayers = { },
                GrayScaleColorOverlays = { },
                NightLayers = {
                    {
                        Name = "Earth at Night 2012",
                        FilePath = "map_service_configs/GIBS/VIIRS_CityLights_2012.xml",
                        Enabled = true,
                        Settings = {
                            Opacity = 1.0,
                            Gamma = 1.5,
                            Multiplier = 15.0,
                        },
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal Earth at Night",
                        FilePath = "map_service_configs/GIBS/Temporal_VIIRS_SNPP_DayNightBand_ENCC.xml"
                    }
                },
                WaterMasks = {
                    {
                        Name = "MODIS_Water_Mask",
                        FilePath = "map_service_configs/GIBS/MODIS_Water_Mask.xml",
                        Enabled = true,
                    },
                    {
                        Name = "GEBCO",
                        FilePath = "map_service_configs/Utah/Gebco.wms",
                    }
                },
                ColorOverlays = {
                    {
                        Name = "Coastlines",
                        FilePath = "map_service_configs/GIBS/Coastlines.xml",
                    },
                    {
                        Name = "Reference_Features",
                        FilePath = "map_service_configs/GIBS/Reference_Features.xml",
                    },
                    {
                        Name = "Reference_Labels",
                        FilePath = "map_service_configs/GIBS/Reference_Labels.xml",
                    },
                    {
                        Type = "TileIndex",
                        Name = "Tile Indices",
                    },
                    {
                        Type = "SizeReference",
                        Name = "Size Reference",
                        Radii = earthEllipsoid,
                        BackgroundImagePath = "../arrows.png",
                    },
                },
                HeightLayers = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/ESRI/TERRAIN.wms",
                        Enabled = true,
                        TilePixelSize = 64,
                    },
                },
            },
        }
    },
}
