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
                Target = "EARTH",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        GuiPath = "/Solar System/Planets/Earth"
    },
    {
    -- The default reference frame for Earth-orbiting satellites
        Name = "EarthInertial",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "J2000",
                DestinationFrame = "GALACTIC"
            }
        },
        GuiPath = "/Solar System/Planets/Earth"
    },
    -- Earth Atmosphere
    {
         Name = "EarthAtmosphere",
         Parent = "Earth",
         Renderable = {
              Type = "RenderableAtmosphere",
              Atmosphere = {
                Type = "RenderableGlobe",
                -- Atmosphere radius in Km
                AtmosphereRadius = 6447.0,
                PlanetRadius    = 6377.0,
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
              Shadow_Group = {
                  Source1 = {
                      Name = "Sun",
                      -- All radius in meters
                      Radius = 696.3E6,
                      },
                  --Source2 = { Name = "Monolith", Radius = 0.01E6, },
                  Caster1 = { 
                      Name = "Moon",
                      -- All radius in meters
                      Radius = 1.737E6,
                      },
                  --Caster2 = { Name = "Independency Day Ship", Radius = 0.0, }
               },
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
                Target = "EARTH",
                Observer = "SUN"
            },
            Color = { 0.5, 0.8, 1.0 },
            -- StartTime = "2016 JUN 01 12:00:00.000",
            -- EndTime = "2017 JAN 01 12:00:00.000",
            -- SampleInterval = 3600
            Period = 365.242,
            Resolution = 1000
        },
        Tag = { "planetTrail_solarSystem", "planetTrail_terrestrial" },
        GuiPath = "/Solar System/Planets/Earth"
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
            }
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = earthEllipsoid,
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "ESRI VIIRS Combo",
                        Type = "ByLevelTileLayer",
                        LevelTileProviders = {
                            {
                                MaxLevel = 3, 
                                TileProvider = {
                                    Name = "Temporal VIIRS SNPP",
                                    Type = "TemporalTileLayer",
                                    FilePath = "map_service_configs/GIBS/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml",
                                    PadTiles = false,
                                }
                            },
                            {
                                MaxLevel = 22,
                                TileProvider = {
                                    Name = "ESRI Imagery World 2D",
                                    FilePath = "map_service_configs/ESRI/ESRI_Imagery_World_2D.wms",
                                    PadTiles = false,
                                }
                            },
                        },
                        Enabled = true,
                        PadTiles = false,
                        Fallback = {
                            Name = "Blue Marble",
                            FilePath = "textures/earth_bluemarble.jpg",
                            Enabled = true,
                        }
                    },
                    {
                        Name = "ESRI Imagery World 2D",
                        FilePath = "map_service_configs/ESRI/ESRI_Imagery_World_2D.wms",
                    },
                    {
                        Name = "Temporal VIIRS SNPP",
                        Type = "TemporalTileLayer",
                        FilePath = "map_service_configs/GIBS/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Name = "BMNG",
                        FilePath = "map_service_configs/Utah/Bmng.wms"
                    },
                    {
                        Name = "Temporal_AMSR2_GCOM_W1_Sea_Ice_Concentration",
                        Type = "TemporalTileLayer",
                        FilePath = "map_service_configs/GIBS/Temporal_AMSR2_GCOM_W1_Sea_Ice_Concentration.xml",
                    },
                    {
                        Name = "MODIS_Terra_Chlorophyll_A",
                        Type = "TemporalTileLayer",
                        FilePath = openspace.globebrowsing.createTemporalGibsGdalXml(
                            "MODIS_Terra_Chlorophyll_A",
                            "2013-07-02",
                            "Yesterday",
                            "1d",
                            "1km",
                            "png"
                        )
                    },
                    {
                        Name = "GHRSST_L4_G1SST_Sea_Surface_Temperature",
                        Type = "TemporalTileLayer",
                        FilePath = openspace.globebrowsing.createTemporalGibsGdalXml(
                            "GHRSST_L4_G1SST_Sea_Surface_Temperature",
                            "2010-06-21",
                            "Yesterday",
                            "1d",
                            "1km",
                            "png"
                        )
                    },
                },
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
                        Fallback = {
                            Name = "Earth Night",
                            FilePath = "textures/earth_night.jpg",
                            Enabled = true,
                            Settings = {
                                Opacity = 1.0,
                                Gamma = 1.5,
                                Multiplier = 15.0,
                            },
                        }
                    },
                    {
                        Name = "Temporal Earth at Night",
                        Type = "TemporalTileLayer",
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
                Overlays = {
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
                        Name = "Tile Indices",
                        Type = "TileIndexTileLayer",
                    },
                    {
                        Name = "Size Reference",
                        Type = "SizeReferenceTileLayer",
                        Radii = earthEllipsoid,
                    },
                },
                HeightLayers = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/ESRI/TERRAIN.wms",
                        Enabled = true,
                        TilePixelSize = 64,
                        Fallback = {
                            Name = "Earth Bluemarble Height",
                            FilePath = "textures/earth_bluemarble_height.jpg",
                            Enabled = true,
                        }
                    }
                }
            }
        },
        Tag = { "planet_solarSystem", "planet_terrestrial" },
        GuiPath = "/Solar System/Planets/Earth"
    }
}
