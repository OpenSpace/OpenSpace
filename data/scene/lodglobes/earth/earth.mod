earthEllipsoid = {6378137.0, 6378137.0, 6356752.314245} -- Earth's radii
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
                                    Type = "TemporalTileLayer",
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
                        Enabled = true,
                    },
					{
						FilePath = "map_service_configs/ESRI/ESRI_Imagery_World_2D.wms",
						Name = "ESRI",
					},
                    {
                        Name = "BMNG",
                        FilePath = "map_service_configs/Utah/Bmng.wms"
                    },
                    {
                        Type = "TemporalTileLayer",
                        Name = "Temporal_AMSR2_GCOM_W1_Sea_Ice_Concentration",
                        FilePath = "map_service_configs/GIBS/Temporal_AMSR2_GCOM_W1_Sea_Ice_Concentration.xml",
                    },
                    {
                        Type = "TemporalTileLayer",
                        Name = "MODIS_Terra_Chlorophyll_A",
                        FilePath = openspace.globebrowsing.createTemporalGibsGdalXml(
                            "MODIS_Terra_Chlorophyll_A",
                            "2013-07-02",
                            "Yesterday",
                            "1d",
                            "1km",
                            "png")
                    },
                    {
                        Type = "TemporalTileLayer",
                        Name = "GHRSST_L4_G1SST_Sea_Surface_Temperature",
                        FilePath = openspace.globebrowsing.createTemporalGibsGdalXml(
                            "GHRSST_L4_G1SST_Sea_Surface_Temperature",
                            "2010-06-21",
                            "Yesterday",
                            "1d",
                            "1km",
                            "png")
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
                    },
                    {
                        Type = "TemporalTileLayer",
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
                        Type = "TileIndexTileLayer",
                        Name = "Tile Indices",
                    },
                    {
                        Type = "SizeReferenceTileLayer",
                        Name = "Size Reference",
                        Radii = earthEllipsoid,
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