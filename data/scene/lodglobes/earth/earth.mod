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
            Layers = {
                ColorLayers = {
                    {
                        Name = "ESRI VIIRS Combo",
                        Type = "ByLevel",
                        LevelTileProviders = {
                            {
                                MaxLevel = 7, 
                                TileProvider = { FilePath = "map_service_configs/GIBS/VIIRS_SNPP_CorrectedReflectance_TrueColor.xml", }, 
                            },
                            {
                                MaxLevel = 22, 
                                TileProvider = { FilePath = "map_service_configs/ESRI/ESRI_Imagery_World_2D.wms" },
                            },
                        },
                        Enabled = true,
                    },
                    {
                        Name = "ESRI Imagery World",
                        FilePath = "map_service_configs/ESRI/ESRI_Imagery_World_2D.wms"
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal VIIRS SNPP",
                        FilePath = "map_service_configs/GIBS/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal_GHRSST_L4_MUR_Sea_Surface_Temperature",
                        FilePath = "map_service_configs/GIBS/Temporal_GHRSST_L4_MUR_Sea_Surface_Temperature.xml",
                    },
                    -- {
                    --     Type = "SingleImage",
                    --     Name = "Debug Tiles",
                    --     FilePath = "../../debugglobe/textures/test_tile.png",
                    -- },
                    {
                        Name = "BMNG",
                        FilePath = "map_service_configs/Utah/Bmng.wms"
                    }
                    -- {
                    --     Type = "Temporal",
                    --     Name = "NOAA RT",
                    --     FilePath = "map_service_configs/other/noaa_rt.xml"
                    -- }
                },
                GrayScaleLayers = { },
                GrayScaleColorOverlays = { },
                NightLayers = {
                    {
                        Name = "Earth at Night 2012",
                        FilePath = "map_service_configs/GIBS/VIIRS_CityLights_2012.xml",
                        Enabled = true,
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal Earth at Night",
                        FilePath = "map_service_configs/GIBS/Temporal_VIIRS_CityLights.xml"
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
                    --[[{
                        Name = "Test",
                        Type = "LevelSpecific",
                        LevelTileProviders = {
                            {
                                MaxLevel = 5,
                                TileProvider = { Type = "TileIndex" },
                            },
                            {
                                MaxLevel = 7,
                                TileProvider = { Type = "SingleImage", FilePath = "../../debugglobe/textures/test_tile.png",},
                            },
                        },
                    },]]
                },
                HeightLayers = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/ESRI/TERRAIN.wms",
                        Enabled = true,
                        MinimumPixelSize = 64,
                        DoPreProcessing = true,
                    },
                },
            },
        }
    },
}