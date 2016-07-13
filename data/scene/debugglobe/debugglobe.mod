return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Static = true,
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
    },
    -- RenderableGlobe module
    {   
        Name = "DebugGlobe",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_EARTH",
            Body = "EARTH",
            --Radii = {6378137.0, 6378137.0, 6356752.314245}, -- Earth's radii
            --Radii = {1738100, 1738100, 1736000}, -- Moon's radii
            Radii = {3396190.0, 3396190.0, 3376200.0}, -- Mars' radii
            --Radii =   {2439700.0, 2439700.0, 2439700.0},
            CameraMinHeight = 1000,
            InteractionDepthBelowEllipsoid = 10000, -- Useful when having negative height map values
            SegmentsPerPatch = 90,
            TextureInitData = {
                ColorTextureMinimumSize = 512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Type = "Temporal",
                        Name = "Temporal VIIRS SNPP",
                        FilePath = "map_service_configs/earth/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Type = "SingleImage",
                        Name = "Debug Tiles",
                        FilePath = "textures/test_tile.png",
                    },
                    --[[{
                        Type = "Temporal",
                        Name = "Temporal MODIS Aqua CorrectedRecflectance TrueColor",
                        FilePath = "map_service_configs/earth/Temporal_MODIS_Aqua_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Name = "MODIS_Terra_CorrectedReflectance_TrueColor",
                        FilePath = "map_service_configs/earth/MODIS_Terra_CorrectedReflectance_TrueColor.xml",
                    },]]

                    {
                        Name = "ESRI Imagery World 2D",
                        FilePath = "map_service_configs/earth/ESRI_Imagery_World_2D.wms",
                        --Enabled = true,
                    },
                    {
                        Name = "MARS_Viking_MDIM21",
                        FilePath = "map_service_configs/mars/MARS_Viking_MDIM21.xml",
                        Enabled = true,
                    },
                    --[[
                    {
                        Name = "Mars Viking Clr",
                        FilePath = "textures/Mars_Viking_ClrMosaic_global_925m_longlat_full.vrt",
                        Enabled = true,
                    },
                    {
                        Name = "On Mercury Color",
                        FilePath = "map_service_configs/mercury/OnMercuryColor.xml",
                    },
                    {
                        Name = "On Mercury Image",
                        FilePath = "map_service_configs/mercury/OnMercuryImage.xml",
                    },
                    ]]
                },
                GrayScaleOverlays = {
                    {
                        Name = "CTX Mosaic",
                        FilePath = "map_service_configs/mars/CTX_Mosaic.xml",
                        Enabled = false,
                    },
                    --[[
                    {
                        Name = "On Moon Color",
                        FilePath = "map_service_configs/moon/OnMoonColor.xml",
                        --Enabled = true,
                    }
                    {
                        Name = "West_Candor_Chasma_longlat_global",
                        FilePath = "map_datasets/West_Candor_Chasma_longlat_global.vrt",
                        Enabled = true,
                    },
                    ]]
                },
                NightTextures = {
                    {
                        Name = "Earth at Night 2012",
                        FilePath = "map_service_configs/earth/VIIRS_CityLights_2012.xml",
                    },
                },
                WaterMasks = {
                    {
                        Name = "MODIS_Water_Mask",
                        FilePath = "map_service_configs/earth/MODIS_Water_Mask.xml",
                    },
                },
                Overlays = {
                    {
                        Name = "Coastlines",
                        FilePath = "map_service_configs/earth/Coastlines.xml",
                    },
                    {
                        Name = "Reference_Features",
                        FilePath = "map_service_configs/earth/Reference_Features.xml",
                    },
                    {
                        Name = "Reference_Labels",
                        FilePath = "map_service_configs/earth/Reference_Labels.xml",
                    },
                    {
                        Type = "ChunkIndex",
                        Name = "Indices",
                        FilePath = "textures/test_tile.png",
                    },
                },
                HeightMaps = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/earth/TERRAIN.wms",
                        --Enabled = true,
                    },
                    {
                        Name = "Mola Elevation",
                        FilePath = "map_service_configs/mars/Mola_Elevation.xml",
                        Enabled = true,
                    },
                    --[[
                    {
                        Name = "On Moon Height",
                        FilePath = "map_service_configs/moon/OnMoonHeight.xml",
                        --Enabled = true,
                    },]]
                    {
                        Name = "On Mercury Height",
                        FilePath = "map_service_configs/mercury/OnMercuryElevationGaskell.xml",
                    },
                },
                HeightMapOverlays = {
                    --[[
                    {
                        Name = "West_Candor_Chasma_DEM_longlat_global2",
                        FilePath = "map_datasets/West_Candor_Chasma_DEM_longlat_global.vrt",
                        Enabled = true,
                    },
                    ]]
                },
            },
        },
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH",
            Reference = "ECLIPJ2000",
            Observer = "EARTH BARYCENTER",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        GuiName = "/Solar/Planets/DebugGlobe"
    },
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "EARTH",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.5, 0.8, 1.0},
            TropicalOrbitPeriod = 365.242,
            EarthOrbitRatio = 1,
            DayLength = 24
        },
        GuiName = "/Solar/EarthTrail"
    }
    --[[,
    {
        Name = "DebugPlane",
        Parent = "DebugGlobe",
        Renderable = {
            Type = "RenderableDebugPlane",
            Texture = 34,
            Size = { 1, 7 }
        }
    }]]
}
