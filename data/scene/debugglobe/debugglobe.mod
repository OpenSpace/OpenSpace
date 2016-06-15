return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "Root",
        Static = true,
        --[[
        Ephemeris = {
            Type = "Kepler",
            Inclination = 0.00041,
            AscendingNode = 349.2,
            Perihelion = 102.8517,
            SemiMajorAxis = 1.00002,
            DailyMotion = 0.9855796,     
            Eccentricity = 0.0166967,
            MeanLongitude = 328.40353
        }
        --]]
        Ephemeris = {
            Type = "Static"
        }
    },
    -- RenderableGlobe module
    {   
        Name = "DebugGlobe",
        Parent = "Root",
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_EARTH",
            Body = "EARTH",
            Radii = {6378137.0, 6378137.0, 6356752.314245}, -- Earth's radii
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 1024,
                OverlayMinimumSize = 2048,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Name = "Temporal VIIRS SNPP",
                        FilePath = "map_service_configs/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml",
                        Enabled = false,
                    },
                    {
                        Name = "Temporal MODIS Aqua CorrectedRecflectance TrueColor",
                        FilePath = "map_service_configs/Temporal_MODIS_Aqua_CorrectedReflectance_TrueColor.xml",
                        Enabled = false,
                    },
                    {
                        Name = "MODIS_Terra_CorrectedReflectance_TrueColor",
                        FilePath = "map_service_configs/MODIS_Terra_CorrectedReflectance_TrueColor.xml",
                        Enabled = false,
                    },
                    {
                        Name = "ESRI Imagery World 2D",
                        FilePath = "map_service_configs/ESRI_Imagery_World_2D.wms",
                        Enabled = true,
                    },
                },
                NightTextures = {
                    {
                        Name = "Earth at Night 2012",
                        FilePath = "map_service_configs/VIIRS_CityLights_2012.xml",
                        Enabled = false,
                    },
                },
                HeightMaps = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/TERRAIN.wms",
                        Enabled = true,
                    },
                },
                WaterMasks = {
                    {
                        Name = "MODIS_Water_Mask",
                        FilePath = "map_service_configs/MODIS_Water_Mask.xml",
                        Enabled = false,
                    },
                },
                Overlays = {
                    {
                        Name = "Coastlines",
                        FilePath = "map_service_configs/Coastlines.xml",
                        Enabled = false,
                    },
                    {
                        Name = "Reference_Features",
                        FilePath = "map_service_configs/Reference_Features.xml",
                        Enabled = false,
                    },
                    {
                        Name = "Reference_Labels",
                        FilePath = "map_service_configs/Reference_Labels.xml",
                        Enabled = false,
                    },
                },
            },
        },
        --[[
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        --]]
        GuiName = "/Solar/Planets/DebugGlobe"
    },
    --[[
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "EarthBarycenter",
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
    },
    --]]
}
