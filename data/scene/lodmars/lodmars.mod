return {
    -- Mars barycenter module
    {
        Name = "MarsBarycenter",
        Parent = "SolarSystemBarycenter",
        Ephemeris = {
            Type = "Static"
        }
    },
    -- RenderableGlobe module
    {   
        Name = "LodMars",
        Parent = "MarsBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "MARS BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_MARS",
            Reference = "ECLIPJ2000"
        },
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_MARS",
            Body = "MARS BARYCENTER",
            Radii = {3396190.0, 3396190.0, 3376200.0}, -- Mars' radii
            CameraMinHeight = 1000,
            InteractionDepthBelowEllipsoid = 10000, -- Useful when having negative height map values
            SegmentsPerPatch = 90,
            TextureInitData = {
                ColorTextureMinimumSize = 512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 90,
            },
            Textures = {
                ColorTextures = {
                    {
                        Type = "SingleImage",
                        Name = "Debug Tiles",
                        FilePath = "textures/test_tile.png",
                    },
                    {
                        Name = "MARS_Viking_MDIM21",
                        FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
                        Enabled = true,
                    },
                    {
                        Name = "Mars Viking Clr",
                        FilePath = "map_datasets/Mars_Viking_ClrMosaic_global_925m_longlat_full.vrt",
                        Enabled = true,
                    },
                },
                GrayScaleOverlays = {
                    {
                        Name = "CTX Mosaic",
                        FilePath = "map_service_configs/CTX_Mosaic.xml",
                        Enabled = true,
                    },
                    {
                        Name = "West_Candor_Chasma_longlat_global",
                        FilePath = "map_datasets/West_Candor_Chasma_longlat_global.vrt",
                        --Enabled = true,
                    },
                    {
                        Name = "Layered Rock Outcrops in Southwest Candor Chasma",
                        FilePath = "map_datasets/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_A.vrt",
                    },
                },
                NightTextures = {

                },
                WaterMasks = {

                },
                Overlays = {
                    {
                        Type = "ChunkIndex",
                        Name = "Indices",
                    },
                },
                HeightMaps = {
                    {
                        Name = "Mola Elevation",
                        FilePath = "map_service_configs/Mola_Elevation.xml",
                        Enabled = true,
                    },
                    {
                        Name = "West_Candor_Chasma_DEM_longlat_global",
                        FilePath = "map_datasets/West_Candor_Chasma_DEM_longlat_global.vrt",
                        --Enabled = true,
                    },
                    {
                        Name = "Layered Rock Outcrops in Southwest Candor Chasma",
                        FilePath = "map_datasets/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_DEM.vrt",
                    },
                },
                HeightMapOverlays = {
                },
            },
        },

        GuiName = "/Solar/Planets/LodMars"
    },
    -- MarsTrail module
    {   
        Name = "MarsTrail",
        Parent = "MarsBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "MARS BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 1, 0.8, 0.5 },
            TropicalOrbitPeriod = 686.973,
            EarthOrbitRatio = 1.881,
            DayLength = 24.6597,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/MarsTrail"
    }
}
