local marsEllipsoid = {3396190.0, 3396190.0, 3376200.0}
return {
    -- Mars barycenter module
    {
        Name = "MarsCenterOfMass",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = "MARS BARYCENTER",
                Reference = "ECLIPJ2000",
                Observer = "SUN",
                Kernels = {
                    "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
                }
            },
        },
    },
    -- RenderableGlobe module
    {   
        Name = "LodMars",
        Parent = "MarsCenterOfMass",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MARS",
                DestinationFrame = "ECLIPJ2000",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = marsEllipsoid, -- Mars' radii
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
                        FilePath = "../debugglobe/textures/test_tile.png",
                    },
                    {
                        Name = "MARS_Viking_MDIM21",
                        FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
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
                    {
                        Type = "SizeReference",
                        Name = "Size Reference",
                        Radii = marsEllipsoid,
                        BackgroundImagePath = "../debugglobe/textures/arrows.png",
                    },
                },
                HeightMaps = {
                    {
                        Name = "Mola Elevation",
                        FilePath = "map_service_configs/Mola_Elevation.xml",
                        Enabled = true,
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
                    },
                    {
                        Name = "West_Candor_Chasma_DEM_longlat_global",
                        FilePath = "map_datasets/West_Candor_Chasma_DEM_longlat_global.vrt",
                        --Enabled = true,
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
                    },
                    {
                        Name = "Layered Rock Outcrops in Southwest Candor Chasma",
                        FilePath = "map_datasets/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_DEM.vrt",
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
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
        Parent = "Sun",
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
